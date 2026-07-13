//! Differential test runner (docs/type-system-scope.md §11.4).
//!
//! Per iteration: generate a random jq program, infer its type, generate
//! inputs from the inferred input shape (plus near-miss mutants), run the
//! real `jq` binary on every input, and check:
//!
//!   - HARD (soundness): when jq succeeds on a satisfying input, every
//!     output value must inhabit the inferred output type.
//!   - METRIC (exactness): non-satisfying inputs should make jq fail;
//!     successes are recorded as the leniency gap, not failures.
//!   - DIFFERENTIAL: tjq_exec must agree with jq on satisfying inputs.
//!   - CRASH HUNT: jq exiting on a signal or hanging is always a finding.
//!
//! Every finding is one JSONL line with the seed that reproduces it.

use std::collections::HashMap;
use std::io::Write as IoWrite;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use tjq_exec::{alloc_guard_tripped, builtin_filters, reset_alloc_guard, Filter, Json};
use tjq_semantics::experimental_type_inference::{
    cannot_fail, compute_shape, solve, solve_arrows, Constraint, Context, TypeOptions,
};
use tjq_semantics::Shape;
use tjq_testing::filtergen::{gen_filter, to_jq_source};
use tjq_testing::inhabit::{inhabit, inhabit_profiled, mutate};
use tjq_testing::jsongen::SizeProfile;
use tjq_testing::rng::Rng;
use tjq_testing::shrink::shrink_case;
use tjq_testing::{denotes, json_equal, parse_json, to_json_string};

const JQ_TIMEOUT: Duration = Duration::from_secs(2);

#[derive(Debug)]
enum JqOutcome {
    Ok(Vec<Json>),
    RuntimeError,
    CompileError,
    Crash(String),
    Timeout,
}

/// Raw outcome of running the jq process, before per-mode classification.
enum JqRun {
    Exited { code: i32, stdout: String },
    Signal(i32),
    Timeout,
    SpawnError(String),
}

/// Spawn jq on `program` with `stdin_data` piped in, pumping stdin/stdout on
/// threads (a full pipe buffer would otherwise deadlock and masquerade as a
/// timeout) and killing it past the timeout. `null_input` adds `-n` so the
/// program reads the piped stream via `inputs` (used for batched runs).
fn spawn_jq_impl(jq: &str, program: &str, stdin_data: String, null_input: bool) -> JqRun {
    let flags = if null_input { "-cn" } else { "-c" };
    let mut child = match Command::new(jq)
        .arg(flags)
        .arg("--")
        .arg(program)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        // stderr is classified by exit code only; never let it fill a pipe
        .stderr(Stdio::null())
        .spawn()
    {
        Ok(c) => c,
        Err(e) => return JqRun::SpawnError(format!("spawn failed: {e}")),
    };

    let stdin_thread = child.stdin.take().map(|mut stdin| {
        std::thread::spawn(move || {
            // jq may exit early on compile errors; ignore EPIPE
            let _ = stdin.write_all(stdin_data.as_bytes());
        })
    });
    let mut stdout_thread = child.stdout.take().map(|mut out| {
        std::thread::spawn(move || {
            use std::io::Read;
            let mut s = String::new();
            let _ = out.read_to_string(&mut s);
            s
        })
    });

    let start = Instant::now();
    loop {
        match child.try_wait() {
            Ok(Some(status)) => {
                let stdout = stdout_thread
                    .take()
                    .and_then(|t| t.join().ok())
                    .unwrap_or_default();
                if let Some(t) = stdin_thread {
                    let _ = t.join();
                }
                #[cfg(unix)]
                {
                    use std::os::unix::process::ExitStatusExt;
                    if let Some(sig) = status.signal() {
                        return JqRun::Signal(sig);
                    }
                }
                return match status.code() {
                    Some(code) => JqRun::Exited { code, stdout },
                    None => JqRun::SpawnError("no exit code".to_string()),
                };
            }
            Ok(None) => {
                if start.elapsed() > JQ_TIMEOUT {
                    let _ = child.kill();
                    let _ = child.wait();
                    if let Some(t) = stdout_thread.take() {
                        let _ = t.join();
                    }
                    if let Some(t) = stdin_thread {
                        let _ = t.join();
                    }
                    return JqRun::Timeout;
                }
                std::thread::sleep(Duration::from_millis(2));
            }
            Err(e) => return JqRun::SpawnError(format!("wait failed: {e}")),
        }
    }
}

fn spawn_jq(jq: &str, program: &str, stdin_data: String) -> JqRun {
    spawn_jq_impl(jq, program, stdin_data, false)
}

/// Single-input run (used by the shrinker): a runtime error surfaces as a
/// non-zero exit, which we report as `RuntimeError`.
fn run_jq(jq: &str, program: &str, input: &str) -> JqOutcome {
    match spawn_jq(jq, program, format!("{input}\n")) {
        JqRun::Exited { code: 0, stdout } => {
            let values: Option<Vec<Json>> = stdout
                .lines()
                .filter(|l| !l.trim().is_empty())
                .map(parse_json)
                .collect();
            match values {
                Some(vs) => JqOutcome::Ok(vs),
                None => JqOutcome::Crash("unparseable jq stdout".to_string()),
            }
        }
        JqRun::Exited { code: 3, .. } => JqOutcome::CompileError,
        JqRun::Exited { .. } => JqOutcome::RuntimeError,
        JqRun::Signal(s) => JqOutcome::Crash(format!("signal {s}")),
        JqRun::Timeout => JqOutcome::Timeout,
        JqRun::SpawnError(e) => JqOutcome::Crash(e),
    }
}

/// One input's outcome within a batched run.
enum InputResult {
    Ok(Vec<Json>),
    Errored,
    Crash(String),
    Timeout,
    CompileError,
}

/// Largest number of inputs fed to a single jq process. Keeps the blast
/// radius of a hung input bounded — a timeout loses at most this many
/// inputs, and the caller re-runs those per-input to isolate it.
const BATCH_SIZE: usize = 64;

/// Outcome of one batched jq run.
enum BatchOutcome {
    /// Per-input results, aligned with the inputs.
    Results(Vec<InputResult>),
    /// The whole program failed to compile — every input is a compile error.
    CompileError,
    /// Timeout, crash, or misaligned output — the caller bisects to isolate
    /// the offending input(s).
    Anomaly,
}

/// Evaluate `program` on many inputs with few jq spawns. Each input's
/// outputs (or its runtime error) are recovered individually via a wrapper
/// that collects `[program]` per input and catches errors in-jq:
/// `inputs | try {r:[program]} catch {e:true}` yields one self-delimiting
/// line per input.
fn eval_inputs(jq: &str, program: &str, inputs: &[Json]) -> Vec<InputResult> {
    let mut out = Vec::with_capacity(inputs.len());
    for chunk in inputs.chunks(BATCH_SIZE) {
        eval_chunk(jq, program, chunk, &mut out);
    }
    out
}

/// Evaluate a chunk, bisecting on an anomaly so a single hung input costs
/// O(log n) batch timeouts to isolate rather than O(n) per-input ones.
fn eval_chunk(jq: &str, program: &str, inputs: &[Json], out: &mut Vec<InputResult>) {
    if inputs.is_empty() {
        return;
    }
    match run_jq_batch(jq, program, inputs) {
        BatchOutcome::Results(results) => out.extend(results),
        BatchOutcome::CompileError => {
            out.extend((0..inputs.len()).map(|_| InputResult::CompileError));
        }
        BatchOutcome::Anomaly if inputs.len() == 1 => {
            // Isolated: classify the single culprit directly.
            out.push(match run_jq(jq, program, &to_json_string(&inputs[0])) {
                JqOutcome::Ok(o) => InputResult::Ok(o),
                JqOutcome::RuntimeError => InputResult::Errored,
                JqOutcome::CompileError => InputResult::CompileError,
                JqOutcome::Crash(m) => InputResult::Crash(m),
                JqOutcome::Timeout => InputResult::Timeout,
            });
        }
        BatchOutcome::Anomaly => {
            let mid = inputs.len() / 2;
            eval_chunk(jq, program, &inputs[..mid], out);
            eval_chunk(jq, program, &inputs[mid..], out);
        }
    }
}

/// Run one batch through the collecting wrapper.
fn run_jq_batch(jq: &str, program: &str, inputs: &[Json]) -> BatchOutcome {
    let wrapped = format!("inputs | try {{r:[{program}]}} catch {{e:true}}");
    let mut stdin_data = String::new();
    for inp in inputs {
        stdin_data.push_str(&to_json_string(inp));
        stdin_data.push('\n');
    }
    // `-n`: null primary input; the wrapper reads the piped stream via
    // `inputs`.
    match spawn_jq_impl(jq, &wrapped, stdin_data, true) {
        JqRun::Exited { code: 0, stdout } => {
            let lines: Vec<&str> = stdout.lines().filter(|l| !l.trim().is_empty()).collect();
            if lines.len() != inputs.len() {
                return BatchOutcome::Anomaly; // partial / misaligned output
            }
            let mut results = Vec::with_capacity(inputs.len());
            for line in lines {
                match parse_json(line) {
                    Some(Json::Object(fields)) => {
                        if let Some((_, Json::Array(outputs))) =
                            fields.iter().find(|(k, _)| k == "r")
                        {
                            results.push(InputResult::Ok(outputs.clone()));
                        } else if fields.iter().any(|(k, _)| k == "e") {
                            results.push(InputResult::Errored);
                        } else {
                            return BatchOutcome::Anomaly;
                        }
                    }
                    _ => return BatchOutcome::Anomaly,
                }
            }
            BatchOutcome::Results(results)
        }
        JqRun::Exited { code: 3, .. } => BatchOutcome::CompileError,
        // Non-zero, non-compile exit shouldn't happen (errors are caught in
        // jq), but treat it as an anomaly to bisect just in case.
        _ => BatchOutcome::Anomaly,
    }
}

enum Inference {
    Typed {
        tin: Shape,
        tout: Shape,
        /// Intersection-of-arrows view: `(D1 -> C1) & (D2 -> C2) & …`,
        /// preserving the input/output correlation the union `tout` loses.
        arrows: Vec<(Shape, Shape)>,
    },
    TypeErrors(Vec<String>),
    Panic(String),
}

/// Flatten an intersection-of-arrows shape into its (domain, codomain)
/// pairs. A non-arrow shape yields no branches.
fn arrow_branches(shape: &Shape) -> Vec<(Shape, Shape)> {
    match shape {
        Shape::Intersection(a, b) => {
            let mut v = arrow_branches(a);
            v.extend(arrow_branches(b));
            v
        }
        Shape::Arrow(d, c) => vec![((**d).clone(), (**c).clone())],
        _ => vec![],
    }
}

fn infer(filter: &Filter, builtins: &HashMap<String, Filter>) -> Inference {
    let result = catch_unwind(AssertUnwindSafe(|| {
        let mut ctx = Context::new();
        // The oracle checks against jq's actual (lenient) semantics
        ctx.options = TypeOptions {
            lenient_absence: true,
        };
        let i = ctx.fresh();
        let o = ctx.fresh();
        let constraints = compute_shape(filter, &mut ctx, i, o, builtins);
        // Only overloaded programs (those producing a top-level disjunction)
        // have a non-trivial arrow view; for everything else the arrow type
        // is a single arrow `tin -> tout`, which the union check already
        // covers. Skipping the second solve keeps the campaign fast.
        //
        // Bindings are excluded: a variable carries a value across the
        // input->output correlation, so `solve_arrows` mis-correlates a
        // binding's comma stream (e.g. `[100 as $y | ($y, .k)]` produces a
        // *mixed* array that escapes the homogeneous per-branch codomains it
        // infers). The union view (`tout`) stays sound for these.
        let has_overload = constraints.iter().any(|c| matches!(c, Constraint::Or(_)));
        let arrows = if has_overload && !program_has_binding(filter) {
            solve_arrows(constraints.clone(), &ctx, i, o)
                .map(|a| arrow_branches(&a.canonicalize()))
                .unwrap_or_default()
        } else {
            vec![]
        };
        match solve(constraints, &ctx) {
            Ok(res) => {
                if res.errors.is_empty() {
                    Inference::Typed {
                        tin: res.get(i).canonicalize(),
                        tout: res.get(o).canonicalize(),
                        arrows,
                    }
                } else {
                    Inference::TypeErrors(res.errors.iter().map(|e| e.message.clone()).collect())
                }
            }
            Err(e) => Inference::TypeErrors(vec![e.message]),
        }
    }));
    match result {
        Ok(inf) => inf,
        Err(payload) => {
            let msg = payload
                .downcast_ref::<&str>()
                .map(|s| s.to_string())
                .or_else(|| payload.downcast_ref::<String>().cloned())
                .unwrap_or_else(|| "unknown panic".to_string());
            Inference::Panic(msg)
        }
    }
}

/// Correlated-codomain soundness check. For a satisfying input `j`, sound
/// output must lie in the union of codomains whose domain `j` inhabits.
/// Returns `Some(reason)` when `j` matches at least one branch, every such
/// branch's codomain is concrete (no type variables), and the output lies
/// in none of them — a genuine correlation violation. Conservative
/// everywhere else (returns None), so imprecise arrow types never cause
/// false positives.
fn arrow_soundness_violation(
    arrows: &[(Shape, Shape)],
    input: &Json,
    output: &Json,
) -> Option<String> {
    let applicable: Vec<&Shape> = arrows
        .iter()
        .filter(|(d, _)| denotes(d, input))
        .map(|(_, c)| c)
        .collect();
    if applicable.is_empty() {
        return None;
    }
    // Only judge when every applicable codomain is fully concrete.
    if applicable
        .iter()
        .any(|c| shape_has_tvar(c) || matches!(c, Shape::Blob))
    {
        return None;
    }
    if applicable.iter().any(|c| denotes(c, output)) {
        return None;
    }
    let codomains: Vec<String> = applicable.iter().map(|c| c.to_string()).collect();
    Some(format!(
        "output escapes correlated codomain(s) {:?}",
        codomains
    ))
}

/// Whether a program introduces or references a variable binding. Such
/// programs are excluded from the correlated-arrow soundness check (a bound
/// variable breaks the input->output correlation the arrow view assumes).
fn program_has_binding(f: &Filter) -> bool {
    match f {
        Filter::Variable(_) | Filter::BindingExpression(_, _) => true,
        Filter::Dot
        | Filter::Null
        | Filter::Boolean(_)
        | Filter::Number(_)
        | Filter::String(_)
        | Filter::ArrayIterator
        | Filter::Empty
        | Filter::Error
        | Filter::Hole => false,
        Filter::Call(_, args) => args.iter().flatten().any(program_has_binding),
        Filter::Pipe(a, b)
        | Filter::Comma(a, b)
        | Filter::Alternative(a, b)
        | Filter::BinOp(a, _, b) => program_has_binding(a) || program_has_binding(b),
        Filter::ObjIndex(a)
        | Filter::ArrayIndex(a)
        | Filter::UnOp(_, a) => program_has_binding(a),
        Filter::IfThenElse(a, b, c) => {
            program_has_binding(a) || program_has_binding(b) || program_has_binding(c)
        }
        Filter::TryCatch(a, b) => {
            program_has_binding(a) || b.as_ref().is_some_and(|h| program_has_binding(h))
        }
        Filter::Array(items) => items.iter().any(program_has_binding),
        Filter::Object(pairs) => pairs
            .iter()
            .any(|(k, v)| program_has_binding(k) || program_has_binding(v)),
        Filter::ReduceExpression(_, a, b, c) => {
            program_has_binding(a) || program_has_binding(b) || program_has_binding(c)
        }
        Filter::ForeachExpression(_, a, b, c, d) => {
            program_has_binding(a)
                || program_has_binding(b)
                || program_has_binding(c)
                || d.as_ref().is_some_and(|e| program_has_binding(e))
        }
        Filter::SliceExpression(a, b) => {
            a.as_ref().is_some_and(|x| program_has_binding(x))
                || b.as_ref().is_some_and(|x| program_has_binding(x))
        }
        Filter::Bound(_, a) => program_has_binding(a),
        Filter::FunctionExpression(defs, body) => {
            defs.values().any(program_has_binding) || program_has_binding(body)
        }
    }
}

fn shape_has_tvar(s: &Shape) -> bool {
    match s {
        Shape::TVar(_) => true,
        Shape::Blob | Shape::Null | Shape::Bool(_) | Shape::Number(_) | Shape::String(_) => false,
        Shape::Array(e, _) => shape_has_tvar(e),
        Shape::Tuple(ts) => ts.iter().any(shape_has_tvar),
        Shape::Object(fs) => fs.iter().any(|f| shape_has_tvar(&f.value)),
        Shape::Union(a, b)
        | Shape::Intersection(a, b)
        | Shape::Mismatch(a, b)
        | Shape::Arrow(a, b) => shape_has_tvar(a) || shape_has_tvar(b),
        Shape::Neg(a) => shape_has_tvar(a),
    }
}

fn run_tjq(
    filter: &Filter,
    input: &Json,
    builtins: &HashMap<String, Filter>,
) -> Option<Result<Vec<Json>, String>> {
    // catch_unwind: interpreter todo!()s are findings, not aborts
    catch_unwind(AssertUnwindSafe(|| {
        // Clear the allocation-guard flag so the caller can tell whether tjq's
        // memory guard fired during *this* run, even if a `?`/`catch` in the
        // program later swallowed the resulting error.
        reset_alloc_guard();
        let mut var_ctx = HashMap::new();
        let results = Filter::filter(input, filter, builtins, &mut var_ctx);
        let mut values = vec![];
        for r in results {
            match r {
                Ok(v) => values.push(v),
                Err(e) => return Err(format!("{e:?}")),
            }
        }
        Ok(values)
    }))
    .ok()
}

/// Maximum failure-predicate evaluations per finding during shrinking.
/// Each evaluation may spawn jq, so this bounds shrink time to a few
/// seconds per finding.
const SHRINK_BUDGET: usize = 400;

fn streams_agree(jq_outputs: &[Json], tjq_outputs: &[Json]) -> bool {
    jq_outputs.len() == tjq_outputs.len()
        && jq_outputs
            .iter()
            .zip(tjq_outputs)
            .all(|(a, b)| json_equal(a, b))
}

/// Soundness failure: input satisfies tin, jq succeeds, and some output
/// escapes a non-vacuous tout.
fn fails_soundness(
    filter: &Filter,
    input: &Json,
    jq: &str,
    builtins: &HashMap<String, Filter>,
) -> bool {
    let Inference::Typed { tin, tout, .. } = infer(filter, builtins) else {
        return false;
    };
    if !denotes(&tin, input) {
        return false;
    }
    if shape_has_tvar(&tout) || matches!(tout, Shape::Blob) {
        return false;
    }
    match run_jq(jq, &to_jq_source(filter), &to_json_string(input)) {
        JqOutcome::Ok(outputs) => outputs.iter().any(|o| !denotes(&tout, o)),
        _ => false,
    }
}

/// Arrow (correlated-codomain) soundness failure, for shrinking.
fn fails_arrow_soundness(
    filter: &Filter,
    input: &Json,
    jq: &str,
    builtins: &HashMap<String, Filter>,
) -> bool {
    let Inference::Typed { tin, arrows, .. } = infer(filter, builtins) else {
        return false;
    };
    if !denotes(&tin, input) {
        return false;
    }
    match run_jq(jq, &to_jq_source(filter), &to_json_string(input)) {
        JqOutcome::Ok(outputs) => outputs
            .iter()
            .any(|o| arrow_soundness_violation(&arrows, input, o).is_some()),
        _ => false,
    }
}

/// Divergence failure: jq succeeds and tjq_exec disagrees (or errors).
fn fails_divergence(
    filter: &Filter,
    input: &Json,
    jq: &str,
    builtins: &HashMap<String, Filter>,
) -> bool {
    match run_jq(jq, &to_jq_source(filter), &to_json_string(input)) {
        JqOutcome::Ok(outputs) => match run_tjq(filter, input, builtins) {
            Some(Ok(tjq_outputs)) => !streams_agree(&outputs, &tjq_outputs),
            // tjq's memory guard is a resource-limit artifact, not a divergence
            Some(Err(e)) => !e.contains("AllocationTooLarge"),
            None => false,
        },
        _ => false,
    }
}

/// Effect failure: the program claims totality but jq errors on the input.
fn fails_effect(filter: &Filter, input: &Json, jq: &str) -> bool {
    cannot_fail(filter)
        && matches!(
            run_jq(jq, &to_jq_source(filter), &to_json_string(input)),
            JqOutcome::RuntimeError
        )
}

/// Crash failure: jq crashes, hangs, or emits unparseable output.
fn fails_crash(filter: &Filter, input: &Json, jq: &str) -> bool {
    matches!(
        run_jq(jq, &to_jq_source(filter), &to_json_string(input)),
        JqOutcome::Crash(_) | JqOutcome::Timeout
    )
}

/// Shrink a failing pair and format the extra JSONL fields.
fn shrunk_fields<P>(filter: &Filter, input: &Json, pred: P, budget: usize) -> String
where
    P: FnMut(&Filter, &Json) -> bool,
{
    let (sf, si) = shrink_case(filter, input, pred, budget);
    format!(
        ",\"shrunk_program\":{},\"shrunk_input\":{}",
        serde_json::to_string(&to_jq_source(&sf)).unwrap(),
        serde_json::to_string(&to_json_string(&si)).unwrap()
    )
}

#[derive(Default)]
struct Counters {
    programs: usize,
    jq_compile_errors: usize,
    inference_panics: usize,
    inference_type_errors: usize,
    typed: usize,
    tin_unconstrained: usize,
    satisfying_inputs: usize,
    outputs_checked: usize,
    soundness_violations: usize,
    arrow_soundness_violations: usize,
    tout_vacuous: usize,
    satisfying_input_errored: usize,
    no_fail_programs: usize,
    effect_violations: usize,
    mutants: usize,
    mutants_rejected_by_jq: usize,
    mutants_accepted_by_jq: usize,
    diff_compared: usize,
    diff_diverged: usize,
    tjq_exec_panics: usize,
    jq_crashes: usize,
    jq_timeouts: usize,
}

fn main() {
    // Findings must survive inference panics; silence the default hook.
    // Set DIFFTEST_PANIC_TRACE=1 to keep panic locations on stderr when
    // triaging inference panics.
    if std::env::var("DIFFTEST_PANIC_TRACE").is_err() {
        std::panic::set_hook(Box::new(|_| {}));
    }

    let args: Vec<String> = std::env::args().collect();
    let get_arg = |name: &str, default: &str| -> String {
        args.iter()
            .position(|a| a == name)
            .and_then(|i| args.get(i + 1))
            .cloned()
            .unwrap_or_else(|| default.to_string())
    };
    let iters: usize = get_arg("--iters", "1000").parse().expect("--iters");
    let seed: u64 = get_arg("--seed", "1").parse().expect("--seed");
    let jq = get_arg("--jq", "jq");
    let depth: usize = get_arg("--depth", "3").parse().expect("--depth");
    // Inputs are cheap (inference is amortized over them) and evaluated in
    // batched jq processes, so exercise each program with tens of thousands.
    let inputs_per_program: usize = get_arg("--inputs", "20000").parse().expect("--inputs");
    // Optional wall-clock budget (seconds). 0 = run all `--iters`. A shard that
    // draws a few pathologically slow programs would otherwise blow past the
    // CI job's 6-hour hard limit and get *killed* (red, no clean summary). With
    // a budget it stops generating new programs once the time is up and exits
    // normally, reporting whatever it found — coverage self-adjusts to the box.
    let max_seconds: u64 = get_arg("--max-seconds", "0").parse().expect("--max-seconds");
    let findings_path = get_arg("--findings", "target/difftest-findings.jsonl");
    let trace = std::env::var("DIFFTEST_TRACE").is_ok();
    let trace_path = format!("{findings_path}.trace");

    let builtins = builtin_filters();
    let mut findings = std::fs::File::create(&findings_path).expect("findings file");
    let mut c = Counters::default();
    let mut panic_kinds: HashMap<String, usize> = HashMap::new();

    let mut emit = |file: &mut std::fs::File, kind: &str, seed: u64, program: &str, rest: &str| {
        let line = format!(
            "{{\"kind\":\"{}\",\"seed\":{},\"program\":{},{}}}",
            kind,
            seed,
            serde_json::to_string(program).unwrap(),
            rest
        );
        let _ = writeln!(file, "{line}");
    };

    let campaign_start = Instant::now();
    for i in 0..iters {
        // Wall-clock budget: stop launching new programs once it is spent, so
        // the shard finishes cleanly instead of being killed at the job limit.
        if max_seconds > 0 && campaign_start.elapsed().as_secs() >= max_seconds {
            println!(
                "time budget {max_seconds}s reached after {} programs; stopping early",
                c.programs
            );
            break;
        }
        let case_seed = seed.wrapping_add(i as u64);
        let mut rng = Rng::new(case_seed);
        let filter = gen_filter(&mut rng, depth);
        // Record the current program so an *uncatchable* abort (e.g. a huge
        // allocation that catch_unwind can't intercept) leaves the culprit
        // on disk next to the findings file. Enable with DIFFTEST_TRACE.
        if trace {
            let _ = std::fs::write(
                &trace_path,
                format!("seed={case_seed}\nprogram={}\n", to_jq_source(&filter)),
            );
        }
        let program = to_jq_source(&filter);
        c.programs += 1;

        let inference = infer(&filter, &builtins);
        let (tin, tout, arrows) = match inference {
            Inference::Panic(msg) => {
                c.inference_panics += 1;
                let key = msg.chars().take(80).collect::<String>();
                if !panic_kinds.contains_key(&key) {
                    // First occurrence of each panic kind is a finding
                    emit(
                        &mut findings,
                        "inference-panic",
                        case_seed,
                        &program,
                        &format!("\"detail\":{}", serde_json::to_string(&msg).unwrap()),
                    );
                }
                *panic_kinds.entry(key).or_default() += 1;
                continue;
            }
            Inference::TypeErrors(_) => {
                c.inference_type_errors += 1;
                continue;
            }
            Inference::Typed { tin, tout, arrows } => (tin, tout, arrows),
        };
        c.typed += 1;

        // Top-level unconstrained only: `{a: T}` still directs input
        // generation even though the field type is open
        let tin_unconstrained = matches!(tin, Shape::Blob | Shape::TVar(_));
        if tin_unconstrained {
            c.tin_unconstrained += 1;
        }
        let tout_vacuous = shape_has_tvar(&tout) || matches!(tout, Shape::Blob);
        // v1 failure effect: a `cannot_fail` claim makes any jq runtime
        // error on any input a hard violation.
        let no_fail = cannot_fail(&filter);
        if no_fail {
            c.no_fail_programs += 1;
        }

        // --- satisfying inputs: soundness + differential ---
        // Generate many inputs up front, then evaluate them through jq in a
        // few batched processes rather than one spawn each.
        let mut sat_inputs = Vec::with_capacity(inputs_per_program);
        for _ in 0..inputs_per_program {
            // Mostly small collision-friendly inputs, with a steady diet of
            // large, deep, and degenerate ones — the shrinker keeps any
            // finding readable regardless of the input that triggered it.
            let profile = match rng.below(10) {
                0..=5 => SizeProfile::Small,
                6..=7 => SizeProfile::Large,
                8 => SizeProfile::Deep,
                _ => SizeProfile::Degenerate,
            };
            if let Some(input) = inhabit_profiled(&tin, &mut rng, profile) {
                sat_inputs.push(input);
            }
        }
        let sat_results = eval_inputs(&jq, &program, &sat_inputs);
        for (input, result) in sat_inputs.iter().zip(sat_results) {
            c.satisfying_inputs += 1;
            let input_text = to_json_string(input);

            match result {
                InputResult::Ok(outputs) => {
                    // Soundness: every output inhabits tout
                    for out in &outputs {
                        c.outputs_checked += 1;
                        if tout_vacuous {
                            c.tout_vacuous += 1;
                        } else if !denotes(&tout, out) {
                            c.soundness_violations += 1;
                            emit(
                                &mut findings,
                                "soundness",
                                case_seed,
                                &program,
                                &format!(
                                    "\"input\":{},\"output\":{},\"tout\":{}{}",
                                    serde_json::to_string(&input_text).unwrap(),
                                    serde_json::to_string(&to_json_string(out)).unwrap(),
                                    serde_json::to_string(&tout.to_string()).unwrap(),
                                    shrunk_fields(
                                        &filter,
                                        input,
                                        |f, i| fails_soundness(f, i, &jq, &builtins),
                                        SHRINK_BUDGET
                                    )
                                ),
                            );
                        }
                        // Correlated arrow-codomain check (stronger than the
                        // union tout: catches input/output correlation bugs).
                        if let Some(reason) = arrow_soundness_violation(&arrows, input, out) {
                            c.arrow_soundness_violations += 1;
                            emit(
                                &mut findings,
                                "arrow-soundness",
                                case_seed,
                                &program,
                                &format!(
                                    "\"input\":{},\"output\":{},\"reason\":{}{}",
                                    serde_json::to_string(&input_text).unwrap(),
                                    serde_json::to_string(&to_json_string(out)).unwrap(),
                                    serde_json::to_string(&reason).unwrap(),
                                    shrunk_fields(
                                        &filter,
                                        input,
                                        |f, i| fails_arrow_soundness(f, i, &jq, &builtins),
                                        SHRINK_BUDGET
                                    )
                                ),
                            );
                        }
                    }
                    // Differential: tjq_exec must agree
                    match run_tjq(&filter, input, &builtins) {
                        // The allocation guard fired somewhere during this run,
                        // but the program's own `?`/`catch` turned the error into
                        // a normal (empty or altered) output. jq attempts the
                        // astronomical allocation instead, so any disagreement
                        // here is a resource-limit artifact, not a semantic
                        // divergence — skip it exactly as we skip the uncaught
                        // guard error below.
                        Some(Ok(_)) if alloc_guard_tripped() => {}
                        Some(Ok(tjq_outputs)) => {
                            c.diff_compared += 1;
                            let agree = tjq_outputs.len() == outputs.len()
                                && tjq_outputs
                                    .iter()
                                    .zip(&outputs)
                                    .all(|(a, b)| json_equal(a, b));
                            if !agree {
                                c.diff_diverged += 1;
                                emit(
                                    &mut findings,
                                    "divergence",
                                    case_seed,
                                    &program,
                                    &format!(
                                        "\"input\":{},\"jq\":{},\"tjq\":{}{}",
                                        serde_json::to_string(&input_text).unwrap(),
                                        serde_json::to_string(
                                            &outputs
                                                .iter()
                                                .map(to_json_string)
                                                .collect::<Vec<_>>()
                                                .join(" ")
                                        )
                                        .unwrap(),
                                        serde_json::to_string(
                                            &tjq_outputs
                                                .iter()
                                                .map(to_json_string)
                                                .collect::<Vec<_>>()
                                                .join(" ")
                                        )
                                        .unwrap(),
                                        shrunk_fields(
                                            &filter,
                                            input,
                                            |f, i| fails_divergence(f, i, &jq, &builtins),
                                            SHRINK_BUDGET,
                                        )
                                    ),
                                );
                            }
                        }
                        // tjq's own memory guard (a huge string build jq
                        // completes but tjq refuses) is a resource-limit
                        // artifact, not a semantic divergence — whether the
                        // guard error propagated to the top (string match) or
                        // tripped in a sub-expression that a `?`/`catch` turned
                        // into some other error (the flag).
                        Some(Err(e)) if e.contains("AllocationTooLarge") || alloc_guard_tripped() => {}
                        Some(Err(_)) => {
                            // jq succeeded, tjq_exec errored: divergence
                            c.diff_compared += 1;
                            c.diff_diverged += 1;
                            emit(
                                &mut findings,
                                "divergence-error",
                                case_seed,
                                &program,
                                &format!(
                                    "\"input\":{},\"jq\":\"ok\",\"tjq\":\"error\"{}",
                                    serde_json::to_string(&input_text).unwrap(),
                                    shrunk_fields(
                                        &filter,
                                        input,
                                        |f, i| fails_divergence(f, i, &jq, &builtins),
                                        SHRINK_BUDGET,
                                    )
                                ),
                            );
                        }
                        None => {
                            c.tjq_exec_panics += 1;
                        }
                    }
                }
                InputResult::Errored => {
                    if no_fail {
                        // The effect analysis claimed this program is total
                        c.effect_violations += 1;
                        emit(
                            &mut findings,
                            "effect-soundness",
                            case_seed,
                            &program,
                            &format!(
                                "\"input\":{}{}",
                                serde_json::to_string(&input_text).unwrap(),
                                shrunk_fields(
                                    &filter,
                                    input,
                                    |f, i| fails_effect(f, i, &jq),
                                    SHRINK_BUDGET
                                )
                            ),
                        );
                    } else {
                        // Failure effect unknown; metric only.
                        c.satisfying_input_errored += 1;
                    }
                }
                InputResult::CompileError => {
                    c.jq_compile_errors += 1;
                    emit(
                        &mut findings,
                        "jq-compile-error",
                        case_seed,
                        &program,
                        "\"note\":\"tjq parses this, jq rejects it\"",
                    );
                    break;
                }
                InputResult::Crash(msg) => {
                    c.jq_crashes += 1;
                    emit(
                        &mut findings,
                        "jq-crash",
                        case_seed,
                        &program,
                        &format!(
                            "\"input\":{},\"detail\":{}{}",
                            serde_json::to_string(&input_text).unwrap(),
                            serde_json::to_string(&msg).unwrap(),
                            shrunk_fields(&filter, input, |f, i| fails_crash(f, i, &jq), 30)
                        ),
                    );
                }
                InputResult::Timeout => {
                    c.jq_timeouts += 1;
                    emit(
                        &mut findings,
                        "jq-timeout",
                        case_seed,
                        &program,
                        &format!(
                            "\"input\":{}{}",
                            serde_json::to_string(&input_text).unwrap(),
                            shrunk_fields(&filter, input, |f, i| fails_crash(f, i, &jq), 30)
                        ),
                    );
                }
            }
        }

        // --- non-satisfying inputs: exactness metric ---
        if !tin_unconstrained {
            let mut mutants = Vec::with_capacity(inputs_per_program);
            for _ in 0..inputs_per_program {
                let Some(base) = inhabit(&tin, &mut rng) else {
                    continue;
                };
                let mutant = mutate(&base, &mut rng);
                // Only keep mutants that escaped tin (true counterexamples).
                if !denotes(&tin, &mutant) {
                    mutants.push(mutant);
                }
            }
            let mutant_results = eval_inputs(&jq, &program, &mutants);
            for (mutant, result) in mutants.iter().zip(mutant_results) {
                c.mutants += 1;
                match result {
                    // jq errored -> tin correctly excluded this input
                    InputResult::Errored => c.mutants_rejected_by_jq += 1,
                    // jq accepted -> the exactness gap (tin too narrow)
                    InputResult::Ok(_) => c.mutants_accepted_by_jq += 1,
                    InputResult::Crash(msg) => {
                        c.jq_crashes += 1;
                        emit(
                            &mut findings,
                            "jq-crash",
                            case_seed,
                            &program,
                            &format!(
                                "\"input\":{},\"detail\":{}{}",
                                serde_json::to_string(&to_json_string(mutant)).unwrap(),
                                serde_json::to_string(&msg).unwrap(),
                                shrunk_fields(&filter, mutant, |f, i| fails_crash(f, i, &jq), 30)
                            ),
                        );
                    }
                    InputResult::Timeout => c.jq_timeouts += 1,
                    InputResult::CompileError => {}
                }
            }
        }
    }

    println!("=== difftest summary (seed {seed}, {iters} iters) ===");
    println!("programs:                {}", c.programs);
    println!("  jq compile errors:     {}", c.jq_compile_errors);
    println!("inference:");
    println!("  panics:                {}", c.inference_panics);
    println!("  type errors:           {}", c.inference_type_errors);
    println!("  typed:                 {}", c.typed);
    println!("  tin unconstrained:     {}", c.tin_unconstrained);
    println!("soundness:");
    println!("  satisfying inputs:     {}", c.satisfying_inputs);
    println!("  outputs checked:       {}", c.outputs_checked);
    println!("  tout vacuous:          {}", c.tout_vacuous);
    println!("  VIOLATIONS:            {}", c.soundness_violations);
    println!("arrow soundness (hard gate):");
    println!("  ARROW VIOLATIONS:      {}", c.arrow_soundness_violations);
    println!("failure effect (v1):");
    println!("  no-fail programs:      {}", c.no_fail_programs);
    println!("  EFFECT VIOLATIONS:     {}", c.effect_violations);
    println!("effect gap (metric):");
    println!("  satisfying errored:    {}", c.satisfying_input_errored);
    println!("exactness (metric):");
    println!("  escaped mutants run:   {}", c.mutants);
    println!("  rejected by jq:        {}", c.mutants_rejected_by_jq);
    println!("  accepted by jq (gap):  {}", c.mutants_accepted_by_jq);
    println!("differential vs tjq_exec:");
    println!("  compared:              {}", c.diff_compared);
    println!("  DIVERGED:              {}", c.diff_diverged);
    println!("  tjq_exec panics:       {}", c.tjq_exec_panics);
    println!("jq health:");
    println!("  crashes:               {}", c.jq_crashes);
    println!("  timeouts:              {}", c.jq_timeouts);
    if !panic_kinds.is_empty() {
        println!("inference panic kinds:");
        let mut kinds: Vec<_> = panic_kinds.into_iter().collect();
        kinds.sort_by_key(|(_, n)| std::cmp::Reverse(*n));
        for (msg, n) in kinds.into_iter().take(10) {
            println!("  {n:>6}  {msg}");
        }
    }
    println!("findings written to {findings_path}");
    // Clean completion: remove the trace so a surviving .trace file marks a
    // shard that aborted mid-run (with the culprit program inside).
    if trace {
        let _ = std::fs::remove_file(&trace_path);
    }

    // Hard findings are genuine bugs (in the type system, the interpreter, or
    // jq): any of these fails CI. Timeouts (the string-repetition
    // resource-exhaustion shape) and the metric gaps are not bugs and do not
    // fail. A machine-readable line makes the gate easy to grep in CI logs.
    //
    // arrow_soundness is a hard gate again. It was briefly demoted when fixing
    // the CI timeouts surfaced a pre-existing arrow-soundness hole (a dead
    // erroring operand of a short-circuit `or`/`and` dropped an otherwise-valid
    // branch and unsoundly narrowed the codomain). That hole is now closed (see
    // the And/Or arm in experimental_type_inference.rs), and a fresh-seed sweep
    // across depths 3-7 found zero arrow violations, so the correlated
    // intersection-of-arrows layer is re-gated. The still-noted model
    // limitation — overlapping-domain branches need a *union* codomain — turned
    // out to already be handled correctly (`then/else <: output` unions), so it
    // is not a live soundness gap.
    let hard = c.soundness_violations
        + c.arrow_soundness_violations
        + c.effect_violations
        + c.diff_diverged
        + c.tjq_exec_panics
        + c.jq_crashes
        + c.inference_panics;
    // arrow_violations stays broken out in the RESULT line for visibility even
    // though it now also counts toward hard_findings.
    println!(
        "RESULT hard_findings={hard} arrow_violations={} timeouts={} seed={seed}",
        c.arrow_soundness_violations, c.jq_timeouts
    );
    if hard > 0 {
        eprintln!("FAIL: {hard} hard finding(s); see {findings_path}");
        std::process::exit(1);
    }
}
