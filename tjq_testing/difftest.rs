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

use tjq_exec::{builtin_filters, Filter, Json};
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

fn run_jq(jq: &str, program: &str, input: &str) -> JqOutcome {
    let mut child = match Command::new(jq)
        .arg("-c")
        // `--` keeps programs that start with `-` (e.g. `-length`) from
        // being taken as CLI flags
        .arg("--")
        .arg(program)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        // stderr is classified by exit code only; never let it fill a pipe
        .stderr(Stdio::null())
        .spawn()
    {
        Ok(c) => c,
        Err(e) => return JqOutcome::Crash(format!("spawn failed: {e}")),
    };

    // Both stdin and stdout must be pumped concurrently with the wait:
    // large inputs/outputs otherwise deadlock on full pipe buffers and
    // masquerade as timeouts.
    let stdin_thread = child.stdin.take().map(|mut stdin| {
        let input_owned = input.to_string();
        std::thread::spawn(move || {
            // jq may exit early on compile errors; ignore EPIPE
            let _ = stdin.write_all(input_owned.as_bytes());
            let _ = stdin.write_all(b"\n");
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
                        return JqOutcome::Crash(format!("signal {sig}"));
                    }
                }
                return match status.code() {
                    Some(0) => {
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
                    Some(3) => JqOutcome::CompileError,
                    Some(_) => JqOutcome::RuntimeError,
                    None => JqOutcome::Crash("no exit code".to_string()),
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
                    return JqOutcome::Timeout;
                }
                std::thread::sleep(Duration::from_millis(2));
            }
            Err(e) => return JqOutcome::Crash(format!("wait failed: {e}")),
        }
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
        let has_overload = constraints.iter().any(|c| matches!(c, Constraint::Or(_)));
        let arrows = if has_overload {
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
            Some(Err(_)) => true,
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
    let inputs_per_program: usize = get_arg("--inputs", "4").parse().expect("--inputs");
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

    for i in 0..iters {
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
            let Some(input) = inhabit_profiled(&tin, &mut rng, profile) else {
                continue;
            };
            c.satisfying_inputs += 1;
            let input_text = to_json_string(&input);

            match run_jq(&jq, &program, &input_text) {
                JqOutcome::Ok(outputs) => {
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
                                        &input,
                                        |f, i| fails_soundness(f, i, &jq, &builtins),
                                        SHRINK_BUDGET
                                    )
                                ),
                            );
                        }
                        // Correlated arrow-codomain check (stronger than the
                        // union tout: catches input/output correlation bugs).
                        if let Some(reason) = arrow_soundness_violation(&arrows, &input, out) {
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
                                        &input,
                                        |f, i| fails_arrow_soundness(f, i, &jq, &builtins),
                                        SHRINK_BUDGET
                                    )
                                ),
                            );
                        }
                    }
                    // Differential: tjq_exec must agree
                    match run_tjq(&filter, &input, &builtins) {
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
                                            &input,
                                            |f, i| fails_divergence(f, i, &jq, &builtins),
                                            SHRINK_BUDGET,
                                        )
                                    ),
                                );
                            }
                        }
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
                                        &input,
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
                JqOutcome::RuntimeError => {
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
                                    &input,
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
                JqOutcome::CompileError => {
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
                JqOutcome::Crash(msg) => {
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
                            shrunk_fields(&filter, &input, |f, i| fails_crash(f, i, &jq), 30)
                        ),
                    );
                }
                JqOutcome::Timeout => {
                    c.jq_timeouts += 1;
                    emit(
                        &mut findings,
                        "jq-timeout",
                        case_seed,
                        &program,
                        &format!(
                            "\"input\":{}{}",
                            serde_json::to_string(&input_text).unwrap(),
                            shrunk_fields(&filter, &input, |f, i| fails_crash(f, i, &jq), 30)
                        ),
                    );
                }
            }
        }

        // --- non-satisfying inputs: exactness metric ---
        if !tin_unconstrained {
            for _ in 0..inputs_per_program {
                let Some(base) = inhabit(&tin, &mut rng) else {
                    continue;
                };
                let mutant = mutate(&base, &mut rng);
                if denotes(&tin, &mutant) {
                    continue; // mutant did not escape; not a counterexample
                }
                c.mutants += 1;
                match run_jq(&jq, &program, &to_json_string(&mutant)) {
                    JqOutcome::RuntimeError => c.mutants_rejected_by_jq += 1,
                    JqOutcome::Ok(_) => c.mutants_accepted_by_jq += 1,
                    JqOutcome::Crash(msg) => {
                        c.jq_crashes += 1;
                        emit(
                            &mut findings,
                            "jq-crash",
                            case_seed,
                            &program,
                            &format!(
                                "\"input\":{},\"detail\":{}{}",
                                serde_json::to_string(&to_json_string(&mutant)).unwrap(),
                                serde_json::to_string(&msg).unwrap(),
                                shrunk_fields(&filter, &mutant, |f, i| fails_crash(f, i, &jq), 30)
                            ),
                        );
                    }
                    JqOutcome::Timeout => c.jq_timeouts += 1,
                    JqOutcome::CompileError => {}
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
    let hard = c.soundness_violations
        + c.arrow_soundness_violations
        + c.effect_violations
        + c.diff_diverged
        + c.tjq_exec_panics
        + c.jq_crashes
        + c.inference_panics;
    println!(
        "RESULT hard_findings={hard} timeouts={} seed={seed}",
        c.jq_timeouts
    );
    if hard > 0 {
        eprintln!("FAIL: {hard} hard finding(s); see {findings_path}");
        std::process::exit(1);
    }
}
