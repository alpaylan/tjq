# Generative testing campaign — July 2026

First full run of the testing plan in `type-system-scope.md` §11, executed
by the new `tjq_testing` crate. This report records the final state, the
bugs the campaign found and fixed, and what the metrics say about where
the type system stands.

## Final state (all layers green)

**Model layer** — every algebra law holds for 200k random shapes × 8 JSON
samples each, across multiple seeds: set operations are boolean algebra on
`check`-denotations, `canonicalize` preserves denotations, `included_in` /
`disjoint_with` claims are sound, the legacy `subtype` honors its
(inverted) contract, and intersections canonicalizing to `Mismatch` are
genuinely empty.

**Differential layer** (second campaign, expanded surface: `type`, `not`,
`and`/`or`, field-access pipelines) — 22,000 random programs (depths 3–5),
83,862 type-derived inputs, 44,625 jq outputs checked against inferred
output types, 44,166 output streams compared between jq 1.7.1 and
`tjq_exec`:

| Property | Result |
| --- | --- |
| Soundness violations (outputs outside `tout`) | **0** |
| Effect violations (`cannot_fail` programs that errored) | **0** (4,640 programs claimed total) |
| jq vs `tjq_exec` divergences | **0** |
| Inference panics | **0** |
| jq crashes / hangs | **0** |
| Inference type-errors (programs rejected) | 2,887 / 22,000 (was 7,816 before native `length`/`type` axioms) |
| Effect gap (satisfying inputs where jq errored, effect unknown) | 39,696 / 83,862 |
| Exactness gap (escaped mutants jq accepted) | 428 / 4,950 |

## What the campaign found (all fixed, see git history)

Shape algebra (found by the model layer):
union absorption inverted (`bool | false → false`); `Neg(A|B)` distributed
to a union instead of an intersection (De Morgan); `Neg(Mismatch)` typed as
empty instead of ⊤; the `(Tuple, Array)` subtype arm was `todo!()`; the
Union/Intersection subtype arms conflated orientations. The tri-state
`subtype` is now *derived* from the model-tested `included_in`.

Parser: `f, g` parsed as `f | g` — stream concatenation became composition.

Inference soundness (found by the differential oracle):
conditional and overload branches tied to unresolved variables were
silently dropped from output unions (three distinct paths: conditional
possibilities, disjunction extraction, structured `<:` bounds); branch
pruning used accumulated *possibilities* as if they were facts; negation
preserved the operand's singleton (`-(2) : 2`); `UnOp` constrained the
program input instead of the operand; multi-output constant programs were
typed by their first output only; `f, g` had no output constraint at all;
truthiness was modeled as `== true` (wrong for every non-boolean
condition); `.a` on null was untypeable (now the first `TypeOptions`
leniency rule, strict by default, lenient in the oracle).

Inference robustness (found by the depth-4/5 shards):
cyclic type-variable equalities made the substitution closure grow shapes
without bound (stack overflow), and un-deduplicated possibility
propagation through nested conditionals was exponential in program depth —
one depth-5 program drove the solver to a 73 GB peak footprint. Fixed with
occurs-checks, a shape-size ceiling, and a budgeted possibility push that
widens to ⊤ at the cap. The same program now solves in ~1 MB.

Interpreter vs jq 1.7.1 (found by the differential leg):
truthiness (`if -2` took the else branch); `%` didn't truncate operands and
NaN'd on zero divisors; `/` by zero produced infinity instead of an error;
`null + x` was an error instead of identity; object `+` was left-biased;
object `*` (recursive merge) missing; string `/` (split) missing, and
splitting `""` must yield `[]`; array `*` was supported (jq rejects it);
binop stream products iterated in the wrong order (jq is right-outer);
`.a` on null errored; `length` was an arrays-only stub (now a native
builtin per §9); object comparison ignored key sorting (jq compares sorted
key arrays first, then values).

## Round 2 (same campaign, after the first clean sweep)

The generator was expanded (`type`, `not`, `and`/`or`, input-consuming
pipelines), `length`/`type` got native axiomatic signatures (§9), and a v1
failure effect landed: `cannot_fail(filter)` under-approximates "never
errors on any input", and the oracle treats any jq runtime error on such a
program as a hard violation. The expansion immediately found and fixed:

- interpreter: `and`/`or` did not short-circuit and iterated the wrong
  stream order (jq is left-outer with per-value short-circuit —
  `true or error` is `true`);
- inference: constant subprograms were evaluated with *empty* builtin
  definitions, so `null | type` errored and its output vanished from the
  type (a soundness hole with mixed-outcome streams);
- inference: overload-branch satisfiability demanded the whole operand
  type be included in the overload domain, wrongly pruning branches for
  union-typed operands (`(0, "b") * length` lost its string overload —
  the sound test is non-disjointness);
- runner: programs starting with `-` (e.g. `-length`) were read by the jq
  CLI as flags; the invocation now uses `jq -c -- <program>`.

## Reading the metrics

- **Type-errors (13%)**: honest rejections (`1 + true`) plus remaining
  stdlib-stub imprecision; further shrinking needs more §9 signatures.
- **Effect gap (47% of inputs)**: inputs to programs where the failure
  effect is *unknown* (only 21% of programs are provably total under the
  v1 syntactic analysis). Growing `cannot_fail` into the type-aware
  effect of §2 converts this gap into checked claims.
- **Exactness gap (9%)**: jq's leniency accepting inputs outside the
  inferred domain (absent fields yielding null, etc.). This is the
  strict-mode lint surface (§5).
- **`tin` unconstrained (92% of typed)**: literal-rooted programs and
  lenient `.a` accesses (whose domain is genuinely near-universal)
  dominate; denser input constraint needs object-shape-directed
  generation.

## Round 3: shrinking and large inputs

A delta-debugging shrinker now minimizes every finding (program and input
jointly, re-checking the finding's failure predicate), and inputs are
generated under mixed size profiles: wide containers (~50–800 nodes),
deep nesting (≤100 levels — jq's parser and serde_json both cap near
128), and degenerate scalars (boundary doubles, 16KB/unicode/escape-heavy
strings). Median inputs used to be single scalars; the profiles plus the
shrinker mean findings stay readable regardless of what triggered them.

The larger inputs immediately caught: quoted object-construction keys in
the interpreter (root cause of two long-standing known-issue tests),
`.[i]`-on-null leniency missing, unclamped infinities (jq clamps overflow
to max-double), missing array/object overloads in `+`'s inference, a
dropped-unknown-branch soundness gap in nested conditionals, plus two
oracle bugs (a pipe deadlock faking jq timeouts; serde_json's default
1-ULP-lossy float parsing faking divergences — fixed with the
`float_roundtrip` feature).

Final round-3 campaign (22,000 programs, 85,304 inputs, 130,410 outputs
checked): zero soundness violations, zero effect violations, zero
inference panics, zero jq crashes; 1 known-class divergence and 4
jq-side timeouts, detailed below. With input-consuming program shapes
and the corrected metric, top-level-unconstrained inputs fell from 92%
to 42%, and inference rejections from 36% to 12%.

## Did we find bugs in jq?

No — no crashes or memory faults in jq 1.7.1 across ~300k invocations
(including malformed-ish near-miss inputs). The 4 "timeouts" in the
round-3 campaign are not jq bugs: they are string-repetition expressions
(`n | length * type` evaluates to `n * "number"`) where a large *data*
value becomes the repeat count, so jq faithfully builds a huge string
until the runner's 2s limit kills it. Worth remembering as an input-size
sensitivity, not something to file.

Two behaviors worth knowing regardless: `"ab" * 0` changed from `null`
(≤1.6) to `""` (1.7), and fractional numbers are silently truncated at
integer sites (`.[1.5]`, `limit(1.9)` — which takes **2**). jq also
prints the same double differently by provenance (`1E+300` for a
preserved literal, `1e+300` for a computed value) — a formatting quirk
tjq has to match, not a defect.

To actually go after jq's C code the generator needs the surface where it
is complex: `limit`/`first`/`label`/`break`, path expressions (`setpath`,
`delpaths`), `@base64d`-style formats, deep recursion (`recurse`), and
huge/degenerate literals — most of which tjq does not parse or type yet.
That expansion can run against jq independently of the type checker.

## Reproducing

```
cargo test -p tjq_testing                     # model + generator layers
cargo run -p tjq_testing --release --bin difftest -- \
  --iters 6000 --seed 100000                  # one differential shard
```

Every finding is a JSONL line with its generator seed; rerun with
`--iters 1 --seed <seed>` to reproduce a case exactly.

## Round 4/5: rows, container access, correlated oracle, CI

Since the round-3 report: object rows (§6), precise container-access typing,
a correlation-aware oracle, and continuous fuzzing in CI.

- **Object rows (§6)**: `Shape::Object` became a row with optional fields
  and open/closed. Field access infers open rows with an optional field
  under lenient semantics; construction infers closed rows. The exactness
  gap fell from ~9% to ~1.8%.
- **Container access typed as intersections of arrows**: `.[i]`
  (`array -> elem|null`), `.[]` (arrays *and* objects), and `.[a:b]` slices
  (`array|string|null`, kind-preserving). This dropped top-level
  unconstrained `tin` from ~92% to ~35%.
- **Correlation-aware oracle**: besides the union `tout`, the runner checks
  the `solve_arrows` intersection-of-arrows — for each satisfying input, the
  output must lie in the codomain of the branch(es) whose domain the input
  inhabits. Catches swapped-correlation bugs the union check is blind to;
  gated to overloaded programs for speed.
- **A solver bug this surfaced**: Phase-2 substitution let a smaller-var
  alias overwrite a concrete equality on the same variable, so `. + .`'s
  null branch was `null -> T2` instead of `null -> null`. Fixed with a
  two-pass build (aliases first, then concretes to class representatives);
  regression-tested.
- **jq's provenance-dependent number formatting** (`1E+308` for a preserved
  value, `1e+308` for a computed one — unmatchable by a provenance-free
  `f64`) is absorbed in the oracle: differing output strings that both parse
  as JSON of equal value compare equal.

Latest full campaign: 22,000 programs, 83,151 outputs checked, **0 hard
findings** (soundness, arrow-soundness, effect, divergence, crash) and 2
timeouts (the string-repetition non-bug).

## Continuous fuzzing (CI)

`.github/workflows/fuzz.yml` runs the rig against a pinned jq 1.7.1 on every
PR (a ~24k-program smoke across 8 sharded runners) and nightly / on-demand
at large scale (~320k programs). `difftest` exits non-zero on any hard
finding, so a genuine bug fails the job; timeouts and the precision metrics
do not. Findings JSONL is uploaded as a per-shard artifact, and each shard
prints `RESULT hard_findings=N … seed=S` for one-line triage — the seed
reproduces the run locally with `--seed S`.

## Round 6: array/object builtins, richer pools, and jq-source coverage

Expanded the generator's builtin surface and value pools, and started
measuring line/branch coverage of the *actual* jq C sources.

- **New builtins wired end-to-end** (interpreter + inference axioms +
  generator): `add`, `sort`, `reverse`, `min`, `max`, `flatten` (nullary)
  plus the higher-order `map(f)` / `select(f)`. Inference: `sort`/`reverse`/
  `flatten` are `array -> array`; `add`/`min`/`max` constrain input to an
  array.
- **Three real tjq parity bugs the expansion surfaced and fixed:**
  1. *String escapes were never decoded.* The parser stripped the quotes but
     stored the body verbatim, so `"\n" | length` was 2 (backslash + `n`)
     instead of 1, `"\\"` was two characters, `"a\"b"` was four. Added a
     JSON/jq unescaper (`\n \t \" \\ \/ \b \f \uXXXX` with UTF-16 surrogate
     pairs). This is the highest-impact fix — every escaped string literal
     was wrong.
  2. *`add`/`flatten`/`reverse` rejected objects.* jq defines them via
     `reduce .[]`, which iterates array elements **or object values**, so
     `add {"a":1,"b":2}` is `3`, `flatten {"a":{"b":1}}` is `[{"b":1}]`.
     tjq errored on objects; now it iterates values.
  3. *`reverse` on length-0 non-arrays.* jq's `[.[length-1-range(0;length)]]`
     yields `[]` for `null`, `""`, `[]`, and `{}` alike (the range is empty),
     while non-empty strings/objects error. tjq only handled arrays and null.
- **Hypothesis-style value pools.** `EXTREME_NUMBER_POOL` grew from 8 to 27
  IEEE edge cases (f64/f32 limits, smallest subnormal/normal, machine
  epsilons, 2^53 ± 1, power-of-two integer boundaries, awkward near-integer
  fractions, signed zero); program numeric literals now draw these too.
  `STRING_POOL` grew from 5 to 18 (keyword/number look-alikes, whitespace,
  quote/backslash, multi-byte Unicode); `KEY_POOL` from 4 to 7. Supporting
  the Unicode keys required teaching the source-printer to quote/bracket
  non-identifier keys (`.["é"]`, `{"é": …}`) and emit JSON-valid escapes.
- **Coverage over real jq.** Built jq 1.7.1 with
  `-fprofile-instr-generate -fcoverage-mapping` and pointed `difftest --jq`
  at it. Across 2,000 generated programs the expanded generator lifts core
  `src/*.c` (excl. decNumber) line coverage from 48.9% to 50.8% and branch
  from 45.3% to 46.8% — with *fewer* programs than the 3,000-program
  baseline. Per-file: `builtin.c` 23.6% → 26.9% lines, `jv_aux.c` 24.6% →
  29.3%, `execute.c` 60.7% → 64.6% (the `map`/`select` backtracking paths).
  The regex engine (oniguruma) stays at 0% — no regex builtins are
  generated yet — so it is excluded from the "core" figure.

Post-expansion differential runs stay clean: 2,000 programs × 40 inputs and
600 × 300, **0 divergences / 0 soundness / arrow / effect violations**. The
only timeouts are `string * huge_number` (e.g. `. * "a"` on a `1e308`
input), where jq itself tries to build an astronomically large string; the
oracle skips jq-timeout and tjq's allocation-guard error, so these are not
findings.

## Round 7: try / catch / `?`, and four interpreter bugs it exposed

Added error handling — `try f`, `try f catch g`, and postfix `f?` — end to
end (parser, interpreter, `cannot_fail`, generator, printer). This targets
the VM's `TRY_BEGIN`/`FORK_OPT`/`BACKTRACK` opcodes, the part of `jq_next`
most distinct from straight-line evaluation.

Semantics: `try` emits the body's outputs until the first error, at which
point the stream stops; `catch g` then runs `g` on the error value, while
bare `try`/`?` yield nothing. Inference types `try` as `any -> any` (sound:
error suppression means the body's *input* constraints must not narrow the
outer input — `try .a` does not require an object), and `cannot_fail`
captures the effect precisely (`f?` never fails; `try f catch g` fails only
if `g` can). Because tjq's error *messages* differ from jq's, generated
`catch` handlers are restricted to input-ignoring constant leaves — enough to
exercise the catch path without fabricating message-text divergences.

Typing `try` as `any -> any` feeds every input (not just type-satisfying
ones) into the body, which surfaced **six pre-existing interpreter bugs**
that type-directed generation had been hiding — mostly cases where tjq
silently produced a value or empty stream where jq raises an error:

1. **`Pipe` dropped left-operand errors.** `error | 5` and `.x | 0.25` on a
   number produced an *empty* stream instead of propagating the error (a
   `flat_map(|r| r.map(…))` whose `Result`-as-iterator yields nothing for
   `Err`). Now errors propagate.
2. **`if` dropped condition errors.** The same `flat_map`/`flatten`
   antipattern: `if error then … end` and `if flatten then …` (on `null`)
   vanished instead of erroring. Fixed identically.
3. **`.[expr]` was not generic indexing.** It only handled arrays with
   numeric indices, *returned the index itself* on a type mismatch
   (`[1] | .["k"]` → `"k"`), never handled object inputs (`{"a":1} | .["a"]`
   errored), and lacked negative indices. Rewritten to dispatch on both input
   and index type (array+int with end-relative negatives, object+string,
   null→null, else error).
4. **Object construction returned the wrong value on a field error.**
   `{b: 5, k: reverse}` on `"a"` (where `reverse` errors) returned `5` — an
   arbitrary field's value — instead of the error. Now it surfaces the first
   actual error in the combination.
5. **`reverse` on the number `0`.** jq's `[.[length-1-range(0;length)]]`
   gives `[]` for any length-0 input, and a number's length is its absolute
   value, so `0 | reverse` is `[]` (only `0`/`-0` qualify; other numbers
   index into themselves and error). tjq errored.
6. **`catch` panicked on some errors.** `catch` stringifies the error value
   via `JQError`'s `Display`, which still had a `todo!()` arm
   (`NonStringObjectKey`, reachable via `try .[0] catch …` on an object).
   Implemented it; `Display` is now total.

To keep the eager interpreter from hanging where jq streams lazily, a
`MAX_STREAM_LEN` guard (4M values) caps the stream-multiplying arms (pipe,
comma, array/object construction); overflow becomes the harness-skipped
`AllocationTooLarge`, turning a would-be hang into a non-finding.

All six fixes are covered by new `tjq_exec` unit tests. After them,
differential runs with `try`/`?`/`catch` live are clean across seeds: e.g.
three 300 × 120 sweeps and a 500-program sweep, **0 divergences / 0
soundness / arrow / effect violations / panics** — down from 3,319
divergences when `try` first fed every input through the buggy paths.

**Coverage payoff.** The whole point was the VM core. `jq_next`'s branch
coverage rose **70.8% → 75.5%** (63 → 53 missed branches) — with a quarter
the programs of the round-6 measurement — because `try`/`catch`/`?` are the
only constructs that emit the `TRY_BEGIN`/`FORK_OPT`/`BACKTRACK` opcode
family. `execute.c` branch coverage rose 63.3% → 66.3%.

**A limitation this exercised.** tjq evaluates eagerly where jq streams
lazily, so a stream-multiplying program on a large input (`.[] op .[]` is a
cartesian product; nested, it is quadratic-and-up) can materialize far more
than jq ever holds at once. Beyond the `MAX_STREAM_LEN` guard on the
multiplying arms (pipe/comma/array/object/binop/and-or, all with an O(1)
product-size pre-check), such programs are slow-but-finite rather than
hanging; the remaining cost falls on the minority of large/deep input
profiles. A per-evaluation wall-clock budget in the harness is the cleaner
long-term bound.

## Round 8: `as $x` bindings, and three more interpreter/oracle bugs

Variable binding — `EXP as $pat | BODY` — added end to end (interpreter,
generator, printer, arrow gate). The binding scopes `BODY` per bound value
with proper save/restore, which is only expressible where both the binding
and its body are visible, so it is handled as a specialized `Pipe` arm (a
bare `EXP as $pat` is `EXP as $pat | .`). Destructuring patterns
(`[$a,$b]`, `{a:$x}`) bind by position/key. The generator emits
`VALUES as $v | BODY` where `BODY` references `$v`, and `$v` only ever
appears inside its own binding, so there are never free (compile-rejected)
variables.

Bindings did *not* move `jq_next` branch coverage (holds at 75.5%): the
`STOREV`/`LOADV` opcodes are already exercised by `map`/`select` in
`defs.jq`, which bind internally. The payoff was correctness — the existing
binding code was badly broken, and the differential campaign found three
more bugs:

1. **`destructure_pattern` panicked** binding `$x` to an array or object
   (`todo!()`); and the stream binding `(1,2,3) as $x | $x` returned the
   *last* value three times, because a shared context leaked (the
   `Pipe(Bind, body)` decomposition never scoped the body per value). Both
   fixed; bindings now match jq for streams, composite values, and nesting.
2. **Binops dropped the right operand's error under an empty left stream.**
   jq iterates the right operand in the outer loop, so `.[] * (type-null)`
   on `[]` raises `type-null`'s error even though `.[]` is empty; tjq's
   `iproduct!` had no pair to carry it. Fixed.
3. **Object construction discarded valid objects when a later combination
   errored.** `{k:(1,error,3)}` should stream `{k:1}` then raise (so
   `{k:(1,error,3)}?` keeps `{k:1}`); tjq returned only the error. Rewritten
   to emit objects in product order, stopping at the first errored
   combination.

One oracle fix: the correlated-arrow soundness check (`solve_arrows`)
mis-correlates a binding's comma stream — `[100 as $y | ($y, .k)]` yields a
*mixed* array that escapes the homogeneous per-branch codomains it infers —
so programs containing a binding are now excluded from that check (the union
`tout` stays sound for them). All interpreter fixes have regression tests;
runs with bindings live are clean across seeds (0 divergences / soundness /
arrow / effect / panics).

## Round 9: `reduce` and `foreach`

The fold constructs, completing the execution-engine parity arc.
`foreach EXP as $x (INIT; UPDATE; EXTRACT)` was newly added end to end
(Filter variant, parser, interpreter, printer, generator, sound `any -> any`
inference); `reduce` existed but was wrong on two counts:

- **`reduce` folded with the *first* update value, not the last.**
  `reduce .[] as $x (0; .+$x, .-$x)` is `-6` in jq (last branch wins), not
  `6`. Fixed to thread the last update output.
- **An empty update errored instead of nulling the accumulator.** jq 1.7
  makes `reduce .[] as $x (0; empty)` yield `null`; tjq raised. Fixed.

`foreach` emits `EXTRACT` (identity if omitted) at each step, threading the
state as the last update output — so `foreach .[] as $x (0; .+$x)` is the
running sum `1, 3, 6`. Both match jq across streams, scans, and array
accumulation, with regression tests. Inference types both soundly as
`any -> any` (a precise fold fixpoint is out of scope), and since the update
references the bound `$x`, these programs are already excluded from the
correlated-arrow check.

Differential runs with `reduce`/`foreach` live are clean (0 divergences /
soundness / arrow / effect / panics). Timeouts tick up slightly (foreach can
emit a large stream that jq materializes past its budget); these are the
harness-skipped non-findings, and tjq's `MAX_STREAM_LEN` guard bounds its
side.

## Round 10: a bytecode compiler + backtracking VM

First working `tjq_exec::bytecode`: a compiler from `Filter` to a flat
instruction vector and a stack VM that backtracks over `Fork` choice points
(a value threads as the top of an operand stack; forks snapshot it and are
resumed to yield a filter's successive outputs). This is the substrate for
the eventual type-directed compilation/speed comparison against jq's own
bytecode engine.

Operator semantics are *shared* with the tree interpreter — `apply_binop`
was extracted so both engines evaluate binops through the same code and
cannot drift. The supported core is generator control flow: identity,
literals, pipe, comma, `.foo`/`.[expr]` indexing, `.[]` iteration,
arithmetic/comparison, short-circuiting `and`/`or` (compiled with
`JumpIf`/`ToBool`, not as cartesian binops), negation, if/then/else, array
construction (`[f]` via a `Collect` sub-run), and object construction
(restricted to single-output fields — a stream-valued field needs error
unwinding across an internal fork that the flat model does not yet express).
Unsupported constructs return `Err(Unsupported)` and are skipped.

Validation is a new differential harness (`examples/bytecheck`) comparing
the VM against the interpreter on generated programs: **0 divergences across
4 seeds × ~34k checks each** (~19% of programs compile to the core). Getting
there surfaced two engine facts worth recording:

- The **binop operand order** is right-outer/left-inner
  (`(1,2)+(10,20)` → `11,12,21,22`), so the VM emits the right operand first
  (making the left backtrack innermost).
- The eager interpreter over-produces *errors* relative to the lazy VM
  (`(a,b) % error` is `[ERR,ERR]` eagerly but `[ERR]` lazily, matching jq) —
  masked in the jq oracle by array-wrapping. The bytecheck comparison
  therefore compares Ok-value sequences exactly and only error *presence*.

**First speed numbers** (`examples/bytebench`, 2000 reps over `[0..1000]`).
The VM is **5–7× faster than the tree interpreter** (e.g. `.[] | (.*2)+1`:
155 ms vs 1066 ms), which alone justifies the engine. Against jq 1.7 —
running the work 2000× inside one jq process to amortize startup — the VM is
**~3× faster on simple maps** (`[.[]|.+1]`: ~117 ms vs jq's ~380 ms) and
comparable on object construction. Caveats: the harnesses differ; tjq uses
`f64` where jq 1.7 uses the slower decNumber library (so part of the gap is
representation, not engine); and the VM numbers even include a full input
clone per rep. The unoptimized part is the fork mechanism — every choice
point snapshots the whole operand stack — which is the first target for the
type-directed-compilation work (skip type checks and forks the inferred type
proves unnecessary).

## Round 11: a BDD representation for the type algebra

`tjq_semantics::bdd` is a reduced, ordered Binary Decision Diagram over type
*atoms* — the representation set-theoretic type systems (Frisch–Castagna
semantic subtyping, as in CDuce and the Elixir checker) use for the boolean
structure of types. A type is a boolean combination of atoms; the BDD makes
`union`/`intersection`/`negation` canonical (structural equality is semantic
equality) and, crucially, gives a **decidable emptiness test** — which is
what makes subtyping decidable (`a ≤ b` iff `a \ b` is empty).

The atoms are the base-type predicates (`null`, booleans, numbers, strings —
each "any" or a singleton — plus the coarse array/object kinds); `is_empty`
is exact over that fragment by testing a *separating* value set (complete for
the mentioned atoms), so it accounts for atom interdependencies the tree
`Shape` reasons about ad-hoc — e.g. `Number(1) ∧ ¬Number` reduces to ⊥ even
though its BDD is not structurally the bottom leaf. Four property tests
(20k cases each) pin it down: boolean-algebra laws and canonicity on the
`holds` denotation, `from_shape` agreeing with `Shape::check` on the base
fragment, and `is_empty`/`subtype` agreeing with brute-force ground truth.

Container-recursive atoms (products/records carrying sub-types) that full
semantic subtyping needs, and swapping the constraint solver's ad-hoc
`included_in`/`disjoint_with` over to the BDD, are the next steps.
