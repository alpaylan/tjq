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
