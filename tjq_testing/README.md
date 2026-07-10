# tjq_testing — generative validation of the tjq type system

Implements the testing plan of `docs/type-system-scope.md` §11 in four
layers, cheapest first. Everything is seeded and dependency-free; every
failure reproduces from the seed in its report.

## Layer 1 — model tests (`tests/model.rs`)

`Shape::check` is the *denotation* of a shape: `denotes(s, j)` says whether
the JSON value `j` inhabits the set `s` describes. Every algebraic
operation is property-tested against it:

| Law | Operation |
| --- | --------- |
| boolean algebra | `Union`/`Intersection`/`Neg` vs `\|\|`/`&&`/`!` on denotations |
| denotation preservation | `canonicalize` |
| inclusion soundness | `included_in` (standard ⊆), `disjoint_with` (∅ overlap) |
| inverted-convention claims | legacy `subtype` (`Subtype` ⇒ ⊇, `Supertype` ⇒ ⊂) |
| emptiness | intersections canonicalizing to `Mismatch` |

Soak with: `TJQ_MODEL_SEED=n TJQ_MODEL_CASES=100000 cargo test -p tjq_testing --release`

## Layer 2 — inhabitants (`inhabit.rs`, `tests/inhabit.rs`)

`inhabit(shape, rng)` is the partial inverse of `check`: it generates JSON
values satisfying a shape (post-checked, so a `Some` answer is always
correct). `mutate` produces one-edit near-misses; the callers keep the
mutants that escape the shape as non-satisfying inputs. Properties:
correctness by construction, inhabitation rate > 80%, escape rate > 35%.

## Layer 3 — program generation (`filtergen.rs`, `tests/filtergen.rs`)

Size-budgeted random `Filter` ASTs over the constructs the inference
targets, printed with a precedence-safe printer (`Filter`'s `Display` is
not source-faithful: it drops parentheses and string quotes). Validated by
the print→parse→print fixpoint property.

## Layer 4 — differential oracle (`difftest.rs`)

```
cargo run -p tjq_testing --release --bin difftest -- \
  --iters 20000 --seed 1 --depth 3 --inputs 4 \
  --jq jq --findings target/findings.jsonl
```

Per program: infer `tin`/`tout` (inference panics are caught and counted —
they are findings, not crashes), generate satisfying inputs from `tin` and
escaped mutants, run the pinned `jq` binary on each input, and check:

- **HARD — soundness**: when jq succeeds on a satisfying input, every
  output must inhabit `tout`. Violations are bugs by definition.
- **METRIC — effect gap**: satisfying inputs on which jq errors. Not a
  failure until the failure effect lands in arrow types (RFC §2).
- **METRIC — exactness**: escaped mutants jq nevertheless accepts (jq's
  leniency; RFC §5).
- **DIFFERENTIAL**: `tjq_exec` must agree with jq output-for-output
  (object key order insensitive — serde_json sorts keys, jq does not).
- **CRASH HUNT**: jq exiting on a signal, hanging past 2s, or emitting
  unparseable output is always reported.

The inference runs with `TypeOptions { lenient_absence: true }` — the
oracle judges against jq's *actual* semantics; strict mode is a lint layer
evaluated separately (RFC §5 principle 1).

Findings are JSONL: `{kind, seed, program, input, ...}`; rerun any seed
with `--iters 1 --seed <seed>` to reproduce.

## Scorecard

The rig found and drove the fixes for (see git history):

- shape algebra: union absorption inverted, De Morgan for `Neg(Union)`
  wrong, `Neg(Mismatch)` wrong, `subtype` Union/Intersection arms unsound,
  `(Tuple, Array)` todo!(), (all via layer 1);
- parser: `f, g` parsed as `f | g` (layer 3 round-trip);
- inference soundness: dropped unresolved conditional/overload branches,
  negation preserving the singleton sign, `Comma`/multi-output constant
  programs typed as first output only, truthiness modeled as `== true`,
  `.a` on null not modeled (now behind `TypeOptions::lenient_absence`);
- interpreter vs jq: truthiness (`if -2`), `%` not truncating and NaN on
  zero divisor, `/` by zero, `null + x`, object `+` left-biased, object
  `*` (recursive merge) missing, string `/` (split) missing, array `*`
  wrongly supported (jq rejects it), binop stream product iterated in the
  wrong order (jq is right-outer), `.a` on null, native `length` (was an
  arrays-only stub);
- robustness: cyclic type-variable equalities (e.g. from `,`) made the
  solver's transitive-closure loops grow shapes without bound — stack
  overflow, found by the depth-5 shard; fixed with occurs-checks and
  bounded closure;
- inference coverage: `<=` was a `todo!()` (5% of generated programs
  panicked the inference);
- typing: `[f]` was typed as a fixed tuple even for stream-valued `f`
  (`[.[]]`); now widens to an array unless every element is provably
  single-output;
- round 2 (expanded surface): `and`/`or` short-circuit and stream order in
  the interpreter; constant subprograms evaluated with empty builtin
  definitions (dropped outputs from types); overload branches wrongly
  pruned for union-typed operands (non-disjointness is the sound test);
  `jq -c -- <program>` needed for programs starting with `-`.

Native builtin signatures (§9): `length`, `type`, `keys`, `floor`,
`tostring`, and `tonumber` are axiomatized in the inference (the defs.jq
`length` is an arrays-only stub; the interpreter implements the natives).
The v1 failure effect (`cannot_fail`) makes "this program never errors" a
checked hard claim for ~20% of generated programs.

## Shrinking and input profiles (round 3)

Every finding is emitted with `shrunk_program`/`shrunk_input`: a greedy
delta-debugging pass (`shrink.rs`) minimizes both sides while re-checking
the finding's own failure predicate. Inputs are generated under mixed
`SizeProfile`s — Small (collision-friendly default), Large (wide
containers), Deep (≤100 nesting, under jq's and serde's parser limits),
and Degenerate (boundary doubles, long/unicode/escape-heavy strings).

Round-3 catches (all fixed): interpreter object-construction keys kept
their quote characters (also the root cause of two long-standing
known-issue tests); `.[i]` on null errored (jq yields null); infinite
arithmetic results weren't clamped to max-double (jq clamps); `+`'s
inference overloads lacked arrays/objects; the conditional-path
disjunction handling dropped branches that don't pin their variables
down (nested-if soundness); and two *oracle* bugs — a stdin/stdout pipe
deadlock masquerading as jq timeouts, and serde_json's default lossy
float parsing (up to 1 ULP) fabricating divergences until
`float_roundtrip` was enabled.

## Correlation-aware (arrow) soundness

The union `tout` is a sound but coarse codomain: it can't distinguish a
correct overloaded type from one with swapped correlation (e.g. slice typed
as `string -> [E]`). The oracle additionally checks the **intersection-of-
arrows** view from `solve_arrows` — `(D1 -> C1) & (D2 -> C2) & …`: for each
satisfying input it verifies the output lies in the union of codomains
whose domain the input actually inhabits. Conservative (only fires when the
applicable codomains are fully concrete), and gated to overloaded programs
so single-arrow programs skip the second solve. This is what makes the
`. + .` type `(number->number) & (string->string) & … & (null->null)`
checkable branch-by-branch rather than as one flattened union.

Container access is typed precisely as such intersections: `.a`, `.[i]`,
`.[]` (arrays *and* objects), and `.[a:b]` all produce the right per-kind
arrows, with lenient-null branches where jq is lenient.

## Number formatting: jq is provenance-dependent

jq formats the same double differently by origin: a preserved/identity
value prints `1E+308` (uppercase) but a *computed* one prints `1e+308`
(lowercase) — `1e308 | .` vs `1e308 | length` differ in case. tjq stores an
`f64` with no provenance, so it cannot match both. The differential oracle
absorbs this in `json_equal`: two differing output strings that both parse
as JSON of equal value are treated as equal (`"[5e-324]"` == `"[5E-324]"`),
which normalizes formatting without masking value differences.

Two jq behaviors to be aware of (neither a bug):

- **Input-size sensitivity**: `n | length * type` evaluates to
  `n * "number"`, so a large data value becomes a string-repetition
  count. The campaign's timeouts are all this shape — jq doing exactly
  what it was told with a big count, killed by the 2s limit.
- **Formatting by provenance**: jq prints the same double differently
  depending on origin — `1e300 | tostring` gives `"1E+300"` (preserved
  literal) but computed values print `"1e+300"`. tjq matches the
  preserved style, so extreme computed `tostring` results can diverge.
