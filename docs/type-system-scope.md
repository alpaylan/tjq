# RFC: Scoping the tjq Type System

Status: draft, round 2. This document fixes the scope and the load-bearing
design decisions of the tjq type system before we invest in the three large
workstreams: (1) large-scale generative testing, (2) a typed port of jq's
bytecode engine, (3) production-quality inference.

The system has three consumers with different needs, and most knobs below
exist because of that tension:

| Consumer          | Wants                                          |
| ----------------- | ---------------------------------------------- |
| Error messages    | Precision, provenance/blame, tolerance         |
| Testing oracle    | Exact domains, faithfulness to jq semantics    |
| Bytecode/codegen  | Widened, stable types; branch-specific arrows  |

## 1. Semantic foundation: set-theoretic types

**Decision.** Every `Shape` denotes a set of JSON values. Subtyping is set
inclusion; union/intersection/negation are the set operations. This is the
Frisch–Castagna–Benzaken semantic-subtyping framework, and it is already
implicitly present in the code (`Union`, `Intersection`, `Neg`, `Arrow`).

Consequences:

- `Shape::check(json)` *is* the denotation function. Every algebraic law in
  `canonicalize`/`subtype` must be validated against it:
  `a.subtype(b) == Subtype` must imply `∀j. check(a,j) ⇒ check(b,j)`.
  This property test is cheap and should land before any new solver work.
- The tri-state `subtype()` (`Subtype`/`Supertype`/`Incompatible`, with its
  reversed-perspective convention) is replaced over time by two primitives:
  `is_subtype(a, b)` and `is_empty(a & b)`. All current call sites conflate
  these.
- The constraint solver in `experimental_type_inference.rs` is converging on
  *tallying* (constraint solving for set-theoretic types with variables,
  Castagna et al. POPL '14/'15). New solver features should be checked
  against that framework rather than invented; the backward propagation
  through conditionals is occurrence typing (POPL '22 treatment).
- Long term, the syntactic match-arm representation gets replaced by the
  standard BDD-based representation (as in CDuce and the Elixir checker);
  that is what makes negation and recursive types tractable.

## 2. Filters are stream transformers with a failure effect

jq filters are not `Json -> Json`; they are `Json -> Stream<Json>` and can
abort with an error. A single `tin/tout` model cannot express `empty`, `,`,
`.[]`, or the difference between "returns null" and "errors".

**Decision.** A filter type is an intersection of arrows
`(I₁ -> O₁) & (I₂ -> O₂) & …` (as `solve_arrows` builds today), where each
codomain `O` is a *stream type*: an output value type plus a cardinality
and a failure flag.

- **Cardinalities**: `0`, `1`, `0..1`, `0..n` (lattice: `1 <: 0..1 <:
  0..n`, `0 <: 0..1`). Regular-expression types over sequences
  (XDuce/XQuery) are the known ceiling; we start with cardinalities and
  upgrade only if `first`/`limit`/`until` prove them too coarse.
- **Failure is an effect marker on the codomain**, not a value type. The
  oracle property "inputs outside the domain fail" is a statement about
  it. `error` has codomain `⊥ !`; `empty` has cardinality `0`. `?`/`try`
  *catch* the effect and turn it into cardinality `0`; `try f catch g`
  reroutes it into `g`. `//` does **not** catch errors (verified,
  jq 1.7.1): `a // b` redirects only null/false *outputs*; wrong-kind
  errors pass through.
- Cardinalities also feed codegen directly: a provably single-output
  filter compiles to straight-line code instead of a stream loop (§12).

`Mismatch` currently plays both "empty type" and "error report". These
split: empty type (`⊥`) is a Shape; failure is an effect; the *report*
(expected/got/path, for error messages) is diagnostic metadata carried by
the checker, not by the type.

## 3. Numbers: integer refinement

jq has one number type (IEEE double), but integerness is worth tracking as
a refinement: `Singleton(n) <: Int <: Number` (a singleton is below `Int`
iff its value is integral).

**Decision.** Track `Int`. It is sound and cheap:

- **Soundness under IEEE doubles.** `Int` is closed under `+`, `-`, `*`:
  every double ≥ 2⁵³ is integral, so rounding an integer-valued result
  always yields an integer-valued double. Not closed under `/` (`1/2 =
  0.5`). jq's `%` truncates *both operands* to ints (verified:
  `5 % 2.9 == 1`), so `% : (number, number) -> Int`.
- **Producers**: `length`, `floor`/`ceil`/`round`, `%`, `tojson|fromjson`
  index builtins (`indices`), `range` over int arguments.
- **Consumer sites coerce, they don't error** (verified, jq 1.7.1):
  `.[1.5]` and `.[1.9]` both index element 1 (truncation); `"ab" * 2.7`
  is `"abab"`; `limit(1.9; …)` takes 2. So int-expecting sites accept
  `number` in the dynamic semantics, and integerness at those sites is a
  *lint*, not a soundness requirement: rule `frac-int-site` (§5) flags a
  provably-fractional value at an index/repeat/limit position.
- Payoffs: array-index reasoning, `range` output types, and unboxed
  integer arithmetic in codegen (§12), where knowing `Int` avoids the
  float path entirely.

## 4. Knob 1 — widening: promote vs. aggregate

Both joins of `"one"` and `"other"` exist in the code today: the possibility
path aggregates (`"one" | "other"`), `canonicalize`/`compute_lub` promote
(`string`). This becomes one policy instead of an accident.

**Decision.** Aggregation is the semantics; promotion is a *widening* — a
sound, deliberate over-approximation applied at joins.

- `Widening::Exact` — only lossless promotions (`true | false -> bool`;
  a union covering all of a finite kind). Nothing else is widened.
- `Widening::Budget(k)` — keep unions of singletons of a kind up to size
  `k`; promote to the base kind beyond. Default mode, `k ≈ 8`.
  With §3, singleton numbers promote to `Int` when all are integral,
  else to `Number`.
- `Widening::Eager` — promote singletons of the same kind at every join.
  For codegen and for fixpoint acceleration in recursive filters.

Notes: widening is also the termination story for recursive-filter
inference (growing singleton unions must eventually widen). Precision
(error messages, oracle) and stability (bytecode) pull in opposite
directions; that is why this is a knob and not a constant.

## 5. Knob 2 — leniency rules (`strict` vs `default`)

jq is lenient about *absence* and about *coercible numbers*, but not about
kind. Verified against jq 1.7.1:

| Expression        | on `{}`   | on `null` | on `1` / `[]` (wrong kind) |
| ----------------- | --------- | --------- | -------------------------- |
| `.a`              | `null`    | `null`    | error                      |
| `.[0]`            | error     | `null`    | error / `null` (OOB)       |
| `.[]`             | empty     | **error** | error                      |
| `.a?`             | `null`    | `null`    | empty (error suppressed)   |
| `null + 1`        | —         | `1`       | —                          |

And about truthiness: only `null` and `false` are falsy; `0` and `""` are
truthy (`if 0 then A else B end` takes `A`).

**Decision.** Strictness is a *preset over per-rule severities*
(`allow | warn | deny`), clippy-style — not a boolean:

| Rule                      | Flags                                        | `default` | `strict` |
| ------------------------- | -------------------------------------------- | --------- | -------- |
| `absent-field`            | `.a` when field provably absent              | allow     | deny     |
| `null-index`              | `.a` / `.[0]` on provably-null input         | allow     | deny     |
| `oob-index`               | `.[i]` provably out of bounds (known tuples) | allow     | deny     |
| `null-arith`              | `null + x` absorption                        | allow     | **warn** |
| `frac-int-site`           | provably-fractional number at an int site    | allow     | deny     |
| `dead-condition-branch`   | condition provably always/never truthy       | allow     | deny     |

Principles:

1. **Acceptance, not semantics.** Runtime behavior is always jq's;
   `strict` only rejects statically. The testing oracle therefore always
   runs against `default` typing; `strict` is evaluated separately, as
   precision/recall of its rejections over corpora (§11).
2. **Only provable badness is deniable.** `{} | .a` is provably absent →
   deny. `.a` on an open object with optional `a` types as `T | null` in
   *both* modes — the honest type — and misuse is caught downstream.
   Indexing an unknown-length array is inherent possible-absence and
   types `T | null` in both modes (the `noUncheckedIndexedAccess`
   treatment).
3. **Truthiness gating is dead-branch detection.** A condition whose type
   is disjoint from `null | false` is always-truthy (dead else); one below
   `null | false` is always-falsy (dead then). This needs no new
   machinery — it falls out of condition narrowing — and it naturally
   spares the idiom `if .a then …` (`.a`'s type includes `null`, so
   neither branch is dead) while catching `if 0 then …`.
4. **`null-arith` only warns even in `strict`**: denying it breaks the
   idiomatic `.counts.x += 1`-on-missing-key pattern, which composes
   `absent-field` with `null-arith`.
5. **Escape hatches stay well-typed under `strict`.** `.a?` and
   `.a // default` are the sanctioned acknowledgments of absence — noting
   they differ: `//` handles null-from-absence but not wrong-kind errors;
   `?` suppresses both.
6. **Implementation site: constraint generation, not the solver.** The
   leniencies are overload branches (exactly like `Add`'s null branches
   today), tagged with rule IDs; `strict` drops or flags those branches,
   shrinking arrow domains. Orthogonal to Knob 1 (solver/canonicalizer).

Why `strict` is worth having even though jq itself is lenient: absence-null
*pollutes* — one `.a` on a maybe-absent field threads `| null` through every
downstream type, degrading both precision and blame (the error surfaces far
from the access). `strict` keeps inferred contracts tight and blames the
access site.

## 6. Object model: rows with optionality and openness — IMPLEMENTED

**Decision.** Object types are rows: `Shape::Object(Row)` where
`Row { fields: Vec<Field>, open: bool }` and
`Field { key, value, optional }`. A JSON object inhabits the row iff every
required field is present and conforms, every optional field conforms when
present, and — for a closed row — it carries no key outside the field set.

Status (2026-07-10, landed): the representation, `check` denotation,
`included_in`/`disjoint_with`, `canonicalize`, and the legacy `subtype`
all honor optionality and openness, validated by the model tests
(`tjq_testing/tests/model.rs`) at 100k+ cases across the new dimensions.
Wired into inference:

- Object *construction* (`{a: 1}`) produces a **closed** row (exact keys).
- Field *access* (`.a`) demands an **open** row `{a: T, ..}`; under lenient
  semantics the field is **optional** (a missing key yields null, not an
  error), so `tin` correctly accepts objects lacking the key.
- `compute_lub` joins rows: shared keys join their value types, keys on one
  side become optional, openness joins to open.
- TypedDict (PEP 589) and Elixir map types are the precedents.

Measured impact: the differential exactness gap (mutated inputs jq
nevertheless accepts) fell from ~9% to ~1.8% — optional fields make the
inferred input domain close to jq's real acceptance set.

Still to build on this base: `.a?` typing, precise object `+`/`*` field
merge (the overloads currently type to "any object"), `has`/`del`.

## 7. Recursive types

JSON is inherently recursive (`μJ. null | bool | number | string | [J] |
{string: J}`), and `..`/`recurse`/`getpath`/`paths` need it.

**Decision.** Recursive types are in scope, but *after* the BDD
representation lands (§1) — syntactic match arms do not extend to μ-types,
while the semantic-subtyping algorithm (memoized coinductive emptiness) is
the standard, decidable recipe. Until then, tree-recursive builtins get
depth-bounded approximations with `Blob` fallback, and we accept the
imprecision.

## 8. Gradual boundary

`dynamic` lives only at the program's input boundary; inference is fully
static inside the pipeline (jq programs are small and closed — we are
better positioned than Elixir or Python here). Unresolved type variables
mean *polymorphism*, not "unknown"; the two must not be conflated.
If a real gradual dimension becomes necessary (e.g. FFI-like builtins),
the framework is Castagna–Lanvin (ICFP '17), which composes with §1.

## 9. Builtin signatures: inferred, with a native floor

**Decision.** Everything with a jq-level definition (`defs.jq`, jq's own
standard library, which defines most builtins in jq itself) gets its type
*inferred*, not hand-written — the stdlib doubles as a stress test for the
inference. Hand-written arrow signatures exist only where inference is
impossible or not worth it:

- **Native builtins** (implemented in C in jq: `length`, `keys`, `type`,
  `test`/`capture` and the regex family, `tostring`, …) have no jq source
  to infer from; they need axiomatic signatures. This floor is
  unavoidable.
- **Curated overrides** are permitted where an inferred type is correct
  but a hand-written one gives materially better error messages; each
  override needs a comment justifying it, and the inferred type is still
  computed and checked to be a supertype (the override must not claim
  more than inference can verify).

## 10. Non-goals (v1)

- **Path/lvalue typing**: `.a = 1`, `.a |= f`, `del`, `setpath` — typing
  updates is a lens/optics problem; explicitly deferred. (`+=` desugars to
  an update; v1 treats update operators as untyped pass-through.)
- **Regex/format builtins precision**: `test`, `capture`, `@csv`, … get
  coarse axiomatic types (§9).
- **User type annotations**: none. Inference-only.
- **SQL-ish builtins, modules/imports, `$ENV`/inputs**: coarse types.

## 11. Testing plan (workstream 1)

Layered, cheapest first:

1. **Model-test the algebra** (now): random Shapes + random JSON;
   `check` as the model for `subtype`, `canonicalize`, `intersection`
   emptiness. Kills bugs in the foundation everything else trusts.
2. **Generate programs over the `Filter` AST** (not text): size-budgeted,
   type-agnostic generation; pretty-print for the oracle; shrink on the
   AST.
3. **Generate inputs from inferred input shapes**: an inhabitant generator
   `gen : Shape -> Json` (inverse of `check`), plus mutation of inhabitants
   for near-miss non-inhabitants (flip a leaf kind, drop a required field,
   make an integer fractional).
4. **Differential oracle against the real `jq` binary** (pin the version —
   1.7 changed semantics, e.g. `"ab" * 0`: `null` → `""`):
   - *Hard property (soundness)*: satisfying inputs never produce a type
     error at runtime, and every output inhabits the output type.
   - *Measured property (exactness)*: non-satisfying inputs should fail;
     violations are a precision metric, not test failures. jq's leniency
     and `try`-like constructs make universal exactness unattainable.
5. **Corpora**: jq's own test suite as seed programs with known outputs;
   real-world jq one-liners for `strict`-mode precision/recall.

## 12. Bytecode (workstream 3) — de-risk first

The type-directed wins (monomorphizing `+` to a single surviving arrow,
unboxing numbers — and with §3, unboxed *integer* arithmetic — deleting
absence-null branches, specializing `.a` access paths) all consume the
*arrow* view. Before porting jq's bytecode engine, validate the thesis
cheaply: specialize `tjq_exec` with inferred types and benchmark against
`jq` on its benchmark programs. If types don't win in the interpreter,
they won't win in bytecode.

## 13. Configuration surface

```rust
pub struct TypeConfig {
    pub widening: Widening,          // Exact | Budget(usize) | Eager
    pub lints: LintLevels,           // per-rule Allow | Warn | Deny (§5)
}
// presets: TypeConfig::default(), TypeConfig::strict()
```

Threaded into constraint generation (lint rules) and the
solver/canonicalizer (widening). CLI: `--strict`, `--widen=<mode>`,
`--allow/--warn/--deny <rule>`. LSP: workspace settings mapping to the
same struct.

## 14. Reading list

- Frisch, Castagna, Benzaken — *Semantic Subtyping* (JACM 2008): the model,
  the BDD algorithmics.
- Castagna et al. — *Polymorphic Functions with Set-Theoretic Types*,
  parts 1–2 (POPL '14/'15): tallying = our constraint solving.
- Castagna et al. — *On Type-Cases, Union Elimination, and Occurrence
  Typing* (POPL '22): our conditional narrowing / backward propagation.
- Castagna, Duboc, Valim — the Elixir type-system design papers:
  pragmatics of retrofitting set-theoretic types, union budgets, dynamic.
- Castagna, Lanvin — *Gradual Typing with Union and Intersection Types*
  (ICFP '17): only if §8 grows.
- Siek, Vitousek, Cimini, Boyland — *Refined Criteria for Gradual Typing*
  (SNAPL '15); PEP 586 (literals), PEP 589 (TypedDict).
- Hosoya, Pierce — XDuce regular-expression types: streams, if
  cardinalities prove too coarse.
- Färber's formal-semantics work on jq (jaq): candidate core calculus, so
  we type a spec rather than jq-the-implementation.

## 15. Open questions

1. Stream cardinalities: are the four (`0`, `1`, `0..1`, `0..n`) enough,
   or do `first`/`limit`/`until` force sequence-regex types earlier than
   we'd like?
2. Arrow inference cap (`MAX_COMBINATIONS = 64` in `solve_arrows`): the
   approach is ratified, but what is the principled fallback beyond the
   cap — widen per-disjunction instead of falling back to the global
   union-based solution?
3. `Int` and the oracle: jq silently truncates at int sites, so the
   inferred type of `.[i]` accepts `number` — should the *exactness*
   metric treat fractional indices as satisfying (they are, dynamically)
   even though `strict` flags them?
4. How aggressively should `dead-condition-branch` fire inside inferred
   `defs.jq` bodies (library code often guards defensively)? Likely:
   lints apply only to user code, not stdlib expansions.
