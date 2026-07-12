# Contributing the tjq type system to jq — audit & plan

Goal: land a static type/analysis pass in jq (upstream `jqlang/jq`) that is
**provably correct**. This document audits what we have, states the remaining
proof obligations honestly, and lays out a sequenced plan. It is meant to be
nudged/approved, not executed blind.

## 1. What exists, and how solid it is

### Rust implementation (`tjq_*`)
- A tree-walking interpreter (`tjq_exec::Filter::filter`) at **parity with jq**
  on the fuzzed + snapshot-tested subset (see `docs/testing-report-2026-07.md`,
  `tjq_testing/corpus`).
- Two inference implementations: `DirectInference` and the constraint-based
  `ConstraintInference` (`experimental_type_inference.rs`). The constraint
  solver is the one under active development.
- A BDD type algebra (`tjq_semantics::bdd`) with **decidable, brute-force-
  validated emptiness/subtyping** over the base fragment.

### Lean formalization (`papers/lean`, Lean 4 v4.30, no mathlib — builds fast)
- `Inference.lean` — an algorithmic `infer : Nat → Filter → Option (Ty × Nat)`
  producing **stream-arrow types** `A → S`, and **`infer_sound` is fully
  proven (0 `sorry`)**: `infer f = some (T, _) → HasType f T`. This is real,
  non-vacuous structural induction — the inference algorithm is sound *with
  respect to the declarative typing relation `HasType`*.
- `Syntax`, `Subtyping`, `Typing`, `Semantics`, `Operational` — definitions,
  the subtyping relation `TyLE`, a denotational value semantics `Value j T`,
  and an operational `Eval f j r`.

### The honest gaps (remaining `sorry`s)
1. **`Operational.type_soundness`** (the headline): `HasType f (A→S) →
   Value j A → ∃ js, Eval f j (stream js) ∧ Trace S js`. Typing ⇒ runtime
   safety. **Sorried; the file itself notes a full proof is multi-day work.**
   This is the link that makes "well-typed ⇒ doesn't go wrong" true.
2. **`Semantics.TyLE.sound` / `ShapeLE.sound`**: subtyping derivations are
   semantically sound (`t₁ ⊑ t₂ ⇒ ⟦t₁⟧ ⊆ ⟦t₂⟧`). Sorried.
3. **`Semantics.Value_negNNF_disjoint`** and the compound
   `ValueShape_NotValueShape_disjoint`: `j` cannot inhabit both `t` and its NNF
   negation. Foundational disjointness; unblocks the subtyping-soundness cases.

So: **inference→typing is proven; typing→semantics and subtyping→semantics are
not.** The end-to-end guarantee needs all three.

## 2. Alignment audit: does the proof describe the code we'd ship?

A proof is only worth porting if it models the actual algorithm. Findings:

- **The Lean `Filter` is a core subset** — dot, pipe, comma, `.k`, `.[n]`,
  `.[]`, literals, `[…]`, `{k: v}` (literal keys), `empty`, `error`,
  if/then/else, binop, unop, zero-arity `call`. It **lags the Rust
  implementation**, which now also has `try/catch`, `?`, `as $x` bindings,
  `reduce`, `foreach`, computed object keys, and higher-order calls (added this
  session). The formalized language ≈ the bytecode VM's subset.
- **Numbers are `Int` in Lean, `f64` in Rust/jq.** Fine for a type system that
  tracks number-*ness*, but any value-level claim (singletons, arithmetic
  overflow/clamp) is not modelled.
- **The Lean `infer` is a *direct* algorithm; the Rust `ConstraintInference`
  is a *constraint solver*.** They are two different algorithms. `infer_sound`
  covers the direct one. So the C port should implement the *Lean* algorithm
  (which is proven), not transcribe the Rust constraint solver.
- **The BDD (`tjq_semantics::bdd`) is a computational model of exactly the
  algebra the Semantics sorries reason about** (union/intersection/negation,
  `Value_negNNF_disjoint` is the complement law). It is validated but *not
  connected to the Lean proof*.

## 3. Plan (sequenced; each step gated on the previous)

### Phase A — make the formalization match a shippable algorithm
A1. Decide the **shippable subset** for a first jq contribution. Recommendation:
    the Lean core subset *plus* `try/?` (the failure-effect part is what makes a
    type system useful for jq users). Extend the Lean `Filter`, `Eval`, `infer`,
    and `HasType` to cover it, keeping `infer_sound` green.
A2. **Reconcile Lean `infer` with a Rust reference implementation.** Port the
    Lean `infer` verbatim to a `tjq_semantics::reference_infer` (direct
    algorithm), and differentially test it against the Lean semantics via the
    existing fuzzer (generate core programs, check `infer` ⇒ the runtime stays
    in the inferred type). This ties the proof to runnable code.

### Phase B — close the proof
B1. **`Value_negNNF_disjoint`** first (foundational; the BDD gives the
    intuition and a decision procedure to cross-check). Then
    `ValueShape_NotValueShape_disjoint`.
B2. **`TyLE.sound` / `ShapeLE.sound`** (subtyping soundness) using B1.
B3. **`type_soundness`** by induction on the typing derivation, using B1/B2 for
    the algebra and a substitution/preservation lemma for the arrow shape
    (`Properties.HasType.arrow_shape`, already partly there). This is the
    multi-day core; break it into per-constructor `soundness_*` lemmas
    (`Operational` already stubs `soundness_objIndex_no_error`,
    `soundness_iter_no_error`).
B4. Optionally: **connect the BDD to the Lean algebra** — prove the BDD
    `is_empty`/`subtype` agree with `TyLE`, giving a verified decision
    procedure (this is the reusable, citable artifact).

### Phase C — port to jq (C), only after B
C1. Fork `jqlang/jq`. Implement the *proven* `infer` as a C pass over jq's AST
    (`block`/`jv` bytecode is lower-level; work at the parsed AST in
    `parser.y`/`builtin`). Zero runtime cost when disabled.
C2. **Validate the C port against the Rust reference + the Lean algorithm**
    with the existing differential harness (feed the same programs, compare
    inferred types). A discrepancy is a port bug, not a soundness gap.
C3. Propose upstream as an optional `jq --check` / static analysis, with the
    Lean proof as the correctness argument.

## 4. Recommendation on avenue

**Lean-first, then C** (matches "the code must be impeccable"). The Lean work
(Phase B) is the hard, valuable part and de-risks the port; the C transcription
(Phase C) of a *proven, direct* algorithm is then mechanical and testable. The
"fork jq and port to C" avenue should not start before B2 at the earliest —
porting an unproven algorithm forfeits the whole point.

Concrete next actions (small, reviewable): A2 (reference `infer` in Rust +
differential test) and B1 (`Value_negNNF_disjoint`), since both are
self-contained and the BDD gives a cross-check for B1.

### Progress (2026-07)

- **B1 largely done.** `Value_negNNF_disjoint` and
  `ValueShape_NotValueShape_disjoint` are now a `mutual` well-founded recursion
  on the `Shape`/`Ty` size. `Value_negNNF_disjoint` is **fully proven**; the
  shape lemma is proven for all cases except the two list-element recursions
  (`tuple`/`object` `wrong_elem`), where only the *termination* obligation
  (`sizeOf` of a `List.zip`/`lookup` element) is unmet — the logic is identical
  to the closed `array` case. Semantics.lean sorries 6 → 4. Finishing those two
  termination goals unblocks B2 (`TyLE.sound`/`ShapeLE.sound`).
- The Lean source is now versioned (previously `papers/` was gitignored).
