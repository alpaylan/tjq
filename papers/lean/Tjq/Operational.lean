import Tjq.Syntax
import Tjq.Subtyping
import Tjq.Typing
import Tjq.Semantics

/-!
# Tjq.Operational

Big-step operational semantics for the core fragment of `Filter`, plus
the **type-soundness theorem statement**:

> If `HasType f (A → S)` and the input `j` inhabits `A`, then evaluating
> `f` on `j` succeeds with a sequence of values matching `S`.

This is the "real" payoff of the type system: well-typed programs don't
get stuck — they don't hit the runtime errors jq throws (`Cannot index
number with string "a"`, etc.).

## Outcome

A jq filter either:

* **succeeds** with a finite list of output values (`Outcome.stream js`), or
* **errors** (`Outcome.error`) — for example, `.foo` on a number,
  `.[n]` on a string, `1 + "x"`.

Type soundness says: well-typed filters at correctly-typed inputs never
take the `.error` branch.

## What's here

* `Outcome` and a `mutual` block defining `Eval : Filter → Json →
  Outcome → Prop` together with a helper `EvalEach` for "running `g`
  pointwise over a list of inputs (used by `pipe`)". The error cases
  for shape-sensitive constructors (`objIndex`, `arrIndex`, `iter`)
  are explicit so type soundness has something to rule out.
* The headline `type_soundness` theorem statement (sorried), plus
  three corollaries: `no_stuck`, `emitted_values_well_typed`, and a
  README-specific instance.

The proofs are all `sorry`s. A complete proof is multi-day Lean work —
induction over `HasType`, with each rule case-analysing every possible
`Eval` derivation and ruling out the error branches given the input
type's constraints.
-/

namespace Tjq

/-- The outcome of evaluating a filter. -/
inductive Outcome : Type where
  /-- Successful evaluation produces a finite sequence of output values. -/
  | stream : List Json → Outcome
  /-- Runtime error (jq's `Cannot index ...`, type mismatch, etc.). -/
  | error  : Outcome

/-! ## Big-step evaluation

    `Eval f j out` says "running filter `f` on input `j` produces `out`".

    `EvalEach g js out` says "running `g` on each element of `js` and
    concatenating the results produces `out`". Used to define `pipe`. -/
mutual

inductive Eval : Filter → Json → Outcome → Prop where
  -- ── Identity ──
  | dot {j} : Eval .dot j (.stream [j])

  -- ── Value literals ──
  | nullLit {j} : Eval .nullLit j (.stream [.nul])
  | boolLit {j b} : Eval (.boolLit b) j (.stream [.boo b])
  | numLit  {j n} : Eval (.numLit n) j (.stream [.num n])
  | strLit  {j s} : Eval (.strLit s) j (.stream [.str s])

  -- ── Empty / error ──
  | empty {j} : Eval .empty j (.stream [])
  | error {j} : Eval .error j .error

  -- ── Comma ──
  | comma_ok {f g j js₁ js₂} :
      Eval f j (.stream js₁) → Eval g j (.stream js₂) →
      Eval (.comma f g) j (.stream (js₁ ++ js₂))
  | comma_err_l {f g j} :
      Eval f j .error → Eval (.comma f g) j .error
  | comma_err_r {f g j js₁} :
      Eval f j (.stream js₁) → Eval g j .error →
      Eval (.comma f g) j .error

  -- ── Pipe (using EvalEach helper) ──
  | pipe_ok {f g j js_f out} :
      Eval f j (.stream js_f) → EvalEach g js_f out →
      Eval (.pipe f g) j out
  | pipe_err {f g j} :
      Eval f j .error → Eval (.pipe f g) j .error

  -- ── Object index ──
  | objIndex_obj_present {k v kvs} :
      kvs.lookup k = some v →
      Eval (.objIndex k) (.obj kvs) (.stream [v])
  | objIndex_obj_absent {k kvs} :
      kvs.lookup k = none →
      Eval (.objIndex k) (.obj kvs) (.stream [.nul])
  | objIndex_nul {k} :
      Eval (.objIndex k) .nul (.stream [.nul])
  | objIndex_err_boo {k b}    : Eval (.objIndex k) (.boo b) .error
  | objIndex_err_num {k n}    : Eval (.objIndex k) (.num n) .error
  | objIndex_err_str {k s}    : Eval (.objIndex k) (.str s) .error
  | objIndex_err_arr {k xs}   : Eval (.objIndex k) (.arr xs) .error

  -- ── Array index ──
  | arrIndex_arr {n xs}       : Eval (.arrIndex n) (.arr xs) (.stream [.nul])
  -- (Simplified: the actual rule splits on whether `n` is in range.
  --  For type soundness purposes — ruling out errors — this suffices.)
  | arrIndex_nul {n}          : Eval (.arrIndex n) .nul (.stream [.nul])
  | arrIndex_err_boo {n b}    : Eval (.arrIndex n) (.boo b) .error
  | arrIndex_err_num {n m}    : Eval (.arrIndex n) (.num m) .error
  | arrIndex_err_str {n s}    : Eval (.arrIndex n) (.str s) .error
  | arrIndex_err_obj {n kvs}  : Eval (.arrIndex n) (.obj kvs) .error

  -- ── Iter ──
  | iter_arr {xs}             : Eval .iter (.arr xs) (.stream xs)
  | iter_obj {kvs}            : Eval .iter (.obj kvs) (.stream (kvs.map Prod.snd))
  | iter_err_nul              : Eval .iter .nul .error
  | iter_err_boo {b}          : Eval .iter (.boo b) .error
  | iter_err_num {n}          : Eval .iter (.num n) .error
  | iter_err_str {s}          : Eval .iter (.str s) .error

  -- ── If-then-else (selected rules) ──
  | if_true {c t e j out_t} :
      Eval c j (.stream [.boo true]) → Eval t j out_t →
      Eval (.ifThenElse c t e) j out_t
  | if_false {c t e j out_e} :
      Eval c j (.stream [.boo false]) → Eval e j out_e →
      Eval (.ifThenElse c t e) j out_e
  | if_err_c {c t e j} :
      Eval c j .error → Eval (.ifThenElse c t e) j .error

  -- ── Construction (placeholder rules) ──
  -- Detailed rules for `array`/`object` constructors elided for brevity;
  -- they would unfold each value-filter's evaluation and bundle into
  -- the resulting JSON value. For type-soundness purposes this is a TODO.
  | array_ok {fs j collected} :
      Eval (.array fs) j (.stream [.arr collected])
  | object_ok {kvs j out_obj} :
      Eval (.object kvs) j (.stream [.obj out_obj])

  -- ── Binop / unop placeholders ──
  -- A complete model would define per-op semantics and the corresponding
  -- error cases. For v1 we expose that they evaluate but don't constrain
  -- to what.
  | binop_ok {op l r j out} : Eval (.binop op l r) j out
  | unop_ok {op f j out}    : Eval (.unop op f) j out

  -- ── Calls ──
  -- Without a definition context, calls have no concrete reduction. A
  -- working semantics threads `Defs : List (String × Filter)`.
  | call_ok {name j out} : Eval (.call name) j out

inductive EvalEach : Filter → List Json → Outcome → Prop where
  | nil  {f}      : EvalEach f [] (.stream [])
  | cons_ok {f x xs js_x js_rest} :
      Eval f x (.stream js_x) →
      EvalEach f xs (.stream js_rest) →
      EvalEach f (x :: xs) (.stream (js_x ++ js_rest))
  | cons_err_l {f x xs} :
      Eval f x .error → EvalEach f (x :: xs) .error
  | cons_err_r {f x xs js_x} :
      Eval f x (.stream js_x) → EvalEach f xs .error →
      EvalEach f (x :: xs) .error

end -- mutual

/-! ## Type-soundness theorems (statements) -/

/-- **Type soundness** (statement): if `f` has type `A → S` and the input
    `j` inhabits `A`, then `f` evaluates to a stream (no error), and
    that stream matches `S` via `Trace`. -/
theorem type_soundness {f : Filter} {A : Ty} {S : Stream}
    (h_ty : HasType f (.arr A S)) {j : Json} (h_val : Value j A) :
    ∃ js, Eval f j (.stream js) ∧ Trace S js := by
  sorry

/-- Corollary: well-typed filters never take the `.error` branch. -/
theorem no_stuck {f : Filter} {A : Ty} {S : Stream}
    (h_ty : HasType f (.arr A S)) {j : Json} (h_val : Value j A) :
    ¬ Eval f j .error := by
  sorry

/-- Corollary: every value emitted by a well-typed filter inhabits the
    filter's codomain element type. -/
theorem emitted_values_well_typed {f : Filter} {A : Ty} {S : Stream}
    (h_ty : HasType f (.arr A S)) {j : Json} (h_val : Value j A)
    {js : List Json} (h_eval : Eval f j (.stream js)) :
    ∀ x, x ∈ js → Value x (elemType S) := by
  sorry

/-! ## Selected per-filter soundness lemmas (statements) -/

/-- `.foo` doesn't error when input is `Null ∪ Object{foo: ?}`. -/
theorem soundness_objIndex_no_error {k : String} {α : Nat} {j : Json}
    (h : Value j (.union (.sh .nul) (.sh (.object [(k, .tvar α)])))) :
    ¬ Eval (.objIndex k) j .error := by
  intro h_eval
  -- The four error constructors `objIndex_err_{boo,num,str,arr}` each
  -- force `j` to be a non-null, non-object shape. The premise `h` says
  -- `j` is either null (`union_l`) or an object (`union_r`), forcing
  -- a constructor mismatch in either branch.
  cases h_eval <;>
    · cases h with
      | union_l hl => cases hl with | sh hsh => cases hsh
      | union_r hr => cases hr with | sh hsh => cases hsh

/-- `.[]` doesn't error when input is `Array(α, ?)`. The `Eval.iter_err_*`
    branches all require `j` to be a non-array, contradicting `Value j
    (.sh (.array ...))`. -/
theorem soundness_iter_no_error {α : Nat} {j : Json}
    (h : Value j (.sh (.array (.tvar α) none))) :
    ¬ Eval .iter j .error := by
  intro h_eval
  -- Case-split on Eval. Only the `iter_err_*` constructors can produce
  -- `Eval .iter j .error`; each forces `j` to a non-array shape.
  -- `Value j (.sh (.array ...))` forces `j = .arr xs`, contradicting each.
  cases h_eval <;>
    · cases h with
      | sh hsh => cases hsh

/-- The README's headline filter `.[] | .age, .name | {v: .a}` doesn't
    error on inputs of its inferred shape. -/
theorem soundness_readme_no_error
    {τ : Nat} {j : Json}
    (h : Value j (.sh (.array
                        (.sh (.object [
                          ("age",  .union (.sh .nul) (.sh (.object [("a", .tvar τ)]))),
                          ("name", .union (.sh .nul) (.sh (.object [("a", .tvar τ)])))]))
                        none))) :
    ¬ Eval (.pipe .iter
              (.pipe (.comma (.objIndex "age") (.objIndex "name"))
                     (.object [("v", .objIndex "a")]))) j .error := by
  -- Direct corollary of `no_stuck` once HasType for the README example
  -- is in scope (Examples §15).
  sorry

end Tjq
