import Tjq.Syntax
import Tjq.Subtyping
import Tjq.Typing

/-!
# Tjq.Inference

A type-inference algorithm for the core fragment of `Filter`, plus a
**correctness theorem** stating that every type the algorithm returns
is a valid `HasType` derivation.

## What the algorithm does

`infer : Nat → Filter → Option (Ty × Nat)` takes:
* a "fresh-tvar counter" (the next unused `tvar` index)

and returns either:
* `some (T, n')` — a type `T` for the filter, with `n'` the new counter, or
* `none` — if the filter requires constraint-solving the algorithm
  doesn't yet do (compositions: `comma`, `pipe`, `binop`, etc.).

For the trivial filters (identity, value literals, single-shape access,
iter, empty, error), the algorithm returns the canonical scheme — the
exact type assigned by the corresponding `HasType` constructor.

## What we prove

`infer_sound : (infer n f) = some (T, n') → HasType f T`

By induction on `Filter`. Each constructor's case maps directly to
the corresponding `HasType` constructor.

## What's deferred

Compositions need either constraint generation + solving, or HM-style
unification. Both are substantial; we leave them for a follow-up. The
file documents what the inference *would* do for those cases.
-/

namespace Tjq

/-- Generate a fresh type variable from a counter; bump the counter. -/
@[simp] def freshTVar (n : Nat) : Ty × Nat := (.tvar n, n + 1)

/-! ## Type-shape extractors

    The inference cases for `unop`, `binop`, etc. need to recognise
    arrow types whose codomain has a particular shape (e.g. `.yld (.sh
    (.num _))`). Rather than embed the deep pattern in `infer`'s `match`
    (which makes the soundness proof enumerate every other Ty/Stream
    combination), we factor the recognition into small predicates and
    prove the inversion lemma once. -/

/-- `asArrYld T = some (A, V)` iff `T = .arr A (.yld V)`. -/
def Ty.asArrYld : Ty → Option (Ty × Ty)
  | .arr A (.yld V) => some (A, V)
  | _               => none

theorem Ty.asArrYld_eq {T A V} :
    Ty.asArrYld T = some (A, V) → T = .arr A (.yld V) := by
  intro h
  cases T with
  | arr A' S =>
      cases S with
      | yld V' =>
          simp [Ty.asArrYld] at h
          obtain ⟨h1, h2⟩ := h
          subst h1; subst h2; rfl
      | eps        => simp [Ty.asArrYld] at h
      | bot        => simp [Ty.asArrYld] at h
      | concat _ _ => simp [Ty.asArrYld] at h
      | star _     => simp [Ty.asArrYld] at h
      | choice _ _ => simp [Ty.asArrYld] at h
      | svar _     => simp [Ty.asArrYld] at h
  | sh _      => simp [Ty.asArrYld] at h
  | inter _ _ => simp [Ty.asArrYld] at h
  | union _ _ => simp [Ty.asArrYld] at h
  | neg_sh _  => simp [Ty.asArrYld] at h
  | tvar _    => simp [Ty.asArrYld] at h

/-- `asArrYldNum T = some (A, m)` iff `T = .arr A (.yld (.sh (.num m)))`. -/
def Ty.asArrYldNum : Ty → Option (Ty × Option Int)
  | .arr A (.yld (.sh (.num m))) => some (A, m)
  | _                            => none

theorem Ty.asArrYldNum_eq {T A m} :
    Ty.asArrYldNum T = some (A, m) → T = .arr A (.yld (.sh (.num m))) := by
  intro h
  cases T with
  | arr A' S =>
      cases S with
      | yld V =>
          cases V with
          | sh sh' =>
              cases sh' with
              | num m' =>
                  simp [Ty.asArrYldNum] at h
                  obtain ⟨h1, h2⟩ := h
                  subst h1; subst h2; rfl
              | top        => simp [Ty.asArrYldNum] at h
              | nul        => simp [Ty.asArrYldNum] at h
              | boo _      => simp [Ty.asArrYldNum] at h
              | str _      => simp [Ty.asArrYldNum] at h
              | array _ _  => simp [Ty.asArrYldNum] at h
              | tuple _    => simp [Ty.asArrYldNum] at h
              | object _   => simp [Ty.asArrYldNum] at h
          | arr _ _    => simp [Ty.asArrYldNum] at h
          | inter _ _  => simp [Ty.asArrYldNum] at h
          | union _ _  => simp [Ty.asArrYldNum] at h
          | neg_sh _   => simp [Ty.asArrYldNum] at h
          | tvar _     => simp [Ty.asArrYldNum] at h
      | eps        => simp [Ty.asArrYldNum] at h
      | bot        => simp [Ty.asArrYldNum] at h
      | concat _ _ => simp [Ty.asArrYldNum] at h
      | star _     => simp [Ty.asArrYldNum] at h
      | choice _ _ => simp [Ty.asArrYldNum] at h
      | svar _     => simp [Ty.asArrYldNum] at h
  | sh _      => simp [Ty.asArrYldNum] at h
  | inter _ _ => simp [Ty.asArrYldNum] at h
  | union _ _ => simp [Ty.asArrYldNum] at h
  | neg_sh _  => simp [Ty.asArrYldNum] at h
  | tvar _    => simp [Ty.asArrYldNum] at h

/-- Subsume `.sh (.num m) ⊑ .sh (.num none)` regardless of `m`. -/
theorem TyLE.num_widen (m : Option Int) :
    TyLE (.sh (.num m)) (.sh (.num none)) := by
  cases m with
  | none   => exact TyLE.refl
  | some _ => exact TyLE.sh ShapeLE.num_some_to_none

/-- `asArrYldBoo T = some (A, b)` iff `T = .arr A (.yld (.sh (.boo b)))`. -/
def Ty.asArrYldBoo : Ty → Option (Ty × Option Bool)
  | .arr A (.yld (.sh (.boo b))) => some (A, b)
  | _                            => none

theorem Ty.asArrYldBoo_eq {T A b} :
    Ty.asArrYldBoo T = some (A, b) → T = .arr A (.yld (.sh (.boo b))) := by
  intro h
  cases T with
  | arr A' S =>
      cases S with
      | yld V =>
          cases V with
          | sh sh' =>
              cases sh' with
              | boo b' =>
                  simp [Ty.asArrYldBoo] at h
                  obtain ⟨h1, h2⟩ := h
                  subst h1; subst h2; rfl
              | top        => simp [Ty.asArrYldBoo] at h
              | nul        => simp [Ty.asArrYldBoo] at h
              | num _      => simp [Ty.asArrYldBoo] at h
              | str _      => simp [Ty.asArrYldBoo] at h
              | array _ _  => simp [Ty.asArrYldBoo] at h
              | tuple _    => simp [Ty.asArrYldBoo] at h
              | object _   => simp [Ty.asArrYldBoo] at h
          | arr _ _    => simp [Ty.asArrYldBoo] at h
          | inter _ _  => simp [Ty.asArrYldBoo] at h
          | union _ _  => simp [Ty.asArrYldBoo] at h
          | neg_sh _   => simp [Ty.asArrYldBoo] at h
          | tvar _     => simp [Ty.asArrYldBoo] at h
      | eps        => simp [Ty.asArrYldBoo] at h
      | bot        => simp [Ty.asArrYldBoo] at h
      | concat _ _ => simp [Ty.asArrYldBoo] at h
      | star _     => simp [Ty.asArrYldBoo] at h
      | choice _ _ => simp [Ty.asArrYldBoo] at h
      | svar _     => simp [Ty.asArrYldBoo] at h
  | sh _      => simp [Ty.asArrYldBoo] at h
  | inter _ _ => simp [Ty.asArrYldBoo] at h
  | union _ _ => simp [Ty.asArrYldBoo] at h
  | neg_sh _  => simp [Ty.asArrYldBoo] at h
  | tvar _    => simp [Ty.asArrYldBoo] at h

/-- Subsume `.sh (.boo b) ⊑ .sh (.boo none)` regardless of `b`. -/
theorem TyLE.boo_widen (b : Option Bool) :
    TyLE (.sh (.boo b)) (.sh (.boo none)) := by
  cases b with
  | none   => exact TyLE.refl
  | some _ => exact TyLE.sh ShapeLE.boo_some_to_none

/-- `asArr T = some (A, S)` iff `T = .arr A S`. -/
def Ty.asArr : Ty → Option (Ty × Stream)
  | .arr A S => some (A, S)
  | _        => none

theorem Ty.asArr_eq {T A S} : Ty.asArr T = some (A, S) → T = .arr A S := by
  intro h
  cases T with
  | arr A' S' =>
      simp [Ty.asArr] at h
      obtain ⟨h1, h2⟩ := h
      subst h1; subst h2; rfl
  | sh _      => simp [Ty.asArr] at h
  | inter _ _ => simp [Ty.asArr] at h
  | union _ _ => simp [Ty.asArr] at h
  | neg_sh _  => simp [Ty.asArr] at h
  | tvar _    => simp [Ty.asArr] at h

/-- `asArrTvar T = some (k, S)` iff `T = .arr (.tvar k) S`. Used by the
    `.pipe f g` inference: when `g`'s inferred input is a tvar, we can
    instantiate it to `elemType S_f` directly, sidestepping the general
    subtyping check. -/
def Ty.asArrTvar : Ty → Option (Nat × Stream)
  | .arr (.tvar k) S => some (k, S)
  | _                => none

theorem Ty.asArrTvar_eq {T k S} :
    Ty.asArrTvar T = some (k, S) → T = .arr (.tvar k) S := by
  intro h
  cases T with
  | arr A' S' =>
      cases A' with
      | tvar k' =>
          simp [Ty.asArrTvar] at h
          obtain ⟨h1, h2⟩ := h
          subst h1; subst h2; rfl
      | sh _      => simp [Ty.asArrTvar] at h
      | arr _ _   => simp [Ty.asArrTvar] at h
      | inter _ _ => simp [Ty.asArrTvar] at h
      | union _ _ => simp [Ty.asArrTvar] at h
      | neg_sh _  => simp [Ty.asArrTvar] at h
  | sh _      => simp [Ty.asArrTvar] at h
  | inter _ _ => simp [Ty.asArrTvar] at h
  | union _ _ => simp [Ty.asArrTvar] at h
  | neg_sh _  => simp [Ty.asArrTvar] at h
  | tvar _    => simp [Ty.asArrTvar] at h

/-- `asArrYldTvar T = some (A, k)` iff `T = .arr A (.yld (.tvar k))`.
    Used to narrow `f`'s codomain via HM `inst`: when `f` yields a
    tvar, we can specialize it to whatever shape the next operation
    needs (`.sh (.num none)` for `.unop .neg` / numerical binops,
    `.sh (.boo none)` for `.and`/`.or` / `if`-guards). -/
def Ty.asArrYldTvar : Ty → Option (Ty × Nat)
  | .arr A (.yld (.tvar k)) => some (A, k)
  | _                       => none

theorem Ty.asArrYldTvar_eq {T A k} :
    Ty.asArrYldTvar T = some (A, k) → T = .arr A (.yld (.tvar k)) := by
  intro h
  cases T with
  | arr A' S =>
      cases S with
      | yld V =>
          cases V with
          | tvar k' =>
              simp [Ty.asArrYldTvar] at h
              obtain ⟨h1, h2⟩ := h
              subst h1; subst h2; rfl
          | sh _      => simp [Ty.asArrYldTvar] at h
          | arr _ _   => simp [Ty.asArrYldTvar] at h
          | inter _ _ => simp [Ty.asArrYldTvar] at h
          | union _ _ => simp [Ty.asArrYldTvar] at h
          | neg_sh _  => simp [Ty.asArrYldTvar] at h
      | eps        => simp [Ty.asArrYldTvar] at h
      | bot        => simp [Ty.asArrYldTvar] at h
      | concat _ _ => simp [Ty.asArrYldTvar] at h
      | star _     => simp [Ty.asArrYldTvar] at h
      | choice _ _ => simp [Ty.asArrYldTvar] at h
      | svar _     => simp [Ty.asArrYldTvar] at h
  | sh _      => simp [Ty.asArrYldTvar] at h
  | inter _ _ => simp [Ty.asArrYldTvar] at h
  | union _ _ => simp [Ty.asArrYldTvar] at h
  | neg_sh _  => simp [Ty.asArrYldTvar] at h
  | tvar _    => simp [Ty.asArrYldTvar] at h

/-- Narrow `T` to an arrow with `.yld (.sh (.num none))` codomain, returning
    the (possibly-substituted) input. Handles BOTH direct match and
    HM specialization of a tvar codomain. -/
def Ty.narrowToNum : Ty → Option Ty
  | .arr A (.yld (.sh (.num _))) => some A
  | .arr A (.yld (.tvar k))      => some (substTy k (.sh (.num none)) A)
  | _                            => none

/-- Soundness for `narrowToNum`: any filter typing at `T` also types at
    `.arr A_out (.yld (.sh (.num none)))` whenever `T.narrowToNum = some A_out`. -/
theorem Ty.narrowToNum_sound {f : Filter} {T A_out : Ty} :
    HasType f T → Ty.narrowToNum T = some A_out →
    HasType f (.arr A_out (.yld (.sh (.num none)))) := by
  intro hf hn
  cases T with
  | arr A' S =>
      cases S with
      | yld V =>
          cases V with
          | sh sh' =>
              cases sh' with
              | num m =>
                  simp [Ty.narrowToNum] at hn
                  subst hn
                  exact HasType.sub hf
                    (TyLE.arr TyLE.refl (StreamLE.yld (TyLE.num_widen m)))
              | top       => simp [Ty.narrowToNum] at hn
              | nul       => simp [Ty.narrowToNum] at hn
              | boo _     => simp [Ty.narrowToNum] at hn
              | str _     => simp [Ty.narrowToNum] at hn
              | array _ _ => simp [Ty.narrowToNum] at hn
              | tuple _   => simp [Ty.narrowToNum] at hn
              | object _  => simp [Ty.narrowToNum] at hn
          | tvar k =>
              simp [Ty.narrowToNum] at hn
              subst hn
              have := HasType.inst k (.sh (.num none)) hf
              simp [substTy, substStream] at this
              exact this
          | arr _ _   => simp [Ty.narrowToNum] at hn
          | inter _ _ => simp [Ty.narrowToNum] at hn
          | union _ _ => simp [Ty.narrowToNum] at hn
          | neg_sh _  => simp [Ty.narrowToNum] at hn
      | eps        => simp [Ty.narrowToNum] at hn
      | bot        => simp [Ty.narrowToNum] at hn
      | concat _ _ => simp [Ty.narrowToNum] at hn
      | star _     => simp [Ty.narrowToNum] at hn
      | choice _ _ => simp [Ty.narrowToNum] at hn
      | svar _     => simp [Ty.narrowToNum] at hn
  | sh _      => simp [Ty.narrowToNum] at hn
  | inter _ _ => simp [Ty.narrowToNum] at hn
  | union _ _ => simp [Ty.narrowToNum] at hn
  | neg_sh _  => simp [Ty.narrowToNum] at hn
  | tvar _    => simp [Ty.narrowToNum] at hn

/-- Same as `narrowToNum` but for `.sh (.boo none)`. -/
def Ty.narrowToBoo : Ty → Option Ty
  | .arr A (.yld (.sh (.boo _))) => some A
  | .arr A (.yld (.tvar k))      => some (substTy k (.sh (.boo none)) A)
  | _                            => none

theorem Ty.narrowToBoo_sound {f : Filter} {T A_out : Ty} :
    HasType f T → Ty.narrowToBoo T = some A_out →
    HasType f (.arr A_out (.yld (.sh (.boo none)))) := by
  intro hf hn
  cases T with
  | arr A' S =>
      cases S with
      | yld V =>
          cases V with
          | sh sh' =>
              cases sh' with
              | boo b =>
                  simp [Ty.narrowToBoo] at hn
                  subst hn
                  exact HasType.sub hf
                    (TyLE.arr TyLE.refl (StreamLE.yld (TyLE.boo_widen b)))
              | top       => simp [Ty.narrowToBoo] at hn
              | nul       => simp [Ty.narrowToBoo] at hn
              | num _     => simp [Ty.narrowToBoo] at hn
              | str _     => simp [Ty.narrowToBoo] at hn
              | array _ _ => simp [Ty.narrowToBoo] at hn
              | tuple _   => simp [Ty.narrowToBoo] at hn
              | object _  => simp [Ty.narrowToBoo] at hn
          | tvar k =>
              simp [Ty.narrowToBoo] at hn
              subst hn
              have := HasType.inst k (.sh (.boo none)) hf
              simp [substTy, substStream] at this
              exact this
          | arr _ _   => simp [Ty.narrowToBoo] at hn
          | inter _ _ => simp [Ty.narrowToBoo] at hn
          | union _ _ => simp [Ty.narrowToBoo] at hn
          | neg_sh _  => simp [Ty.narrowToBoo] at hn
      | eps        => simp [Ty.narrowToBoo] at hn
      | bot        => simp [Ty.narrowToBoo] at hn
      | concat _ _ => simp [Ty.narrowToBoo] at hn
      | star _     => simp [Ty.narrowToBoo] at hn
      | choice _ _ => simp [Ty.narrowToBoo] at hn
      | svar _     => simp [Ty.narrowToBoo] at hn
  | sh _      => simp [Ty.narrowToBoo] at hn
  | inter _ _ => simp [Ty.narrowToBoo] at hn
  | union _ _ => simp [Ty.narrowToBoo] at hn
  | neg_sh _  => simp [Ty.narrowToBoo] at hn
  | tvar _    => simp [Ty.narrowToBoo] at hn

/-! ## Decidable subtyping fragment

    `Ty.subCheck` recognises a *small but useful* fragment of the
    subtyping relation as a `Bool`-valued check. It's sound by
    construction: every `true` corresponds to a real `TyLE` derivation
    (the matching constructor is given inline in `subCheck_sound`).

    Currently recognised patterns:
    * `.sh (.tuple []) ⊑ .union T₁ (.sh (.array T₂ none))` — empty
      tuple is below an arrayed-union (covers `[][0]` from defs.jq).
    * `.sh .nul ⊑ .union (.sh .nul) T₂` — null in the left of a union.

    Add new patterns by extending both the function and the `match`
    in `subCheck_sound`. Each new pattern is independently sound. -/
def Ty.subCheck : Ty → Ty → Bool
  | .sh (.tuple []), .union _ (.sh (.array _ none)) => true
  | .sh .nul,        .union (.sh .nul) _            => true
  | _,               _                              => false

/-- Soundness for `Ty.subCheck`: every `true` is a real `TyLE`. -/
theorem Ty.subCheck_sound : ∀ {T₁ T₂ : Ty},
    Ty.subCheck T₁ T₂ = true → TyLE T₁ T₂
  | .sh (.tuple []), .union _ (.sh (.array _ none)), _ =>
      TyLE.trans
        (TyLE.sh (ShapeLE.tuple_array
          (by trivial)
          (fun i h_i => by simp at h_i)))
        TyLE.union_intro_r
  | .sh .nul, .union (.sh .nul) _, _ =>
      TyLE.union_intro_l
  -- All other pattern combinations: subCheck returns false, so the
  -- hypothesis `false = true` is contradictory.
  | .sh .top,        _,            h => by simp [Ty.subCheck] at h
  | .sh (.boo _),    _,            h => by simp [Ty.subCheck] at h
  | .sh (.num _),    _,            h => by simp [Ty.subCheck] at h
  | .sh (.str _),    _,            h => by simp [Ty.subCheck] at h
  | .sh (.array _ _), _,           h => by simp [Ty.subCheck] at h
  | .sh (.tuple (_ :: _)), _,      h => by simp [Ty.subCheck] at h
  | .sh (.object _), _,            h => by simp [Ty.subCheck] at h
  | .arr _ _,        _,            h => by simp [Ty.subCheck] at h
  | .inter _ _,      _,            h => by simp [Ty.subCheck] at h
  | .union _ _,      _,            h => by simp [Ty.subCheck] at h
  | .neg_sh _,       _,            h => by simp [Ty.subCheck] at h
  | .tvar _,         _,            h => by simp [Ty.subCheck] at h
  | .sh .nul,        .sh _,        h => by simp [Ty.subCheck] at h
  | .sh .nul,        .arr _ _,     h => by simp [Ty.subCheck] at h
  | .sh .nul,        .inter _ _,   h => by simp [Ty.subCheck] at h
  | .sh .nul,        .neg_sh _,    h => by simp [Ty.subCheck] at h
  | .sh .nul,        .tvar _,      h => by simp [Ty.subCheck] at h
  | .sh .nul,        .union (.sh .top) _,         h => by simp [Ty.subCheck] at h
  | .sh .nul,        .union (.sh (.boo _)) _,     h => by simp [Ty.subCheck] at h
  | .sh .nul,        .union (.sh (.num _)) _,     h => by simp [Ty.subCheck] at h
  | .sh .nul,        .union (.sh (.str _)) _,     h => by simp [Ty.subCheck] at h
  | .sh .nul,        .union (.sh (.array _ _)) _, h => by simp [Ty.subCheck] at h
  | .sh .nul,        .union (.sh (.tuple _)) _,   h => by simp [Ty.subCheck] at h
  | .sh .nul,        .union (.sh (.object _)) _,  h => by simp [Ty.subCheck] at h
  | .sh .nul,        .union (.arr _ _) _,         h => by simp [Ty.subCheck] at h
  | .sh .nul,        .union (.inter _ _) _,       h => by simp [Ty.subCheck] at h
  | .sh .nul,        .union (.union _ _) _,       h => by simp [Ty.subCheck] at h
  | .sh .nul,        .union (.neg_sh _) _,        h => by simp [Ty.subCheck] at h
  | .sh .nul,        .union (.tvar _) _,          h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .sh _,           h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .arr _ _,        h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .inter _ _,      h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .neg_sh _,       h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .tvar _,         h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .union _ (.sh .top),         h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .union _ (.sh .nul),         h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .union _ (.sh (.boo _)),     h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .union _ (.sh (.num _)),     h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .union _ (.sh (.str _)),     h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .union _ (.sh (.array _ (some _))), h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .union _ (.sh (.tuple _)),   h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .union _ (.sh (.object _)),  h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .union _ (.arr _ _),         h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .union _ (.inter _ _),       h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .union _ (.union _ _),       h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .union _ (.neg_sh _),        h => by simp [Ty.subCheck] at h
  | .sh (.tuple []), .union _ (.tvar _),          h => by simp [Ty.subCheck] at h

/-- A *definition context*: the schemes assigned to named filters. A
    `.call name` filter's inferred type is the scheme found here, or a
    fresh tvar if `name` is unbound (in which case inference defers to
    the unconstrained `HasType.call` rule). -/
abbrev Defs : Type := List (String × Ty)

-- The inference algorithm. Mutually recursive with `inferList` for
-- arbitrary-length array/object literals. `defs` is a section
-- variable threaded through every recursive call.
section
variable (defs : Defs)

mutual

def infer : Nat → Filter → Option (Ty × Nat)
  -- ── Identity ──
  | n, .dot =>
      some (.arr (.tvar n) (.yld (.tvar n)), n + 1)

  -- ── Value literals ──
  | n, .nullLit =>
      some (.arr (.tvar n) (.yld (.sh .nul)), n + 1)
  | n, .boolLit b =>
      some (.arr (.tvar n) (.yld (.sh (.boo (some b)))), n + 1)
  | n, .numLit m =>
      some (.arr (.tvar n) (.yld (.sh (.num (some m)))), n + 1)
  | n, .strLit s =>
      some (.arr (.tvar n) (.yld (.sh (.str (some s)))), n + 1)

  -- ── Empty / error ──
  -- empty produces no values then continues; error aborts.
  | n, .empty =>
      some (.arr (.tvar n) .eps, n + 1)
  | n, .error =>
      some (.arr (.tvar n) .bot, n + 1)

  -- ── Field / index / iter ──
  | n, .objIndex k =>
      some (.arr (.union (.sh .nul) (.sh (.object [(k, .tvar n)])))
                 (.yld (.union (.tvar n) (.sh .nul))),
            n + 1)
  | n, .arrIndex _ =>
      some (.arr (.union (.sh .nul) (.sh (.array (.tvar n) none)))
                 (.yld (.union (.tvar n) (.sh .nul))),
            n + 1)
  | n, .iter =>
      some (.arr (.sh (.array (.tvar n) none)) (.star (.yld (.tvar n))),
            n + 1)

  -- ── Comma ──
  -- Infer both branches; combine input types via intersection and
  -- codomain streams via concat. No unification needed: the
  -- intersection is *constructed* and is already a subtype of each
  -- branch's input.
  | n, .comma f g =>
      match infer n f with
      | some (.arr Af Sf, n') =>
          match infer n' g with
          | some (.arr Ag Sg, n'') =>
              some (.arr (.inter Af Ag) (.concat Sf Sg), n'')
          | _ => none
      | _ => none

  -- ── Empty array literal: A → ⟨tuple []⟩ ──
  | n, .array [] =>
      some (.arr (.tvar n) (.yld (.sh (.tuple []))), n + 1)

  -- ── Empty object literal: A → ⟨object []⟩ ──
  | n, .object [] =>
      some (.arr (.tvar n) (.yld (.sh (.object []))), n + 1)

  -- ── Calls: look up the scheme in `defs`; fall back to a fresh tvar
  --   if the name is unbound (HasType.call permits any T). ──
  | n, .call name =>
      match defs.lookup name with
      | some T => some (T, n)
      | none   => some (.tvar n, n + 1)

  -- ── Unary negation: codomain must be a number — either directly,
  -- or specializable from a tvar via HM inst. ──
  | n, .unop .neg f =>
      match infer n f with
      | some (T_f, n') =>
          match T_f.asArrYldNum with
          | some (A, _) => some (.arr A (.yld (.sh (.num none))), n')
          | none        =>
              match T_f.asArrYldTvar with
              | some (A, k) =>
                  some (.arr (substTy k (.sh (.num none)) A)
                             (.yld (.sh (.num none))), n')
              | none => none
      | none => none

  -- ── Binary equality: any operand types via Overload.eq_any. ──
  -- Result codomain is always ⟨bool⟩.
  | n, .binop .eq l r =>
      match infer n l with
      | some (T_l, n') =>
          match T_l.asArrYld with
          | some (A_l, _) =>
              match infer n' r with
              | some (T_r, n'') =>
                  match T_r.asArrYld with
                  | some (A_r, _) =>
                      some (.arr (.inter A_l A_r) (.yld (.sh (.boo none))), n'')
                  | none => none
              | none => none
          | none => none
      | none => none

  -- ── Binary inequality: any operand types via Overload.ne_any. ──
  | n, .binop .ne l r =>
      match infer n l with
      | some (T_l, n') =>
          match T_l.asArrYld with
          | some (A_l, _) =>
              match infer n' r with
              | some (T_r, n'') =>
                  match T_r.asArrYld with
                  | some (A_r, _) =>
                      some (.arr (.inter A_l A_r) (.yld (.sh (.boo none))), n'')
                  | none => none
              | none => none
          | none => none
      | none => none

  -- ── Numerical binops: ⟨num⟩ op ⟨num⟩ → ⟨num⟩. ──
  | n, .binop .add l r =>
      match infer n l with
      | some (T_l, n') =>
          match T_l.narrowToNum with
          | some A_l =>
              match infer n' r with
              | some (T_r, n'') =>
                  match T_r.narrowToNum with
                  | some A_r =>
                      some (.arr (.inter A_l A_r) (.yld (.sh (.num none))), n'')
                  | none => none
              | none => none
          | none => none
      | none => none
  | n, .binop .sub l r =>
      match infer n l with
      | some (T_l, n') =>
          match T_l.narrowToNum with
          | some A_l =>
              match infer n' r with
              | some (T_r, n'') =>
                  match T_r.narrowToNum with
                  | some A_r =>
                      some (.arr (.inter A_l A_r) (.yld (.sh (.num none))), n'')
                  | none => none
              | none => none
          | none => none
      | none => none
  | n, .binop .mul l r =>
      match infer n l with
      | some (T_l, n') =>
          match T_l.narrowToNum with
          | some A_l =>
              match infer n' r with
              | some (T_r, n'') =>
                  match T_r.narrowToNum with
                  | some A_r =>
                      some (.arr (.inter A_l A_r) (.yld (.sh (.num none))), n'')
                  | none => none
              | none => none
          | none => none
      | none => none
  | n, .binop .div l r =>
      match infer n l with
      | some (T_l, n') =>
          match T_l.narrowToNum with
          | some A_l =>
              match infer n' r with
              | some (T_r, n'') =>
                  match T_r.narrowToNum with
                  | some A_r =>
                      some (.arr (.inter A_l A_r) (.yld (.sh (.num none))), n'')
                  | none => none
              | none => none
          | none => none
      | none => none
  | n, .binop .mod l r =>
      match infer n l with
      | some (T_l, n') =>
          match T_l.narrowToNum with
          | some A_l =>
              match infer n' r with
              | some (T_r, n'') =>
                  match T_r.narrowToNum with
                  | some A_r =>
                      some (.arr (.inter A_l A_r) (.yld (.sh (.num none))), n'')
                  | none => none
              | none => none
          | none => none
      | none => none

  -- ── Comparison ops: any operand types via Overload.{lt,le,gt,ge}_any. ──
  | n, .binop .lt l r =>
      match infer n l with
      | some (T_l, n') =>
          match T_l.asArrYld with
          | some (A_l, _) =>
              match infer n' r with
              | some (T_r, n'') =>
                  match T_r.asArrYld with
                  | some (A_r, _) =>
                      some (.arr (.inter A_l A_r) (.yld (.sh (.boo none))), n'')
                  | none => none
              | none => none
          | none => none
      | none => none
  | n, .binop .le l r =>
      match infer n l with
      | some (T_l, n') =>
          match T_l.asArrYld with
          | some (A_l, _) =>
              match infer n' r with
              | some (T_r, n'') =>
                  match T_r.asArrYld with
                  | some (A_r, _) =>
                      some (.arr (.inter A_l A_r) (.yld (.sh (.boo none))), n'')
                  | none => none
              | none => none
          | none => none
      | none => none
  | n, .binop .gt l r =>
      match infer n l with
      | some (T_l, n') =>
          match T_l.asArrYld with
          | some (A_l, _) =>
              match infer n' r with
              | some (T_r, n'') =>
                  match T_r.asArrYld with
                  | some (A_r, _) =>
                      some (.arr (.inter A_l A_r) (.yld (.sh (.boo none))), n'')
                  | none => none
              | none => none
          | none => none
      | none => none
  | n, .binop .ge l r =>
      match infer n l with
      | some (T_l, n') =>
          match T_l.asArrYld with
          | some (A_l, _) =>
              match infer n' r with
              | some (T_r, n'') =>
                  match T_r.asArrYld with
                  | some (A_r, _) =>
                      some (.arr (.inter A_l A_r) (.yld (.sh (.boo none))), n'')
                  | none => none
              | none => none
          | none => none
      | none => none

  -- ── Boolean connectives: ⟨bool⟩ op ⟨bool⟩ → ⟨bool⟩. ──
  | n, .binop .and l r =>
      match infer n l with
      | some (T_l, n') =>
          match T_l.narrowToBoo with
          | some A_l =>
              match infer n' r with
              | some (T_r, n'') =>
                  match T_r.narrowToBoo with
                  | some A_r =>
                      some (.arr (.inter A_l A_r) (.yld (.sh (.boo none))), n'')
                  | none => none
              | none => none
          | none => none
      | none => none
  | n, .binop .or l r =>
      match infer n l with
      | some (T_l, n') =>
          match T_l.narrowToBoo with
          | some A_l =>
              match infer n' r with
              | some (T_r, n'') =>
                  match T_r.narrowToBoo with
                  | some A_r =>
                      some (.arr (.inter A_l A_r) (.yld (.sh (.boo none))), n'')
                  | none => none
              | none => none
          | none => none
      | none => none

  -- ── Singleton array literal: [f] : A → ⟨collect (S ; ε)⟩ ──
  | n, .array (f :: []) =>
      match infer n f with
      | some (T_f, n') =>
          match T_f.asArr with
          | some (A, S) =>
              some (.arr A (.yld (collect (.concat S .eps))), n')
          | none => none
      | none => none

  -- ── Two-element array literal: [f, g] : (A_f ∩ A_g) → ⟨collect (S_f ; S_g ; ε)⟩
  | n, .array (f :: g :: []) =>
      match infer n f with
      | some (T_f, n') =>
          match T_f.asArr with
          | some (A_f, S_f) =>
              match infer n' g with
              | some (T_g, n'') =>
                  match T_g.asArr with
                  | some (A_g, S_g) =>
                      some (.arr (.inter A_f A_g)
                                 (.yld (collect (.concat S_f (.concat S_g .eps)))),
                            n'')
                  | none => none
              | none => none
          | none => none
      | none => none

  -- ── Three-element array literal ──
  | n, .array (f :: g :: h :: []) =>
      match infer n f with
      | some (T_f, n₁) =>
          match T_f.asArr with
          | some (A_f, S_f) =>
              match infer n₁ g with
              | some (T_g, n₂) =>
                  match T_g.asArr with
                  | some (A_g, S_g) =>
                      match infer n₂ h with
                      | some (T_h, n₃) =>
                          match T_h.asArr with
                          | some (A_h, S_h) =>
                              some (.arr (.inter (.inter A_f A_g) A_h)
                                         (.yld (collect (.concat S_f
                                                          (.concat S_g
                                                            (.concat S_h .eps))))),
                                    n₃)
                          | none => none
                      | none => none
                  | none => none
              | none => none
          | none => none
      | none => none

  -- ── Singleton object literal: {k: v} : A → ⟨object [(k, V)]⟩
  -- where v has codomain ⟨V⟩ ──
  | n, .object ((k, v) :: []) =>
      match infer n v with
      | some (T_v, n') =>
          match T_v.asArrYld with
          | some (A, V) =>
              some (.arr A (.yld (.sh (.object [(k, V)]))), n')
          | none => none
      | none => none

  -- ── Two-field object literal ──
  | n, .object ((k₁, v₁) :: (k₂, v₂) :: []) =>
      match infer n v₁ with
      | some (T₁, n') =>
          match T₁.asArrYld with
          | some (A₁, V₁) =>
              match infer n' v₂ with
              | some (T₂, n'') =>
                  match T₂.asArrYld with
                  | some (A₂, V₂) =>
                      some (.arr (.inter A₁ A₂)
                                 (.yld (.sh (.object [(k₁, V₁), (k₂, V₂)]))),
                            n'')
                  | none => none
              | none => none
          | none => none
      | none => none

  -- ── If-then-else: imprecise OR precise variant ──
  -- - imprecise (refine c true = ⊤): result is `A → (S_t ⊕ S_e)`.
  -- - precise (refine c true ≠ ⊤): result is the intersection
  --   `(refine c true → S_t) ∩ (refine c false → S_e)` — projecting
  --   `inter_intro_l/r` recovers per-branch arrow.
  | n, .ifThenElse c t e =>
      match infer n c with
      | some (T_c, n') =>
          match T_c.narrowToBoo with
          | some A_c =>
              match infer n' t with
              | some (T_t, n'') =>
                  match T_t.asArr with
                  | some (A_t, S_t) =>
                      match infer n'' e with
                      | some (T_e, n''') =>
                          match T_e.asArr with
                          | some (A_e, S_e) =>
                              if (refine c true).isShTop then
                                some (.arr (.inter (.inter A_c A_t) A_e)
                                           (.choice S_t S_e),
                                      n''')
                              else
                                some (.inter
                                        (.arr (refine c true) S_t)
                                        (.arr (refine c false) S_e),
                                      n''')
                          | none => none
                      | none => none
                  | none => none
              | none => none
          | none => none
      | none => none

  -- ── Pipe `f | g` ──
  -- Two paths:
  -- 1. HM inst: when `g`'s input is a tvar, specialise via inst.
  -- 2. `subCheck` fallback: try the decidable subtyping fragment.
  | n, .pipe f g =>
      match infer n f with
      | some (T_f, n') =>
          match T_f.asArr with
          | some (A_f, S_f) =>
              match infer n' g with
              | some (T_g, n'') =>
                  match T_g.asArrTvar with
                  | some (k, S_g) =>
                      let elem := elemType S_f
                      some (.arr A_f
                                 (flatmap S_f (fun _ => substStream k elem S_g)),
                            n'')
                  | none =>
                      match T_g.asArr with
                      | some (A_g, S_g) =>
                          if (elemType S_f).subCheck A_g then
                              some (.arr A_f (flatmap S_f (fun _ => S_g)), n'')
                          else none
                      | none => none
              | none => none
          | none => none
      | none => none

  -- ── Arbitrary-length array literal (≥ 4 elements) via inferList. ──
  | n, .array (f₁ :: f₂ :: f₃ :: f₄ :: rest) =>
      match inferList n (f₁ :: f₂ :: f₃ :: f₄ :: rest) with
      | some (A, Ss, n') =>
          some (.arr A (.yld (collect (Ss.foldr Stream.concat .eps))), n')
      | none => none

  -- ── Arbitrary-length object literal (≥ 3 fields) via inferObjList. ──
  | n, .object (kv₁ :: kv₂ :: kv₃ :: rest) =>
      match inferObjList n (kv₁ :: kv₂ :: kv₃ :: rest) with
      | some (A, Ts, n') =>
          some (.arr A (.yld (.sh (.object
                  (List.zipWith (fun kv t => (kv.fst, t))
                                (kv₁ :: kv₂ :: kv₃ :: rest) Ts)))),
                n')
      | none => none

/-- Helper for array typing. Returns the combined input type
    (right-folded intersection), the per-element stream list, and the
    bumped counter.

    Empty list: returns `(.tvar n, [], n+1)` (fresh tvar input). For
    `[f]`: `(A_f, [S_f], n')`. For `f :: fs`: `(.inter A_f A_rest, S_f
    :: Ss_rest, n'')`. The list-of-streams form lets the soundness
    proof construct `helem` index-by-index. -/
def inferList : Nat → List Filter → Option (Ty × List Stream × Nat)
  | n, [] => some (.tvar n, [], n + 1)
  | n, [f] =>
      match infer n f with
      | some (T_f, n') =>
          match T_f.asArr with
          | some (A, S) => some (A, [S], n')
          | none        => none
      | none => none
  | n, f :: fs =>
      match infer n f with
      | some (T_f, n') =>
          match T_f.asArr with
          | some (A_f, S_f) =>
              match inferList n' fs with
              | some (A_rest, Ss_rest, n'') =>
                  some (.inter A_f A_rest, S_f :: Ss_rest, n'')
              | none => none
          | none => none
      | none => none

/-- Helper for object typing. Like `inferList`, but each filter must
    yield a single value (`.arr A (.yld V)`). Returns `(A, Ts, n')`
    where `Ts` is the list of value-types. Keys come from the caller. -/
def inferObjList : Nat → List (String × Filter) → Option (Ty × List Ty × Nat)
  | n, [] => some (.tvar n, [], n + 1)
  | n, [(_, v)] =>
      match infer n v with
      | some (T_v, n') =>
          match T_v.asArrYld with
          | some (A, V) => some (A, [V], n')
          | none        => none
      | none => none
  | n, (_, v) :: kvs =>
      match infer n v with
      | some (T_v, n') =>
          match T_v.asArrYld with
          | some (A_v, V_v) =>
              match inferObjList n' kvs with
              | some (A_rest, Ts_rest, n'') =>
                  some (.inter A_v A_rest, V_v :: Ts_rest, n'')
              | none => none
          | none => none
      | none => none

end -- mutual

end -- section (defs)

/-! ## Correctness theorem

    Every successful inference produces a valid `HasType` derivation.
    `infer_sound` and `inferList_sound` are mutually recursive: the
    array-length-≥4 case of `infer_sound` calls `inferList_sound`, and
    `inferList_sound` calls `infer_sound` on each list element. -/

section
variable (defs : Defs)

mutual

theorem infer_sound : ∀ {f : Filter} {T : Ty} {n n' : Nat},
    infer defs n f = some (T, n') → HasType f T
  | .dot,        _, _, _, h => by
      simp [infer] at h
      obtain ⟨hT, _⟩ := h
      rw [← hT]
      exact HasType.dot _
  | .nullLit,    _, _, _, h => by
      simp [infer] at h
      obtain ⟨hT, _⟩ := h
      rw [← hT]
      exact HasType.nullLit _
  | .boolLit b,  _, _, _, h => by
      simp [infer] at h
      obtain ⟨hT, _⟩ := h
      rw [← hT]
      exact HasType.boolLit _ b
  | .numLit m,   _, _, _, h => by
      simp [infer] at h
      obtain ⟨hT, _⟩ := h
      rw [← hT]
      exact HasType.numLit _ m
  | .strLit s,   _, _, _, h => by
      simp [infer] at h
      obtain ⟨hT, _⟩ := h
      rw [← hT]
      exact HasType.strLit _ s
  | .empty,      _, _, _, h => by
      simp [infer] at h
      obtain ⟨hT, _⟩ := h
      rw [← hT]
      exact HasType.empty _
  | .error,      _, _, _, h => by
      simp [infer] at h
      obtain ⟨hT, _⟩ := h
      rw [← hT]
      exact HasType.error _
  | .objIndex k, _, _, _, h => by
      simp [infer] at h
      obtain ⟨hT, _⟩ := h
      rw [← hT]
      exact HasType.objIndex _ k
  | .arrIndex m, _, _, _, h => by
      simp [infer] at h
      obtain ⟨hT, _⟩ := h
      rw [← hT]
      exact HasType.arrIndex _ m
  | .iter,       _, _, _, h => by
      simp [infer] at h
      obtain ⟨hT, _⟩ := h
      rw [← hT]
      exact HasType.iterArray _
  -- Comma: case-split on the inner `infer` results, subsume each
  -- branch to the shared (intersection) input, apply HasType.comma.
  | .comma f g,        _, n, _, h => by
      unfold infer at h
      match h_f : infer defs n f with
      | none                       => rw [h_f] at h; simp at h
      | some (.sh _, _)            => rw [h_f] at h; simp at h
      | some (.inter _ _, _)       => rw [h_f] at h; simp at h
      | some (.union _ _, _)       => rw [h_f] at h; simp at h
      | some (.neg_sh _, _)        => rw [h_f] at h; simp at h
      | some (.tvar _, _)          => rw [h_f] at h; simp at h
      | some (.arr Af Sf, n_f)     =>
          rw [h_f] at h
          simp at h
          match h_g : infer defs n_f g with
          | none                     => rw [h_g] at h; simp at h
          | some (.sh _, _)          => rw [h_g] at h; simp at h
          | some (.inter _ _, _)     => rw [h_g] at h; simp at h
          | some (.union _ _, _)     => rw [h_g] at h; simp at h
          | some (.neg_sh _, _)      => rw [h_g] at h; simp at h
          | some (.tvar _, _)        => rw [h_g] at h; simp at h
          | some (.arr Ag Sg, n_g)   =>
              rw [h_g] at h
              simp at h
              obtain ⟨hT, _⟩ := h
              rw [← hT]
              have hf_t : HasType f (.arr Af Sf) := infer_sound h_f
              have hg_t : HasType g (.arr Ag Sg) := infer_sound h_g
              exact HasType.comma
                (HasType.sub hf_t
                  (TyLE.arr (TyLE.inter_intro_l TyLE.refl) StreamLE.refl))
                (HasType.sub hg_t
                  (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl))
  -- Empty array literal: type is A → ⟨tuple []⟩.
  | .array [],         _, _, _, h => by
      simp [infer] at h
      obtain ⟨hT, _⟩ := h
      rw [← hT]
      -- collect ([].foldr Stream.concat .eps) = collect .eps = .sh (.tuple [])
      exact HasType.array [] rfl (fun i hi => by simp at hi)
  -- Empty object literal: type is A → ⟨object []⟩.
  | .object [],        _, _, _, h => by
      simp [infer] at h
      obtain ⟨hT, _⟩ := h
      rw [← hT]
      exact HasType.object [] rfl (fun i hi => by simp at hi)
  -- Calls: HasType.call admits any T, so the inferred type — whether
  -- looked up from `defs` or freshly tvar'd — is fine.
  | .call name,        _, _, _, _ => HasType.call name _
  -- Unary negation: codomain must be ⟨num _⟩, either directly or via
  -- HM inst on a tvar codomain.
  | .unop .neg f,      _, n, _, h => by
      unfold infer at h
      match h_f : infer defs n f with
      | none           => rw [h_f] at h; simp at h
      | some (T_f, n_f) =>
          rw [h_f] at h
          simp at h
          match h_a : T_f.asArrYldNum with
          | some (A, m) =>
              rw [h_a] at h
              simp at h
              obtain ⟨hT, _⟩ := h
              rw [← hT]
              have h_T_eq : T_f = .arr A (.yld (.sh (.num m))) :=
                Ty.asArrYldNum_eq h_a
              rw [h_T_eq] at h_f
              exact HasType.unopNeg
                (HasType.sub (infer_sound h_f)
                  (TyLE.arr TyLE.refl (StreamLE.yld (TyLE.num_widen m))))
          | none =>
              rw [h_a] at h
              simp at h
              match h_t : T_f.asArrYldTvar with
              | none        => rw [h_t] at h; simp at h
              | some (A, k) =>
                  rw [h_t] at h
                  simp at h
                  obtain ⟨hT, _⟩ := h
                  rw [← hT]
                  have h_T_eq : T_f = .arr A (.yld (.tvar k)) :=
                    Ty.asArrYldTvar_eq h_t
                  rw [h_T_eq] at h_f
                  have hf_t : HasType f (.arr A (.yld (.tvar k))) :=
                    infer_sound h_f
                  -- Specialize via HM inst: substitute k := .sh (.num none).
                  have hf_inst : HasType f
                      (.arr (substTy k (.sh (.num none)) A) (.yld (.sh (.num none)))) := by
                    have := HasType.inst k (.sh (.num none)) hf_t
                    simp [substTy, substStream] at this
                    exact this
                  exact HasType.unopNeg hf_inst
  -- Equality: any operand yield types via Overload.eq_any.
  | .binop .eq l r,    _, n, _, h => by
      unfold infer at h
      match h_l : infer defs n l with
      | none             => rw [h_l] at h; simp at h
      | some (T_l, n_l)  =>
          rw [h_l] at h
          simp at h
          match h_al : T_l.asArrYld with
          | none              => rw [h_al] at h; simp at h
          | some (A_l, V_l)   =>
              rw [h_al] at h
              simp at h
              match h_r : infer defs n_l r with
              | none             => rw [h_r] at h; simp at h
              | some (T_r, n_r)  =>
                  rw [h_r] at h
                  simp at h
                  match h_ar : T_r.asArrYld with
                  | none              => rw [h_ar] at h; simp at h
                  | some (A_r, V_r)   =>
                      rw [h_ar] at h
                      simp at h
                      obtain ⟨hT, _⟩ := h
                      rw [← hT]
                      have h_Tl_eq : T_l = .arr A_l (.yld V_l) :=
                        Ty.asArrYld_eq h_al
                      have h_Tr_eq : T_r = .arr A_r (.yld V_r) :=
                        Ty.asArrYld_eq h_ar
                      rw [h_Tl_eq] at h_l
                      rw [h_Tr_eq] at h_r
                      exact HasType.binop
                        (HasType.sub (infer_sound h_l)
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl)
                                    StreamLE.refl))
                        (HasType.sub (infer_sound h_r)
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl)
                                    StreamLE.refl))
                        Overload.eq_any
  -- Addition for numbers: ⟨num⟩ + ⟨num⟩ → ⟨num⟩.
  | .binop .add l r,   _, n, _, h => by
      unfold infer at h
      match h_l : infer defs n l with
      | none             => rw [h_l] at h; simp at h
      | some (T_l, n_l)  =>
          rw [h_l] at h
          simp at h
          match h_al : T_l.narrowToNum with
          | none      => rw [h_al] at h; simp at h
          | some A_l  =>
              rw [h_al] at h
              simp at h
              match h_r : infer defs n_l r with
              | none             => rw [h_r] at h; simp at h
              | some (T_r, n_r)  =>
                  rw [h_r] at h
                  simp at h
                  match h_ar : T_r.narrowToNum with
                  | none      => rw [h_ar] at h; simp at h
                  | some A_r  =>
                      rw [h_ar] at h
                      simp at h
                      obtain ⟨hT, _⟩ := h
                      rw [← hT]
                      -- narrowToNum_sound widens the operand codomain to
                      -- ⟨num none⟩ (handling both direct and tvar cases).
                      have hl_t : HasType l (.arr A_l (.yld (.sh (.num none)))) :=
                        Ty.narrowToNum_sound (infer_sound h_l) h_al
                      have hr_t : HasType r (.arr A_r (.yld (.sh (.num none)))) :=
                        Ty.narrowToNum_sound (infer_sound h_r) h_ar
                      exact HasType.binop
                        (HasType.sub hl_t
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl) StreamLE.refl))
                        (HasType.sub hr_t
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl))
                        (Overload.add_num TyLE.refl TyLE.refl)
  -- Inequality: any operand types via Overload.ne_any.
  | .binop .ne l r,    _, n, _, h => by
      unfold infer at h
      match h_l : infer defs n l with
      | none             => rw [h_l] at h; simp at h
      | some (T_l, n_l)  =>
          rw [h_l] at h
          simp at h
          match h_al : T_l.asArrYld with
          | none              => rw [h_al] at h; simp at h
          | some (A_l, V_l)   =>
              rw [h_al] at h
              simp at h
              match h_r : infer defs n_l r with
              | none             => rw [h_r] at h; simp at h
              | some (T_r, n_r)  =>
                  rw [h_r] at h
                  simp at h
                  match h_ar : T_r.asArrYld with
                  | none              => rw [h_ar] at h; simp at h
                  | some (A_r, V_r)   =>
                      rw [h_ar] at h
                      simp at h
                      obtain ⟨hT, _⟩ := h
                      rw [← hT]
                      rw [Ty.asArrYld_eq h_al] at h_l
                      rw [Ty.asArrYld_eq h_ar] at h_r
                      exact HasType.binop
                        (HasType.sub (infer_sound h_l)
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl) StreamLE.refl))
                        (HasType.sub (infer_sound h_r)
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl))
                        Overload.ne_any
  -- Subtraction: like add.
  | .binop .sub l r,   _, n, _, h => by
      unfold infer at h
      match h_l : infer defs n l with
      | none             => rw [h_l] at h; simp at h
      | some (T_l, n_l)  =>
          rw [h_l] at h; simp at h
          match h_al : T_l.narrowToNum with
          | none      => rw [h_al] at h; simp at h
          | some A_l  =>
              rw [h_al] at h; simp at h
              match h_r : infer defs n_l r with
              | none             => rw [h_r] at h; simp at h
              | some (T_r, n_r)  =>
                  rw [h_r] at h; simp at h
                  match h_ar : T_r.narrowToNum with
                  | none      => rw [h_ar] at h; simp at h
                  | some A_r  =>
                      rw [h_ar] at h; simp at h
                      obtain ⟨hT, _⟩ := h
                      rw [← hT]
                      have hl_t : HasType l (.arr A_l (.yld (.sh (.num none)))) :=
                        Ty.narrowToNum_sound (infer_sound h_l) h_al
                      have hr_t : HasType r (.arr A_r (.yld (.sh (.num none)))) :=
                        Ty.narrowToNum_sound (infer_sound h_r) h_ar
                      exact HasType.binop
                        (HasType.sub hl_t
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl) StreamLE.refl))
                        (HasType.sub hr_t
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl))
                        (Overload.sub_num TyLE.refl TyLE.refl)
  -- Multiplication: like add.
  | .binop .mul l r,   _, n, _, h => by
      unfold infer at h
      match h_l : infer defs n l with
      | none             => rw [h_l] at h; simp at h
      | some (T_l, n_l)  =>
          rw [h_l] at h; simp at h
          match h_al : T_l.narrowToNum with
          | none      => rw [h_al] at h; simp at h
          | some A_l  =>
              rw [h_al] at h; simp at h
              match h_r : infer defs n_l r with
              | none             => rw [h_r] at h; simp at h
              | some (T_r, n_r)  =>
                  rw [h_r] at h; simp at h
                  match h_ar : T_r.narrowToNum with
                  | none      => rw [h_ar] at h; simp at h
                  | some A_r  =>
                      rw [h_ar] at h; simp at h
                      obtain ⟨hT, _⟩ := h
                      rw [← hT]
                      have hl_t : HasType l (.arr A_l (.yld (.sh (.num none)))) :=
                        Ty.narrowToNum_sound (infer_sound h_l) h_al
                      have hr_t : HasType r (.arr A_r (.yld (.sh (.num none)))) :=
                        Ty.narrowToNum_sound (infer_sound h_r) h_ar
                      exact HasType.binop
                        (HasType.sub hl_t
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl) StreamLE.refl))
                        (HasType.sub hr_t
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl))
                        (Overload.mul_num TyLE.refl TyLE.refl)
  -- Division: like add.
  | .binop .div l r,   _, n, _, h => by
      unfold infer at h
      match h_l : infer defs n l with
      | none             => rw [h_l] at h; simp at h
      | some (T_l, n_l)  =>
          rw [h_l] at h; simp at h
          match h_al : T_l.narrowToNum with
          | none      => rw [h_al] at h; simp at h
          | some A_l  =>
              rw [h_al] at h; simp at h
              match h_r : infer defs n_l r with
              | none             => rw [h_r] at h; simp at h
              | some (T_r, n_r)  =>
                  rw [h_r] at h; simp at h
                  match h_ar : T_r.narrowToNum with
                  | none      => rw [h_ar] at h; simp at h
                  | some A_r  =>
                      rw [h_ar] at h; simp at h
                      obtain ⟨hT, _⟩ := h
                      rw [← hT]
                      have hl_t : HasType l (.arr A_l (.yld (.sh (.num none)))) :=
                        Ty.narrowToNum_sound (infer_sound h_l) h_al
                      have hr_t : HasType r (.arr A_r (.yld (.sh (.num none)))) :=
                        Ty.narrowToNum_sound (infer_sound h_r) h_ar
                      exact HasType.binop
                        (HasType.sub hl_t
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl) StreamLE.refl))
                        (HasType.sub hr_t
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl))
                        (Overload.div_num TyLE.refl TyLE.refl)
  -- Modulo: like add.
  | .binop .mod l r,   _, n, _, h => by
      unfold infer at h
      match h_l : infer defs n l with
      | none             => rw [h_l] at h; simp at h
      | some (T_l, n_l)  =>
          rw [h_l] at h; simp at h
          match h_al : T_l.narrowToNum with
          | none      => rw [h_al] at h; simp at h
          | some A_l  =>
              rw [h_al] at h; simp at h
              match h_r : infer defs n_l r with
              | none             => rw [h_r] at h; simp at h
              | some (T_r, n_r)  =>
                  rw [h_r] at h; simp at h
                  match h_ar : T_r.narrowToNum with
                  | none      => rw [h_ar] at h; simp at h
                  | some A_r  =>
                      rw [h_ar] at h; simp at h
                      obtain ⟨hT, _⟩ := h
                      rw [← hT]
                      have hl_t : HasType l (.arr A_l (.yld (.sh (.num none)))) :=
                        Ty.narrowToNum_sound (infer_sound h_l) h_al
                      have hr_t : HasType r (.arr A_r (.yld (.sh (.num none)))) :=
                        Ty.narrowToNum_sound (infer_sound h_r) h_ar
                      exact HasType.binop
                        (HasType.sub hl_t
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl) StreamLE.refl))
                        (HasType.sub hr_t
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl))
                        (Overload.mod_num TyLE.refl TyLE.refl)
  -- Comparison: like eq, but with each comparison's `*_any` overload.
  | .binop .lt l r,    _, n, _, h => by
      unfold infer at h
      match h_l : infer defs n l with
      | none             => rw [h_l] at h; simp at h
      | some (T_l, n_l)  =>
          rw [h_l] at h; simp at h
          match h_al : T_l.asArrYld with
          | none              => rw [h_al] at h; simp at h
          | some (A_l, V_l)   =>
              rw [h_al] at h; simp at h
              match h_r : infer defs n_l r with
              | none             => rw [h_r] at h; simp at h
              | some (T_r, n_r)  =>
                  rw [h_r] at h; simp at h
                  match h_ar : T_r.asArrYld with
                  | none              => rw [h_ar] at h; simp at h
                  | some (A_r, V_r)   =>
                      rw [h_ar] at h; simp at h
                      obtain ⟨hT, _⟩ := h
                      rw [← hT]
                      rw [Ty.asArrYld_eq h_al] at h_l
                      rw [Ty.asArrYld_eq h_ar] at h_r
                      exact HasType.binop
                        (HasType.sub (infer_sound h_l)
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl) StreamLE.refl))
                        (HasType.sub (infer_sound h_r)
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl))
                        Overload.lt_any
  | .binop .le l r,    _, n, _, h => by
      unfold infer at h
      match h_l : infer defs n l with
      | none             => rw [h_l] at h; simp at h
      | some (T_l, n_l)  =>
          rw [h_l] at h; simp at h
          match h_al : T_l.asArrYld with
          | none              => rw [h_al] at h; simp at h
          | some (A_l, V_l)   =>
              rw [h_al] at h; simp at h
              match h_r : infer defs n_l r with
              | none             => rw [h_r] at h; simp at h
              | some (T_r, n_r)  =>
                  rw [h_r] at h; simp at h
                  match h_ar : T_r.asArrYld with
                  | none              => rw [h_ar] at h; simp at h
                  | some (A_r, V_r)   =>
                      rw [h_ar] at h; simp at h
                      obtain ⟨hT, _⟩ := h
                      rw [← hT]
                      rw [Ty.asArrYld_eq h_al] at h_l
                      rw [Ty.asArrYld_eq h_ar] at h_r
                      exact HasType.binop
                        (HasType.sub (infer_sound h_l)
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl) StreamLE.refl))
                        (HasType.sub (infer_sound h_r)
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl))
                        Overload.le_any
  | .binop .gt l r,    _, n, _, h => by
      unfold infer at h
      match h_l : infer defs n l with
      | none             => rw [h_l] at h; simp at h
      | some (T_l, n_l)  =>
          rw [h_l] at h; simp at h
          match h_al : T_l.asArrYld with
          | none              => rw [h_al] at h; simp at h
          | some (A_l, V_l)   =>
              rw [h_al] at h; simp at h
              match h_r : infer defs n_l r with
              | none             => rw [h_r] at h; simp at h
              | some (T_r, n_r)  =>
                  rw [h_r] at h; simp at h
                  match h_ar : T_r.asArrYld with
                  | none              => rw [h_ar] at h; simp at h
                  | some (A_r, V_r)   =>
                      rw [h_ar] at h; simp at h
                      obtain ⟨hT, _⟩ := h
                      rw [← hT]
                      rw [Ty.asArrYld_eq h_al] at h_l
                      rw [Ty.asArrYld_eq h_ar] at h_r
                      exact HasType.binop
                        (HasType.sub (infer_sound h_l)
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl) StreamLE.refl))
                        (HasType.sub (infer_sound h_r)
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl))
                        Overload.gt_any
  | .binop .ge l r,    _, n, _, h => by
      unfold infer at h
      match h_l : infer defs n l with
      | none             => rw [h_l] at h; simp at h
      | some (T_l, n_l)  =>
          rw [h_l] at h; simp at h
          match h_al : T_l.asArrYld with
          | none              => rw [h_al] at h; simp at h
          | some (A_l, V_l)   =>
              rw [h_al] at h; simp at h
              match h_r : infer defs n_l r with
              | none             => rw [h_r] at h; simp at h
              | some (T_r, n_r)  =>
                  rw [h_r] at h; simp at h
                  match h_ar : T_r.asArrYld with
                  | none              => rw [h_ar] at h; simp at h
                  | some (A_r, V_r)   =>
                      rw [h_ar] at h; simp at h
                      obtain ⟨hT, _⟩ := h
                      rw [← hT]
                      rw [Ty.asArrYld_eq h_al] at h_l
                      rw [Ty.asArrYld_eq h_ar] at h_r
                      exact HasType.binop
                        (HasType.sub (infer_sound h_l)
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl) StreamLE.refl))
                        (HasType.sub (infer_sound h_r)
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl))
                        Overload.ge_any
  -- And/Or: bool operands; use boo_widen.
  | .binop .and l r,   _, n, _, h => by
      unfold infer at h
      match h_l : infer defs n l with
      | none             => rw [h_l] at h; simp at h
      | some (T_l, n_l)  =>
          rw [h_l] at h; simp at h
          match h_al : T_l.narrowToBoo with
          | none      => rw [h_al] at h; simp at h
          | some A_l  =>
              rw [h_al] at h; simp at h
              match h_r : infer defs n_l r with
              | none             => rw [h_r] at h; simp at h
              | some (T_r, n_r)  =>
                  rw [h_r] at h; simp at h
                  match h_ar : T_r.narrowToBoo with
                  | none      => rw [h_ar] at h; simp at h
                  | some A_r  =>
                      rw [h_ar] at h; simp at h
                      obtain ⟨hT, _⟩ := h
                      rw [← hT]
                      have hl_t : HasType l (.arr A_l (.yld (.sh (.boo none)))) :=
                        Ty.narrowToBoo_sound (infer_sound h_l) h_al
                      have hr_t : HasType r (.arr A_r (.yld (.sh (.boo none)))) :=
                        Ty.narrowToBoo_sound (infer_sound h_r) h_ar
                      exact HasType.binop
                        (HasType.sub hl_t
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl) StreamLE.refl))
                        (HasType.sub hr_t
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl))
                        (Overload.and_bool TyLE.refl TyLE.refl)
  | .binop .or l r,    _, n, _, h => by
      unfold infer at h
      match h_l : infer defs n l with
      | none             => rw [h_l] at h; simp at h
      | some (T_l, n_l)  =>
          rw [h_l] at h; simp at h
          match h_al : T_l.narrowToBoo with
          | none      => rw [h_al] at h; simp at h
          | some A_l  =>
              rw [h_al] at h; simp at h
              match h_r : infer defs n_l r with
              | none             => rw [h_r] at h; simp at h
              | some (T_r, n_r)  =>
                  rw [h_r] at h; simp at h
                  match h_ar : T_r.narrowToBoo with
                  | none      => rw [h_ar] at h; simp at h
                  | some A_r  =>
                      rw [h_ar] at h; simp at h
                      obtain ⟨hT, _⟩ := h
                      rw [← hT]
                      have hl_t : HasType l (.arr A_l (.yld (.sh (.boo none)))) :=
                        Ty.narrowToBoo_sound (infer_sound h_l) h_al
                      have hr_t : HasType r (.arr A_r (.yld (.sh (.boo none)))) :=
                        Ty.narrowToBoo_sound (infer_sound h_r) h_ar
                      exact HasType.binop
                        (HasType.sub hl_t
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl) StreamLE.refl))
                        (HasType.sub hr_t
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl))
                        (Overload.or_bool TyLE.refl TyLE.refl)
  -- Singleton array literal: helem only needs the f case.
  | .array (f :: []),  _, n, _, h => by
      unfold infer at h
      match h_f : infer defs n f with
      | none             => rw [h_f] at h; simp at h
      | some (T_f, n_f)  =>
          rw [h_f] at h
          simp at h
          match h_a : T_f.asArr with
          | none            => rw [h_a] at h; simp at h
          | some (A, S)     =>
              rw [h_a] at h
              simp at h
              obtain ⟨hT, _⟩ := h
              rw [← hT]
              rw [Ty.asArr_eq h_a] at h_f
              have hf_t : HasType f (.arr A S) := infer_sound h_f
              -- HasType.array with fs = [f], Ss = [S].
              -- helem: i = 0 → HasType ([f][0]'_) (.arr A ([S][0]'_))
              --                = HasType f (.arr A S)
              exact HasType.array [S] rfl
                (fun i hi => by
                  simp at hi
                  have h0 : i = 0 := by omega
                  subst h0
                  exact hf_t)
  -- Singleton object literal: helem only needs the v case (same shape as array).
  | .object ((k, v) :: []), _, n, _, h => by
      unfold infer at h
      match h_v : infer defs n v with
      | none             => rw [h_v] at h; simp at h
      | some (T_v, n_v)  =>
          rw [h_v] at h
          simp at h
          match h_a : T_v.asArrYld with
          | none            => rw [h_a] at h; simp at h
          | some (A, V)     =>
              rw [h_a] at h
              simp at h
              obtain ⟨hT, _⟩ := h
              rw [← hT]
              rw [Ty.asArrYld_eq h_a] at h_v
              have hv_t : HasType v (.arr A (.yld V)) := infer_sound h_v
              -- HasType.object with kvs = [(k, v)], Ts = [V].
              -- The result type uses zipWith [(k, v)] [V] = [(k, V)].
              exact HasType.object [V] rfl
                (fun i hi => by
                  simp at hi
                  have h0 : i = 0 := by omega
                  subst h0
                  exact hv_t)
  -- If-then-else (precise + imprecise): the algorithm dispatches on
  -- `(refine c true).isShTop`. Either branch combines all four
  -- sub-inferences and subsumes to a shared input.
  | .ifThenElse c t e,     _, n, _, h => by
      unfold infer at h
      match h_c : infer defs n c with
      | none             => rw [h_c] at h; simp at h
      | some (T_c, n_c)  =>
          rw [h_c] at h; simp at h
          match h_ac : T_c.narrowToBoo with
          | none      => rw [h_ac] at h; simp at h
          | some A_c  =>
              rw [h_ac] at h; simp at h
              match h_t : infer defs n_c t with
              | none             => rw [h_t] at h; simp at h
              | some (T_t, n_t)  =>
                  rw [h_t] at h; simp at h
                  match h_at : T_t.asArr with
                  | none              => rw [h_at] at h; simp at h
                  | some (A_t, S_t)   =>
                      rw [h_at] at h; simp at h
                      match h_e : infer defs n_t e with
                      | none             => rw [h_e] at h; simp at h
                      | some (T_e, n_e)  =>
                          rw [h_e] at h; simp at h
                          match h_ae : T_e.asArr with
                          | none              => rw [h_ae] at h; simp at h
                          | some (A_e, S_e)   =>
                              rw [h_ae] at h; simp at h
                              -- narrowToBoo handles both direct (.sh (.boo m))
                              -- and tvar codomain via HM inst.
                              have hc_t : HasType c (.arr A_c (.yld (.sh (.boo none)))) :=
                                Ty.narrowToBoo_sound (infer_sound h_c) h_ac
                              rw [Ty.asArr_eq h_at] at h_t
                              rw [Ty.asArr_eq h_ae] at h_e
                              -- Branch on isShTop.
                              match h_imp : (refine c true).isShTop with
                              | true =>
                                  rw [h_imp] at h
                                  simp at h
                                  obtain ⟨hT, _⟩ := h
                                  rw [← hT]
                                  -- Imprecise rule: A := A_c ∩ A_t ∩ A_e.
                                  exact HasType.ifImprecise
                                    (HasType.sub hc_t
                                      (TyLE.arr
                                        (TyLE.inter_intro_l
                                          (TyLE.inter_intro_l TyLE.refl))
                                        StreamLE.refl))
                                    (Ty.isShTop_eq h_imp)
                                    (HasType.sub (infer_sound h_t)
                                      (TyLE.arr
                                        (TyLE.inter_intro_l
                                          (TyLE.inter_intro_r TyLE.refl))
                                        StreamLE.refl))
                                    (HasType.sub (infer_sound h_e)
                                      (TyLE.arr
                                        (TyLE.inter_intro_r TyLE.refl)
                                        StreamLE.refl))
                              | false =>
                                  rw [h_imp] at h
                                  simp at h
                                  obtain ⟨hT, _⟩ := h
                                  rw [← hT]
                                  -- Precise rule: A := A_c ∩ A_t ∩ A_e.
                                  -- t at input (.inter A (refine c true)),
                                  -- e at input (.inter A (refine c false)).
                                  exact HasType.ifPrecise
                                    (HasType.sub hc_t
                                      (TyLE.arr
                                        (TyLE.inter_intro_l
                                          (TyLE.inter_intro_l TyLE.refl))
                                        StreamLE.refl))
                                    (Ty.isShTop_ne h_imp)
                                    (HasType.sub (infer_sound h_t)
                                      (TyLE.arr
                                        (TyLE.inter_intro_l
                                          (TyLE.inter_intro_l
                                            (TyLE.inter_intro_r TyLE.refl)))
                                        StreamLE.refl))
                                    (HasType.sub (infer_sound h_e)
                                      (TyLE.arr
                                        (TyLE.inter_intro_l
                                          (TyLE.inter_intro_r TyLE.refl))
                                        StreamLE.refl))
  -- Two-element array literal.
  | .array (f :: g :: []),  _, n, _, h => by
      unfold infer at h
      match h_f : infer defs n f with
      | none             => rw [h_f] at h; simp at h
      | some (T_f, n_f)  =>
          rw [h_f] at h; simp at h
          match h_af : T_f.asArr with
          | none              => rw [h_af] at h; simp at h
          | some (A_f, S_f)   =>
              rw [h_af] at h; simp at h
              match h_g : infer defs n_f g with
              | none             => rw [h_g] at h; simp at h
              | some (T_g, n_g)  =>
                  rw [h_g] at h; simp at h
                  match h_ag : T_g.asArr with
                  | none              => rw [h_ag] at h; simp at h
                  | some (A_g, S_g)   =>
                      rw [h_ag] at h; simp at h
                      obtain ⟨hT, _⟩ := h
                      rw [← hT]
                      rw [Ty.asArr_eq h_af] at h_f
                      rw [Ty.asArr_eq h_ag] at h_g
                      have hf_t : HasType f (.arr A_f S_f) := infer_sound h_f
                      have hg_t : HasType g (.arr A_g S_g) := infer_sound h_g
                      have hf_sub : HasType f (.arr (.inter A_f A_g) S_f) :=
                        HasType.sub hf_t
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl) StreamLE.refl)
                      have hg_sub : HasType g (.arr (.inter A_f A_g) S_g) :=
                        HasType.sub hg_t
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl)
                      exact HasType.array [S_f, S_g] rfl
                        (fun i hi => by
                          simp at hi
                          match i, hi with
                          | 0, _ => exact hf_sub
                          | 1, _ => exact hg_sub)
  -- Two-field object literal.
  | .object ((k₁, v₁) :: (k₂, v₂) :: []),  _, n, _, h => by
      unfold infer at h
      match h_1 : infer defs n v₁ with
      | none             => rw [h_1] at h; simp at h
      | some (T_1, n_1)  =>
          rw [h_1] at h; simp at h
          match h_a1 : T_1.asArrYld with
          | none              => rw [h_a1] at h; simp at h
          | some (A_1, V_1)   =>
              rw [h_a1] at h; simp at h
              match h_2 : infer defs n_1 v₂ with
              | none             => rw [h_2] at h; simp at h
              | some (T_2, n_2)  =>
                  rw [h_2] at h; simp at h
                  match h_a2 : T_2.asArrYld with
                  | none              => rw [h_a2] at h; simp at h
                  | some (A_2, V_2)   =>
                      rw [h_a2] at h; simp at h
                      obtain ⟨hT, _⟩ := h
                      rw [← hT]
                      rw [Ty.asArrYld_eq h_a1] at h_1
                      rw [Ty.asArrYld_eq h_a2] at h_2
                      have h1_t : HasType v₁ (.arr A_1 (.yld V_1)) := infer_sound h_1
                      have h2_t : HasType v₂ (.arr A_2 (.yld V_2)) := infer_sound h_2
                      have h1_sub : HasType v₁ (.arr (.inter A_1 A_2) (.yld V_1)) :=
                        HasType.sub h1_t
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl) StreamLE.refl)
                      have h2_sub : HasType v₂ (.arr (.inter A_1 A_2) (.yld V_2)) :=
                        HasType.sub h2_t
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl)
                      exact HasType.object [V_1, V_2] rfl
                        (fun i hi => by
                          simp at hi
                          match i, hi with
                          | 0, _ => exact h1_sub
                          | 1, _ => exact h2_sub)
  -- Three-element array literal.
  | .array (f :: g :: h' :: []), _, n, _, h => by
      unfold infer at h
      match h_f : infer defs n f with
      | none             => rw [h_f] at h; simp at h
      | some (T_f, n_f)  =>
          rw [h_f] at h; simp at h
          match h_af : T_f.asArr with
          | none              => rw [h_af] at h; simp at h
          | some (A_f, S_f)   =>
              rw [h_af] at h; simp at h
              match h_g : infer defs n_f g with
              | none             => rw [h_g] at h; simp at h
              | some (T_g, n_g)  =>
                  rw [h_g] at h; simp at h
                  match h_ag : T_g.asArr with
                  | none              => rw [h_ag] at h; simp at h
                  | some (A_g, S_g)   =>
                      rw [h_ag] at h; simp at h
                      match h_h : infer defs n_g h' with
                      | none             => rw [h_h] at h; simp at h
                      | some (T_h, n_h)  =>
                          rw [h_h] at h; simp at h
                          match h_ah : T_h.asArr with
                          | none              => rw [h_ah] at h; simp at h
                          | some (A_h, S_h)   =>
                              rw [h_ah] at h; simp at h
                              obtain ⟨hT, _⟩ := h
                              rw [← hT]
                              rw [Ty.asArr_eq h_af] at h_f
                              rw [Ty.asArr_eq h_ag] at h_g
                              rw [Ty.asArr_eq h_ah] at h_h
                              have hf_t : HasType f (.arr A_f S_f) := infer_sound h_f
                              have hg_t : HasType g (.arr A_g S_g) := infer_sound h_g
                              have hh_t : HasType h' (.arr A_h S_h) := infer_sound h_h
                              -- Shared input is the triple intersection.
                              let A := Ty.inter (.inter A_f A_g) A_h
                              have hf_sub : HasType f (.arr A S_f) :=
                                HasType.sub hf_t
                                  (TyLE.arr (TyLE.inter_intro_l (TyLE.inter_intro_l TyLE.refl))
                                            StreamLE.refl)
                              have hg_sub : HasType g (.arr A S_g) :=
                                HasType.sub hg_t
                                  (TyLE.arr (TyLE.inter_intro_l (TyLE.inter_intro_r TyLE.refl))
                                            StreamLE.refl)
                              have hh_sub : HasType h' (.arr A S_h) :=
                                HasType.sub hh_t
                                  (TyLE.arr (TyLE.inter_intro_r TyLE.refl)
                                            StreamLE.refl)
                              exact HasType.array [S_f, S_g, S_h] rfl
                                (fun i hi => by
                                  simp at hi
                                  match i, hi with
                                  | 0, _ => exact hf_sub
                                  | 1, _ => exact hg_sub
                                  | 2, _ => exact hh_sub)
  -- Pipe `f | g`: handles BOTH the tvar-input case (HM inst on g's
  -- input tvar) AND the specific `g = .arrIndex idx` subtyping path.
  | .pipe f g,                       _, n, _, h => by
      unfold infer at h
      match h_f : infer defs n f with
      | none             => rw [h_f] at h; simp at h
      | some (T_f, n_f)  =>
          rw [h_f] at h; simp at h
          match h_a : T_f.asArr with
          | none              => rw [h_a] at h; simp at h
          | some (A_f, S_f)   =>
              rw [h_a] at h; simp at h
              match h_g : infer defs n_f g with
              | none             => rw [h_g] at h; simp at h
              | some (T_g, n_g)  =>
                  rw [h_g] at h; simp at h
                  match h_at : T_g.asArrTvar with
                  | some (k, S_g)  =>
                      rw [h_at] at h; simp at h
                      obtain ⟨hT, _⟩ := h
                      rw [← hT]
                      rw [Ty.asArr_eq h_a] at h_f
                      rw [Ty.asArrTvar_eq h_at] at h_g
                      have hf_t : HasType f (.arr A_f S_f) := infer_sound h_f
                      have hg_t : HasType g (.arr (.tvar k) S_g) :=
                        infer_sound h_g
                      -- Specialise g via HM inst: substitute k := elemType S_f.
                      have hg_inst :
                          HasType g (.arr (elemType S_f) (substStream k (elemType S_f) S_g)) := by
                        have := HasType.inst k (elemType S_f) hg_t
                        simp [substTy] at this
                        exact this
                      -- Apply pipe with B := elemType S_f.
                      exact HasType.pipe hf_t hg_inst TyLE.refl
                  | none =>
                      rw [h_at] at h; simp at h
                      -- subCheck-based fallback.
                      match h_a' : T_g.asArr with
                      | none              => rw [h_a'] at h; simp at h
                      | some (A_g, S_g)   =>
                          rw [h_a'] at h; simp at h
                          -- After simp, h has form:
                          -- subCheck = true ∧ result_equal ∧ counter_equal.
                          obtain ⟨h_cond, hT, _⟩ := h
                          rw [← hT]
                          rw [Ty.asArr_eq h_a] at h_f
                          rw [Ty.asArr_eq h_a'] at h_g
                          have hf_t : HasType f (.arr A_f S_f) := infer_sound h_f
                          have hg_t : HasType g (.arr A_g S_g) := infer_sound h_g
                          -- subCheck_sound gives us elemType S_f ⊑ A_g.
                          exact HasType.pipe hf_t hg_t (Ty.subCheck_sound h_cond)
  | .array (f₁ :: f₂ :: f₃ :: f₄ :: rest), _, n, _, h => by
      unfold infer at h
      match h_il : inferList defs n (f₁ :: f₂ :: f₃ :: f₄ :: rest) with
      | none             => rw [h_il] at h; simp at h
      | some (A, Ss, n') =>
          rw [h_il] at h
          simp at h
          obtain ⟨hT, _⟩ := h
          rw [← hT]
          have ⟨hlen, helem⟩ := inferList_sound h_il
          exact HasType.array Ss hlen
            (fun i h_fs => helem i h_fs (by omega))
  | .object (kv₁ :: kv₂ :: kv₃ :: rest), _, n, _, h => by
      unfold infer at h
      match h_il : inferObjList defs n (kv₁ :: kv₂ :: kv₃ :: rest) with
      | none             => rw [h_il] at h; simp at h
      | some (A, Ts, n') =>
          rw [h_il] at h
          simp at h
          obtain ⟨hT, _⟩ := h
          rw [← hT]
          have ⟨hlen, helem⟩ := inferObjList_sound h_il
          exact HasType.object Ts hlen
            (fun i h_kvs => helem i h_kvs (by omega))

/-- Soundness for `inferList`. Returns the length equation and a
    per-index typing fact taking BOTH bounds (so the caller can use
    either; HasType.array's helem uses `i < fs.length` paired with
    `by omega` for the Ss bound). -/
theorem inferList_sound : ∀ {fs : List Filter} {A : Ty} {Ss : List Stream} {n n' : Nat},
    inferList defs n fs = some (A, Ss, n') →
    fs.length = Ss.length ∧
    (∀ i (h_fs : i < fs.length) (h_ss : i < Ss.length),
      HasType (fs[i]'h_fs) (.arr A (Ss[i]'h_ss)))
  | [], _, _, _, _, h => by
      simp [inferList] at h
      obtain ⟨_, hSs, _⟩ := h
      subst hSs
      refine ⟨rfl, ?_⟩
      intro i h_fs _
      simp at h_fs
  | [f], _, _, n, _, h => by
      unfold inferList at h
      match h_f : infer defs n f with
      | none            => rw [h_f] at h; simp at h
      | some (T_f, n_f) =>
          rw [h_f] at h; simp at h
          match h_a : T_f.asArr with
          | none           => rw [h_a] at h; simp at h
          | some (Af, Sf)  =>
              rw [h_a] at h; simp at h
              obtain ⟨hA, hSs, _⟩ := h
              subst hA; subst hSs
              rw [Ty.asArr_eq h_a] at h_f
              have hf_t : HasType f (.arr Af Sf) := infer_sound h_f
              refine ⟨rfl, ?_⟩
              intro i h_fs _
              simp at h_fs
              have h0 : i = 0 := by omega
              subst h0
              exact hf_t
  | f :: g :: fs, _, _, n, _, h => by
      unfold inferList at h
      match h_f : infer defs n f with
      | none            => rw [h_f] at h; simp at h
      | some (T_f, n_f) =>
          rw [h_f] at h; simp at h
          match h_a : T_f.asArr with
          | none            => rw [h_a] at h; simp at h
          | some (A_f, S_f) =>
              rw [h_a] at h; simp at h
              match h_il : inferList defs n_f (g :: fs) with
              | none                     => rw [h_il] at h; simp at h
              | some (A_rest, Ss_rest, _) =>
                  rw [h_il] at h; simp at h
                  obtain ⟨hA, hSs, _⟩ := h
                  subst hA; subst hSs
                  rw [Ty.asArr_eq h_a] at h_f
                  have hf_t : HasType f (.arr A_f S_f) := infer_sound h_f
                  have ⟨hlen_rest, helem_rest⟩ := inferList_sound h_il
                  refine ⟨?_, ?_⟩
                  · simp; exact hlen_rest
                  · intro i h_fs h_ss
                    cases i with
                    | zero =>
                        exact HasType.sub hf_t
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl) StreamLE.refl)
                    | succ k =>
                        have hk_fs : k < (g :: fs).length := by
                          simp at h_fs; omega
                        have hk_ss : k < Ss_rest.length := by
                          simp at h_ss; omega
                        have := helem_rest k hk_fs hk_ss
                        exact HasType.sub this
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl)

/-- Soundness for `inferObjList`. Mirrors `inferList_sound` but yields
    typings of the form `.arr A (.yld V_i)` (since each value-filter
    must yield a singleton). -/
theorem inferObjList_sound : ∀ {kvs : List (String × Filter)}
    {A : Ty} {Ts : List Ty} {n n' : Nat},
    inferObjList defs n kvs = some (A, Ts, n') →
    kvs.length = Ts.length ∧
    (∀ i (h_kvs : i < kvs.length) (h_ts : i < Ts.length),
      HasType ((kvs[i]'h_kvs).snd) (.arr A (.yld (Ts[i]'h_ts))))
  | [], _, _, _, _, h => by
      simp [inferObjList] at h
      obtain ⟨_, hTs, _⟩ := h
      subst hTs
      refine ⟨rfl, ?_⟩
      intro i h_kvs _
      simp at h_kvs
  | [(k, v)], _, _, n, _, h => by
      unfold inferObjList at h
      match h_v : infer defs n v with
      | none            => rw [h_v] at h; simp at h
      | some (T_v, n_v) =>
          rw [h_v] at h; simp at h
          match h_a : T_v.asArrYld with
          | none           => rw [h_a] at h; simp at h
          | some (A', V)   =>
              rw [h_a] at h; simp at h
              obtain ⟨hA, hTs, _⟩ := h
              subst hA; subst hTs
              rw [Ty.asArrYld_eq h_a] at h_v
              have hv_t : HasType v (.arr A' (.yld V)) := infer_sound h_v
              refine ⟨rfl, ?_⟩
              intro i h_kvs h_ts
              simp at h_kvs
              have h0 : i = 0 := by omega
              subst h0
              exact hv_t
  | (k, v) :: kv' :: kvs', _, _, n, _, h => by
      unfold inferObjList at h
      match h_v : infer defs n v with
      | none            => rw [h_v] at h; simp at h
      | some (T_v, n_v) =>
          rw [h_v] at h; simp at h
          match h_a : T_v.asArrYld with
          | none             => rw [h_a] at h; simp at h
          | some (A_v, V_v)  =>
              rw [h_a] at h; simp at h
              match h_il : inferObjList defs n_v (kv' :: kvs') with
              | none                       => rw [h_il] at h; simp at h
              | some (A_rest, Ts_rest, _)  =>
                  rw [h_il] at h; simp at h
                  obtain ⟨hA, hTs, _⟩ := h
                  subst hA; subst hTs
                  rw [Ty.asArrYld_eq h_a] at h_v
                  have hv_t : HasType v (.arr A_v (.yld V_v)) := infer_sound h_v
                  have ⟨hlen_rest, helem_rest⟩ := inferObjList_sound h_il
                  refine ⟨?_, ?_⟩
                  · simp; exact hlen_rest
                  · intro i h_kvs h_ts
                    cases i with
                    | zero =>
                        exact HasType.sub hv_t
                          (TyLE.arr (TyLE.inter_intro_l TyLE.refl)
                                    (StreamLE.yld TyLE.refl))
                    | succ k =>
                        have hk_kvs : k < (kv' :: kvs').length := by
                          simp at h_kvs; omega
                        have hk_ts : k < Ts_rest.length := by
                          simp at h_ts; omega
                        have := helem_rest k hk_kvs hk_ts
                        exact HasType.sub this
                          (TyLE.arr (TyLE.inter_intro_r TyLE.refl)
                                    (StreamLE.yld TyLE.refl))

end -- mutual

end -- section (defs)

/-! ## Executable examples

    Each case of the algorithm yields the *exact* canonical type for
    its filter — by `rfl`, no tactic needed. -/

example : infer [] 0 .dot = some (.arr (.tvar 0) (.yld (.tvar 0)), 1) := rfl

example : infer [] 0 (.numLit 42) =
    some (.arr (.tvar 0) (.yld (.sh (.num (some 42)))), 1) := rfl

example : infer [] 0 (.objIndex "foo") =
    some (.arr (.union (.sh .nul) (.sh (.object [("foo", .tvar 0)])))
               (.yld (.union (.tvar 0) (.sh .nul))), 1) := rfl

example : infer [] 5 .iter =
    some (.arr (.sh (.array (.tvar 5) none)) (.star (.yld (.tvar 5))), 6) := rfl

-- Comma combines two filters; counter advances and inputs intersect.
example : infer [] 0 (.comma .dot .dot) =
    some (.arr (.inter (.tvar 0) (.tvar 1))
               (.concat (.yld (.tvar 0)) (.yld (.tvar 1))), 2) := rfl

example : infer [] 0 (.comma (.numLit 1) (.numLit 2)) =
    some (.arr (.inter (.tvar 0) (.tvar 1))
               (.concat (.yld (.sh (.num (some 1)))) (.yld (.sh (.num (some 2))))),
          2) := rfl

-- Empty array literal: A → ⟨tuple []⟩.
example : infer [] 0 (.array []) =
    some (.arr (.tvar 0) (.yld (.sh (.tuple []))), 1) := rfl

-- Calls produce a fresh tvar.
example : infer [] 7 (.call "myfunc") = some (.tvar 7, 8) := rfl

-- Unary negation widens literals to ⟨num⟩.
example : infer [] 0 (.unop .neg (.numLit 42)) =
    some (.arr (.tvar 0) (.yld (.sh (.num none))), 1) := rfl

-- Equality returns ⟨bool⟩.
example : infer [] 0 (.binop .eq (.numLit 1) (.numLit 2)) =
    some (.arr (.inter (.tvar 0) (.tvar 1)) (.yld (.sh (.boo none))), 2) := rfl

-- Numerical addition.
example : infer [] 0 (.binop .add (.numLit 1) (.numLit 2)) =
    some (.arr (.inter (.tvar 0) (.tvar 1)) (.yld (.sh (.num none))), 2) := by rfl

-- Boolean conjunction.
example : infer [] 0 (.binop .and (.boolLit true) (.boolLit false)) =
    some (.arr (.inter (.tvar 0) (.tvar 1)) (.yld (.sh (.boo none))), 2) := rfl

-- Comparison.
example : infer [] 0 (.binop .lt (.numLit 1) (.numLit 2)) =
    some (.arr (.inter (.tvar 0) (.tvar 1)) (.yld (.sh (.boo none))), 2) := rfl

-- Composed: 1 + 2 == 3 yields ⟨bool⟩.
example : infer [] 0
    (.binop .eq (.binop .add (.numLit 1) (.numLit 2)) (.numLit 3)) =
    some (.arr (.inter (.inter (.tvar 0) (.tvar 1)) (.tvar 2))
               (.yld (.sh (.boo none))),
          3) := rfl

-- error has bottom-stream codomain (distinct from empty's eps).
example : infer [] 0 .error = some (.arr (.tvar 0) .bot, 1) := rfl
example : infer [] 0 .empty = some (.arr (.tvar 0) .eps, 1) := rfl

-- Singleton array: [42] reads as a 1-tuple of nums.
example : infer [] 0 (.array [.numLit 42]) =
    some (.arr (.tvar 0) (.yld (.sh (.tuple [.sh (.num (some 42))]))), 1) := rfl

-- Singleton object: {"a": 1} reads as a record with one field.
example : infer [] 0 (.object [("a", .numLit 1)]) =
    some (.arr (.tvar 0) (.yld (.sh (.object [("a", .sh (.num (some 1)))]))),
          1) := rfl

-- Two-element array: [1, 2] reads as a 2-tuple.
example : infer [] 0 (.array [.numLit 1, .numLit 2]) =
    some (.arr (.inter (.tvar 0) (.tvar 1))
               (.yld (.sh (.tuple [.sh (.num (some 1)), .sh (.num (some 2))]))),
          2) := rfl

-- Three-element array: [1, 2, 3].
example : infer [] 0 (.array [.numLit 1, .numLit 2, .numLit 3]) =
    some (.arr (.inter (.inter (.tvar 0) (.tvar 1)) (.tvar 2))
               (.yld (.sh (.tuple [.sh (.num (some 1)),
                                    .sh (.num (some 2)),
                                    .sh (.num (some 3))]))),
          3) := rfl

-- Five-element array via the recursive `inferList` path.
example : infer [] 0
    (.array [.numLit 1, .numLit 2, .numLit 3, .numLit 4, .numLit 5]) =
    some (.arr (.inter (.tvar 0)
                  (.inter (.tvar 1)
                    (.inter (.tvar 2)
                      (.inter (.tvar 3) (.tvar 4)))))
               (.yld (.sh (.tuple [.sh (.num (some 1)),
                                    .sh (.num (some 2)),
                                    .sh (.num (some 3)),
                                    .sh (.num (some 4)),
                                    .sh (.num (some 5))]))),
          5) := rfl

-- Three-field object via the recursive `inferObjList` path.
example : infer [] 0
    (.object [("a", .numLit 1), ("b", .numLit 2), ("c", .numLit 3)]) =
    some (.arr (.inter (.tvar 0) (.inter (.tvar 1) (.tvar 2)))
               (.yld (.sh (.object [("a", .sh (.num (some 1))),
                                     ("b", .sh (.num (some 2))),
                                     ("c", .sh (.num (some 3)))]))),
          3) := rfl

-- Four-field object.
example : infer [] 0
    (.object [("a", .numLit 1), ("b", .strLit "hi"),
              ("c", .boolLit true), ("d", .nullLit)]) =
    some (.arr (.inter (.tvar 0)
                  (.inter (.tvar 1)
                    (.inter (.tvar 2) (.tvar 3))))
               (.yld (.sh (.object [("a", .sh (.num (some 1))),
                                     ("b", .sh (.str (some "hi"))),
                                     ("c", .sh (.boo (some true))),
                                     ("d", .sh .nul)]))),
          4) := rfl

-- Pipe with identity on the right: `42 | .` is just `42`-shaped (after
-- the elemType widening, here a no-op since the stream is a singleton).
-- The general pipe rule infers `.dot` separately, so two tvars are
-- introduced (one for `.numLit`, one for `.dot`).
example : infer [] 0 (.pipe (.numLit 42) .dot) =
    some (.arr (.tvar 0) (.yld (.sh (.num (some 42)))), 2) := rfl

-- `(.[] | .)` over an array: each element flows through identity.
example : infer [] 0 (.pipe .iter .dot) =
    some (.arr (.sh (.array (.tvar 0) none))
               (.star (.yld (.tvar 0))),
          2) := rfl

-- Pipe a number literal into another (constant) literal.
example : infer [] 0 (.pipe (.numLit 42) (.numLit 7)) =
    some (.arr (.tvar 0) (.yld (.sh (.num (some 7)))), 2) := rfl

-- Pipe into a filter whose input is *not* a tvar fails: `.iter` needs
-- an array input. (The general pipe needs a real subtyping check.)
example : infer [] 0 (.pipe (.numLit 42) .iter) = none := rfl

-- Pipe into `.objIndex`: same reason — input is `nul ∪ object`.
example : infer [] 0 (.pipe (.numLit 42) (.objIndex "k")) = none := rfl

-- Two-field object: {"a": 1, "b": "hi"}.
example : infer [] 0 (.object [("a", .numLit 1), ("b", .strLit "hi")]) =
    some (.arr (.inter (.tvar 0) (.tvar 1))
               (.yld (.sh (.object [("a", .sh (.num (some 1))),
                                     ("b", .sh (.str (some "hi")))]))),
          2) := rfl

-- Composed: !(1 + 2) — unop wraps a binop yielding ⟨num⟩.
example : infer [] 0
    (.unop .neg (.binop .add (.numLit 1) (.numLit 2))) =
    some (.arr (.inter (.tvar 0) (.tvar 1)) (.yld (.sh (.num none))), 2) := rfl

-- if-then-else with a `.dot` guard: NOW infers via `narrowToBoo`'s
-- HM-inst path, specializing `.dot`'s tvar codomain to `.sh (.boo none)`.
-- The result substitutes `.dot`'s input tvar with `.sh (.boo none)` too.
-- (The `Ty` shape is complex; we just check it succeeds.)
example : (infer [] 0 (.ifThenElse .dot (.numLit 1) (.numLit 2))).isSome := by
  decide

-- if-then-else with a guard that has *no* useful refinement.
-- A `.boolLit` literal isn't a recognised refinable guard, so refine
-- returns `.sh .top` and the imprecise rule fires.
example : infer [] 0
    (.ifThenElse (.boolLit true) (.numLit 100) (.numLit 200)) =
    some (.arr (.inter (.inter (.tvar 0) (.tvar 1)) (.tvar 2))
               (.choice (.yld (.sh (.num (some 100))))
                        (.yld (.sh (.num (some 200))))),
          3) := rfl

-- A bool-yielding `.binop .eq` whose neither operand is `.dot`:
-- refine falls through to `.sh .top`, so imprecise rule fires.
example : infer [] 0
    (.ifThenElse (.binop .eq (.numLit 1) (.numLit 1))
                 (.numLit 100)
                 (.numLit 200)) =
    some (.arr (.inter (.inter (.inter (.tvar 0) (.tvar 1)) (.tvar 2)) (.tvar 3))
               (.choice (.yld (.sh (.num (some 100))))
                        (.yld (.sh (.num (some 200))))),
          4) := rfl

-- A guard `.dot == 1` *does* refine — uses the precise rule.
-- refine c true = .sh (.num (some 1)), refine c false = .neg_sh (.num (some 1)).
-- Result: (num 1 → ⟨100⟩) ∩ (¬num 1 → ⟨200⟩).
example : infer [] 0
    (.ifThenElse (.binop .eq .dot (.numLit 1))
                 (.numLit 100)
                 (.numLit 200)) =
    some (.inter
            (.arr (.sh (.num (some 1))) (.yld (.sh (.num (some 100)))))
            (.arr (.neg_sh (.num (some 1))) (.yld (.sh (.num (some 200))))),
          4) := rfl

/-! ## What completeness *would* say

    A completeness statement: every well-typed filter is inferable.

        ∀ f T, HasType f T → ∃ n', ∀ n, ∃ T', T' ⊑ T ∧ infer n f = some (T', n')

    (For now, only the simple constructors; compositions need the
    constraint solver.) -/

end Tjq
