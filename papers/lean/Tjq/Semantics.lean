import Tjq.Syntax
import Tjq.Subtyping

/-!
# Tjq.Semantics

The denotational semantics of `Ty` over JSON values, and the soundness
statement of the syntactic subtyping kernel relative to that semantics.

## Why a recursive `def`, not an inductive `Prop`

We had to define `Value : Json → Ty → Prop` as a *recursive function*,
not as an `inductive`, because the natural rule

    | neg : ¬ Value j t → Value j (Neg t)

violates inductive *strict positivity*: `Value` appears on the
contravariant side of the implication `Value j t → False`.

The recursive-`def` formulation handles this naturally — `¬ Value j t`
is just a value of type `Prop`, computed by recursion on `t`. The
structural decrease is on the `Ty` argument (the `Json` is unchanged).
-/

namespace Tjq

/-! ## §1. The inductive `Value` semantics

    With the NNF restriction on `Ty` (`neg_sh : Shape → Ty`, no `neg` on
    compound types), we can define `Value` as an inductive predicate.
    The `neg_sh` constructor uses a separate `NotValueShape` predicate,
    so the strict-positivity check passes.

    Three mutually-defined inductives:

    * `Value : Json → Ty → Prop` — JSON inhabits Ty.
    * `ValueShape : Json → Shape → Prop` — JSON inhabits Shape (positive).
    * `NotValueShape : Json → Shape → Prop` — JSON does *not* inhabit
      Shape (negative). Constructors enumerate the failure cases.

    The compound shape cases (`array`, `tuple`, `object`) recurse into
    `Value` via `(∀ x ∈ xs, Value x t)`-style premises. This is in the
    *positive* position for Value, so it's accepted. The `negNNF`
    helper lets the array's `wrong-element` case state "the element is
    in the *complement* of the element type" without nesting `Neg` in
    `Ty`. -/

mutual

inductive Value : Json → Ty → Prop where
  | sh      : ValueShape j s    → Value j (.sh s)
  | neg_sh  : NotValueShape j s → Value j (.neg_sh s)
  | inter   : Value j t₁ → Value j t₂ → Value j (.inter t₁ t₂)
  | union_l : Value j t₁ → Value j (.union t₁ t₂)
  | union_r : Value j t₂ → Value j (.union t₁ t₂)
  -- No rules for `arr a s` (filter types — uninhabited at JSON level).
  -- No rules for `tvar n` (placeholder — uninhabited semantically).

inductive ValueShape : Json → Shape → Prop where
  -- `Sh top` accepts any JSON value.
  | top {j} : ValueShape j .top
  -- Atomic kinds.
  | nul             : ValueShape .nul .nul
  | boo_some {b}    : ValueShape (.boo b) (.boo (some b))
  | boo_none {b}    : ValueShape (.boo b) (.boo none)
  | num_some {n}    : ValueShape (.num n) (.num (some n))
  | num_none {n}    : ValueShape (.num n) (.num none)
  | str_some {s}    : ValueShape (.str s) (.str (some s))
  | str_none {s}    : ValueShape (.str s) (.str none)
  -- Arrays.
  | array {xs t n}  :
      (∀ x, x ∈ xs → Value x t) →
      n.elim True (fun k => xs.length ≥ k) →
      ValueShape (.arr xs) (.array t n)
  -- Tuples (positional prefix).
  | tuple {xs ts}   :
      ts.length ≤ xs.length →
      (∀ p, p ∈ List.zip ts xs → Value p.2 p.1) →
      ValueShape (.arr xs) (.tuple ts)
  -- Objects (open record): each declared key is present at the right type.
  -- We avoid `∃ v, …` in the premise (Lean rejects nested `Exists`) by
  -- using `Option.isSome` to assert presence and pairing it with the
  -- "if present then well-typed" universal.
  | object {kvObj kvs} :
      (∀ k t', (k, t') ∈ kvs →
        ∀ v, kvObj.lookup k = some v → Value v t') →
      (∀ k t', (k, t') ∈ kvs → (kvObj.lookup k).isSome = true) →
      ValueShape (.obj kvObj) (.object kvs)

inductive NotValueShape : Json → Shape → Prop where
  -- `Sh top` is universal — its negation is uninhabited (no constructor).
  -- Atomic-shape negations: `j` simply isn't the matching constructor.
  | nul_other {j}             : j ≠ .nul → NotValueShape j .nul
  | boo_some_other {j b}      : j ≠ .boo b → NotValueShape j (.boo (some b))
  | boo_none_other {j}        : (∀ b, j ≠ .boo b) → NotValueShape j (.boo none)
  | num_some_other {j n}      : j ≠ .num n → NotValueShape j (.num (some n))
  | num_none_other {j}        : (∀ n, j ≠ .num n) → NotValueShape j (.num none)
  | str_some_other {j s}      : j ≠ .str s → NotValueShape j (.str (some s))
  | str_none_other {j}        : (∀ s, j ≠ .str s) → NotValueShape j (.str none)
  -- Array: not an array, OR an element fails the type, OR too short.
  | array_not_arr {j t n}     : (∀ xs, j ≠ .arr xs) → NotValueShape j (.array t n)
  | array_wrong_elem {xs t n x} :
      x ∈ xs → Value x (negNNF t) → NotValueShape (.arr xs) (.array t n)
  | array_too_short {xs t k}  :
      xs.length < k → NotValueShape (.arr xs) (.array t (some k))
  -- Tuple: not an array, OR too short, OR a prefix element fails.
  | tuple_not_arr {j ts}      : (∀ xs, j ≠ .arr xs) → NotValueShape j (.tuple ts)
  | tuple_too_short {xs ts}   :
      xs.length < ts.length → NotValueShape (.arr xs) (.tuple ts)
  | tuple_wrong_elem {xs ts p}  :
      p ∈ List.zip ts xs → Value p.2 (negNNF p.1) →
      NotValueShape (.arr xs) (.tuple ts)
  -- Object: not an object, OR missing key, OR wrong-type key.
  | object_not_obj {j kvs}    : (∀ kvObj, j ≠ .obj kvObj) → NotValueShape j (.object kvs)
  | object_missing_key {kvObj kvs k t} :
      (k, t) ∈ kvs → kvObj.lookup k = none →
      NotValueShape (.obj kvObj) (.object kvs)
  | object_wrong_value {kvObj kvs k t v} :
      (k, t) ∈ kvs → kvObj.lookup k = some v → Value v (negNNF t) →
      NotValueShape (.obj kvObj) (.object kvs)

end -- mutual

/-- `Trace s js` — the JSON-value sequence `js` matches stream type `s`. -/
inductive Trace : Stream → List Json → Prop where
  | eps : Trace .eps []
  | yld {j t} : Value j t → Trace (.yld t) [j]
  | concat {s₁ s₂ js₁ js₂} :
      Trace s₁ js₁ → Trace s₂ js₂ → Trace (.concat s₁ s₂) (js₁ ++ js₂)
  | star_nil {s} : Trace (.star s) []
  | star_cons {s js₁ js₂} :
      Trace s js₁ → Trace (.star s) js₂ →
      Trace (.star s) (js₁ ++ js₂)
  | choice_l {s₁ s₂ js} : Trace s₁ js → Trace (.choice s₁ s₂) js
  | choice_r {s₁ s₂ js} : Trace s₂ js → Trace (.choice s₁ s₂) js

/-! Aliases for set-theoretic semantic subtyping. -/

abbrev TyLE_sem    (t₁ t₂ : Ty)    : Prop := ∀ j, Value j t₁ → Value j t₂
abbrev ShapeLE_sem (s₁ s₂ : Shape) : Prop := ∀ j, Value j (.sh s₁) → Value j (.sh s₂)

/-! ## Soundness — statement

    `TyLE` is sound relative to `Value`-set inclusion: every syntactic
    subtyping witness yields a semantic value-set inclusion.

    The proofs (`TyLE.sound` / `ShapeLE.sound`) are a mutual `cases`
    recursion at the end of this file, dispatching each rule to its
    per-rule witness (below). `StreamLE` is not needed: arrow types are
    uninhabited by `Value`, so `TyLE.arr` is vacuous. -/

/-! ## Soundness — per-rule witnesses

    Easy cases proved; harder ones sorried with detailed rationale. -/

-- ─────── Trivial ───────

theorem TyLE.refl_sound (t : Ty) : TyLE_sem t t :=
  fun _ h => h

theorem TyLE.trans_sound {t₁ t₂ t₃ : Ty}
    (h₁ : TyLE_sem t₁ t₂) (h₂ : TyLE_sem t₂ t₃) :
    TyLE_sem t₁ t₃ :=
  fun j hj => h₂ j (h₁ j hj)

-- ─────── Set algebra (proved) ───────

/-- Set-algebra soundness — proved directly by case-analysis on `Value`,
    which is now an inductive Prop with explicit constructors for
    `inter`, `union_l`, `union_r`, and `neg_sh`. -/

theorem TyLE.inter_intro_l_sound {t₁ t₂ t : Ty}
    (h : TyLE_sem t₁ t) : TyLE_sem (.inter t₁ t₂) t := by
  intro j hj
  cases hj with
  | inter h₁ _ => exact h j h₁

theorem TyLE.inter_intro_r_sound {t₁ t₂ t : Ty}
    (h : TyLE_sem t₂ t) : TyLE_sem (.inter t₁ t₂) t := by
  intro j hj
  cases hj with
  | inter _ h₂ => exact h j h₂

theorem TyLE.inter_elim_sound {t t₁ t₂ : Ty}
    (h₁ : TyLE_sem t t₁) (h₂ : TyLE_sem t t₂) :
    TyLE_sem t (.inter t₁ t₂) :=
  fun j hj => Value.inter (h₁ j hj) (h₂ j hj)

theorem TyLE.union_intro_l_sound {t₁ t₂ : Ty} :
    TyLE_sem t₁ (.union t₁ t₂) :=
  fun _ h => Value.union_l h

theorem TyLE.union_intro_r_sound {t₁ t₂ : Ty} :
    TyLE_sem t₂ (.union t₁ t₂) :=
  fun _ h => Value.union_r h

theorem TyLE.union_elim_sound {t₁ t₂ t : Ty}
    (h₁ : TyLE_sem t₁ t) (h₂ : TyLE_sem t₂ t) :
    TyLE_sem (.union t₁ t₂) t := by
  intro j hj
  cases hj with
  | union_l h => exact h₁ j h
  | union_r h => exact h₂ j h

-- ─────── ShapeLE structural witnesses (proved) ───────

/-- `ShapeLE.refl` is sound. -/
theorem ShapeLE.refl_sound (s : Shape) : ShapeLE_sem s s :=
  fun _ h => h

/-- `ShapeLE.trans` is sound. -/
theorem ShapeLE.trans_sound {s₁ s₂ s₃ : Shape}
    (h₁ : ShapeLE_sem s₁ s₂) (h₂ : ShapeLE_sem s₂ s₃) :
    ShapeLE_sem s₁ s₃ :=
  fun j hj => h₂ j (h₁ j hj)

/-- `ShapeLE.top` is sound: every value inhabits `Sh ⊤`. -/
theorem ShapeLE.top_sound (s : Shape) : ShapeLE_sem s .top :=
  fun _ _ => Value.sh ValueShape.top

/-- A `some`-refined boolean value also inhabits the unrefined `boo none`. -/
theorem ShapeLE.boo_some_to_none_sound {b : Bool} :
    ShapeLE_sem (.boo (some b)) (.boo none) := by
  intro j h
  cases h with | sh hs => cases hs with | boo_some => exact Value.sh ValueShape.boo_none

/-- A `some`-refined numeric value also inhabits the unrefined `num none`. -/
theorem ShapeLE.num_some_to_none_sound {n : Int} :
    ShapeLE_sem (.num (some n)) (.num none) := by
  intro j h
  cases h with | sh hs => cases hs with | num_some => exact Value.sh ValueShape.num_none

/-- A `some`-refined string value also inhabits the unrefined `str none`. -/
theorem ShapeLE.str_some_to_none_sound {s : String} :
    ShapeLE_sem (.str (some s)) (.str none) := by
  intro j h
  cases h with | sh hs => cases hs with | str_some => exact Value.sh ValueShape.str_none

/-- Every object inhabits the empty open record `{}` — its (vacuous) key
    obligations hold trivially. -/
theorem ShapeLE.object_nil_sound {obj : List (String × Ty)} :
    ShapeLE_sem (.object obj) (.object []) := by
  intro j h
  cases h with
  | sh hs =>
      cases hs with
      | @object kvObj _ _ _ =>
          refine Value.sh (ValueShape.object ?_ ?_) <;>
            intro k t' hkt <;> simp at hkt

/-- Array covariance: elements refine pointwise (`ht`) and the min-length
    only relaxes (`MinLE`). -/
theorem ShapeLE.array_sound {t t' : Ty} {n n' : Option Nat}
    (ht : TyLE_sem t t') (hn : MinLE n n') :
    ShapeLE_sem (.array t n) (.array t' n') := by
  intro j h
  cases h with
  | sh hs =>
      cases hs with
      | @array xs _ _ h_elem h_len =>
          refine Value.sh (ValueShape.array (fun x hx => ht x (h_elem x hx)) ?_)
          cases n' with
          | none => trivial
          | some m =>
              cases n with
              | none => exact hn.elim
              | some k =>
                  have hk : k ≥ m := hn
                  have hxk : xs.length ≥ k := h_len
                  simp only [Option.elim]
                  omega

/-- Open-record extension: the head key `(k, u)` is present in `obj` at a
    subtype `t ≤ u` (`hmem`, `hu`), and the tail `rest` is already covered
    (`hrest`). -/
theorem ShapeLE.object_cons_sound {obj rest : List (String × Ty)}
    {k : String} {t u : Ty}
    (hmem : (k, t) ∈ obj) (hu : TyLE_sem t u)
    (hrest : ShapeLE_sem (.object obj) (.object rest)) :
    ShapeLE_sem (.object obj) (.object ((k, u) :: rest)) := by
  intro j h
  cases h with
  | sh hs =>
      cases hs with
      | @object kvObj _ h_match h_present =>
          have hobj : Value (.obj kvObj) (.sh (.object obj)) :=
            Value.sh (ValueShape.object h_match h_present)
          have hr := hrest _ hobj
          cases hr with
          | sh hrs =>
              cases hrs with
              | @object _ _ h_match_rest h_present_rest =>
                  refine Value.sh (ValueShape.object ?_ ?_)
                  · intro k' t' hk't' v hlk
                    rw [List.mem_cons] at hk't'
                    rcases hk't' with heq | hin
                    · simp only [Prod.mk.injEq] at heq
                      obtain ⟨hk_eq, ht_eq⟩ := heq
                      rw [ht_eq]
                      rw [hk_eq] at hlk
                      exact hu v (h_match k t hmem v hlk)
                    · exact h_match_rest k' t' hin v hlk
                  · intro k' t' hk't'
                    rw [List.mem_cons] at hk't'
                    rcases hk't' with heq | hin
                    · simp only [Prod.mk.injEq] at heq
                      obtain ⟨hk_eq, _⟩ := heq
                      rw [hk_eq]
                      exact h_present k t hmem
                    · exact h_present_rest k' t' hin

/-- Tuple → tuple: `us` is a prefix of `ts` (`hlen`) and refines it
    pointwise (`helem`). The array underlying the value is at least as long
    as `ts`, hence as `us`, and each `us`-position is covered by the
    corresponding `ts`-position through `helem`. -/
theorem ShapeLE.tuple_tuple_sound {ts us : List Ty}
    (hlen : ts.length ≥ us.length)
    (helem : ∀ i (h : i < us.length),
        TyLE_sem (ts[i]'(by omega)) (us[i]'h)) :
    ShapeLE_sem (.tuple ts) (.tuple us) := by
  intro j hv
  cases hv with
  | sh hs =>
      cases hs with
      | @tuple xs _ h_len_ts h_elem =>
          refine Value.sh (ValueShape.tuple ?_ ?_)
          · omega
          · intro q hq
            obtain ⟨i, hi, hqi⟩ := List.mem_iff_getElem.mp hq
            rw [List.length_zip] at hi
            have hi_us : i < us.length := by omega
            have hi_xs : i < xs.length := by omega
            have hi_ts : i < ts.length := by omega
            rw [List.getElem_zip] at hqi
            have hmem_ts : (ts[i], xs[i]) ∈ ts.zip xs := by
              rw [List.mem_iff_getElem]
              exact ⟨i, by rw [List.length_zip]; omega, by rw [List.getElem_zip]⟩
            have hval : Value xs[i] ts[i] := h_elem _ hmem_ts
            have hconv : Value xs[i] us[i] := helem i hi_us xs[i] hval
            rw [← hqi]
            exact hconv

/-- Array → tuple: an array `t`-typed and at least `n ≥ ts.length` long is
    below the (open, prefix) tuple `ts` — its first `ts.length` elements are
    each `t ≤ ts[i]` (`helem`), and there are enough of them (`hge`). -/
theorem ShapeLE.array_tuple_sound {t : Ty} {ts : List Ty} {n : Nat}
    (hge : n ≥ ts.length)
    (helem : ∀ i (h : i < ts.length), TyLE_sem t (ts[i]'h)) :
    ShapeLE_sem (.array t (some n)) (.tuple ts) := by
  intro j hv
  cases hv with
  | sh hs =>
      cases hs with
      | @array xs _ _ h_elem h_len =>
          have hxn : xs.length ≥ n := h_len
          refine Value.sh (ValueShape.tuple ?_ ?_)
          · omega
          · intro q hq
            obtain ⟨i, hi, hqi⟩ := List.mem_iff_getElem.mp hq
            rw [List.length_zip] at hi
            have hi_ts : i < ts.length := by omega
            have hi_xs : i < xs.length := by omega
            rw [List.getElem_zip] at hqi
            have hval : Value xs[i] t := h_elem _ (List.getElem_mem _)
            have hconv : Value xs[i] ts[i] := helem i hi_ts xs[i] hval
            rw [← hqi]
            exact hconv

/-! ## Disjointness lemmas

    `ValueShape` and `NotValueShape` should be mutually exclusive. The
    atomic-shape cases (top/nul/boo/num/str) are proved directly. The
    compound-shape cases (array/tuple/object) recurse into `Value` for
    the element/field types — see `Value_negNNF_disjoint` — and that
    recursion currently runs afoul of Lean's structural-termination
    heuristic. The compound cases are sorried with detailed comments.

    A complete proof would either:
    * Provide explicit `decreasing_by` for the structural recursion
      through `List.zip` and `kvObj.lookup`; or
    * Restate the disjointness claim with a strong-induction principle
      derived once (e.g. on `sizeOf t`) and applied uniformly. -/

-- The two disjointness facts are mutually recursive — `ValueShape` vs
-- `NotValueShape` on a container recurses through `Value` vs `negNNF Value`
-- on its element types, and vice versa. Proved together by well-founded
-- recursion on the `Shape`/`Ty` size.
mutual

theorem ValueShape_NotValueShape_disjoint : ∀ {j : Json} {s : Shape},
    ValueShape j s → NotValueShape j s → False
  | _, .top, _, hn => by cases hn
  | _, .nul, hv, hn => by
      cases hv with | nul => cases hn with | nul_other h => exact h rfl
  | _, .boo (some _), hv, hn => by
      cases hv with | boo_some => cases hn with | boo_some_other h => exact h rfl
  | _, .boo none, hv, hn => by
      cases hv with | boo_none => cases hn with | boo_none_other h => exact (h _) rfl
  | _, .num (some _), hv, hn => by
      cases hv with | num_some => cases hn with | num_some_other h => exact h rfl
  | _, .num none, hv, hn => by
      cases hv with | num_none => cases hn with | num_none_other h => exact (h _) rfl
  | _, .str (some _), hv, hn => by
      cases hv with | str_some => cases hn with | str_some_other h => exact h rfl
  | _, .str none, hv, hn => by
      cases hv with | str_none => cases hn with | str_none_other h => exact (h _) rfl
  | _, .array t n, hv, hn => by
      cases hv with
      | @array xs _ _ h_elem h_len =>
        cases hn with
        | array_not_arr h => exact (h xs) rfl
        | @array_wrong_elem _ _ _ x h_mem h_neg =>
            exact Value_negNNF_disjoint (h_elem x h_mem) h_neg
        | @array_too_short _ _ k h_short =>
            have : xs.length ≥ k := h_len
            omega
  | _, .tuple ts, hv, hn => by
      cases hv with
      | @tuple xs _ h_len h_elem =>
        cases hn with
        | tuple_not_arr h => exact (h xs) rfl
        | tuple_too_short h_short => omega
        | @tuple_wrong_elem _ _ p h_pmem h_pneg =>
            -- `p.1 ∈ ts` (via `List.of_mem_zip`) makes the recursion on the
            -- element type `p.1` well-founded: `sizeOf p.1 < sizeOf ts <
            -- sizeOf (tuple ts)` (discharged in `decreasing_by`).
            exact Value_negNNF_disjoint (h_elem p h_pmem) h_pneg
  | _, .object kvs, hv, hn => by
      cases hv with
      | @object kvObj _ h_match h_present =>
        cases hn with
        | object_not_obj h => exact (h kvObj) rfl
        | @object_missing_key _ _ k t h_kt_in h_lookup =>
            have h := h_present k t h_kt_in
            rw [h_lookup] at h
            simp at h
        | @object_wrong_value _ _ k t v h_kt_in h_lookup h_neg =>
            -- `(k, t) ∈ kvs` (h_kt_in) makes the recursion on the value type
            -- `t` well-founded: `sizeOf t < sizeOf (k, t) ≤ sizeOf kvs <
            -- sizeOf (object kvs)` (discharged in `decreasing_by`).
            exact Value_negNNF_disjoint (h_match k t h_kt_in v h_lookup) h_neg
termination_by _ s => sizeOf s
decreasing_by
  -- Atomic/array recursions are structural (`decreasing_tactic`). The two
  -- list-element recursions reach the element type through `List.zip`
  -- (tuple) / membership (object), so we discharge them explicitly: the
  -- element type is a member of the shape's list, hence strictly smaller.
  all_goals
    first
      | decreasing_tactic
      | -- tuple: `sizeOf p.fst < sizeOf (tuple ts)` via `p.fst ∈ ts`
        (have h := List.sizeOf_lt_of_mem (List.of_mem_zip h_pmem).1
         simp only [Shape.tuple.sizeOf_spec]
         omega)
      | -- object: `sizeOf t < sizeOf (object kvs)` via `(k, t) ∈ kvs`
        (have h := List.sizeOf_lt_of_mem h_kt_in
         simp only [Shape.object.sizeOf_spec, Prod.mk.sizeOf_spec] at h ⊢
         omega)

/-- Companion lemma: a value can't simultaneously inhabit `t` and its
    NNF negation `negNNF t`. -/
theorem Value_negNNF_disjoint {j t}
    (hv : Value j t) (hn : Value j (negNNF t)) : False := by
  match t, hv, hn with
  | .sh s, hv, hn =>
      cases hv with
      | sh hvs =>
        simp only [negNNF] at hn
        cases hn with
        | neg_sh hns => exact ValueShape_NotValueShape_disjoint hvs hns
  | .neg_sh s, hv, hn =>
      cases hv with
      | neg_sh hvn =>
        simp only [negNNF] at hn
        cases hn with
        | sh hns => exact ValueShape_NotValueShape_disjoint hns hvn
  | .inter t₁ t₂, hv, hn =>
      cases hv with
      | inter hv₁ hv₂ =>
        simp only [negNNF] at hn
        cases hn with
        | union_l hn₁ => exact Value_negNNF_disjoint hv₁ hn₁
        | union_r hn₂ => exact Value_negNNF_disjoint hv₂ hn₂
  | .union t₁ t₂, hv, hn =>
      simp only [negNNF] at hn
      cases hn with
      | inter hn₁ hn₂ =>
        cases hv with
        | union_l hv₁ => exact Value_negNNF_disjoint hv₁ hn₁
        | union_r hv₂ => exact Value_negNNF_disjoint hv₂ hn₂
  | .arr _ _, hv, _ => cases hv
  | .tvar _, hv, _ => cases hv
termination_by sizeOf t

end

/-- Negation contravariance at the shape level. Stated using ShapeLE
    semantically (∀ j, ValueShape j s' → ValueShape j s) — i.e. s' ≤ s
    ⟹ neg_sh s ≤ neg_sh s'. -/
theorem TyLE.neg_sh_sound {s s' : Shape}
    (h_shape : ∀ j, ValueShape j s' → ValueShape j s)
    (h_not : ∀ j, NotValueShape j s → NotValueShape j s') :
    TyLE_sem (.neg_sh s) (.neg_sh s') := by
  intro j hj
  cases hj with
  | neg_sh h_neg => exact Value.neg_sh (h_not j h_neg)

/-- Ex falso: `Sh(s) ∩ ¬Sh(s)` is empty — the shape and its complement
    have no JSON value in common. -/
theorem TyLE.bot_min_sh_sound {s : Shape} {t' : Ty} :
    TyLE_sem (.inter (.sh s) (.neg_sh s)) t' := by
  intro j hj
  cases hj with
  | inter hs hns =>
      cases hs with
      | sh hvs =>
          cases hns with
          | neg_sh hnvs =>
              exact (ValueShape_NotValueShape_disjoint hvs hnvs).elim

/-- Vacuous: arrow intersection has no JSON inhabitants. -/
theorem TyLE.arrow_inter_dom_sound {a₁ a₂ : Ty} {s : Stream} :
    TyLE_sem (.inter (.arr a₁ s) (.arr a₂ s))
             (.arr (.union a₁ a₂) s) := by
  intro j hj
  cases hj with
  | inter h_l _ =>
      -- h_l : Value j (.arr a₁ s) — this has no constructor, contradiction.
      cases h_l

-- ─────── Phase 1–3 (sorried, with semantic justifications) ───────

/-- The kind of a JSON value, defined by its constructor. -/
def jsonKind : Json → Kind
  | .nul    => .Nul
  | .boo _  => .Boo
  | .num _  => .Num
  | .str _  => .Str
  | .arr _  => .Arr
  | .obj _  => .Obj

/-- Helper lemma: if `j` inhabits shape `s` and `s` has kind `k`,
    then `jsonKind j = k`. -/
theorem ValueShape_jsonKind {j s k} (hv : ValueShape j s)
    (hk : ShapeKind s k) : jsonKind j = k := by
  cases hv with
  | top         => cases hk
  | nul         => cases hk; rfl
  | boo_some    => cases hk; rfl
  | boo_none    => cases hk; rfl
  | num_some    => cases hk; rfl
  | num_none    => cases hk; rfl
  | str_some    => cases hk; rfl
  | str_none    => cases hk; rfl
  | array _ _   => cases hk; rfl
  | tuple _ _   => cases hk; rfl
  | object _ _  => cases hk; rfl

/-- **Kind disjointness** is sound: shapes of different kinds have
    no JSON value in common. -/
theorem TyLE.kind_disjoint_sound {s₁ s₂ k₁ k₂ t}
    (h₁ : ShapeKind s₁ k₁) (h₂ : ShapeKind s₂ k₂) (hne : k₁ ≠ k₂) :
    TyLE_sem (.inter (.sh s₁) (.sh s₂)) t := by
  intro j hj
  cases hj with
  | inter hi₁ hi₂ =>
      cases hi₁ with
      | sh hvs₁ =>
          cases hi₂ with
          | sh hvs₂ =>
              have eq₁ := ValueShape_jsonKind hvs₁ h₁
              have eq₂ := ValueShape_jsonKind hvs₂ h₂
              exact absurd (eq₁.symm.trans eq₂) hne

/-- **Top decomposition** is sound: every JSON value is in one of the
    six kind-tops. Case-analyses the JSON constructor. -/
theorem TyLE.top_decomp_sound :
    TyLE_sem (.sh .top)
             (.union (.sh .nul)
                (.union (.sh (.boo none))
                  (.union (.sh (.num none))
                    (.union (.sh (.str none))
                      (.union (.sh (.array (.sh .top) none))
                        (.sh (.object []))))))) := by
  intro j _
  -- The hypothesis `Value j (.sh .top)` is true for any j (by `ValueShape.top`).
  -- We case-split on j directly to land in the right union branch.
  cases j with
  | nul =>
      exact Value.union_l (Value.sh ValueShape.nul)
  | boo b =>
      exact Value.union_r (Value.union_l (Value.sh ValueShape.boo_none))
  | num n =>
      exact Value.union_r (Value.union_r (Value.union_l
        (Value.sh ValueShape.num_none)))
  | str s =>
      exact Value.union_r (Value.union_r (Value.union_r (Value.union_l
        (Value.sh ValueShape.str_none))))
  | arr xs =>
      -- Need: Value (.arr xs) (.sh (.array (.sh .top) none)).
      -- ValueShape.array takes (∀ x ∈ xs, Value x (.sh .top)) and the min-length premise.
      refine Value.union_r (Value.union_r (Value.union_r (Value.union_r
        (Value.union_l (Value.sh (ValueShape.array ?_ ?_))))))
      · intro x _; exact Value.sh ValueShape.top
      · -- n.elim True ... with n = none → True.
        exact True.intro
  | obj kvObj =>
      -- Need: Value (.obj kvObj) (.sh (.object [])).
      -- ValueShape.object with kvs = [] — the universals are vacuous.
      refine Value.union_r (Value.union_r (Value.union_r (Value.union_r
        (Value.union_r (Value.sh (ValueShape.object ?_ ?_))))))
      · intro k t' h_mem; exact absurd h_mem (by simp)
      · intro k t' h_mem; exact absurd h_mem (by simp)

/-- **Singleton disjointness for booleans** is sound. The two singletons
    have no value in common — `Json.boo true ≠ Json.boo false`. -/
theorem TyLE.singleton_disjoint_boo_sound {b₁ b₂ : Bool} {t : Ty}
    (h : b₁ ≠ b₂) :
    TyLE_sem (.inter (.sh (.boo (some b₁))) (.sh (.boo (some b₂)))) t := by
  intro j hj
  cases hj with
  | inter hi₁ hi₂ =>
      cases hi₁ with
      | sh hvs₁ =>
          cases hvs₁ with
          | boo_some =>
              -- Now `j = .boo b₁`. Showing `Value (.boo b₁) (.sh (.boo (some b₂)))`
              -- forces `b₁ = b₂`.
              cases hi₂ with
              | sh hvs₂ =>
                  cases hvs₂  -- this requires .boo b₁ = .boo b₂, i.e., b₁ = b₂
                  -- Lean unifies b₁ with b₂; combined with h : b₁ ≠ b₂, contradiction.
                  exact absurd rfl h

/-- **Bool split** is sound. Any value of `Sh(boo none)` is a `Json.boo b`
    for some `b`, hence in `Sh(boo (some true)) ∪ Sh(boo (some false))`. -/
theorem TyLE.bool_split_sound :
    TyLE_sem (.sh (.boo none))
             (.union (.sh (.boo (some true))) (.sh (.boo (some false)))) := by
  intro j hj
  cases hj with
  | sh hvs =>
      cases hvs with
      | @boo_none b =>
          -- After `cases`, `j` is unified with `.boo b`.
          -- Case-split on `b` to land in either union branch.
          cases b with
          | true  => exact Value.union_l (Value.sh ValueShape.boo_some)
          | false => exact Value.union_r (Value.sh ValueShape.boo_some)

/-- **`neg_kind_top_boo`** is sound. A value not in `Sh(boo none)` is not
    a `Json.boo`, so it must be one of the other five JSON constructors. -/
theorem TyLE.neg_kind_top_boo_sound :
    TyLE_sem (.neg_sh (.boo none))
             (.union (.sh .nul)
                (.union (.sh (.num none))
                  (.union (.sh (.str none))
                    (.union (.sh (.array (.sh .top) none))
                      (.sh (.object [])))))) := by
  intro j hj
  cases hj with
  | neg_sh hnvs =>
      cases hnvs with
      | boo_none_other h_not_bool =>
          cases j with
          | nul =>
              exact Value.union_l (Value.sh ValueShape.nul)
          | boo b => exact absurd rfl (h_not_bool b)
          | num _ =>
              exact Value.union_r (Value.union_l (Value.sh ValueShape.num_none))
          | str _ =>
              exact Value.union_r (Value.union_r (Value.union_l
                (Value.sh ValueShape.str_none)))
          | arr xs =>
              refine Value.union_r (Value.union_r (Value.union_r (Value.union_l
                (Value.sh (ValueShape.array ?_ True.intro)))))
              intro x _; exact Value.sh ValueShape.top
          | obj kvObj =>
              refine Value.union_r (Value.union_r (Value.union_r (Value.union_r
                (Value.sh (ValueShape.object ?_ ?_)))))
              · intro k t' h_mem; exact absurd h_mem (by simp)
              · intro k t' h_mem; exact absurd h_mem (by simp)

/-- **`neg_kind_top_nul`** is sound. -/
theorem TyLE.neg_kind_top_nul_sound :
    TyLE_sem (.neg_sh .nul)
             (.union (.sh (.boo none))
                (.union (.sh (.num none))
                  (.union (.sh (.str none))
                    (.union (.sh (.array (.sh .top) none))
                      (.sh (.object [])))))) := by
  intro j hj
  cases hj with
  | neg_sh hnvs =>
      cases hnvs with
      | nul_other h_not_nul =>
          cases j with
          | nul => exact absurd rfl h_not_nul
          | boo _ =>
              exact Value.union_l (Value.sh ValueShape.boo_none)
          | num _ =>
              exact Value.union_r (Value.union_l (Value.sh ValueShape.num_none))
          | str _ =>
              exact Value.union_r (Value.union_r (Value.union_l
                (Value.sh ValueShape.str_none)))
          | arr xs =>
              refine Value.union_r (Value.union_r (Value.union_r (Value.union_l
                (Value.sh (ValueShape.array ?_ True.intro)))))
              intro x _; exact Value.sh ValueShape.top
          | obj kvObj =>
              refine Value.union_r (Value.union_r (Value.union_r (Value.union_r
                (Value.sh (ValueShape.object ?_ ?_)))))
              · intro k t' h_mem; exact absurd h_mem (by simp)
              · intro k t' h_mem; exact absurd h_mem (by simp)

/-- **`neg_kind_top_num`** is sound. -/
theorem TyLE.neg_kind_top_num_sound :
    TyLE_sem (.neg_sh (.num none))
             (.union (.sh .nul)
                (.union (.sh (.boo none))
                  (.union (.sh (.str none))
                    (.union (.sh (.array (.sh .top) none))
                      (.sh (.object [])))))) := by
  intro j hj
  cases hj with
  | neg_sh hnvs =>
      cases hnvs with
      | num_none_other h_not_num =>
          cases j with
          | nul =>
              exact Value.union_l (Value.sh ValueShape.nul)
          | boo _ =>
              exact Value.union_r (Value.union_l (Value.sh ValueShape.boo_none))
          | num n => exact absurd rfl (h_not_num n)
          | str _ =>
              exact Value.union_r (Value.union_r (Value.union_l
                (Value.sh ValueShape.str_none)))
          | arr xs =>
              refine Value.union_r (Value.union_r (Value.union_r (Value.union_l
                (Value.sh (ValueShape.array ?_ True.intro)))))
              intro x _; exact Value.sh ValueShape.top
          | obj kvObj =>
              refine Value.union_r (Value.union_r (Value.union_r (Value.union_r
                (Value.sh (ValueShape.object ?_ ?_)))))
              · intro k t' h_mem; exact absurd h_mem (by simp)
              · intro k t' h_mem; exact absurd h_mem (by simp)

/-- **`neg_kind_top_str`** is sound. -/
theorem TyLE.neg_kind_top_str_sound :
    TyLE_sem (.neg_sh (.str none))
             (.union (.sh .nul)
                (.union (.sh (.boo none))
                  (.union (.sh (.num none))
                    (.union (.sh (.array (.sh .top) none))
                      (.sh (.object [])))))) := by
  intro j hj
  cases hj with
  | neg_sh hnvs =>
      cases hnvs with
      | str_none_other h_not_str =>
          cases j with
          | nul =>
              exact Value.union_l (Value.sh ValueShape.nul)
          | boo _ =>
              exact Value.union_r (Value.union_l (Value.sh ValueShape.boo_none))
          | num _ =>
              exact Value.union_r (Value.union_r (Value.union_l
                (Value.sh ValueShape.num_none)))
          | str s => exact absurd rfl (h_not_str s)
          | arr xs =>
              refine Value.union_r (Value.union_r (Value.union_r (Value.union_l
                (Value.sh (ValueShape.array ?_ True.intro)))))
              intro x _; exact Value.sh ValueShape.top
          | obj kvObj =>
              refine Value.union_r (Value.union_r (Value.union_r (Value.union_r
                (Value.sh (ValueShape.object ?_ ?_)))))
              · intro k t' h_mem; exact absurd h_mem (by simp)
              · intro k t' h_mem; exact absurd h_mem (by simp)

/-- **`singleton_disjoint_num`** is sound. Distinct number literals
    have no value in common. -/
theorem TyLE.singleton_disjoint_num_sound {n₁ n₂ : Int} {t : Ty}
    (h : n₁ ≠ n₂) :
    TyLE_sem (.inter (.sh (.num (some n₁))) (.sh (.num (some n₂)))) t := by
  intro j hj
  cases hj with
  | inter hi₁ hi₂ =>
      cases hi₁ with
      | sh hvs₁ =>
          cases hvs₁ with
          | num_some =>
              cases hi₂ with
              | sh hvs₂ =>
                  cases hvs₂
                  exact absurd rfl h

/-- **`singleton_disjoint_str`** is sound. Distinct string literals
    have no value in common. -/
theorem TyLE.singleton_disjoint_str_sound {s₁ s₂ : String} {t : Ty}
    (h : s₁ ≠ s₂) :
    TyLE_sem (.inter (.sh (.str (some s₁))) (.sh (.str (some s₂)))) t := by
  intro j hj
  cases hj with
  | inter hi₁ hi₂ =>
      cases hi₁ with
      | sh hvs₁ =>
          cases hvs₁ with
          | str_some =>
              cases hi₂ with
              | sh hvs₂ =>
                  cases hvs₂
                  exact absurd rfl h

/-- **`kind_top_intro`** is sound. Every shape `s` of kind `k` is below
    the kind's top shape `k.top`. -/
theorem TyLE.kind_top_intro_sound {s : Shape} {k : Kind}
    (hk : ShapeKind s k) : TyLE_sem (.sh s) (.sh k.top) := by
  intro j hj
  cases hj with
  | sh hvs =>
      cases hk with
      | nul =>
          -- s = .nul, k.top = .nul. Already at top.
          exact Value.sh hvs
      | @boo b =>
          -- s = .boo b, k.top = .boo none. Use boo_none.
          cases hvs with
          | boo_some => exact Value.sh ValueShape.boo_none
          | boo_none => exact Value.sh ValueShape.boo_none
      | @num n =>
          cases hvs with
          | num_some => exact Value.sh ValueShape.num_none
          | num_none => exact Value.sh ValueShape.num_none
      | @str s =>
          cases hvs with
          | str_some => exact Value.sh ValueShape.str_none
          | str_none => exact Value.sh ValueShape.str_none
      | @array t n =>
          -- s = .array t n, k.top = .array (.sh .top) none.
          cases hvs with
          | @array xs t n h_elem h_len =>
              refine Value.sh (ValueShape.array ?_ True.intro)
              intro x _; exact Value.sh ValueShape.top
      | @tuple ts =>
          -- s = .tuple ts, k.top = .array (.sh .top) none.
          cases hvs with
          | @tuple xs ts h_len h_elem =>
              refine Value.sh (ValueShape.array ?_ True.intro)
              intro x _; exact Value.sh ValueShape.top
      | @object kvs =>
          -- s = .object kvs, k.top = .object [].
          cases hvs with
          | @object kvObj kvs h_match h_present =>
              refine Value.sh (ValueShape.object ?_ ?_)
              · intro k t' h_mem; exact absurd h_mem (by simp)
              · intro k t' h_mem; exact absurd h_mem (by simp)

/-- **`neg_kind_top_arr`** is sound. A value not in `Sh(array top none)`
    is not a `Json.arr` (the wrong-element/too-short branches are
    vacuous: `array_wrong_elem` would need `Value x ¬⊤` (none) and
    `array_too_short` doesn't trigger when `n = none`). -/
theorem TyLE.neg_kind_top_arr_sound :
    TyLE_sem (.neg_sh (.array (.sh .top) none))
             (.union (.sh .nul)
                (.union (.sh (.boo none))
                  (.union (.sh (.num none))
                    (.union (.sh (.str none))
                      (.sh (.object [])))))) := by
  intro j hj
  cases hj with
  | neg_sh hnvs =>
      cases hnvs with
      | array_not_arr h_not_arr =>
          cases j with
          | nul =>
              exact Value.union_l (Value.sh ValueShape.nul)
          | boo _ =>
              exact Value.union_r (Value.union_l (Value.sh ValueShape.boo_none))
          | num _ =>
              exact Value.union_r (Value.union_r (Value.union_l
                (Value.sh ValueShape.num_none)))
          | str _ =>
              exact Value.union_r (Value.union_r (Value.union_r (Value.union_l
                (Value.sh ValueShape.str_none))))
          | arr xs => exact absurd rfl (h_not_arr xs)
          | obj kvObj =>
              refine Value.union_r (Value.union_r (Value.union_r (Value.union_r
                (Value.sh (ValueShape.object ?_ ?_)))))
              · intro k t' h_mem; exact absurd h_mem (by simp)
              · intro k t' h_mem; exact absurd h_mem (by simp)
      | array_wrong_elem _ h_neg =>
          -- h_neg : Value x (negNNF (.sh .top)) = Value x (.neg_sh .top).
          -- NotValueShape j .top has no constructors → uninhabited.
          cases h_neg with
          | neg_sh hntop => cases hntop

/-- **`neg_kind_top_obj`** is sound. A value not in `Sh(object [])` is
    not a `Json.obj` (the missing-key/wrong-value branches are vacuous
    because `kvs = []`). -/
theorem TyLE.neg_kind_top_obj_sound :
    TyLE_sem (.neg_sh (.object []))
             (.union (.sh .nul)
                (.union (.sh (.boo none))
                  (.union (.sh (.num none))
                    (.union (.sh (.str none))
                      (.sh (.array (.sh .top) none)))))) := by
  intro j hj
  cases hj with
  | neg_sh hnvs =>
      cases hnvs with
      | object_not_obj h_not_obj =>
          cases j with
          | nul =>
              exact Value.union_l (Value.sh ValueShape.nul)
          | boo _ =>
              exact Value.union_r (Value.union_l (Value.sh ValueShape.boo_none))
          | num _ =>
              exact Value.union_r (Value.union_r (Value.union_l
                (Value.sh ValueShape.num_none)))
          | str _ =>
              exact Value.union_r (Value.union_r (Value.union_r (Value.union_l
                (Value.sh ValueShape.str_none))))
          | arr xs =>
              refine Value.union_r (Value.union_r (Value.union_r (Value.union_r
                (Value.sh (ValueShape.array ?_ True.intro)))))
              intro x _; exact Value.sh ValueShape.top
          | obj kvObj => exact absurd rfl (h_not_obj kvObj)
      | object_missing_key h_kt_in _ => exact absurd h_kt_in (by simp)
      | object_wrong_value h_kt_in _ _ => exact absurd h_kt_in (by simp)

/-- **Single-field structural negation** is sound. A value not matching
    `Sh(object [(k, T)])` is either not an object, an object missing
    `k`, or an object with `k` of type ¬T. -/
theorem TyLE.neg_object_single_field_sound {k : String} {t : Ty} :
    TyLE_sem (.neg_sh (.object [(k, t)]))
             (.union
                (.neg_sh (.object []))
                (.union
                  (.inter (.sh (.object []))
                          (.neg_sh (.object [(k, .sh .top)])))
                  (.sh (.object [(k, negNNF t)])))) := by
  intro j hj
  cases hj with
  | neg_sh hnvs =>
      cases hnvs with
      | object_not_obj h_not_obj =>
          -- j is not an object at all → first disjunct.
          exact Value.union_l (Value.neg_sh (NotValueShape.object_not_obj h_not_obj))
      | @object_missing_key kvObj kvs k' t' h_kt_in h_lookup =>
          -- (k', t') ∈ [(k, t)] forces (k', t') = (k, t).
          cases h_kt_in with
          | head =>
              -- j is an object missing k → second disjunct.
              refine Value.union_r (Value.union_l (Value.inter ?_ ?_))
              · -- j ∈ Sh(object []) — the empty record top.
                exact Value.sh (ValueShape.object
                  (fun k₀ t₀ h_mem => absurd h_mem (by simp))
                  (fun k₀ t₀ h_mem => absurd h_mem (by simp)))
              · -- j ∉ Sh(object [(k, top)]) — missing the key.
                exact Value.neg_sh
                  (NotValueShape.object_missing_key (List.Mem.head _) h_lookup)
          | tail _ h_in_rest => cases h_in_rest
      | @object_wrong_value kvObj kvs k' t' v h_kt_in h_lookup h_neg =>
          cases h_kt_in with
          | head =>
              -- j has k of wrong type → third disjunct.
              refine Value.union_r (Value.union_r ?_)
              -- Goal: Value (.obj kvObj) (.sh (.object [(k, negNNF t)]))
              refine Value.sh (ValueShape.object ?_ ?_)
              · intro k₀ t₀ h_mem v₀ h_lk
                cases h_mem with
                | head =>
                    -- (k₀, t₀) = (k, negNNF t).
                    rw [h_lookup] at h_lk
                    cases h_lk
                    exact h_neg
                | tail _ h_in => cases h_in
              · intro k₀ t₀ h_mem
                cases h_mem with
                | head =>
                    rw [h_lookup]; rfl
                | tail _ h_in => cases h_in
          | tail _ h_in_rest => cases h_in_rest

/-- **`neg_object_multi_split`** is sound. A value not matching
    `Sh(object ((k, t) :: rest))` either fails on the first field
    or fails on the rest. -/
theorem TyLE.neg_object_multi_split_sound
    {k : String} {t : Ty} {rest : List (String × Ty)} :
    TyLE_sem (.neg_sh (.object ((k, t) :: rest)))
             (.union (.neg_sh (.object [(k, t)]))
                     (.neg_sh (.object rest))) := by
  intro j hj
  cases hj with
  | neg_sh hnvs =>
      cases hnvs with
      | object_not_obj h_not_obj =>
          -- j is not an object. Either neg_sh works.
          exact Value.union_l (Value.neg_sh (NotValueShape.object_not_obj h_not_obj))
      | @object_missing_key kvObj kvs k' t' h_kt_in h_lookup =>
          -- (k', t') ∈ (k, t) :: rest. Two cases via List.Mem.
          cases h_kt_in with
          | head =>
              -- (k', t') = (k, t). Missing key k in [(k, t)].
              exact Value.union_l (Value.neg_sh
                (NotValueShape.object_missing_key (List.Mem.head _) h_lookup))
          | tail _ h_in_rest =>
              -- (k', t') ∈ rest.
              exact Value.union_r (Value.neg_sh
                (NotValueShape.object_missing_key h_in_rest h_lookup))
      | @object_wrong_value kvObj kvs k' t' v h_kt_in h_lookup h_neg =>
          cases h_kt_in with
          | head =>
              exact Value.union_l (Value.neg_sh
                (NotValueShape.object_wrong_value (List.Mem.head _) h_lookup h_neg))
          | tail _ h_in_rest =>
              exact Value.union_r (Value.neg_sh
                (NotValueShape.object_wrong_value h_in_rest h_lookup h_neg))

/-! ## Main soundness theorems

    `TyLE.sound` / `ShapeLE.sound` — every subtyping derivation is a
    semantic value-set inclusion. Proved by mutual structural recursion on
    the derivation (`cases` + recursive calls; `induction` does not support
    mutual inductives). Each rule dispatches to its per-rule witness above.

    Five cases remain `sorry` — the genuinely hard ones whose witnesses are
    not yet proved: `neg_sh` (contravariant `NotValueShape` monotonicity),
    `neg_array_decomp`, and the three `tuple_*`/`array_tuple` positional
    rules (list-index reasoning through `List.zip`). Everything else is
    fully discharged. -/
-- Term-mode `match` (not the `cases` tactic): the equation compiler needs
-- the `match` to see the derivation's structure for the self-recursive
-- constructors (`trans`, `inter_elim`, …) to be accepted as structural.
mutual

theorem TyLE.sound {t₁ t₂ : Ty} (h : TyLE t₁ t₂) : TyLE_sem t₁ t₂ :=
  match h with
  | .refl => TyLE.refl_sound _
  | .trans h₁ h₂ => TyLE.trans_sound (TyLE.sound h₁) (TyLE.sound h₂)
  | .sh hs => ShapeLE.sound hs
  | .arr _ _ => fun _ hj => nomatch hj      -- arrow types uninhabited by Value
  | .inter_intro_l h => TyLE.inter_intro_l_sound (TyLE.sound h)
  | .inter_intro_r h => TyLE.inter_intro_r_sound (TyLE.sound h)
  | .inter_elim h₁ h₂ => TyLE.inter_elim_sound (TyLE.sound h₁) (TyLE.sound h₂)
  | .union_intro_l => TyLE.union_intro_l_sound
  | .union_intro_r => TyLE.union_intro_r_sound
  | .union_elim h₁ h₂ => TyLE.union_elim_sound (TyLE.sound h₁) (TyLE.sound h₂)
  | .neg_sh _ => sorry  -- contravariant NotValueShape monotonicity (unproved)
  | .bot_min_sh => TyLE.bot_min_sh_sound
  | .arrow_inter_dom => TyLE.arrow_inter_dom_sound
  | .bool_split => TyLE.bool_split_sound
  | .kind_disjoint h₁ h₂ hne => TyLE.kind_disjoint_sound h₁ h₂ hne
  | .kind_top_intro hk => TyLE.kind_top_intro_sound hk
  | .top_decomp => TyLE.top_decomp_sound
  | .neg_kind_top_nul => TyLE.neg_kind_top_nul_sound
  | .neg_kind_top_boo => TyLE.neg_kind_top_boo_sound
  | .neg_kind_top_num => TyLE.neg_kind_top_num_sound
  | .neg_kind_top_str => TyLE.neg_kind_top_str_sound
  | .neg_kind_top_arr => TyLE.neg_kind_top_arr_sound
  | .neg_kind_top_obj => TyLE.neg_kind_top_obj_sound
  | .singleton_disjoint_boo h => TyLE.singleton_disjoint_boo_sound h
  | .singleton_disjoint_num h => TyLE.singleton_disjoint_num_sound h
  | .singleton_disjoint_str h => TyLE.singleton_disjoint_str_sound h
  | .neg_object_single_field => TyLE.neg_object_single_field_sound
  | .neg_object_multi_split => TyLE.neg_object_multi_split_sound
  | .neg_array_decomp => sorry  -- three-way array-negation decomposition (unproved)

theorem ShapeLE.sound {s₁ s₂ : Shape} (h : ShapeLE s₁ s₂) : ShapeLE_sem s₁ s₂ :=
  match h with
  | .refl => ShapeLE.refl_sound _
  | .trans h₁ h₂ => ShapeLE.trans_sound (ShapeLE.sound h₁) (ShapeLE.sound h₂)
  | .top => ShapeLE.top_sound _
  | .boo_some_to_none => ShapeLE.boo_some_to_none_sound
  | .num_some_to_none => ShapeLE.num_some_to_none_sound
  | .str_some_to_none => ShapeLE.str_some_to_none_sound
  | .array ht hn => ShapeLE.array_sound (TyLE.sound ht) hn
  | .tuple_tuple hlen helem =>
      ShapeLE.tuple_tuple_sound hlen (fun i h => TyLE.sound (helem i h))
  | .tuple_array _ _ => sorry  -- UNSOUND rule (open tuples); see tuple_array_unsound
  | .array_tuple _ hge helem =>
      ShapeLE.array_tuple_sound hge (fun i h => TyLE.sound (helem i h))
  | .object_nil => ShapeLE.object_nil_sound
  | .object_cons hmem hu hrest =>
      ShapeLE.object_cons_sound hmem (TyLE.sound hu) (ShapeLE.sound hrest)

end

/-! ## A soundness bug surfaced by the proof: `tuple_array`

    Attempting `ShapeLE.sound` exposes that the `tuple_array` rule is
    **unsound** under the *open* (prefix) tuple semantics of `ValueShape`
    (`ValueShape.tuple` only constrains the first `ts.length` elements;
    `xs` may be longer). The rule claims `tuple ts ≤ array t n`, which
    requires *every* element to be `t`. The theorem below proves the
    general soundness statement is false, so the `tuple_array` case of
    `ShapeLE.sound` is not merely unproved — it is unfillable as stated.

    Resolving it is a design decision for the kernel, not a proof gap:
      * make tuples *closed* (`ValueShape.tuple` with `ts.length =
        xs.length`) — then `tuple_array` is sound but `array_tuple`
        becomes unsound (an over-long array is no longer a tuple); or
      * drop / restrict `tuple_array` (e.g. only `tuple ts ≤ array t
        (some ts.length)` with an exactness side-condition).
    The other ~35 rules are sound (fully proved or, for the two remaining
    `sorry`s, believed sound). -/
theorem tuple_array_unsound :
    ¬ (∀ {s₁ s₂ : Shape}, ShapeLE s₁ s₂ → ShapeLE_sem s₁ s₂) := by
  intro hsound
  -- `tuple_array` derives `tuple [num] ≤ array num` …
  have hle : ShapeLE (.tuple [.sh (.num none)]) (.array (.sh (.num none)) none) :=
    ShapeLE.tuple_array (n := none) trivial
      (fun i h => by
        have : i = 0 := by simp at h; omega
        subst this; exact TyLE.refl)
  -- … but `[1, "x"]` inhabits the tuple (first element is a number) …
  have hin : Value (.arr [.num 1, .str "x"]) (.sh (.tuple [.sh (.num none)])) := by
    refine Value.sh (ValueShape.tuple (by simp) ?_)
    intro p hp
    simp [List.zip, List.zipWith] at hp
    obtain ⟨rfl, rfl⟩ := hp
    exact Value.sh ValueShape.num_none
  -- … so soundness would force `"x" : num`, which is false.
  have hout := hsound hle _ hin
  cases hout with
  | sh hs =>
      cases hs with
      | @array _ _ _ h_elem _ =>
          have hbad : Value (.str "x") (.sh (.num none)) := h_elem (.str "x") (by simp)
          cases hbad with
          | sh hs2 => cases hs2

end Tjq
