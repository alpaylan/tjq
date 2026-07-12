import Tjq.Syntax
import Tjq.Subtyping
import Tjq.Typing

/-!
# Tjq.Properties

Theorem statements that we expect to hold of the v1 typing rules. Each is
stated and left as `sorry`. The goal is to fix the proof obligations now,
in one place, so that downstream changes have to either preserve them or
explicitly invalidate them.

Sections:

1. **Subtyping algebra.** Reflexivity / transitivity (already constructors,
   but restated as plain lemmas for ergonomics). Congruence of TyLE under
   union, intersection, negation.

2. **Stream / regex laws.** Concat is monoid-like (associative, ε-unit).
   Star and choice satisfy the usual regex inclusion identities.

3. **Auxiliary function laws.** `flatmap` is the Kleisli operator of the
   stream monad: identity on `Stream.yld`, associative under composition.
   `elemType` is monotone in stream subtyping. `collect` on a singleton
   stream is a one-element tuple.

4. **Refine soundness.** When `refine c b ≠ ⊤`, every value satisfying the
   refinement makes `c` evaluate to a stream containing `b`. (Stating this
   formally requires an operational semantics; for v1 we state a weak
   version: if `refine c b = T` then `T` is a sub-type-of-input under
   which the guard "must" produce `b`. The strong version is deferred.)

5. **Well-formedness of inferred types.** `HasType f T` implies `T` is an
   arrow, an intersection of arrows, or a supertype of one of those.

All proofs are `sorry`. The intent is to track what we owe the formalisation
without committing to a proof effort yet.
-/

namespace Tjq

/-! ## §1. Subtyping algebra -/

/-- `TyLE.refl` as a plain lemma. Trivially true (it's a constructor). -/
theorem TyLE.refl' (t : Ty) : t ⊑ t := TyLE.refl

/-- `TyLE.trans` as a plain lemma. -/
theorem TyLE.trans' {t₁ t₂ t₃ : Ty} (h₁ : t₁ ⊑ t₂) (h₂ : t₂ ⊑ t₃) : t₁ ⊑ t₃ :=
  TyLE.trans h₁ h₂

/-- Congruence of `∪` in its left argument. -/
theorem TyLE.union_congr_l {t₁ t₁' t₂ : Ty} (h : t₁ ⊑ t₁') :
    Ty.union t₁ t₂ ⊑ Ty.union t₁' t₂ :=
  TyLE.union_elim
    (TyLE.trans h TyLE.union_intro_l)
    TyLE.union_intro_r

/-- Congruence of `∪` in its right argument. -/
theorem TyLE.union_congr_r {t₁ t₂ t₂' : Ty} (h : t₂ ⊑ t₂') :
    Ty.union t₁ t₂ ⊑ Ty.union t₁ t₂' :=
  TyLE.union_elim
    TyLE.union_intro_l
    (TyLE.trans h TyLE.union_intro_r)

/-- Congruence of `∩` in its left argument. -/
theorem TyLE.inter_congr_l {t₁ t₁' t₂ : Ty} (h : t₁ ⊑ t₁') :
    Ty.inter t₁ t₂ ⊑ Ty.inter t₁' t₂ :=
  TyLE.inter_elim
    (TyLE.trans (TyLE.inter_intro_l TyLE.refl) h)
    (TyLE.inter_intro_r TyLE.refl)

/-- Congruence of `∩` in its right argument. -/
theorem TyLE.inter_congr_r {t₁ t₂ t₂' : Ty} (h : t₂ ⊑ t₂') :
    Ty.inter t₁ t₂ ⊑ Ty.inter t₁ t₂' :=
  TyLE.inter_elim
    (TyLE.inter_intro_l TyLE.refl)
    (TyLE.trans (TyLE.inter_intro_r TyLE.refl) h)

/-- Negation reverses subtyping. -/
theorem TyLE.neg_sh_anti {s s' : Shape} (h : ShapeLE s s') :
    Ty.neg_sh s' ⊑ Ty.neg_sh s :=
  TyLE.neg_sh h

/-- Sh-top is above any shape-typed type. (We do **not** claim
    `t ⊑ Ty.sh Shape.top` for arrow-typed `t` — that would be a category
    confusion absent additional kernel rules.) -/
theorem TyLE.sh_le_sh_top (s : Shape) : Ty.sh s ⊑ Ty.sh Shape.top :=
  TyLE.sh ShapeLE.top

/-- Bottom is below shape-top. (Weak form; `Ty.bot ⊑ t` for arbitrary `t`
    requires an ex-falso rule that the v1 syntactic kernel does not have.) -/
theorem TyLE.bot_le_sh_top : Ty.bot ⊑ Ty.sh Shape.top :=
  TyLE.inter_intro_l TyLE.refl

/-- Union absorbs subtypes: if `A ⊑ B` then `A ∪ B ⊑ B`. -/
theorem TyLE.union_absorb_left {t₁ t₂ : Ty} (h : t₁ ⊑ t₂) :
    Ty.union t₁ t₂ ⊑ t₂ :=
  TyLE.union_elim h TyLE.refl

/-- Intersection absorbs supertypes: if `A ⊑ B` then `A ⊑ A ∩ B`. -/
theorem TyLE.inter_absorb_left {t₁ t₂ : Ty} (h : t₁ ⊑ t₂) :
    t₁ ⊑ Ty.inter t₁ t₂ :=
  TyLE.inter_elim TyLE.refl h

/-! ## §2. Stream / regex laws -/

/-- Concat-associativity at the regex level. -/
theorem StreamLE.concat_assoc (s₁ s₂ s₃ : Stream) :
    (Stream.concat (Stream.concat s₁ s₂) s₃) ⊑ₛ
    (Stream.concat s₁ (Stream.concat s₂ s₃)) :=
  StreamLE.concat_assoc_l

/-- Concat-associativity, the other direction. -/
theorem StreamLE.concat_assoc' (s₁ s₂ s₃ : Stream) :
    (Stream.concat s₁ (Stream.concat s₂ s₃)) ⊑ₛ
    (Stream.concat (Stream.concat s₁ s₂) s₃) :=
  StreamLE.concat_assoc_r

/-- ε is a left unit for concat. -/
theorem StreamLE.eps_concat (s : Stream) :
    Stream.concat Stream.eps s ⊑ₛ s :=
  StreamLE.eps_concat_l

theorem StreamLE.concat_eps (s : Stream) :
    Stream.concat s Stream.eps ⊑ₛ s :=
  StreamLE.concat_eps_l

/-- Choice is symmetric (one direction; the converse holds too). -/
theorem StreamLE.choice_comm (s₁ s₂ : Stream) :
    Stream.choice s₁ s₂ ⊑ₛ Stream.choice s₂ s₁ :=
  StreamLE.choice_elim
    (StreamLE.choice_intro_r StreamLE.refl)
    (StreamLE.choice_intro_l StreamLE.refl)

/-- Star is idempotent: `(S*)* ⊑ S*`. -/
theorem StreamLE.star_idem (s : Stream) :
    Stream.star (Stream.star s) ⊑ₛ Stream.star s :=
  StreamLE.star_star

/-! ## §3. Auxiliary function laws -/

/-- `flatmap` with `Stream.yld` is the identity. This is the left-unit law
    of the stream monad and justifies `f | .` having the same type as `f`.

    Since `Stream` is in a mutual block, `induction` is unavailable; we
    use pattern-match recursion (Lean checks structural termination). -/
theorem flatmap_yld : (s : Stream) → flatmap s Stream.yld = s
  | .eps => rfl
  | .yld _ => rfl
  | .concat s₁ s₂ => by
      simp [flatmap, flatmap_yld s₁, flatmap_yld s₂]
  | .star s => by
      simp [flatmap, flatmap_yld s]
  | .choice s₁ s₂ => by
      simp [flatmap, flatmap_yld s₁, flatmap_yld s₂]
  | .svar _ => rfl

/-- `flatmap` is associative: `(s ⋙ k₁) ⋙ k₂ = s ⋙ (λT. k₁ T ⋙ k₂)`.
    Justifies `f | g | h` being unambiguous. -/
theorem flatmap_assoc :
    (s : Stream) → (k₁ k₂ : Ty → Stream) →
    flatmap (flatmap s k₁) k₂ = flatmap s (fun t => flatmap (k₁ t) k₂)
  | .eps,          _,  _  => rfl
  | .yld _,        _,  _  => rfl
  | .concat s₁ s₂, k₁, k₂ => by
      simp [flatmap, flatmap_assoc s₁ k₁ k₂, flatmap_assoc s₂ k₁ k₂]
  | .star s,       k₁, k₂ => by
      simp [flatmap, flatmap_assoc s k₁ k₂]
  | .choice s₁ s₂, k₁, k₂ => by
      simp [flatmap, flatmap_assoc s₁ k₁ k₂, flatmap_assoc s₂ k₁ k₂]
  | .svar _,       _,  _  => rfl

/-- Pointwise subtyping of kernels carries to flatmap, when the streams
    on the two sides are *equal*. (The fully general version with
    `s ⊑ₛ s'` requires a kernel-monotonicity assumption that the basic
    `flatmap_mono` statement glosses over.) -/
theorem flatmap_mono_kernel :
    ∀ (s : Stream) {k k' : Ty → Stream},
      (∀ t, k t ⊑ₛ k' t) → flatmap s k ⊑ₛ flatmap s k'
  | .eps, _, _, _ => StreamLE.refl
  | .yld t, _, _, hk => hk t
  | .concat s₁ s₂, _, _, hk =>
      StreamLE.concat (flatmap_mono_kernel s₁ hk) (flatmap_mono_kernel s₂ hk)
  | .star s, _, _, hk =>
      StreamLE.star (flatmap_mono_kernel s hk)
  | .choice s₁ s₂, _, _, hk =>
      StreamLE.choice_elim
        (StreamLE.trans (flatmap_mono_kernel s₁ hk)
                        (StreamLE.choice_intro_l StreamLE.refl))
        (StreamLE.trans (flatmap_mono_kernel s₂ hk)
                        (StreamLE.choice_intro_r StreamLE.refl))
  | .svar _, _, _, _ => StreamLE.refl

/-- `elemType` is monotone in stream subtyping: bigger stream has bigger
    element-type union.

    Proved by recursion on the StreamLE derivation (the proof object). -/
theorem elemType_mono :
    {a b : Tjq.Stream} → a ⊑ₛ b → elemType a ⊑ elemType b
  | _, _, .refl => TyLE.refl
  | _, _, .trans h₁ h₂ =>
      TyLE.trans (elemType_mono h₁) (elemType_mono h₂)
  | _, _, .eps => TyLE.refl
  | _, _, .yld h_t => h_t
  | _, _, .concat h₁ h₂ =>
      TyLE.union_elim
        (TyLE.trans (elemType_mono h₁) TyLE.union_intro_l)
        (TyLE.trans (elemType_mono h₂) TyLE.union_intro_r)
  | _, _, .eps_star => TyLE.bot_min_sh
  | _, _, .yld_star h => by
      -- elemType (star s) = elemType s by definition; reveal it.
      unfold elemType
      exact elemType_mono h
  | _, _, .star h => by
      -- elemType (star s) = elemType s on both sides.
      unfold elemType
      exact elemType_mono h
  | _, _, .star_concat => TyLE.union_elim TyLE.refl TyLE.refl
  | _, _, .star_star => TyLE.refl
  | _, _, .eps_concat_l => TyLE.union_elim TyLE.bot_min_sh TyLE.refl
  | _, _, .eps_concat_r => TyLE.union_intro_r
  | _, _, .concat_eps_l => TyLE.union_elim TyLE.refl TyLE.bot_min_sh
  | _, _, .concat_eps_r => TyLE.union_intro_l
  | _, _, .concat_assoc_l =>
      TyLE.union_elim
        (TyLE.union_elim
          TyLE.union_intro_l
          (TyLE.trans TyLE.union_intro_l TyLE.union_intro_r))
        (TyLE.trans TyLE.union_intro_r TyLE.union_intro_r)
  | _, _, .concat_assoc_r =>
      TyLE.union_elim
        (TyLE.trans TyLE.union_intro_l TyLE.union_intro_l)
        (TyLE.union_elim
          (TyLE.trans TyLE.union_intro_r TyLE.union_intro_l)
          TyLE.union_intro_r)
  | _, _, .choice_intro_l h => TyLE.trans (elemType_mono h) TyLE.union_intro_l
  | _, _, .choice_intro_r h => TyLE.trans (elemType_mono h) TyLE.union_intro_r
  | _, _, .choice_elim h₁ h₂ =>
      TyLE.union_elim (elemType_mono h₁) (elemType_mono h₂)

/-- `collect` on a singleton yields a one-element tuple. -/
theorem collect_yld (t : Ty) :
    collect (Stream.yld t) = Ty.sh (Shape.tuple [t]) := by
  rfl

/-- `collect` on ε yields the empty tuple. -/
theorem collect_eps :
    collect Stream.eps = Ty.sh (Shape.tuple []) := by
  rfl

/-- `collect` is monotone in stream subtyping.

    **Sorry.** The proof structure is induction on the StreamLE derivation
    (similar to `elemType_mono`), but every case has to traverse the
    `collectTuple` `match` inside `collect`'s `concat` arm. The cases that
    bite the most:

    * `eps_concat_*` / `concat_eps_*`: need to argue about how `collectTuple
      (.concat .eps s)` reduces relative to `collectTuple s`. Provable by
      cases on whether `collectTuple s` is `some` or `none`, but tedious.

    * `yld_star`: requires lifting `t ⊑ elemType s` (via `elemType_mono`)
      to `Sh (Tuple [t]) ⊑ Sh (Array (elemType s) none)` via the
      `ShapeLE.tuple_array` rule.

    * `star_concat`, `concat_assoc_*`: need a small algebra of
      tuple/array shape subtyping that we haven't proved as separate lemmas.

    None of this is conceptually hard; it's ~150 lines of careful case
    analysis. Left as future work. -/
theorem collect_mono {s s' : Stream} (h : s ⊑ₛ s') :
    collect s ⊑ collect s' := by
  sorry

/-! ## §4. Refine soundness (weak form)

The full statement requires an operational semantics for `Filter` over
`Json`, which is out of v1 scope. Below is a weaker statement: if a guard
has a non-trivial refinement, the rule's premises (input refined by both
branches) cover the original input.

Concretely: for any input type `A`,
  A ⊑ (A ∩ refine c true) ∪ (A ∩ refine c false).

This is a sanity check that the if-precise rule's two branch hypotheses
together still cover the original input. -/

/-- **Sorry.** This statement reduces (by distributivity) to
    `A ⊑ A ∩ (refine c true ∪ refine c false)`, which in turn requires
    `A ⊑ refine c true ∪ refine c false`. The latter is a structural
    property of `refine` — for the recognised guard forms, the two
    refinements are complementary and their union covers `⊤`. Proving
    this requires:

    1. A type-level law of excluded middle: `t ⊑ T ∪ ¬T` for any `t, T`.
       Not in the v1 syntactic kernel; would be added as a constructor.
    2. Induction on the `Filter` structure (matching the cases in `refine`)
       to show each form's two refinements are complementary.

    With those two pieces, `refine_covers` follows. -/
theorem refine_covers (c : Filter) (A : Ty) :
    A ⊑ Ty.union (Ty.inter A (refine c true)) (Ty.inter A (refine c false)) := by
  sorry

/-! ## §5. Well-formedness of inferred filter types

The intended invariant: if `HasType f T`, then `T` is *of arrow-shape*,
i.e., either an arrow `A → S`, an intersection of arrows, or (via
subsumption) a supertype of one of those.

We state a weaker corollary first: every well-typed `f` has *some* arrow
type — possibly via subsumption, but the underlying judgment lands on an
arrow shape.

The "arrow-shape" predicate. -/
inductive IsArrowShape : Ty → Prop where
  | arr : IsArrowShape (Ty.arr a s)
  | inter : IsArrowShape t₁ → IsArrowShape t₂ → IsArrowShape (Ty.inter t₁ t₂)

/-- The well-formedness theorem: every typing derivation has, modulo
    subsumption, an arrow-shaped type. We expose the underlying derivation
    as an existential.

    Proof: induction on `h`. Every non-`sub` constructor produces an arrow
    or intersection-of-arrows directly; `sub` chains through the IH. -/
theorem HasType.arrow_shape {f : Filter} {T : Ty} (h : HasType f T) :
    ∃ T₀, IsArrowShape T₀ ∧ HasType f T₀ ∧ (T₀ ⊑ T) := by
  induction h with
  | dot α => exact ⟨_, .arr, .dot α, TyLE.refl⟩
  | comma h₁ h₂ _ _ => exact ⟨_, .arr, .comma h₁ h₂, TyLE.refl⟩
  | pipe h₁ h₂ hp _ _ => exact ⟨_, .arr, .pipe h₁ h₂ hp, TyLE.refl⟩
  | empty α => exact ⟨_, .arr, .empty α, TyLE.refl⟩
  | error α => exact ⟨_, .arr, .error α, TyLE.refl⟩
  | nullLit α => exact ⟨_, .arr, .nullLit α, TyLE.refl⟩
  | boolLit α b => exact ⟨_, .arr, .boolLit α b, TyLE.refl⟩
  | numLit α n => exact ⟨_, .arr, .numLit α n, TyLE.refl⟩
  | strLit α s => exact ⟨_, .arr, .strLit α s, TyLE.refl⟩
  | objIndex α k => exact ⟨_, .arr, .objIndex α k, TyLE.refl⟩
  | arrIndex α n => exact ⟨_, .arr, .arrIndex α n, TyLE.refl⟩
  | iterArray α => exact ⟨_, .arr, .iterArray α, TyLE.refl⟩
  | array Ss hlen helem _ =>
      exact ⟨_, .arr, .array Ss hlen helem, TyLE.refl⟩
  | object Ts hlen helem _ =>
      exact ⟨_, .arr, .object Ts hlen helem, TyLE.refl⟩
  | ifPrecise hc hr ht he _ _ _ =>
      exact ⟨_, .inter .arr .arr, .ifPrecise hc hr ht he, TyLE.refl⟩
  | ifImprecise hc hr ht he _ _ _ =>
      exact ⟨_, .arr, .ifImprecise hc hr ht he, TyLE.refl⟩
  | binop hl hr hov _ _ =>
      exact ⟨_, .arr, .binop hl hr hov, TyLE.refl⟩
  | unopNeg hf _ => exact ⟨_, .arr, .unopNeg hf, TyLE.refl⟩
  | sub _ hle ih =>
      obtain ⟨T₀, hshape, h₀, hle₀⟩ := ih
      exact ⟨T₀, hshape, h₀, TyLE.trans hle₀ hle⟩
  | @inst f T n U _ ih =>
      -- HasType f (substTy n U T) ; need an arrow-shape witness.
      -- Strategy: instantiate the IH's witness, since substitution preserves
      -- the outermost arrow / inter-of-arrows structure (which we'd need to
      -- prove as a `subst_preserves_arrow_shape` lemma — left as sorry).
      sorry
  | call name T =>
      -- The call rule asserts an arbitrary type T for a named filter.
      -- The "arrow-shape" property cannot be guaranteed at this level
      -- because T is user-supplied; it's the caller's responsibility to
      -- provide an arrow-shaped scheme. Sorried until we restrict T.
      sorry
  | inter_intro _ _ ih₁ ih₂ =>
      -- HasType f (inter T₁ T₂) ; combine arrow-shape witnesses for each.
      obtain ⟨T₀₁, hsh₁, h₀₁, hle₁⟩ := ih₁
      obtain ⟨T₀₂, hsh₂, h₀₂, hle₂⟩ := ih₂
      exact ⟨_, .inter hsh₁ hsh₂, .inter_intro h₀₁ h₀₂,
             TyLE.inter_elim
               (TyLE.trans (TyLE.inter_intro_l TyLE.refl) hle₁)
               (TyLE.trans (TyLE.inter_intro_r TyLE.refl) hle₂)⟩

/- Restricted form: a subsumption-free derivation lands on an arrow shape.
   Stating this requires distinguishing subsumption from the structural
   rules; we'd encode that with a separate inductive. Deferred:
       theorem HasType.arrow_shape_strict ... := sorry
-/

end Tjq
