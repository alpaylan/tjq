import Tjq.Syntax

/-!
# Tjq.Subtyping

Three mutually inductive subtyping judgments, one per sort:

* `TyLE  : Ty → Ty → Prop`
* `ShapeLE : Shape → Shape → Prop`
* `StreamLE : Stream → Stream → Prop`

This is a **syntactic kernel** — sound rules, not necessarily complete relative
to full set-theoretic / regex semantics. Rules omitted (and acknowledged in
`papers/typing-rules.md` §6):

* Distributivity of `∩` over `∪` (and vice versa).
* CDuce-style semantic decomposition for `T <: T₁ ∪ T₂`.
* Brzozowski-derivative-style stream inclusion beyond the structural cases.

Reflexivity and transitivity are explicit constructors so we don't need to
prove them up front. We can specialise / replace these rules later when we
move to a decidable algorithm.

Arrows have no rule under value-typing because filters are not JSON values;
the contravariant arrow rule is part of `TyLE` directly.
-/

namespace Tjq

/-- Min-length subtyping: `Some n <: Some m` iff `n ≥ m`; `None` is the loosest. -/
def MinLE : Option Nat → Option Nat → Prop
  | _,        none   => True               -- any min-length is below "no min"
  | none,     some _ => False              -- "no min" cannot satisfy a min req.
  | some n,   some m => n ≥ m

mutual

/-- Subtyping on types. -/
inductive TyLE : Ty → Ty → Prop where
  -- Reflexivity & transitivity (kept explicit for now).
  | refl  : TyLE t t
  | trans : TyLE t₁ t₂ → TyLE t₂ t₃ → TyLE t₁ t₃

  -- Embed shape subtyping.
  | sh    : ShapeLE s s' → TyLE (.sh s) (.sh s')

  -- Arrows: contravariant in the input, covariant in the codomain stream.
  | arr   : TyLE a' a → StreamLE st st' → TyLE (.arr a st) (.arr a' st')

  -- Intersection.
  | inter_intro_l : TyLE t₁ t → TyLE (.inter t₁ t₂) t
  | inter_intro_r : TyLE t₂ t → TyLE (.inter t₁ t₂) t
  | inter_elim    : TyLE t t₁ → TyLE t t₂ → TyLE t (.inter t₁ t₂)

  -- Union.
  | union_intro_l : TyLE t (.union t t₂)
  | union_intro_r : TyLE t (.union t₁ t)
  | union_elim    : TyLE t₁ t → TyLE t₂ t → TyLE (.union t₁ t₂) t

  -- Negation (shape-level): contravariant.
  -- `¬Sh(s₁) <: ¬Sh(s₂)` iff `s₂ <: s₁`.
  | neg_sh   : ShapeLE s' s → TyLE (.neg_sh s) (.neg_sh s')

  -- Ex-falso (bottom is universal lower bound). `Sh(s) ∩ ¬Sh(s)` is
  -- empty for any shape s. (For compound types use `negNNF` to reduce
  -- to NNF first, then chain.)
  | bot_min_sh : TyLE (.inter (.sh s) (.neg_sh s)) t'

  -- Distribute intersection of arrows over union of domains:
  --     (A₁ → S) ∩ (A₂ → S)  ⊑  (A₁ ∪ A₂) → S
  -- The reverse direction is derivable from `union_intro_l/r` + `inter_elim`,
  -- so we only need this direction as a primitive.
  --
  -- This is what lets a finite case-analysis intersection (`bool[true] →
  -- ⟨true⟩) ∩ (bool[false] → ⟨true⟩`) collapse to a single arrow over the
  -- union of the cases (`bool → ⟨true⟩`).
  | arrow_inter_dom :
      TyLE (.inter (.arr a₁ s) (.arr a₂ s)) (.arr (.union a₁ a₂) s)

  -- Type-decomposition axioms (value-set exhaustiveness).
  --
  -- `bool none` is the union of its two singletons. Set-theoretically
  -- a bool value is one of `true` / `false`; the syntactic kernel needs
  -- this stated as a rule because it doesn't follow from the other rules
  -- alone (you can't decompose `boo none` from `union_intro_l/r` without
  -- already knowing the value).
  | bool_split :
      TyLE (.sh (.boo none))
           (.union (.sh (.boo (some true))) (.sh (.boo (some false))))

  -- ─────────────────────────────────────────────────────────────────────
  -- Kind framework: every JSON value is in exactly one of the six base
  -- kinds (Nul, Boo, Num, Str, Arr, Obj). This drives the algebra of
  -- negation reasoning.
  -- ─────────────────────────────────────────────────────────────────────

  -- Disjoint kinds: shapes of two different definite kinds have no value
  -- in common; their intersection is bottom.
  | kind_disjoint {s₁ s₂ k₁ k₂ t} :
      ShapeKind s₁ k₁ → ShapeKind s₂ k₂ → k₁ ≠ k₂ →
      TyLE (.inter (.sh s₁) (.sh s₂)) t

  -- A definite-kind shape is below its kind-top.
  | kind_top_intro {s k} :
      ShapeKind s k → TyLE (.sh s) (.sh k.top)

  -- Universe (`Sh top`) decomposes into the union of all six kind-tops.
  -- This is the exhaustiveness axiom: every value is in some kind.
  | top_decomp :
      TyLE (.sh .top)
           (.union (.sh .nul)
              (.union (.sh (.boo none))
                (.union (.sh (.num none))
                  (.union (.sh (.str none))
                    (.union (.sh (.array (.sh .top) none))
                      (.sh (.object [])))))))

  -- ── Kind-top negations ──
  -- These rules handle ONLY the negation of a kind-top (e.g.
  -- `¬Sh(boo none)` = "no boolean at all"). They are *not* sound for
  -- negations of more specific shapes (e.g. `¬Sh(object [(a, num)])`
  -- includes some objects too — see Phase 3 for the full story).
  | neg_kind_top_nul :
      TyLE (.neg_sh .nul)
           (.union (.sh (.boo none))
              (.union (.sh (.num none))
                (.union (.sh (.str none))
                  (.union (.sh (.array (.sh .top) none))
                    (.sh (.object []))))))
  | neg_kind_top_boo :
      TyLE (.neg_sh (.boo none))
           (.union (.sh .nul)
              (.union (.sh (.num none))
                (.union (.sh (.str none))
                  (.union (.sh (.array (.sh .top) none))
                    (.sh (.object []))))))
  | neg_kind_top_num :
      TyLE (.neg_sh (.num none))
           (.union (.sh .nul)
              (.union (.sh (.boo none))
                (.union (.sh (.str none))
                  (.union (.sh (.array (.sh .top) none))
                    (.sh (.object []))))))
  | neg_kind_top_str :
      TyLE (.neg_sh (.str none))
           (.union (.sh .nul)
              (.union (.sh (.boo none))
                (.union (.sh (.num none))
                  (.union (.sh (.array (.sh .top) none))
                    (.sh (.object []))))))
  | neg_kind_top_arr :
      TyLE (.neg_sh (.array (.sh .top) none))
           (.union (.sh .nul)
              (.union (.sh (.boo none))
                (.union (.sh (.num none))
                  (.union (.sh (.str none))
                    (.sh (.object []))))))
  | neg_kind_top_obj :
      TyLE (.neg_sh (.object []))
           (.union (.sh .nul)
              (.union (.sh (.boo none))
                (.union (.sh (.num none))
                  (.union (.sh (.str none))
                    (.sh (.array (.sh .top) none))))))

  -- ── Singleton disjointness (Phase 2) ──
  -- Distinct singleton refinements within the same kind have no value
  -- in common: `Boo(true) ∩ Boo(false) = ⊥`, etc. The `Num` and `Str`
  -- versions handle infinitely many cases via the `≠` premise.
  | singleton_disjoint_boo {b₁ b₂ : Bool} {t : Ty} :
      b₁ ≠ b₂ →
      TyLE (.inter (.sh (.boo (some b₁))) (.sh (.boo (some b₂)))) t
  | singleton_disjoint_num {n₁ n₂ : Int} {t : Ty} :
      n₁ ≠ n₂ →
      TyLE (.inter (.sh (.num (some n₁))) (.sh (.num (some n₂)))) t
  | singleton_disjoint_str {s₁ s₂ : String} {t : Ty} :
      s₁ ≠ s₂ →
      TyLE (.inter (.sh (.str (some s₁))) (.sh (.str (some s₂)))) t

  -- ─────────────────────────────────────────────────────────────────────
  -- Phase 3: Structural negation decomposition.
  --
  -- For records (`object kvs`), set-theoretic negation distributes into
  -- three disjoint cases:
  --
  --   1. "Not an object at all" — handled by the kind-top decomposition.
  --   2. "An object missing a required key" — expressed as
  --        `Sh(object [])  ∩  Neg(Sh(object [(k, top)]))`.
  --      The intersection forces the value to be an object (via open
  --      record top); the negation forces key `k` to be absent (since
  --      `Sh(object [(k, top)])` denotes "objects with k present at any
  --      type", and we negate that out).
  --   3. "An object with a required key at the wrong type" — expressed
  --      as `Sh(object [(k, Neg T)])`. The open record forces k to be
  --      present, with value `∉ T`.
  --
  -- These two rules together let any `Neg(Sh(object kvs))` decompose
  -- recursively. No `Shape` extension is needed: the existing
  -- constructors suffice.
  -- ─────────────────────────────────────────────────────────────────────

  /-- Distribute negation over a single-field record: a value not in
      `object [(k, T)]` is either (1) not an object, (2) an object
      missing `k`, or (3) an object with `k` of type ≠`T`.

      The "wrong type" component uses `negNNF t` to put the field's
      complement in NNF (since `t` may be compound). -/
  | neg_object_single_field {k : String} {t : Ty} :
      TyLE (.neg_sh (.object [(k, t)]))
           (.union
              (.neg_sh (.object []))
              (.union
                (.inter (.sh (.object []))
                        (.neg_sh (.object [(k, .sh .top)])))
                (.sh (.object [(k, negNNF t)]))))

  /-- Multi-field record negation: a value not in `object ((k,t) :: rest)`
      is either not in `object [(k,t)]` or not in `object rest`. -/
  | neg_object_multi_split {k : String} {t : Ty} {rest : List (String × Ty)} :
      TyLE (.neg_sh (.object ((k, t) :: rest)))
           (.union (.neg_sh (.object [(k, t)]))
                   (.neg_sh (.object rest)))

  /-- Distribute negation over arrays. A value not in `array T n` is
      either (1) not an array, (2) an array of length < n (when n is
      `some`), or (3) an array whose elements are in the complement
      of T (NNF: `negNNF T`). -/
  | neg_array_decomp {t : Ty} {n : Option Nat} :
      TyLE (.neg_sh (.array t n))
           (.union
              (.neg_sh (.array (.sh .top) none))
              (.union
                (.inter (.sh (.array (.sh .top) none))
                        (.neg_sh (.array (.sh .top) n)))
                (.sh (.array (negNNF t) n))))

/-- Subtyping on shapes. Open records / open arrays. -/
inductive ShapeLE : Shape → Shape → Prop where
  | refl   : ShapeLE s s
  | trans  : ShapeLE s₁ s₂ → ShapeLE s₂ s₃ → ShapeLE s₁ s₃

  -- ⊤ is top.
  | top    : ShapeLE s .top

  -- Bool / num / str: a literal refines the unrefined form.
  | boo_some_to_none : ShapeLE (.boo (some b)) (.boo none)
  | num_some_to_none : ShapeLE (.num (some n)) (.num none)
  | str_some_to_none : ShapeLE (.str (some s)) (.str none)

  -- Arrays, with min-length bookkeeping.
  | array  : TyLE t t' → MinLE n n' → ShapeLE (.array t n) (.array t' n')

  -- Tuple → tuple: at least as long, and prefix is pointwise sub.
  | tuple_tuple
      (hlen : ts.length ≥ us.length)
      (helem : ∀ i (h : i < us.length),
                  TyLE (ts[i]'(by omega)) (us[i]'h)) :
      ShapeLE (.tuple ts) (.tuple us)

  -- Tuple → array: every position must be ≤ the array element type, and
  -- the tuple's length is the (unique) min length we can claim.
  | tuple_array
      (hmin : MinLE (some ts.length) n)
      (helem : ∀ i (h : i < ts.length), TyLE (ts[i]'h) t) :
      ShapeLE (.tuple ts) (.array t n)

  -- Array → tuple: only valid if the min-length proves we have enough
  -- positions, and each element type ≤ the corresponding tuple cell.
  | array_tuple
      (n : Nat)
      (hge : n ≥ ts.length)
      (helem : ∀ i (h : i < ts.length), TyLE t (ts[i]'h)) :
      ShapeLE (.array t (some n)) (.tuple ts)

  -- Open records: walk the supertype's field list (`obj'`), requiring each
  -- declared key to be present in the subtype's fields with a sub-type.
  --
  -- We avoid `∃ t, …` here because nested `Exists` cannot mention local
  -- variables of the mutual block (Lean kernel restriction). Instead the
  -- rule recurses on the supertype's field list and binds the witness `t`
  -- as an implicit parameter at each step.
  | object_nil :
      ShapeLE (.object obj) (.object [])
  | object_cons {t : Ty} :
      (k, t) ∈ obj →
      TyLE t u →
      ShapeLE (.object obj) (.object rest) →
      ShapeLE (.object obj) (.object ((k, u) :: rest))

/-- Subtyping on streams (regex inclusion, structural fragment). -/
inductive StreamLE : Stream → Stream → Prop where
  | refl    : StreamLE s s
  | trans   : StreamLE s₁ s₂ → StreamLE s₂ s₃ → StreamLE s₁ s₃

  | eps     : StreamLE .eps .eps
  | yld     : TyLE t t' → StreamLE (.yld t) (.yld t')

  | concat  : StreamLE s₁ s₁' → StreamLE s₂ s₂' →
              StreamLE (.concat s₁ s₂) (.concat s₁' s₂')

  -- ε is in any star.
  | eps_star    : StreamLE .eps (.star s)
  -- A single emission is in `S*` if it is in `S`.
  | yld_star    : StreamLE (.yld t) s → StreamLE (.yld t) (.star s)
  -- Star monotone in its body.
  | star        : StreamLE s s' → StreamLE (.star s) (.star s')
  -- Concatenation of two stars under the same body folds in.
  | star_concat : StreamLE (.concat (.star s) (.star s)) (.star s)
  -- Star is idempotent: (S*)* ⊑ S*.
  | star_star   : StreamLE (.star (.star s)) (.star s)

  -- ε is a left/right unit for concat (both directions, since we want ≡).
  | eps_concat_l : StreamLE (.concat .eps s) s
  | eps_concat_r : StreamLE s (.concat .eps s)
  | concat_eps_l : StreamLE (.concat s .eps) s
  | concat_eps_r : StreamLE s (.concat s .eps)

  -- Concat is associative.
  | concat_assoc_l :
      StreamLE (.concat (.concat s₁ s₂) s₃) (.concat s₁ (.concat s₂ s₃))
  | concat_assoc_r :
      StreamLE (.concat s₁ (.concat s₂ s₃)) (.concat (.concat s₁ s₂) s₃)

  -- Choice introductions/elimination.
  | choice_intro_l : StreamLE s s₁ → StreamLE s (.choice s₁ s₂)
  | choice_intro_r : StreamLE s s₂ → StreamLE s (.choice s₁ s₂)
  | choice_elim    : StreamLE s₁ s → StreamLE s₂ s → StreamLE (.choice s₁ s₂) s

  -- ⊥_stream is below every stream type. This is what gives `error : A → ⊥`
  -- its meaning: an aborting filter can be subsumed to any output stream
  -- shape needed at use sites (e.g. inside an if-then-else branch).
  | bot_min : StreamLE .bot s

end -- mutual

/-- Notation for subtyping at the type level. -/
infix:50 " ⊑ "  => TyLE

/-- Notation for subtyping at the stream level. -/
infix:50 " ⊑ₛ " => StreamLE

/-- Notation for subtyping at the shape level. -/
infix:50 " ⊑ₕ " => ShapeLE

/-! ## Convenience: bottom and top -/

/-- The bottom type: `T ∩ ¬T`. Has no inhabitants in the value semantics. -/
def Ty.bot : Ty := .inter (.sh .top) (.neg_sh .top)

/-- The top type: `⊤` lifted from shapes. -/
def Ty.top : Ty := .sh .top

end Tjq
