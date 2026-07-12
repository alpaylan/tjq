import Tjq.Syntax
import Tjq.Subtyping

/-!
# Tjq.Typing

Auxiliary operations (`collect`, `flatmap`, `refine`) and the `HasType`
inductive judgment for the core fragment of `Filter`.

The judgment shape is `HasType f T`: `f` is a filter, `T` is a `Ty` (which
will always be an arrow `A → S` for a well-typed filter, but we expose the
single-`Ty` form so that the if-then-else "intersection of arrows" rule fits
naturally).

What's *not* covered (acknowledged in `papers/typing-rules.md` §6):
* `reduce` / `foreach` (LUB inference, not in the core).
* User-defined filters and calls (HM let-polymorphism scaffolding).
* `as $x | g` and `$x` (variable binding context).
* Slice expressions.
-/

namespace Tjq

/-! ## `substTy` — single-tvar substitution

    Replaces `Ty.tvar n` with `U` everywhere in a `Ty`, recursing through
    shapes, arrows, streams, and the lists nested in `Tuple` / `Object`.
    Mutual with `substShape` / `substStream` / list helpers. -/

mutual

def substTy (n : Nat) (U : Ty) : Ty → Ty
  | .sh s        => .sh (substShape n U s)
  | .arr a s     => .arr (substTy n U a) (substStream n U s)
  | .inter t₁ t₂ => .inter (substTy n U t₁) (substTy n U t₂)
  | .union t₁ t₂ => .union (substTy n U t₁) (substTy n U t₂)
  | .neg_sh s    => .neg_sh (substShape n U s)
  | .tvar k      => if k = n then U else .tvar k

def substShape (n : Nat) (U : Ty) : Shape → Shape
  | .top         => .top
  | .nul         => .nul
  | .boo b       => .boo b
  | .num m       => .num m
  | .str s       => .str s
  | .array t k   => .array (substTy n U t) k
  | .tuple ts    => .tuple (substTyList n U ts)
  | .object kvs  => .object (substKVList n U kvs)

def substStream (n : Nat) (U : Ty) : Stream → Stream
  | .eps          => .eps
  | .bot          => .bot
  | .yld t        => .yld (substTy n U t)
  | .concat s₁ s₂ => .concat (substStream n U s₁) (substStream n U s₂)
  | .star s       => .star (substStream n U s)
  | .choice s₁ s₂ => .choice (substStream n U s₁) (substStream n U s₂)
  | .svar k       => .svar k

def substTyList (n : Nat) (U : Ty) : List Ty → List Ty
  | []      => []
  | t :: ts => substTy n U t :: substTyList n U ts

def substKVList (n : Nat) (U : Ty) : List (String × Ty) → List (String × Ty)
  | []           => []
  | (k, t) :: ts => (k, substTy n U t) :: substKVList n U ts

end

/-! ## `flatmap` — Kleisli substitution at the stream level -/

/-- `flatmap S k` substitutes each `⟨T⟩` in `S` by `k T`. Used to type pipes:
    `f | g` produces `flatmap S_f (λT. instantiate(g, T))`.
    Argument order matches the markdown spec. -/
def flatmap : Stream → (Ty → Stream) → Stream
  | .eps,          _ => .eps
  | .bot,          _ => .bot          -- piping after an abort still aborts
  | .yld t,        k => k t
  | .concat s₁ s₂, k => .concat (flatmap s₁ k) (flatmap s₂ k)
  | .star s,       k => .star (flatmap s k)
  | .choice s₁ s₂, k => .choice (flatmap s₁ k) (flatmap s₂ k)
  | .svar n,       _ => .svar n  -- conservative on free stream vars

/-! ## `elemType` — projection of a stream to "any element's type" -/

/-- The least upper bound of all element types appearing in a stream.
    Conservative: returns `union` of all positions. -/
def elemType : Stream → Ty
  | .eps             => Ty.bot
  | .bot             => Ty.bot       -- nothing yielded (and aborted)
  | .yld t           => t
  | .concat s₁ s₂    => .union (elemType s₁) (elemType s₂)
  | .star s          => elemType s
  | .choice s₁ s₂    => .union (elemType s₁) (elemType s₂)
  | .svar _          => .sh .top  -- unknown stream var: be permissive

/-! ## `collect` — read a stream into a value-shape

    Used to type `[f]`. Tuple structure is preserved when the stream is a
    finite concat-of-singletons; star streams collapse to `Array`. -/

mutual

/-- The "raw" tuple-list reading of a stream: just the types yielded, in order.
    Returns `none` if the stream contains a `*` or `⊕` (which means the result
    is not a fixed tuple). -/
def collectTuple : Stream → Option (List Ty)
  | .eps             => some []
  | .bot             => some []      -- aborts → no values collected
  | .yld t           => some [t]
  | .concat s₁ s₂    =>
      match collectTuple s₁, collectTuple s₂ with
      | some xs, some ys => some (xs ++ ys)
      | _,       _       => none
  | .star _          => none
  | .choice _ _      => none
  | .svar _          => none

/-- Read a stream into a value-typing.

    * Pure-singleton streams (with concats) become a `Tuple`.
    * Streams with `*` become `Array(elemType, ?)`.
    * `⊕` is read into a union of the two collects.
    * Free stream variables become `⊤`. -/
def collect : Stream → Ty
  | .eps             => .sh (.tuple [])
  | .bot             => .sh (.tuple [])  -- aborts collect to "no values"
  | .yld t           => .sh (.tuple [t])
  | .concat s₁ s₂    =>
      match collectTuple (.concat s₁ s₂) with
      | some ts => .sh (.tuple ts)
      | none    => .sh (.array (.union (elemType s₁) (elemType s₂)) none)
  | .star s          => .sh (.array (elemType s) none)
  | .choice s₁ s₂    => .union (collect s₁) (collect s₂)
  | .svar _          => .sh .top

end

/-! ## `refine` — input narrowing from a guard

    Returns the type a value must inhabit for the guard to evaluate the
    given way. `top` means "no useful refinement available" — that's the
    signal the if-then-else rule uses to fall through to the imprecise
    (stream-choice) case. -/

/-- Helper: literal-shape of a `Filter` value form. Returns `none` if the
    filter is not a value literal. -/
def Filter.litShape : Filter → Option Shape
  | .nullLit       => some .nul
  | .boolLit b     => some (.boo (some b))
  | .numLit n      => some (.num (some n))
  | .strLit s      => some (.str (some s))
  | _              => none

/-- Refinement: `refine f b` is the input type that, when fed to `f`, makes
    `f` evaluate to `b`. Falls through to `⊤` for unrecognised forms. -/
def refine : Filter → Bool → Ty
  -- Equality / disequality with a value literal on the right.
  | .binop .eq .dot lit, true =>
      match lit.litShape with
      | some s => .sh s
      | none   => .sh .top
  | .binop .eq .dot lit, false =>
      match lit.litShape with
      | some s => .neg_sh s
      | none   => .sh .top
  | .binop .ne .dot lit, true =>
      match lit.litShape with
      | some s => .neg_sh s
      | none   => .sh .top
  | .binop .ne .dot lit, false =>
      match lit.litShape with
      | some s => .sh s
      | none   => .sh .top
  -- Boolean connectives: distribute.
  | .binop .and p q, true  => .inter (refine p true)  (refine q true)
  | .binop .and p q, false => .union (refine p false) (refine q false)
  | .binop .or  p q, true  => .union (refine p true)  (refine q true)
  | .binop .or  p q, false => .inter (refine p false) (refine q false)
  -- Default: no useful refinement.
  | _, _ => .sh .top

/-! ## Overload table for binary operators -/

/-- Inductive characterisation of legal overloadings of a binary operator.
    `Overload op tl tr to` says "op : tl × tr → to is in the overload set,
    given the actual operand types `tl` and `tr`".

    This is *not* exhaustive — only the major overloads are listed. v1 enough
    to type the common arithmetic / boolean / comparison patterns. -/
inductive Overload : BinOp → Ty → Ty → Ty → Prop where
  -- + : num × num → num
  | add_num : (l ⊑ .sh (.num none)) → (r ⊑ .sh (.num none)) →
              Overload .add l r (.sh (.num none))
  -- + : str × str → str
  | add_str : (l ⊑ .sh (.str none)) → (r ⊑ .sh (.str none)) →
              Overload .add l r (.sh (.str none))
  -- + : null × T → T (and the symmetric case).
  | add_null_l : (l ⊑ .sh .nul) → Overload .add l r r
  | add_null_r : (r ⊑ .sh .nul) → Overload .add l r l
  -- - : num × num → num
  | sub_num : (l ⊑ .sh (.num none)) → (r ⊑ .sh (.num none)) →
              Overload .sub l r (.sh (.num none))
  -- * / % div : num × num → num
  | mul_num : (l ⊑ .sh (.num none)) → (r ⊑ .sh (.num none)) →
              Overload .mul l r (.sh (.num none))
  | div_num : (l ⊑ .sh (.num none)) → (r ⊑ .sh (.num none)) →
              Overload .div l r (.sh (.num none))
  | mod_num : (l ⊑ .sh (.num none)) → (r ⊑ .sh (.num none)) →
              Overload .mod l r (.sh (.num none))
  -- Comparisons: T × T → bool (subtyping checked at the use site).
  | eq_any  : Overload .eq l r (.sh (.boo none))
  | ne_any  : Overload .ne l r (.sh (.boo none))
  | lt_any  : Overload .lt l r (.sh (.boo none))
  | le_any  : Overload .le l r (.sh (.boo none))
  | gt_any  : Overload .gt l r (.sh (.boo none))
  | ge_any  : Overload .ge l r (.sh (.boo none))
  -- Boolean connectives: bool × bool → bool.
  | and_bool : (l ⊑ .sh (.boo none)) → (r ⊑ .sh (.boo none)) →
               Overload .and l r (.sh (.boo none))
  | or_bool  : (l ⊑ .sh (.boo none)) → (r ⊑ .sh (.boo none)) →
               Overload .or l r (.sh (.boo none))

  -- ---------- Refined overloads (literal-aware) ----------
  --
  -- These let derivations track concrete results when both operands are
  -- value literals at the type level. They are what allows the body of
  -- `def isboolean: . == true or . == false` to type as the intersection
  -- of arrows `(bool → ⟨true⟩) ∩ (¬bool → ⟨false⟩)`, when combined with
  -- `HasType.inter_intro` and `HasType.inst`.

  -- Equality of matching bool literals: `b == b` ↦ `true`.
  | eq_bool_eq (b : Bool) :
      Overload .eq (.sh (.boo (some b))) (.sh (.boo (some b)))
                   (.sh (.boo (some true)))
  -- Equality of distinct bool literals: `b₁ == b₂` (b₁ ≠ b₂) ↦ `false`.
  | eq_bool_neq (b₁ b₂ : Bool) (h : b₁ ≠ b₂) :
      Overload .eq (.sh (.boo (some b₁))) (.sh (.boo (some b₂)))
                   (.sh (.boo (some false)))

  -- `or` short-circuits on `true` (either side).
  | or_lit_true_l (X : Ty) (h : X ⊑ .sh (.boo none)) :
      Overload .or (.sh (.boo (some true))) X (.sh (.boo (some true)))
  | or_lit_true_r (X : Ty) (h : X ⊑ .sh (.boo none)) :
      Overload .or X (.sh (.boo (some true))) (.sh (.boo (some true)))
  -- `false || false = false`.
  | or_false_false :
      Overload .or (.sh (.boo (some false))) (.sh (.boo (some false)))
                   (.sh (.boo (some false)))

  -- Cross-type equality: comparing a bool to a number/string is always
  -- `false` (the types are disjoint). Same for the symmetric directions.
  -- These let us derive the `¬bool → ⟨false⟩` half of `isboolean`'s
  -- precise scheme by typing the body at non-bool inputs.
  | eq_disjoint_bool_num (b : Option Bool) (n : Option Int) :
      Overload .eq (.sh (.boo b)) (.sh (.num n)) (.sh (.boo (some false)))
  | eq_disjoint_num_bool (b : Option Bool) (n : Option Int) :
      Overload .eq (.sh (.num n)) (.sh (.boo b)) (.sh (.boo (some false)))
  | eq_disjoint_bool_str (b : Option Bool) (s : Option String) :
      Overload .eq (.sh (.boo b)) (.sh (.str s)) (.sh (.boo (some false)))
  | eq_disjoint_str_bool (b : Option Bool) (s : Option String) :
      Overload .eq (.sh (.str s)) (.sh (.boo b)) (.sh (.boo (some false)))
  | eq_disjoint_bool_nul (b : Option Bool) :
      Overload .eq (.sh (.boo b)) (.sh .nul) (.sh (.boo (some false)))
  | eq_disjoint_nul_bool (b : Option Bool) :
      Overload .eq (.sh .nul) (.sh (.boo b)) (.sh (.boo (some false)))
  | eq_disjoint_bool_array (b : Option Bool) (t : Ty) (n : Option Nat) :
      Overload .eq (.sh (.boo b)) (.sh (.array t n)) (.sh (.boo (some false)))
  | eq_disjoint_array_bool (b : Option Bool) (t : Ty) (n : Option Nat) :
      Overload .eq (.sh (.array t n)) (.sh (.boo b)) (.sh (.boo (some false)))
  | eq_disjoint_bool_object (b : Option Bool) (kvs : List (String × Ty)) :
      Overload .eq (.sh (.boo b)) (.sh (.object kvs)) (.sh (.boo (some false)))
  | eq_disjoint_object_bool (b : Option Bool) (kvs : List (String × Ty)) :
      Overload .eq (.sh (.object kvs)) (.sh (.boo b)) (.sh (.boo (some false)))

  -- More cross-type eq overloads (for `isnull`-style derivations).
  -- nul == nul is the only `eq_*_eq` rule for nul (only one inhabitant).
  | eq_nul_eq : Overload .eq (.sh .nul) (.sh .nul) (.sh (.boo (some true)))
  | eq_disjoint_num_nul (n : Option Int) :
      Overload .eq (.sh (.num n)) (.sh .nul) (.sh (.boo (some false)))
  | eq_disjoint_nul_num (n : Option Int) :
      Overload .eq (.sh .nul) (.sh (.num n)) (.sh (.boo (some false)))
  | eq_disjoint_str_nul (s : Option String) :
      Overload .eq (.sh (.str s)) (.sh .nul) (.sh (.boo (some false)))
  | eq_disjoint_nul_str (s : Option String) :
      Overload .eq (.sh .nul) (.sh (.str s)) (.sh (.boo (some false)))
  | eq_disjoint_array_nul (t : Ty) (n : Option Nat) :
      Overload .eq (.sh (.array t n)) (.sh .nul) (.sh (.boo (some false)))
  | eq_disjoint_nul_array (t : Ty) (n : Option Nat) :
      Overload .eq (.sh .nul) (.sh (.array t n)) (.sh (.boo (some false)))
  | eq_disjoint_object_nul (kvs : List (String × Ty)) :
      Overload .eq (.sh (.object kvs)) (.sh .nul) (.sh (.boo (some false)))
  | eq_disjoint_nul_object (kvs : List (String × Ty)) :
      Overload .eq (.sh .nul) (.sh (.object kvs)) (.sh (.boo (some false)))

  -- ─────────────────────────────────────────────────────────────────────
  -- Comparison overloads for `<`, `<=`, `>`, `>=`.
  --
  -- jq's value-comparison: across kinds, follows the kind-order
  -- nul < bool < num < str < array < object (`Kind.rank`). Within a kind,
  -- values are compared structurally (lexicographic for strings/arrays/
  -- objects, numerically for numbers, true > false for bools).
  --
  -- The cross-kind rules are parametric over `ShapeKind` and `Kind.rank`.
  -- The same-kind rules are the few "anchor" comparisons needed by the
  -- bodies of the type predicates in `defs.jq`.
  -- ─────────────────────────────────────────────────────────────────────

  -- Cross-kind: lower-rank vs higher-rank.
  | lt_cross_kind_lower {s₁ s₂ k₁ k₂} :
      ShapeKind s₁ k₁ → ShapeKind s₂ k₂ → k₁.rank < k₂.rank →
      Overload .lt (.sh s₁) (.sh s₂) (.sh (.boo (some true)))
  | lt_cross_kind_higher {s₁ s₂ k₁ k₂} :
      ShapeKind s₁ k₁ → ShapeKind s₂ k₂ → k₂.rank < k₁.rank →
      Overload .lt (.sh s₁) (.sh s₂) (.sh (.boo (some false)))
  | gt_cross_kind_lower {s₁ s₂ k₁ k₂} :
      ShapeKind s₁ k₁ → ShapeKind s₂ k₂ → k₁.rank < k₂.rank →
      Overload .gt (.sh s₁) (.sh s₂) (.sh (.boo (some false)))
  | gt_cross_kind_higher {s₁ s₂ k₁ k₂} :
      ShapeKind s₁ k₁ → ShapeKind s₂ k₂ → k₂.rank < k₁.rank →
      Overload .gt (.sh s₁) (.sh s₂) (.sh (.boo (some true)))
  | le_cross_kind_lower {s₁ s₂ k₁ k₂} :
      ShapeKind s₁ k₁ → ShapeKind s₂ k₂ → k₁.rank < k₂.rank →
      Overload .le (.sh s₁) (.sh s₂) (.sh (.boo (some true)))
  | le_cross_kind_higher {s₁ s₂ k₁ k₂} :
      ShapeKind s₁ k₁ → ShapeKind s₂ k₂ → k₂.rank < k₁.rank →
      Overload .le (.sh s₁) (.sh s₂) (.sh (.boo (some false)))
  | ge_cross_kind_lower {s₁ s₂ k₁ k₂} :
      ShapeKind s₁ k₁ → ShapeKind s₂ k₂ → k₁.rank < k₂.rank →
      Overload .ge (.sh s₁) (.sh s₂) (.sh (.boo (some false)))
  | ge_cross_kind_higher {s₁ s₂ k₁ k₂} :
      ShapeKind s₁ k₁ → ShapeKind s₂ k₂ → k₂.rank < k₁.rank →
      Overload .ge (.sh s₁) (.sh s₂) (.sh (.boo (some true)))

  -- Same-kind anchor comparisons.
  -- bool > bool[true]: always false (true is the max bool).
  | gt_anything_bool_true (b : Option Bool) :
      Overload .gt (.sh (.boo b)) (.sh (.boo (some true))) (.sh (.boo (some false)))
  -- str s >= str "": always true ("" is the min str).
  | ge_str_str_empty (s : Option String) :
      Overload .ge (.sh (.str s)) (.sh (.str (some ""))) (.sh (.boo (some true)))
  -- str s < str "": always false.
  | lt_str_str_empty (s : Option String) :
      Overload .lt (.sh (.str s)) (.sh (.str (some ""))) (.sh (.boo (some false)))
  -- array t n >= tuple []: always true (any array is ≥ the empty array).
  | ge_array_tuple_empty (t : Ty) (n : Option Nat) :
      Overload .ge (.sh (.array t n)) (.sh (.tuple [])) (.sh (.boo (some true)))
  -- tuple ts >= tuple []: always true.
  | ge_tuple_tuple_empty (ts : List Ty) :
      Overload .ge (.sh (.tuple ts)) (.sh (.tuple [])) (.sh (.boo (some true)))
  -- array t n < tuple []: always false.
  | lt_array_tuple_empty (t : Ty) (n : Option Nat) :
      Overload .lt (.sh (.array t n)) (.sh (.tuple [])) (.sh (.boo (some false)))
  -- tuple ts < tuple []: always false.
  | lt_tuple_tuple_empty (ts : List Ty) :
      Overload .lt (.sh (.tuple ts)) (.sh (.tuple [])) (.sh (.boo (some false)))
  -- object kvs >= object []: always true ({} is min object).
  | ge_object_object_empty (kvs : List (String × Ty)) :
      Overload .ge (.sh (.object kvs)) (.sh (.object [])) (.sh (.boo (some true)))
  -- object kvs < object []: always false.
  | lt_object_object_empty (kvs : List (String × Ty)) :
      Overload .lt (.sh (.object kvs)) (.sh (.object [])) (.sh (.boo (some false)))

  -- ─────────────────────────────────────────────────────────────────────
  -- `and` literal-aware overloads.
  -- ─────────────────────────────────────────────────────────────────────
  | and_true_true :
      Overload .and (.sh (.boo (some true))) (.sh (.boo (some true)))
                    (.sh (.boo (some true)))
  | and_lit_false_l (X : Ty) (h : X ⊑ .sh (.boo none)) :
      Overload .and (.sh (.boo (some false))) X (.sh (.boo (some false)))
  | and_lit_false_r (X : Ty) (h : X ⊑ .sh (.boo none)) :
      Overload .and X (.sh (.boo (some false))) (.sh (.boo (some false)))

/-! ## The typing judgment

    `HasType f T` says "filter `f` has type `T`". For a well-typed filter
    `T` is always (at the outermost level) an arrow or an intersection of
    arrows; this is enforced rule-by-rule rather than as a global invariant. -/

inductive HasType : Filter → Ty → Prop where
  -- Identity: . : ∀α, α → ⟨α⟩
  | dot (α : Nat) :
      HasType .dot (.arr (.tvar α) (.yld (.tvar α)))

  -- Comma: f, g : A → S_f ; S_g
  | comma {f g A S₁ S₂} :
      HasType f (.arr A S₁) →
      HasType g (.arr A S₂) →
      HasType (.comma f g) (.arr A (.concat S₁ S₂))

  -- Pipe: f | g, with side condition that elem-type(S_f) <: B
  | pipe {f g A S_f B S_g} :
      HasType f (.arr A S_f) →
      HasType g (.arr B S_g) →
      (elemType S_f ⊑ B) →
      -- Codomain stream uses flatmap; the lambda just returns S_g
      -- (v1: we don't yet handle dependence of S_g on the input).
      HasType (.pipe f g) (.arr A (flatmap S_f (fun _ => S_g)))

  -- Empty: produces no values, continues. α → ε
  | empty (α : Nat) : HasType .empty (.arr (.tvar α) .eps)
  -- Error: aborts. α → ⊥ (subtype of every stream — distinguishes
  -- "produces nothing then continues" from "produces nothing then aborts").
  | error (α : Nat) : HasType .error (.arr (.tvar α) .bot)

  -- Value constructors: α → ⟨literal⟩
  | nullLit (α : Nat) :
      HasType .nullLit (.arr (.tvar α) (.yld (.sh .nul)))
  | boolLit (α : Nat) (b : Bool) :
      HasType (.boolLit b) (.arr (.tvar α) (.yld (.sh (.boo (some b)))))
  | numLit (α : Nat) (n : Int) :
      HasType (.numLit n) (.arr (.tvar α) (.yld (.sh (.num (some n)))))
  | strLit (α : Nat) (s : String) :
      HasType (.strLit s) (.arr (.tvar α) (.yld (.sh (.str (some s)))))

  -- .k : (Null ∪ Object({k:α})) → ⟨α ∪ Null⟩
  | objIndex (α : Nat) (k : String) :
      HasType (.objIndex k)
              (.arr (.union (.sh .nul)
                            (.sh (.object [(k, .tvar α)])))
                    (.yld (.union (.tvar α) (.sh .nul))))

  -- .[n] : (Null ∪ Array(α, ?)) → ⟨α ∪ Null⟩
  | arrIndex (α : Nat) (n : Int) :
      HasType (.arrIndex n)
              (.arr (.union (.sh .nul)
                            (.sh (.array (.tvar α) none)))
                    (.yld (.union (.tvar α) (.sh .nul))))

  -- .[] over Array(α, ?) : Array(α, ?) → ⟨α⟩*
  | iterArray (α : Nat) :
      HasType .iter
              (.arr (.sh (.array (.tvar α) none))
                    (.star (.yld (.tvar α))))

  -- [f₁, …, fₙ]: collect the comma-stream.
  -- We desugar the rule by treating `array fs` as `[(comma fs)]`; the
  -- premise asks each `fᵢ` to type at `A → Sᵢ` and the result is
  -- `A → ⟨ collect(S₁ ; … ; Sₙ) ⟩`. The conjunction over `fs` is encoded
  -- as a `List.Forall₂`-like Prop.
  | array {fs A} (Ss : List Stream)
      (hlen : fs.length = Ss.length)
      (helem : ∀ i (h : i < fs.length),
          HasType (fs[i]'h) (.arr A (Ss[i]'(by omega)))) :
      HasType (.array fs)
              (.arr A (.yld (collect (Ss.foldr Stream.concat .eps))))

  -- {k₁: v₁, …, kₙ: vₙ}: each value-filter must yield a singleton.
  | object {kvs A} (Ts : List Ty)
      (hlen : kvs.length = Ts.length)
      (helem : ∀ i (h : i < kvs.length),
          HasType ((kvs[i]'h).snd) (.arr A (.yld (Ts[i]'(by omega))))) :
      HasType (.object kvs)
              (.arr A (.yld (.sh (.object
                  (List.zipWith (fun (kv : String × Filter) (t : Ty) =>
                                    (kv.fst, t)) kvs Ts)))))

  -- if c then t else e — precise guard variant.
  -- The result is an *intersection of arrows*: each branch refines the
  -- input and yields its own stream.
  | ifPrecise {c t e A St Se} :
      HasType c (.arr A (.yld (.sh (.boo none)))) →
      refine c true ≠ .sh .top →
      HasType t (.arr (.inter A (refine c true)) St) →
      HasType e (.arr (.inter A (refine c false)) Se) →
      HasType (.ifThenElse c t e)
              (.inter (.arr (refine c true) St)
                      (.arr (refine c false) Se))

  -- if c then t else e — imprecise guard (refine returns ⊤).
  | ifImprecise {c t e A St Se} :
      HasType c (.arr A (.yld (.sh (.boo none)))) →
      refine c true = .sh .top →
      HasType t (.arr A St) →
      HasType e (.arr A Se) →
      HasType (.ifThenElse c t e) (.arr A (.choice St Se))

  -- Binary ops: pick an overload from the table.
  | binop {op l r A Tl Tr To} :
      HasType l (.arr A (.yld Tl)) →
      HasType r (.arr A (.yld Tr)) →
      Overload op Tl Tr To →
      HasType (.binop op l r) (.arr A (.yld To))

  -- Unary negation: input must be a number.
  | unopNeg {f A} :
      HasType f (.arr A (.yld (.sh (.num none)))) →
      HasType (.unop .neg f) (.arr A (.yld (.sh (.num none))))

  -- Subsumption: a filter that has a type also has any supertype.
  | sub {f T T'} :
      HasType f T → (T ⊑ T') → HasType f T'

  -- HM instantiation: any type variable can be specialised to any type.
  -- Soundness: every constructor introduces type variables that are
  -- universally quantified — substituting at any of them yields a valid
  -- type. This is "let-polymorphism without let": specialise at use sites.
  | inst {f T} (n : Nat) (U : Ty) :
      HasType f T → HasType f (substTy n U T)

  -- Named filter call. The rule assumes a *definition context* (informally:
  -- a `Defs := List (String × Ty)` mapping names to schemes) and consults
  -- it. v1 doesn't carry that context as a judgment index — instead the
  -- rule takes the scheme `T` as an explicit argument, and the user
  -- provides one matching the (separately-derived) type of the body.
  --
  -- A use of `call` thus reads: "assume `name` has been defined with
  -- scheme `T`; conclude that calling `name` produces a value of type `T`."
  -- An honest implementation would replace this with an indexed
  -- judgment `HasType Ξ` and a lookup premise `(name, T) ∈ Ξ`.
  | call (name : String) (T : Ty) : HasType (.call name) T

  -- Intersection introduction: a filter that has two types has their
  -- intersection. Soundness: `Sh(s)` is the *set* of values having shape
  -- `s`; the value-set semantics of `T₁ ∩ T₂` is exactly the intersection
  -- of value-sets. For arrows, the same principle: a filter that behaves
  -- as `T₁` *and* as `T₂` has type `T₁ ∩ T₂`.
  --
  -- This rule is what lets a filter's "ad-hoc polymorphic" scheme be
  -- *derived* (rather than asserted) from typing it once per input case
  -- and combining the witnesses.
  | inter_intro {f T₁ T₂} :
      HasType f T₁ → HasType f T₂ → HasType f (.inter T₁ T₂)

end Tjq
