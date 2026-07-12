/-!
# Tjq.Syntax

The three sorts (Shape, Ty, Stream) and the Filter AST.

Mirrors §1 of `papers/typing-rules.md`. Numeric literals use `Int` as a
stand-in for jq's Float (decidable equality is convenient and the difference
isn't the point of this formalization).
-/

namespace Tjq

/-! ## Shapes, types, and streams (mutually inductive)

Shapes describe the structural form of a JSON value.
`Ty` and `Stream` are mutually inductive with `Shape`. -/

mutual

inductive Shape where
  /-- Top: any JSON value. -/
  | top : Shape
  /-- The null value. -/
  | nul : Shape
  /-- A boolean, possibly refined to a specific value. `none` = any bool. -/
  | boo : Option Bool → Shape
  /-- A number, possibly refined. -/
  | num : Option Int → Shape
  /-- A string, possibly refined. -/
  | str : Option String → Shape
  /-- Homogeneous array; the optional `Nat` is the *minimum* length. -/
  | array : Ty → Option Nat → Shape
  /-- Positional tuple. The underlying array may be longer than `ts.length`
      (open arrays — same flavour as open records). -/
  | tuple : List Ty → Shape
  /-- Open record: declared keys must be present at the listed type;
      additional keys may exist with any type. -/
  | object : List (String × Ty) → Shape

-- The type language. Includes set-theoretic combinators and arrows.
--
-- Negation is restricted to shapes (`neg_sh : Shape → Ty`) so the type
-- language is in *negation normal form* (NNF) by construction. To express
-- the complement of a compound type, use `negNNF` (defined below) — it
-- pushes De Morgan and double-negation eagerly to the leaves.
--
-- Why: the NNF restriction makes `Value : Json → Ty → Prop` definable as
-- an *inductive* predicate (not just opaque), because the negation case
-- becomes `Value j (.neg_sh s) ← NotValueShape j s` — and `NotValueShape`
-- is a separate predicate, so positivity is preserved.
inductive Ty where
  | sh : Shape → Ty
  /-- Filter arrow: input type → output stream. -/
  | arr : Ty → Stream → Ty
  | inter : Ty → Ty → Ty
  | union : Ty → Ty → Ty
  /-- Negation of a *shape* (NNF restriction). For compound type
      negations, use `negNNF`. -/
  | neg_sh : Shape → Ty
  /-- Type variable for HM inference. -/
  | tvar : Nat → Ty

-- The stream language: regex of types.
inductive Stream where
  /-- ε: the empty stream (no emissions). -/
  | eps : Stream
  /-- ⊥: the bottom stream — used for filters that abort (e.g. `error`).
      Subtype of every stream type. Distinguishes "produces nothing then
      aborts" from "produces nothing then continues" (= `eps`). -/
  | bot : Stream
  /-- ⟨T⟩: singleton emission. -/
  | yld : Ty → Stream
  /-- Ordered concatenation (`,`). -/
  | concat : Stream → Stream → Stream
  /-- Kleene star (e.g. iteration of arrays). -/
  | star : Stream → Stream
  /-- Choice (`⊕`): input-dependent branching. -/
  | choice : Stream → Stream → Stream
  /-- Stream variable. -/
  | svar : Nat → Stream

end

/-- Recogniser for the type `.sh .top` (used by inference to branch
    between the precise/imprecise if-then-else rules). -/
def Ty.isShTop : Ty → Bool
  | .sh .top => true
  | _        => false

theorem Ty.isShTop_eq : ∀ {t : Ty}, t.isShTop = true → t = .sh .top := by
  intro t h
  cases t with
  | sh s =>
      cases s with
      | top => rfl
      | nul        => simp [Ty.isShTop] at h
      | boo _      => simp [Ty.isShTop] at h
      | num _      => simp [Ty.isShTop] at h
      | str _      => simp [Ty.isShTop] at h
      | array _ _  => simp [Ty.isShTop] at h
      | tuple _    => simp [Ty.isShTop] at h
      | object _   => simp [Ty.isShTop] at h
  | arr _ _    => simp [Ty.isShTop] at h
  | inter _ _  => simp [Ty.isShTop] at h
  | union _ _  => simp [Ty.isShTop] at h
  | neg_sh _   => simp [Ty.isShTop] at h
  | tvar _     => simp [Ty.isShTop] at h

theorem Ty.isShTop_ne : ∀ {t : Ty}, t.isShTop = false → t ≠ .sh .top := by
  intro t h heq
  rw [heq] at h
  simp [Ty.isShTop] at h

/-- `negNNF t` — compute the type-level complement of `t`, kept in
    negation normal form.

    Pushes De Morgan and double-negation eagerly to the leaves. Arrows
    have no inhabitants (they're filter types, not JSON values), so
    their "complement" is conservatively the universe (`Sh top`).
    Tvars are left as-is (we don't have a better answer for "complement
    of an unknown type"). -/
def negNNF : Ty → Ty
  | .sh s        => .neg_sh s
  | .neg_sh s    => .sh s
  | .inter t₁ t₂ => .union (negNNF t₁) (negNNF t₂)
  | .union t₁ t₂ => .inter (negNNF t₁) (negNNF t₂)
  | .arr _ _     => .sh .top
  | .tvar n      => .tvar n

/-- Smart constructor: restrict creation of `neg_sh` to non-arrow shapes.
    (We could also forbid `Sh top`'s negation as `bot`, but allow it for
    expressiveness.) -/
def Ty.neg_safe : Shape → Ty := Ty.neg_sh

/-- The six base kinds of a JSON value. Every concrete value belongs to
    exactly one of these (this is the foundational disjointness fact that
    drives set-theoretic negation reasoning). -/
inductive Kind : Type where
  | Nul
  | Boo
  | Num
  | Str
  | Arr
  | Obj
  deriving DecidableEq, Repr

/-- Which `Kind` a `Shape` belongs to (when defined — `top` does not have
    a definite kind, since it is the universe). -/
inductive ShapeKind : Shape → Kind → Prop where
  | nul                                  : ShapeKind .nul Kind.Nul
  | boo (b : Option Bool)                : ShapeKind (.boo b) Kind.Boo
  | num (n : Option Int)                 : ShapeKind (.num n) Kind.Num
  | str (s : Option String)              : ShapeKind (.str s) Kind.Str
  | array (t : Ty) (n : Option Nat)      : ShapeKind (.array t n) Kind.Arr
  /-- Tuples are arrays. -/
  | tuple (ts : List Ty)                 : ShapeKind (.tuple ts) Kind.Arr
  | object (kvs : List (String × Ty))    : ShapeKind (.object kvs) Kind.Obj

/-- The "top" shape of each kind — the supertype of all shapes of that kind. -/
def Kind.top : Kind → Shape
  | .Nul => .nul
  | .Boo => .boo none
  | .Num => .num none
  | .Str => .str none
  | .Arr => .array (.sh .top) none
  | .Obj => .object []

/-- The "type-order rank" of each kind — jq's value comparison falls back
    to this rank when comparing values of different kinds:
        nul < bool < num < str < array < object. -/
def Kind.rank : Kind → Nat
  | .Nul => 0
  | .Boo => 1
  | .Num => 2
  | .Str => 3
  | .Arr => 4
  | .Obj => 5

/-- Concrete JSON values. Used to give meaning to value-typing judgments. -/
inductive Json where
  | nul : Json
  | boo : Bool → Json
  | num : Int → Json
  | str : String → Json
  | arr : List Json → Json
  | obj : List (String × Json) → Json

/-- Binary operators in the filter language. -/
inductive BinOp where
  | add | sub | mul | div | mod
  | eq | ne | lt | le | gt | ge
  | and | or
  deriving DecidableEq, Repr

/-- Unary operators. -/
inductive UnOp where
  | neg
  deriving DecidableEq, Repr

/-- The core fragment of the Filter AST.

    Excluded from v1: reduce/foreach, calls, variable binding, slicing,
    recursive descent, try/catch, path expressions, assignments. -/
inductive Filter where
  | dot : Filter
  | pipe : Filter → Filter → Filter
  | comma : Filter → Filter → Filter
  /-- `.k` — object index by literal key. -/
  | objIndex : String → Filter
  /-- `.[n]` — array index by literal integer. -/
  | arrIndex : Int → Filter
  /-- `.[]` — iterate. -/
  | iter : Filter
  | nullLit : Filter
  | boolLit : Bool → Filter
  | numLit : Int → Filter
  | strLit : String → Filter
  /-- `[f₁, …, fₙ]` — collect the comma-stream into an array.
      We keep the list shape to match the existing AST; semantically this is
      `[f₁ , f₂ , … , fₙ]`. -/
  | array : List Filter → Filter
  /-- `{k₁: v₁, …}` — build an object with literal keys and filter-valued
      cells. Computed keys deferred to a later version. -/
  | object : List (String × Filter) → Filter
  | empty : Filter
  | error : Filter
  | ifThenElse : Filter → Filter → Filter → Filter
  | binop : BinOp → Filter → Filter → Filter
  | unop : UnOp → Filter → Filter
  /-- Call a named filter (e.g., `isarray`, `length`, user-defined `def`s).
      Zero-arity for v1 — higher-order calls deferred. -/
  | call : String → Filter

/-- Smart constructor: complement of a `Ty` via `negNNF`.
    Arrow types are mapped to `Sh top` (their semantic complement is
    "anything that's not a function" = the universe of JSON values). -/
def Ty.negSafe : Ty → Ty := negNNF

/-- An arrow type, with its argument and codomain stream made syntactic. -/
def Ty.mkArrow (input : Ty) (out : Stream) : Ty := .arr input out

end Tjq
