import Tjq.Syntax
import Tjq.Subtyping
import Tjq.Typing

/-!
# Tjq.Examples

Construct full typing derivations for representative jq expressions. Each
`example` corresponds to a real jq program; the derivation witnesses that
our rules type-check it the way we want.

These are end-to-end tests of the rules — every constructor and aux
operator gets exercised at least once.
-/

namespace Tjq

/-! ## §1. Literal values

    `1` and friends type as `α → ⟨literal⟩`. Trivial. -/

-- jq: 1
example : HasType (.numLit 1)
    (.arr (.tvar 0) (.yld (.sh (.num (some 1))))) :=
  .numLit 0 1

-- jq: "hello"
example : HasType (.strLit "hello")
    (.arr (.tvar 0) (.yld (.sh (.str (some "hello"))))) :=
  .strLit 0 "hello"

-- jq: null
example : HasType .nullLit
    (.arr (.tvar 0) (.yld (.sh .nul))) :=
  .nullLit 0

/-! ## §2. The dot

    `.` is the polymorphic identity on streams. -/

-- jq: .
example : HasType .dot (.arr (.tvar 0) (.yld (.tvar 0))) :=
  .dot 0

-- jq: ., .  — outputs the input twice
example : HasType (.comma .dot .dot)
    (.arr (.tvar 0) (.concat (.yld (.tvar 0)) (.yld (.tvar 0)))) :=
  .comma (.dot 0) (.dot 0)

/-! ## §3. Comma — stream concatenation

    `f, g` produces the concat of f's and g's streams. -/

-- jq: 1, 2
example : HasType (.comma (.numLit 1) (.numLit 2))
    (.arr (.tvar 0)
          (.concat (.yld (.sh (.num (some 1))))
                   (.yld (.sh (.num (some 2)))))) :=
  .comma (.numLit 0 1) (.numLit 0 2)

-- jq: 1, "a", true   — heterogeneous stream
example : HasType
    (.comma (.numLit 1) (.comma (.strLit "a") (.boolLit true)))
    (.arr (.tvar 0)
          (.concat (.yld (.sh (.num (some 1))))
            (.concat (.yld (.sh (.str (some "a"))))
                     (.yld (.sh (.boo (some true))))))) :=
  .comma (.numLit 0 1)
    (.comma (.strLit 0 "a") (.boolLit 0 true))

/-! ## §4. Binary operators

    `1 + 1` — overload selection via `Overload.add_num`. The result type
    is `num` (the "concrete value forgotten" form, since we don't do
    constant folding in the rules). -/

-- jq: 1 + 1
example : HasType (.binop .add (.numLit 1) (.numLit 1))
    (.arr (.tvar 0) (.yld (.sh (.num none)))) :=
  .binop
    (.numLit 0 1)
    (.numLit 0 1)
    (.add_num
      (.sh .num_some_to_none)
      (.sh .num_some_to_none))

-- jq: "hello" + "world"
example : HasType (.binop .add (.strLit "hello") (.strLit "world"))
    (.arr (.tvar 0) (.yld (.sh (.str none)))) :=
  .binop
    (.strLit 0 "hello")
    (.strLit 0 "world")
    (.add_str
      (.sh .str_some_to_none)
      (.sh .str_some_to_none))

-- jq: 1 == 1   — equality is universal
example : HasType (.binop .eq (.numLit 1) (.numLit 1))
    (.arr (.tvar 0) (.yld (.sh (.boo none)))) :=
  .binop (.numLit 0 1) (.numLit 0 1) .eq_any

-- jq: 1 < 2
example : HasType (.binop .lt (.numLit 1) (.numLit 2))
    (.arr (.tvar 0) (.yld (.sh (.boo none)))) :=
  .binop (.numLit 0 1) (.numLit 0 2) .lt_any

/-! ## §5. Construction — `[ … ]` and `{ … }` -/

-- jq: [1, 2, 3]   — fixed-length tuple at the type level
example : HasType
    (.array [.numLit 1, .numLit 2, .numLit 3])
    (.arr (.tvar 0)
          (.yld (.sh (.tuple
            [.sh (.num (some 1)),
             .sh (.num (some 2)),
             .sh (.num (some 3))])))) :=
  .array
    [.yld (.sh (.num (some 1))),
     .yld (.sh (.num (some 2))),
     .yld (.sh (.num (some 3)))]
    rfl
    (fun i h =>
      match i, h with
      | 0, _ => .numLit 0 1
      | 1, _ => .numLit 0 2
      | 2, _ => .numLit 0 3)

-- jq: {"a": 1, "b": "x"}
example : HasType
    (.object [("a", .numLit 1), ("b", .strLit "x")])
    (.arr (.tvar 0)
          (.yld (.sh (.object
            [("a", .sh (.num (some 1))),
             ("b", .sh (.str (some "x")))])))) :=
  .object
    [.sh (.num (some 1)), .sh (.str (some "x"))]
    rfl
    (fun i h =>
      match i, h with
      | 0, _ => .numLit 0 1
      | 1, _ => .strLit 0 "x")

/-! ## §6. If-then-else (imprecise)

    For an unrecognised guard like `.boolLit true`, `refine` returns `⊤`
    and we fall into the imprecise rule. The result type unions both
    branches' streams with `⊕`.

    The `boolLit` constructor produces a literal-valued bool; we sub it
    down to `boo none` so the if-rule's "guard yields a bool" premise
    is matched. -/

-- jq: if true then 1 else 2 end
example : HasType
    (.ifThenElse (.boolLit true) (.numLit 1) (.numLit 2))
    (.arr (.tvar 0)
          (.choice (.yld (.sh (.num (some 1))))
                   (.yld (.sh (.num (some 2)))))) := by
  apply HasType.ifImprecise
  · -- guard types as bool (after subsumption from `boo (some true)`)
    apply HasType.sub (.boolLit 0 true)
    apply TyLE.arr TyLE.refl
    exact .yld (TyLE.sh .boo_some_to_none)
  · -- refine = ⊤ (default case in refine since boolLit is unrecognised)
    rfl
  · exact .numLit 0 1
  · exact .numLit 0 2

/-! ## §7. Field and array access -/

-- jq: .foo
example : HasType (.objIndex "foo")
    (.arr (.union (.sh .nul)
                  (.sh (.object [("foo", .tvar 0)])))
          (.yld (.union (.tvar 0) (.sh .nul)))) :=
  .objIndex 0 "foo"

-- jq: .[3]
example : HasType (.arrIndex 3)
    (.arr (.union (.sh .nul)
                  (.sh (.array (.tvar 0) none)))
          (.yld (.union (.tvar 0) (.sh .nul)))) :=
  .arrIndex 0 3

-- jq: .[]
example : HasType .iter
    (.arr (.sh (.array (.tvar 0) none))
          (.star (.yld (.tvar 0)))) :=
  .iterArray 0

/-! ## §8. The empty / error filters

    Both produce `ε` (the empty stream). Distinguishing them needs
    error/abnormal-exit machinery deferred to v2. -/

-- jq: empty
example : HasType .empty (.arr (.tvar 0) .eps) :=
  .empty 0

-- jq: error
example : HasType .error (.arr (.tvar 0) .eps) :=
  .error 0

/-! ## §9. Composition: combinations of the above -/

-- jq: [1 + 1, 2 + 2]   — array of binop results
example : HasType
    (.array [.binop .add (.numLit 1) (.numLit 1),
             .binop .add (.numLit 2) (.numLit 2)])
    (.arr (.tvar 0)
          (.yld (.sh (.tuple
            [.sh (.num none),
             .sh (.num none)])))) :=
  .array
    [.yld (.sh (.num none)), .yld (.sh (.num none))]
    rfl
    (fun i h =>
      match i, h with
      | 0, _ =>
          .binop (.numLit 0 1) (.numLit 0 1)
            (.add_num (.sh .num_some_to_none) (.sh .num_some_to_none))
      | 1, _ =>
          .binop (.numLit 0 2) (.numLit 0 2)
            (.add_num (.sh .num_some_to_none) (.sh .num_some_to_none)))

-- jq: { "sum": 1 + 1 }
example : HasType
    (.object [("sum", .binop .add (.numLit 1) (.numLit 1))])
    (.arr (.tvar 0)
          (.yld (.sh (.object [("sum", .sh (.num none))])))) :=
  .object
    [.sh (.num none)]
    rfl
    (fun i h =>
      match i, h with
      | 0, _ =>
          .binop (.numLit 0 1) (.numLit 0 1)
            (.add_num (.sh .num_some_to_none) (.sh .num_some_to_none)))

/-! ## §10. Pipes (with HM instantiation)

    With the `HasType.inst` rule in place, we can type pipes whose
    intermediate stream's element type needs to be specialised.

    Strategy for `.[] | .x`:

    1. Type `.iter` with `α := 0`: `Array (TVar 0) ? → (yld (TVar 0))*`.
    2. Pick `U = (Null ∪ Object{x: TVar 1})` and instantiate TVar 0 to U.
       The iter now types as `Array U ? → (yld U)*`.
    3. Type `.objIndex "x"` with `β := 1`: `(Null ∪ Object{x: TVar 1})
       → ⟨TVar 1 ∪ Null⟩`.
    4. Apply pipe: `elemType (yld U)* = U`, and the side condition
       `U ⊑ U` is `TyLE.refl`. -/

-- jq: .[] | .x
example : HasType
    (.pipe .iter (.objIndex "x"))
    (.arr (.sh (.array
                  (.union (.sh .nul)
                          (.sh (.object [("x", .tvar 1)])))
                  none))
          (flatmap
            (.star (.yld
              (.union (.sh .nul)
                      (.sh (.object [("x", .tvar 1)])))))
            (fun _ => .yld (.union (.tvar 1) (.sh .nul))))) :=
  -- The iter, instantiated at TVar 0 := (Null ∪ Object{x: TVar 1}).
  let U : Ty := .union (.sh .nul) (.sh (.object [("x", .tvar 1)]))
  let h_iter_specialised :
      HasType .iter
        (.arr (.sh (.array U none)) (.star (.yld U))) :=
    HasType.inst 0 U (HasType.iterArray 0)
  let h_obj : HasType (.objIndex "x")
      (.arr U (.yld (.union (.tvar 1) (.sh .nul)))) :=
    HasType.objIndex 1 "x"
  -- elemType (yld U)* = U; side condition is U ⊑ U by refl.
  HasType.pipe h_iter_specialised h_obj TyLE.refl

/-! ## §11. Following the pattern: `.[] | .[0]` -/

-- jq: .[] | .[0]   — array of arrays, take the first of each
example : HasType
    (.pipe .iter (.arrIndex 0))
    (.arr (.sh (.array
                  (.union (.sh .nul)
                          (.sh (.array (.tvar 1) none)))
                  none))
          (flatmap
            (.star (.yld
              (.union (.sh .nul)
                      (.sh (.array (.tvar 1) none)))))
            (fun _ => .yld (.union (.tvar 1) (.sh .nul))))) :=
  let U : Ty := .union (.sh .nul) (.sh (.array (.tvar 1) none))
  HasType.pipe
    (HasType.inst 0 U (HasType.iterArray 0))
    (HasType.arrIndex 1 0)
    TyLE.refl

/-! ## §12. Polymorphic binop: `1 + .`

    Both operands share an input type via the binop rule. We instantiate
    `dot`'s tvar to `num` so that the `add_num` overload fires. -/

-- jq: 1 + .
example : HasType
    (.binop .add (.numLit 1) .dot)
    (.arr (.sh (.num none)) (.yld (.sh (.num none)))) :=
  let U : Ty := .sh (.num none)
  HasType.binop
    (HasType.inst 0 U (HasType.numLit 0 1))
    (HasType.inst 0 U (HasType.dot 0))
    (.add_num (.sh .num_some_to_none) TyLE.refl)

/-! ## §13. Chained pipes: `.[] | .x | .y`

    Two levels of instantiation. The first specialises the iter's element
    type to the field-access input; the second specialises the inner field
    type to the next field-access input. The side condition for the outer
    pipe needs `union (Null ∪ …) Null ⊑ Null ∪ …` — a small union-elim
    derivation. -/

-- jq: .[] | .x | .y
example :
    let V : Ty := .union (.sh .nul) (.sh (.object [("y", .tvar 2)]))
    let U : Ty := .union (.sh .nul) (.sh (.object [("x", V)]))
    HasType
      (.pipe (.pipe .iter (.objIndex "x")) (.objIndex "y"))
      (.arr (.sh (.array U none))
            (flatmap
              (.star (.yld (.union V (.sh .nul))))
              (fun _ => .yld (.union (.tvar 2) (.sh .nul))))) :=
  let V : Ty := .union (.sh .nul) (.sh (.object [("y", .tvar 2)]))
  let U_x : Ty := .union (.sh .nul) (.sh (.object [("x", .tvar 1)]))
  -- Step A: type `.[] | .x` after instantiating tvar 0 := U_x.
  let h_pipe_x :
      HasType (.pipe .iter (.objIndex "x"))
        (.arr (.sh (.array U_x none))
              (.star (.yld (.union (.tvar 1) (.sh .nul))))) :=
    HasType.pipe
      (HasType.inst 0 U_x (HasType.iterArray 0))
      (HasType.objIndex 1 "x")
      TyLE.refl
  -- Step B: instantiate tvar 1 := V in h_pipe_x's type.
  -- After substTy 1 V, the U_x becomes U (with V threaded in), and
  -- the codomain stream's element changes accordingly.
  let h_pipe_x_inst :
      HasType (.pipe .iter (.objIndex "x"))
        (.arr (.sh (.array (.union (.sh .nul)
                                   (.sh (.object [("x", V)]))) none))
              (.star (.yld (.union V (.sh .nul))))) :=
    HasType.inst 1 V h_pipe_x
  -- Step C: type `.y` at input V.
  let h_obj_y :
      HasType (.objIndex "y")
        (.arr V (.yld (.union (.tvar 2) (.sh .nul)))) :=
    HasType.objIndex 2 "y"
  -- Step D: outer pipe. Side condition: elemType (star (yld (union V nul))) = union V nul ⊑ V.
  -- By union_elim: V ⊑ V (refl) and nul ⊑ V (union_intro_l refl since V = union nul ...).
  let h_side : (.union V (.sh .nul)) ⊑ V :=
    TyLE.union_elim TyLE.refl TyLE.union_intro_l
  HasType.pipe h_pipe_x_inst h_obj_y h_side

/-! ## §14. Comma after iter: `.[] | .x , .y`

    In jq's precedence, `,` binds tighter than `|`, so this parses as
    `.[] | (.x, .y)`. The inner comma demands its two arms accept the same
    input — we widen each `.k` from `Null ∪ Object{k:_}` to a single
    `Object{x:_, y:_}` via `HasType.sub` + an open-record subtyping step.

    This finally exercises every shape-level subtyping rule we proved. -/

-- jq: .[] | (.x, .y)
example :
    let A : Ty := .sh (.object [("x", .tvar 1), ("y", .tvar 2)])
    HasType
      (.pipe .iter (.comma (.objIndex "x") (.objIndex "y")))
      (.arr (.sh (.array A none))
            (flatmap
              (.star (.yld A))
              (fun _ =>
                .concat (.yld (.union (.tvar 1) (.sh .nul)))
                        (.yld (.union (.tvar 2) (.sh .nul)))))) :=
  let A : Ty := .sh (.object [("x", .tvar 1), ("y", .tvar 2)])
  -- Widen .x to accept input A.
  let h_x : HasType (.objIndex "x") (.arr A (.yld (.union (.tvar 1) (.sh .nul)))) :=
    HasType.sub (HasType.objIndex 1 "x")
      (TyLE.arr
        -- A ⊑ Null ∪ Object{x: tvar 1}
        (TyLE.trans
          (t₂ := .sh (.object [("x", .tvar 1)]))
          (TyLE.sh (ShapeLE.object_cons (List.Mem.head _) TyLE.refl ShapeLE.object_nil))
          TyLE.union_intro_r)
        StreamLE.refl)
  -- Widen .y similarly.
  let h_y : HasType (.objIndex "y") (.arr A (.yld (.union (.tvar 2) (.sh .nul)))) :=
    HasType.sub (HasType.objIndex 2 "y")
      (TyLE.arr
        (TyLE.trans
          (t₂ := .sh (.object [("y", .tvar 2)]))
          (TyLE.sh
            (ShapeLE.object_cons (List.Mem.tail _ (List.Mem.head _))
                                 TyLE.refl ShapeLE.object_nil))
          TyLE.union_intro_r)
        StreamLE.refl)
  -- Comma joins them.
  let h_comma :
      HasType (.comma (.objIndex "x") (.objIndex "y"))
        (.arr A (.concat (.yld (.union (.tvar 1) (.sh .nul)))
                         (.yld (.union (.tvar 2) (.sh .nul))))) :=
    HasType.comma h_x h_y
  -- Pipe iter into comma; instantiate iter's tvar 0 := A.
  HasType.pipe (HasType.inst 0 A (HasType.iterArray 0)) h_comma TyLE.refl

/-! ## §15. The README headline: `.[] | .age, .name | {v: .a}`

    The example from the project README. In jq's precedence this parses as
    `.[] | ((.age, .name) | {v: .a})`. The intended semantics: for each
    element of the array, emit two values (its age and its name), then
    construct `{v: <emitted>.a}` from each.

    We type this end-to-end. The inferred input shape is

      Array(Object{age: A, name: A}, ?)        where A = Null ∪ Object{a: τ}

    which encodes "every age and every name field must itself be an object
    with an `a` field" — exactly the constraint that catches the README's
    motivating bug. -/

-- jq: .[] | .age, .name | {v: .a}
-- Parses (jq precedence) as `.[] | ((.age, .name) | {v: .a})`.
example :
    let A_obj : Ty := .union (.sh .nul) (.sh (.object [("a", .tvar 5)]))
    let A_in  : Ty := .sh (.object [("age", A_obj), ("name", A_obj)])
    let T_a   : Ty := .union (.tvar 5) (.sh .nul)
    let T_arm : Ty := .union A_obj (.sh .nul)
    let T_v   : Ty := .sh (.object [("v", T_a)])
    HasType
      (.pipe .iter
        (.pipe (.comma (.objIndex "age") (.objIndex "name"))
               (.object [("v", .objIndex "a")])))
      (.arr (.sh (.array A_in none))
            (flatmap (.star (.yld A_in))
              (fun _ =>
                flatmap (.concat (.yld T_arm) (.yld T_arm))
                  (fun _ => .yld T_v)))) :=
  let A_obj : Ty := .union (.sh .nul) (.sh (.object [("a", .tvar 5)]))
  let A_in  : Ty := .sh (.object [("age", A_obj), ("name", A_obj)])
  let T_a   : Ty := .union (.tvar 5) (.sh .nul)
  let T_arm : Ty := .union A_obj (.sh .nul)
  let T_v   : Ty := .sh (.object [("v", T_a)])
  -- Step 1: type each `.k` at input `A_in` and yield `union A_obj nul`.
  --
  -- Build by: instantiate the field tvar to A_obj (so `.k`'s codomain
  -- becomes `yld (union A_obj nul)`), then subsume to widen the input
  -- from `Null ∪ Object{k: A_obj}` up to `A_in = Object{age: A_obj,
  -- name: A_obj}`.
  let h_age : HasType (.objIndex "age") (.arr A_in (.yld T_arm)) :=
    HasType.sub (HasType.inst 1 A_obj (HasType.objIndex 1 "age"))
      (TyLE.arr
        (TyLE.trans
          (t₂ := .sh (.object [("age", A_obj)]))
          (TyLE.sh (ShapeLE.object_cons (List.Mem.head _)
                                        TyLE.refl ShapeLE.object_nil))
          TyLE.union_intro_r)
        StreamLE.refl)
  let h_name : HasType (.objIndex "name") (.arr A_in (.yld T_arm)) :=
    HasType.sub (HasType.inst 2 A_obj (HasType.objIndex 2 "name"))
      (TyLE.arr
        (TyLE.trans
          (t₂ := .sh (.object [("name", A_obj)]))
          (TyLE.sh
            (ShapeLE.object_cons (List.Mem.tail _ (List.Mem.head _))
                                 TyLE.refl ShapeLE.object_nil))
          TyLE.union_intro_r)
        StreamLE.refl)
  -- Step 2: comma joins them at A_in.
  let h_comma :
      HasType (.comma (.objIndex "age") (.objIndex "name"))
        (.arr A_in (.concat (.yld T_arm) (.yld T_arm))) :=
    HasType.comma h_age h_name
  -- Step 3: type `{v: .a}` at input A_obj.
  let h_obj :
      HasType (.object [("v", .objIndex "a")])
        (.arr A_obj (.yld T_v)) :=
    HasType.object [T_a] rfl
      (fun i h =>
        match i, h with
        | 0, _ => HasType.objIndex 5 "a")
  -- Step 4: inner pipe — comma feeds object. Side condition reduces to
  -- `union T_arm T_arm ⊑ A_obj` which by union_elim halves to
  -- `T_arm ⊑ A_obj`, then to `A_obj ⊑ A_obj` (refl) and `nul ⊑ A_obj`
  -- (union_intro_l refl, since A_obj = union nul (...)).
  let h_branch : T_arm ⊑ A_obj :=
    TyLE.union_elim TyLE.refl TyLE.union_intro_l
  let h_side : (.union T_arm T_arm) ⊑ A_obj :=
    TyLE.union_elim h_branch h_branch
  let h_inner_pipe :=
    HasType.pipe h_comma h_obj h_side
  -- Step 5: outer pipe — iter (instantiated at A_in) feeds the inner pipe.
  HasType.pipe (HasType.inst 0 A_in (HasType.iterArray 0))
               h_inner_pipe TyLE.refl

/-! ## §16. If-then-else (precise)

    When the guard has the form `. == lit` (or its allies), `refine`
    returns a non-⊤ refinement. The if-precise rule then yields an
    *intersection of arrows*: one arrow per branch, each with the
    refined input type.

    For `if . == 5 then 1 else error end`:
    * `refine (. == 5) true  = num[5]`
    * `refine (. == 5) false = ¬num[5]`
    * Result type: `(num[5] → ⟨1⟩) ∩ (¬num[5] → ε)` —
      "if input is 5, return 1; otherwise no output (error)". -/

-- jq: if . == 5 then 1 else error end
example : HasType
    (.ifThenElse
      (.binop .eq .dot (.numLit 5))
      (.numLit 1)
      .error)
    (.inter
      (.arr (.sh (.num (some 5))) (.yld (.sh (.num (some 1)))))
      (.arr (.neg_sh (.num (some 5))) .eps)) :=
  HasType.ifPrecise
    -- guard types as bool
    (HasType.binop (HasType.dot 0) (HasType.numLit 0 5) .eq_any)
    -- refine c true ≠ ⊤  (refine reduces to sh(num 5), not sh top)
    (by intro h; injection h with h; injection h)
    -- then branch: 1 at refined input (inter (tvar 0) (sh (num 5)))
    (HasType.inst 0 (.inter (.tvar 0) (.sh (.num (some 5))))
      (HasType.numLit 0 1))
    -- else branch: error at refined input (inter (tvar 0) (neg (sh (num 5))))
    (HasType.inst 0 (.inter (.tvar 0) (.neg_sh (.num (some 5))))
      (HasType.error 0))

/-! ## §17. If-then-else (precise) with `or`

    `if . == 1 or . == 2 then "small" else "other" end`

    `refine (p or q) true = refine(p,true) ∪ refine(q,true)`, so the
    refinement is a union: `num[1] ∪ num[2]`. -/

-- jq: if . == 1 or . == 2 then "small" else "other" end
example : HasType
    (.ifThenElse
      (.binop .or
        (.binop .eq .dot (.numLit 1))
        (.binop .eq .dot (.numLit 2)))
      (.strLit "small")
      (.strLit "other"))
    (.inter
      (.arr (.union (.sh (.num (some 1))) (.sh (.num (some 2))))
            (.yld (.sh (.str (some "small")))))
      (.arr (.inter (.neg_sh (.num (some 1)))
                    (.neg_sh (.num (some 2))))
            (.yld (.sh (.str (some "other")))))) :=
  -- The OR-of-eq guard types as bool. Each . == numLit operand types
  -- via .eq_any (universal). The .or overload requires both sides bool.
  let h_eq1 : HasType (.binop .eq .dot (.numLit 1))
      (.arr (.tvar 0) (.yld (.sh (.boo none)))) :=
    HasType.binop (HasType.dot 0) (HasType.numLit 0 1) .eq_any
  let h_eq2 : HasType (.binop .eq .dot (.numLit 2))
      (.arr (.tvar 0) (.yld (.sh (.boo none)))) :=
    HasType.binop (HasType.dot 0) (HasType.numLit 0 2) .eq_any
  HasType.ifPrecise
    (HasType.binop h_eq1 h_eq2 (.or_bool TyLE.refl TyLE.refl))
    -- refine result is a union: not equal to sh top
    (by intro h; injection h)
    (HasType.inst 0
      (.inter (.tvar 0) (.union (.sh (.num (some 1))) (.sh (.num (some 2)))))
      (HasType.strLit 0 "small"))
    (HasType.inst 0
      (.inter (.tvar 0)
        (.inter (.neg_sh (.num (some 1))) (.neg_sh (.num (some 2)))))
      (HasType.strLit 0 "other"))

/-! ## §18. Filter calls and *deriving* their schemes

    The `HasType.call` rule lets a derivation assume a scheme for a named
    filter. The interesting question is: what scheme would we *derive*
    from the body? `isboolean`'s body in `defs.jq` is
    `. == true or . == false`. The precise scheme it admits is

        (bool → ⟨true⟩) ∩ (¬bool → ⟨false⟩).

    Here we walk the derivation. The refined `eq_bool_eq` /
    `eq_bool_neq` / `or_lit_*` / `eq_disjoint_*` overloads let each
    sub-expression reduce to a literal-valued bool. `HasType.inter_intro`
    combines per-input-case derivations into the intersection scheme.

    The full `(bool → ⟨true⟩) ∩ (¬bool → ⟨false⟩)` requires arrow-
    intersection-distribution `(A → S) ∩ (B → S) ⊑ (A∪B) → S` and a
    finite enumeration of all "non-bool" base shapes — so we present
    a *refined-but-finite* version below: at the four concrete inputs
    `bool[true]`, `bool[false]`, `num`, `str`, `nul`. Adding more
    cross-type overloads + the distribution rule extends this. -/

-- The body of isboolean.
private def isboolean_body : Filter :=
  .binop .or
    (.binop .eq .dot (.boolLit true))
    (.binop .eq .dot (.boolLit false))

-- Step 1: derive `bool[true] → ⟨true⟩` for the body.
private def h_isbool_at_true :
    HasType isboolean_body
      (.arr (.sh (.boo (some true))) (.yld (.sh (.boo (some true))))) :=
  let A : Ty := .sh (.boo (some true))
  let h_dot : HasType .dot (.arr A (.yld A)) :=
    HasType.inst 0 A (HasType.dot 0)
  let h_true : HasType (.boolLit true)
      (.arr A (.yld (.sh (.boo (some true))))) :=
    HasType.inst 0 A (HasType.boolLit 0 true)
  let h_false : HasType (.boolLit false)
      (.arr A (.yld (.sh (.boo (some false))))) :=
    HasType.inst 0 A (HasType.boolLit 0 false)
  let h_eq_true : HasType (.binop .eq .dot (.boolLit true))
      (.arr A (.yld (.sh (.boo (some true))))) :=
    HasType.binop h_dot h_true (.eq_bool_eq true)
  let h_eq_false : HasType (.binop .eq .dot (.boolLit false))
      (.arr A (.yld (.sh (.boo (some false))))) :=
    HasType.binop h_dot h_false (.eq_bool_neq true false (by decide))
  HasType.binop h_eq_true h_eq_false
    (.or_lit_true_l (.sh (.boo (some false))) (.sh .boo_some_to_none))

-- Step 2: derive `bool[false] → ⟨true⟩`.
private def h_isbool_at_false :
    HasType isboolean_body
      (.arr (.sh (.boo (some false))) (.yld (.sh (.boo (some true))))) :=
  let A : Ty := .sh (.boo (some false))
  let h_dot : HasType .dot (.arr A (.yld A)) :=
    HasType.inst 0 A (HasType.dot 0)
  let h_true : HasType (.boolLit true)
      (.arr A (.yld (.sh (.boo (some true))))) :=
    HasType.inst 0 A (HasType.boolLit 0 true)
  let h_false : HasType (.boolLit false)
      (.arr A (.yld (.sh (.boo (some false))))) :=
    HasType.inst 0 A (HasType.boolLit 0 false)
  -- (false == true) = false  via eq_bool_neq false true.
  let h_eq_true : HasType (.binop .eq .dot (.boolLit true))
      (.arr A (.yld (.sh (.boo (some false))))) :=
    HasType.binop h_dot h_true (.eq_bool_neq false true (by decide))
  -- (false == false) = true  via eq_bool_eq false.
  let h_eq_false : HasType (.binop .eq .dot (.boolLit false))
      (.arr A (.yld (.sh (.boo (some true))))) :=
    HasType.binop h_dot h_false (.eq_bool_eq false)
  -- false || true = true  via or_lit_true_r.
  HasType.binop h_eq_true h_eq_false
    (.or_lit_true_r (.sh (.boo (some false))) (.sh .boo_some_to_none))

-- Step 3: derive `num → ⟨false⟩`.
private def h_isbool_at_num :
    HasType isboolean_body
      (.arr (.sh (.num none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.num none)
  let h_dot : HasType .dot (.arr A (.yld A)) :=
    HasType.inst 0 A (HasType.dot 0)
  let h_true : HasType (.boolLit true)
      (.arr A (.yld (.sh (.boo (some true))))) :=
    HasType.inst 0 A (HasType.boolLit 0 true)
  let h_false : HasType (.boolLit false)
      (.arr A (.yld (.sh (.boo (some false))))) :=
    HasType.inst 0 A (HasType.boolLit 0 false)
  -- (num == bool[true]) = false  via eq_disjoint_num_bool.
  let h_eq_true : HasType (.binop .eq .dot (.boolLit true))
      (.arr A (.yld (.sh (.boo (some false))))) :=
    HasType.binop h_dot h_true (.eq_disjoint_num_bool (some true) none)
  let h_eq_false : HasType (.binop .eq .dot (.boolLit false))
      (.arr A (.yld (.sh (.boo (some false))))) :=
    HasType.binop h_dot h_false (.eq_disjoint_num_bool (some false) none)
  -- false || false = false  via or_false_false.
  HasType.binop h_eq_true h_eq_false .or_false_false

-- Step 4: combine all three with `inter_intro` to get the partial
-- intersection scheme — `isboolean` types as the four-way intersection
-- spanning `bool[true]`, `bool[false]`, and `num` inputs.
example :
    HasType isboolean_body
      (.inter
        (.inter
          (.arr (.sh (.boo (some true))) (.yld (.sh (.boo (some true)))))
          (.arr (.sh (.boo (some false))) (.yld (.sh (.boo (some true))))))
        (.arr (.sh (.num none)) (.yld (.sh (.boo (some false)))))) :=
  HasType.inter_intro
    (HasType.inter_intro h_isbool_at_true h_isbool_at_false)
    h_isbool_at_num

/-! Step 5: collapse the `bool[true] / bool[false]` cases into a single
    arrow over `bool`, using two new subtyping rules: `arrow_inter_dom`
    (distribute ∩ over →'s domain) and `bool_split` (a `bool` is one of
    its two singletons).

    With these, the partial intersection collapses to
    `(bool → ⟨true⟩) ∩ (num → ⟨false⟩)` — a *single* arrow on the bool
    side (no longer two cases) and the num case still standalone. -/

example :
    HasType isboolean_body
      (.inter
        (.arr (.sh (.boo none)) (.yld (.sh (.boo (some true)))))
        (.arr (.sh (.num none)) (.yld (.sh (.boo (some false)))))) :=
  -- Step A: combine the two bool cases into an intersection.
  let h_bool_inter :
      HasType isboolean_body
        (.inter
          (.arr (.sh (.boo (some true))) (.yld (.sh (.boo (some true)))))
          (.arr (.sh (.boo (some false))) (.yld (.sh (.boo (some true)))))) :=
    HasType.inter_intro h_isbool_at_true h_isbool_at_false
  -- Step B: distribute over the union: ⊑ (bool[true] ∪ bool[false]) → ⟨true⟩.
  let h_bool_union :
      HasType isboolean_body
        (.arr
          (.union (.sh (.boo (some true))) (.sh (.boo (some false))))
          (.yld (.sh (.boo (some true))))) :=
    HasType.sub h_bool_inter TyLE.arrow_inter_dom
  -- Step C: subsume input from `bool none` to the union via `bool_split`.
  -- Arrow contravariance: need `bool none ⊑ bool[true] ∪ bool[false]`.
  let h_bool :
      HasType isboolean_body
        (.arr (.sh (.boo none)) (.yld (.sh (.boo (some true))))) :=
    HasType.sub h_bool_union (TyLE.arr TyLE.bool_split StreamLE.refl)
  -- Step D: combine the collapsed bool arrow with the standalone num arrow.
  HasType.inter_intro h_bool h_isbool_at_num

/-! ## §18b. The full precise scheme: `(bool → ⟨true⟩) ∩ (¬bool → ⟨false⟩)`

    Generalising §18 step 5 to all five non-bool base shapes and using
    `neg_bool_decomp` to collapse the union to `¬bool`. Each non-bool
    sub-derivation has the same structure: `dot` instantiated at the
    specific shape, both `==`s reduce to `false` via the appropriate
    cross-type `eq_disjoint_*` overload, then `or_false_false` combines.

    Result: `def isboolean: . == true or . == false` admits the
    intersection-of-arrows scheme

        (bool → ⟨true⟩) ∩ (¬bool → ⟨false⟩)

    *derived from the body*, not asserted. -/

private def h_isbool_at_str :
    HasType isboolean_body
      (.arr (.sh (.str none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.str none)
  let h_dot := HasType.inst 0 A (HasType.dot 0)
  let h_true := HasType.inst 0 A (HasType.boolLit 0 true)
  let h_false := HasType.inst 0 A (HasType.boolLit 0 false)
  HasType.binop
    (HasType.binop h_dot h_true (.eq_disjoint_str_bool (some true) none))
    (HasType.binop h_dot h_false (.eq_disjoint_str_bool (some false) none))
    .or_false_false

private def h_isbool_at_nul :
    HasType isboolean_body
      (.arr (.sh .nul) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh .nul
  let h_dot := HasType.inst 0 A (HasType.dot 0)
  let h_true := HasType.inst 0 A (HasType.boolLit 0 true)
  let h_false := HasType.inst 0 A (HasType.boolLit 0 false)
  HasType.binop
    (HasType.binop h_dot h_true (.eq_disjoint_nul_bool (some true)))
    (HasType.binop h_dot h_false (.eq_disjoint_nul_bool (some false)))
    .or_false_false

private def h_isbool_at_array :
    HasType isboolean_body
      (.arr (.sh (.array (.sh .top) none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.array (.sh .top) none)
  let h_dot := HasType.inst 0 A (HasType.dot 0)
  let h_true := HasType.inst 0 A (HasType.boolLit 0 true)
  let h_false := HasType.inst 0 A (HasType.boolLit 0 false)
  HasType.binop
    (HasType.binop h_dot h_true (.eq_disjoint_array_bool (some true) (.sh .top) none))
    (HasType.binop h_dot h_false (.eq_disjoint_array_bool (some false) (.sh .top) none))
    .or_false_false

private def h_isbool_at_object :
    HasType isboolean_body
      (.arr (.sh (.object [])) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.object [])
  let h_dot := HasType.inst 0 A (HasType.dot 0)
  let h_true := HasType.inst 0 A (HasType.boolLit 0 true)
  let h_false := HasType.inst 0 A (HasType.boolLit 0 false)
  HasType.binop
    (HasType.binop h_dot h_true (.eq_disjoint_object_bool (some true) []))
    (HasType.binop h_dot h_false (.eq_disjoint_object_bool (some false) []))
    .or_false_false

/-- The full precise scheme. Built by:
    1. The collapsed bool half (from §18 step C).
    2. A right-associated 5-way intersection of the non-bool cases.
    3. Repeated `arrow_inter_dom` to fuse the 5 arrows into one over
       the union of all five non-bool base shapes.
    4. `neg_bool_decomp` to subsume the input from that union to `¬bool`. -/
example :
    HasType isboolean_body
      (.inter
        (.arr (.sh (.boo none)) (.yld (.sh (.boo (some true)))))
        (.arr (.neg_sh (.boo none)) (.yld (.sh (.boo (some false)))))) :=
  -- Recompute the bool half (same as §18 step C).
  let h_bool : HasType isboolean_body
      (.arr (.sh (.boo none)) (.yld (.sh (.boo (some true))))) :=
    HasType.sub
      (HasType.sub
        (HasType.inter_intro h_isbool_at_true h_isbool_at_false)
        TyLE.arrow_inter_dom)
      (TyLE.arr TyLE.bool_split StreamLE.refl)
  -- Build the right-associated 5-way intersection (innermost first), and
  -- apply `arrow_inter_dom` at each layer to collapse to a single arrow
  -- whose domain is the right-associated union of the five base shapes.
  let h_45 :
      HasType isboolean_body
        (.arr (.union (.sh (.array (.sh .top) none)) (.sh (.object [])))
              (.yld (.sh (.boo (some false))))) :=
    HasType.sub
      (HasType.inter_intro h_isbool_at_array h_isbool_at_object)
      TyLE.arrow_inter_dom
  let h_345 :
      HasType isboolean_body
        (.arr
          (.union (.sh (.str none))
            (.union (.sh (.array (.sh .top) none)) (.sh (.object []))))
          (.yld (.sh (.boo (some false))))) :=
    HasType.sub
      (HasType.inter_intro h_isbool_at_str h_45)
      TyLE.arrow_inter_dom
  let h_2345 :
      HasType isboolean_body
        (.arr
          (.union (.sh (.num none))
            (.union (.sh (.str none))
              (.union (.sh (.array (.sh .top) none)) (.sh (.object [])))))
          (.yld (.sh (.boo (some false))))) :=
    HasType.sub
      (HasType.inter_intro h_isbool_at_num h_345)
      TyLE.arrow_inter_dom
  let h_5way :
      HasType isboolean_body
        (.arr
          (.union (.sh .nul)
            (.union (.sh (.num none))
              (.union (.sh (.str none))
                (.union (.sh (.array (.sh .top) none)) (.sh (.object []))))))
          (.yld (.sh (.boo (some false))))) :=
    HasType.sub
      (HasType.inter_intro h_isbool_at_nul h_2345)
      TyLE.arrow_inter_dom
  -- Subsume input: `neg bool` is contained in the 5-way union.
  let h_neg_bool : HasType isboolean_body
      (.arr (.neg_sh (.boo none)) (.yld (.sh (.boo (some false))))) :=
    HasType.sub h_5way (TyLE.arr TyLE.neg_kind_top_boo StreamLE.refl)
  -- Combine the two halves.
  HasType.inter_intro h_bool h_neg_bool

/-! ## §19. The `def type` body

    From `defs.jq`:

        def type:
            if . == null then "null"
          elif isboolean then "boolean"
          elif . < "" then "number"
          elif . < [] then "string"
          elif . < {} then "array"
          else             "object" end;

    A 6-way nested if-elif-else returning a string. We type the *body*
    (the `if` chain itself); this is what the `def type` body reduces
    to once the function header is stripped.

    All guards except `. == null` are imprecise to our `refine` (which
    only recognises equality with literals and boolean connectives over
    those). The `. == null` clause is precise; the rest fall into
    `ifImprecise` and yield stream-choices.

    For brevity we type only the outer two branches (`null` and the
    rest); the elif chain underneath would type analogously. The full
    derivation is the same shape repeated four more times. -/

-- jq: if . == null then "null" else <rest> end
-- We model "<rest>" as a placeholder string ("non-null") for this example.
example : HasType
    (.ifThenElse
      (.binop .eq .dot .nullLit)
      (.strLit "null")
      (.strLit "non-null"))
    (.inter
      (.arr (.sh .nul) (.yld (.sh (.str (some "null")))))
      (.arr (.neg_sh .nul) (.yld (.sh (.str (some "non-null")))))) :=
  HasType.ifPrecise
    (HasType.binop (HasType.dot 0) (HasType.nullLit 0) .eq_any)
    (by intro h; injection h with h; injection h)
    (HasType.inst 0 (.inter (.tvar 0) (.sh .nul))
      (HasType.strLit 0 "null"))
    (HasType.inst 0 (.inter (.tvar 0) (.neg_sh .nul))
      (HasType.strLit 0 "non-null"))

/-! Two-level `def type` snippet: distinguishes null, boolean, and "rest".
    The outer `if` is precise (null check); the inner `elif` uses a
    `call "isboolean"` and is imprecise (`refine` doesn't see through
    the call). -/

example : HasType
    (.ifThenElse
      (.binop .eq .dot .nullLit)
      (.strLit "null")
      (.ifThenElse
        (.call "isboolean")
        (.strLit "boolean")
        (.strLit "other")))
    (.inter
      (.arr (.sh .nul) (.yld (.sh (.str (some "null")))))
      (.arr (.neg_sh .nul)
            (.choice (.yld (.sh (.str (some "boolean"))))
                     (.yld (.sh (.str (some "other"))))))) :=
  -- Inner if: isboolean is imprecise (refine of (call "isboolean") = ⊤),
  -- so we use ifImprecise.
  let h_isbool : HasType (.call "isboolean")
      (.arr (.neg_sh .nul) (.yld (.sh (.boo none)))) :=
    HasType.call "isboolean" _
  let h_inner :
      HasType (.ifThenElse (.call "isboolean")
                           (.strLit "boolean") (.strLit "other"))
        (.arr (.neg_sh .nul)
              (.choice (.yld (.sh (.str (some "boolean"))))
                       (.yld (.sh (.str (some "other")))))) :=
    HasType.ifImprecise
      h_isbool
      rfl  -- refine (call _) true = ⊤ (default case)
      (HasType.inst 0 (.neg_sh .nul) (HasType.strLit 0 "boolean"))
      (HasType.inst 0 (.neg_sh .nul) (HasType.strLit 0 "other"))
  HasType.ifPrecise
    (HasType.binop (HasType.dot 0) (HasType.nullLit 0) .eq_any)
    (by intro h; injection h with h; injection h)
    (HasType.inst 0 (.inter (.tvar 0) (.sh .nul))
      (HasType.strLit 0 "null"))
    -- For the else branch, re-input is (inter (tvar 0) (neg sh nul));
    -- the inner if expects input (neg sh nul). Subsume.
    (HasType.sub h_inner
      (TyLE.arr (TyLE.inter_intro_r TyLE.refl) StreamLE.refl))

/-! ## §19c. The other type predicates derived

    Same blueprint as `isboolean`: type the body at each base-kind input,
    combine via `inter_intro`, distribute via `arrow_inter_dom`, subsume
    via `neg_kind_top_*`.

    Two simplifications relative to `isboolean`:

    1. *No singleton split is needed* for kinds other than bool. The
       body is uniform across the kind (e.g., for `isnull`, every
       non-`null` value yields `false` regardless of which one it is),
       so we can derive directly at the kind-top.
    2. The `bool_split` step disappears in §18 / §19a / §19b, replaced
       by direct arrow-distribution from a single bool-kind arrow. -/

/-! ### `isnull: . == null` -/

private def isnull_body : Filter := .binop .eq .dot .nullLit

private def h_isnull_at_nul :
    HasType isnull_body
      (.arr (.sh .nul) (.yld (.sh (.boo (some true))))) :=
  let A : Ty := .sh .nul
  HasType.binop
    (HasType.inst 0 A (HasType.dot 0))
    (HasType.inst 0 A (HasType.nullLit 0))
    .eq_nul_eq

private def h_isnull_at_bool :
    HasType isnull_body
      (.arr (.sh (.boo none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.boo none)
  HasType.binop
    (HasType.inst 0 A (HasType.dot 0))
    (HasType.inst 0 A (HasType.nullLit 0))
    (.eq_disjoint_bool_nul none)

private def h_isnull_at_num :
    HasType isnull_body
      (.arr (.sh (.num none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.num none)
  HasType.binop
    (HasType.inst 0 A (HasType.dot 0))
    (HasType.inst 0 A (HasType.nullLit 0))
    (.eq_disjoint_num_nul none)

private def h_isnull_at_str :
    HasType isnull_body
      (.arr (.sh (.str none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.str none)
  HasType.binop
    (HasType.inst 0 A (HasType.dot 0))
    (HasType.inst 0 A (HasType.nullLit 0))
    (.eq_disjoint_str_nul none)

private def h_isnull_at_array :
    HasType isnull_body
      (.arr (.sh (.array (.sh .top) none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.array (.sh .top) none)
  HasType.binop
    (HasType.inst 0 A (HasType.dot 0))
    (HasType.inst 0 A (HasType.nullLit 0))
    (.eq_disjoint_array_nul (.sh .top) none)

private def h_isnull_at_object :
    HasType isnull_body
      (.arr (.sh (.object [])) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.object [])
  HasType.binop
    (HasType.inst 0 A (HasType.dot 0))
    (HasType.inst 0 A (HasType.nullLit 0))
    (.eq_disjoint_object_nul [])

/-- The full precise scheme for `isnull`. -/
example :
    HasType isnull_body
      (.inter
        (.arr (.sh .nul) (.yld (.sh (.boo (some true)))))
        (.arr (.neg_sh .nul) (.yld (.sh (.boo (some false)))))) :=
  let h_45 : HasType isnull_body
      (.arr (.union (.sh (.array (.sh .top) none)) (.sh (.object [])))
            (.yld (.sh (.boo (some false))))) :=
    HasType.sub (HasType.inter_intro h_isnull_at_array h_isnull_at_object)
                TyLE.arrow_inter_dom
  let h_345 : HasType isnull_body
      (.arr (.union (.sh (.str none))
              (.union (.sh (.array (.sh .top) none)) (.sh (.object []))))
            (.yld (.sh (.boo (some false))))) :=
    HasType.sub (HasType.inter_intro h_isnull_at_str h_45) TyLE.arrow_inter_dom
  let h_2345 : HasType isnull_body
      (.arr (.union (.sh (.num none))
              (.union (.sh (.str none))
                (.union (.sh (.array (.sh .top) none)) (.sh (.object [])))))
            (.yld (.sh (.boo (some false))))) :=
    HasType.sub (HasType.inter_intro h_isnull_at_num h_345) TyLE.arrow_inter_dom
  let h_5way : HasType isnull_body
      (.arr (.union (.sh (.boo none))
              (.union (.sh (.num none))
                (.union (.sh (.str none))
                  (.union (.sh (.array (.sh .top) none)) (.sh (.object []))))))
            (.yld (.sh (.boo (some false))))) :=
    HasType.sub (HasType.inter_intro h_isnull_at_bool h_2345) TyLE.arrow_inter_dom
  let h_neg_nul : HasType isnull_body
      (.arr (.neg_sh .nul) (.yld (.sh (.boo (some false))))) :=
    HasType.sub h_5way (TyLE.arr TyLE.neg_kind_top_nul StreamLE.refl)
  HasType.inter_intro h_isnull_at_nul h_neg_nul

/-! ### `isobject: . >= {}` -/

private def isobject_body : Filter := .binop .ge .dot (.object [])

-- The empty-object literal types as `arr A (yld (sh (object [])))`.
private def empty_obj_lit_at (A : Ty) :
    HasType (.object []) (.arr A (.yld (.sh (.object [])))) :=
  HasType.object [] rfl (fun _ h => by simp at h)

private def h_isobject_at_object :
    HasType isobject_body
      (.arr (.sh (.object [])) (.yld (.sh (.boo (some true))))) :=
  let A : Ty := .sh (.object [])
  HasType.binop
    (HasType.inst 0 A (HasType.dot 0))
    (empty_obj_lit_at A)
    (.ge_object_object_empty [])

-- For non-object inputs, ge with object [] is bool[false] by cross-kind.
private def h_isobject_at_nul :
    HasType isobject_body
      (.arr (.sh .nul) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh .nul
  HasType.binop
    (HasType.inst 0 A (HasType.dot 0))
    (empty_obj_lit_at A)
    (.ge_cross_kind_lower .nul (.object []) (by decide))

private def h_isobject_at_bool :
    HasType isobject_body
      (.arr (.sh (.boo none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.boo none)
  HasType.binop
    (HasType.inst 0 A (HasType.dot 0))
    (empty_obj_lit_at A)
    (.ge_cross_kind_lower (.boo none) (.object []) (by decide))

private def h_isobject_at_num :
    HasType isobject_body
      (.arr (.sh (.num none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.num none)
  HasType.binop
    (HasType.inst 0 A (HasType.dot 0))
    (empty_obj_lit_at A)
    (.ge_cross_kind_lower (.num none) (.object []) (by decide))

private def h_isobject_at_str :
    HasType isobject_body
      (.arr (.sh (.str none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.str none)
  HasType.binop
    (HasType.inst 0 A (HasType.dot 0))
    (empty_obj_lit_at A)
    (.ge_cross_kind_lower (.str none) (.object []) (by decide))

private def h_isobject_at_array :
    HasType isobject_body
      (.arr (.sh (.array (.sh .top) none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.array (.sh .top) none)
  HasType.binop
    (HasType.inst 0 A (HasType.dot 0))
    (empty_obj_lit_at A)
    (.ge_cross_kind_lower (.array (.sh .top) none) (.object []) (by decide))

/-- Precise scheme for `isobject`. -/
example :
    HasType isobject_body
      (.inter
        (.arr (.sh (.object [])) (.yld (.sh (.boo (some true)))))
        (.arr (.neg_sh (.object [])) (.yld (.sh (.boo (some false)))))) :=
  let h_45 :=
    HasType.sub (HasType.inter_intro h_isobject_at_str h_isobject_at_array)
                TyLE.arrow_inter_dom
  let h_345 :=
    HasType.sub (HasType.inter_intro h_isobject_at_num h_45) TyLE.arrow_inter_dom
  let h_2345 :=
    HasType.sub (HasType.inter_intro h_isobject_at_bool h_345) TyLE.arrow_inter_dom
  let h_5way :=
    HasType.sub (HasType.inter_intro h_isobject_at_nul h_2345) TyLE.arrow_inter_dom
  let h_neg :=
    HasType.sub h_5way (TyLE.arr TyLE.neg_kind_top_obj StreamLE.refl)
  HasType.inter_intro h_isobject_at_object h_neg

/-! ### `isarray: . >= [] and . < {}` -/

private def isarray_body : Filter :=
  .binop .and (.binop .ge .dot (.array [])) (.binop .lt .dot (.object []))

-- The empty-array literal `[]` types as `arr A (yld (sh (tuple [])))`.
private def empty_arr_lit_at (A : Ty) :
    HasType (.array []) (.arr A (.yld (.sh (.tuple [])))) := by
  -- Ss.foldr Stream.concat .eps with Ss=[] is .eps; collect .eps = sh (tuple []).
  exact HasType.array [] rfl (fun _ h => by simp at h)

private def h_isarray_at_array :
    HasType isarray_body
      (.arr (.sh (.array (.sh .top) none)) (.yld (.sh (.boo (some true))))) :=
  let A : Ty := .sh (.array (.sh .top) none)
  -- (array >= []) → bool[true]
  let h_ge : HasType (.binop .ge .dot (.array []))
      (.arr A (.yld (.sh (.boo (some true))))) :=
    HasType.binop (HasType.inst 0 A (HasType.dot 0)) (empty_arr_lit_at A)
      (.ge_array_tuple_empty (.sh .top) none)
  -- (array < {}) → bool[true] by cross-kind (array < object).
  let h_lt : HasType (.binop .lt .dot (.object []))
      (.arr A (.yld (.sh (.boo (some true))))) :=
    HasType.binop (HasType.inst 0 A (HasType.dot 0)) (empty_obj_lit_at A)
      (.lt_cross_kind_lower (.array (.sh .top) none) (.object []) (by decide))
  HasType.binop h_ge h_lt .and_true_true

-- For non-array inputs, the AND short-circuits via the `>=`-side `bool[false]`
-- (when input rank < array rank) or via the `<`-side (when input rank > array rank).
private def h_isarray_at_object :
    HasType isarray_body
      (.arr (.sh (.object [])) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.object [])
  -- (object >= []) → bool[true] (object > array, cross-kind reverse).
  let h_ge : HasType (.binop .ge .dot (.array []))
      (.arr A (.yld (.sh (.boo (some true))))) :=
    HasType.binop (HasType.inst 0 A (HasType.dot 0)) (empty_arr_lit_at A)
      (.ge_cross_kind_higher (.object []) (.tuple []) (by decide))
  -- (object < {}) → bool[false] (object kvs < object [] is false; object is not < itself).
  let h_lt : HasType (.binop .lt .dot (.object []))
      (.arr A (.yld (.sh (.boo (some false))))) :=
    HasType.binop (HasType.inst 0 A (HasType.dot 0)) (empty_obj_lit_at A)
      (.lt_object_object_empty [])
  HasType.binop h_ge h_lt (.and_lit_false_r _ (.sh .boo_some_to_none))

private def h_isarray_at_nul :
    HasType isarray_body
      (.arr (.sh .nul) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh .nul
  let h_ge : HasType (.binop .ge .dot (.array []))
      (.arr A (.yld (.sh (.boo (some false))))) :=
    HasType.binop (HasType.inst 0 A (HasType.dot 0)) (empty_arr_lit_at A)
      (.ge_cross_kind_lower .nul (.tuple []) (by decide))
  let h_lt : HasType (.binop .lt .dot (.object []))
      (.arr A (.yld (.sh (.boo (some true))))) :=
    HasType.binop (HasType.inst 0 A (HasType.dot 0)) (empty_obj_lit_at A)
      (.lt_cross_kind_lower .nul (.object []) (by decide))
  HasType.binop h_ge h_lt (.and_lit_false_l _ (.sh .boo_some_to_none))

private def h_isarray_at_bool :
    HasType isarray_body
      (.arr (.sh (.boo none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.boo none)
  let h_ge : HasType (.binop .ge .dot (.array []))
      (.arr A (.yld (.sh (.boo (some false))))) :=
    HasType.binop (HasType.inst 0 A (HasType.dot 0)) (empty_arr_lit_at A)
      (.ge_cross_kind_lower (.boo none) (.tuple []) (by decide))
  let h_lt : HasType (.binop .lt .dot (.object []))
      (.arr A (.yld (.sh (.boo (some true))))) :=
    HasType.binop (HasType.inst 0 A (HasType.dot 0)) (empty_obj_lit_at A)
      (.lt_cross_kind_lower (.boo none) (.object []) (by decide))
  HasType.binop h_ge h_lt (.and_lit_false_l _ (.sh .boo_some_to_none))

private def h_isarray_at_num :
    HasType isarray_body
      (.arr (.sh (.num none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.num none)
  let h_ge : HasType (.binop .ge .dot (.array []))
      (.arr A (.yld (.sh (.boo (some false))))) :=
    HasType.binop (HasType.inst 0 A (HasType.dot 0)) (empty_arr_lit_at A)
      (.ge_cross_kind_lower (.num none) (.tuple []) (by decide))
  let h_lt : HasType (.binop .lt .dot (.object []))
      (.arr A (.yld (.sh (.boo (some true))))) :=
    HasType.binop (HasType.inst 0 A (HasType.dot 0)) (empty_obj_lit_at A)
      (.lt_cross_kind_lower (.num none) (.object []) (by decide))
  HasType.binop h_ge h_lt (.and_lit_false_l _ (.sh .boo_some_to_none))

private def h_isarray_at_str :
    HasType isarray_body
      (.arr (.sh (.str none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.str none)
  let h_ge : HasType (.binop .ge .dot (.array []))
      (.arr A (.yld (.sh (.boo (some false))))) :=
    HasType.binop (HasType.inst 0 A (HasType.dot 0)) (empty_arr_lit_at A)
      (.ge_cross_kind_lower (.str none) (.tuple []) (by decide))
  let h_lt : HasType (.binop .lt .dot (.object []))
      (.arr A (.yld (.sh (.boo (some true))))) :=
    HasType.binop (HasType.inst 0 A (HasType.dot 0)) (empty_obj_lit_at A)
      (.lt_cross_kind_lower (.str none) (.object []) (by decide))
  HasType.binop h_ge h_lt (.and_lit_false_l _ (.sh .boo_some_to_none))

/-- Precise scheme for `isarray`. -/
example :
    HasType isarray_body
      (.inter
        (.arr (.sh (.array (.sh .top) none)) (.yld (.sh (.boo (some true)))))
        (.arr (.neg_sh (.array (.sh .top) none))
              (.yld (.sh (.boo (some false)))))) :=
  let h_45 :=
    HasType.sub (HasType.inter_intro h_isarray_at_str h_isarray_at_object)
                TyLE.arrow_inter_dom
  let h_345 :=
    HasType.sub (HasType.inter_intro h_isarray_at_num h_45) TyLE.arrow_inter_dom
  let h_2345 :=
    HasType.sub (HasType.inter_intro h_isarray_at_bool h_345) TyLE.arrow_inter_dom
  let h_5way :=
    HasType.sub (HasType.inter_intro h_isarray_at_nul h_2345) TyLE.arrow_inter_dom
  let h_neg :=
    HasType.sub h_5way (TyLE.arr TyLE.neg_kind_top_arr StreamLE.refl)
  HasType.inter_intro h_isarray_at_array h_neg

/-! ### `isstring: . >= "" and . < []` -/

private def isstring_body : Filter :=
  .binop .and (.binop .ge .dot (.strLit "")) (.binop .lt .dot (.array []))

private def empty_str_lit_at (A : Ty) :
    HasType (.strLit "") (.arr A (.yld (.sh (.str (some ""))))) :=
  HasType.inst 0 A (HasType.strLit 0 "")

private def h_isstring_at_str :
    HasType isstring_body
      (.arr (.sh (.str none)) (.yld (.sh (.boo (some true))))) :=
  let A : Ty := .sh (.str none)
  let h_dot := HasType.inst 0 A (HasType.dot 0)
  let h_ge := HasType.binop h_dot (empty_str_lit_at A) (.ge_str_str_empty none)
  let h_lt := HasType.binop h_dot (empty_arr_lit_at A)
    (.lt_cross_kind_lower (.str none) (.tuple []) (by decide))
  HasType.binop h_ge h_lt .and_true_true

private def h_isstring_at_nul :
    HasType isstring_body
      (.arr (.sh .nul) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh .nul
  let h_dot := HasType.inst 0 A (HasType.dot 0)
  let h_ge := HasType.binop h_dot (empty_str_lit_at A)
    (.ge_cross_kind_lower .nul (.str (some "")) (by decide))
  let h_lt := HasType.binop h_dot (empty_arr_lit_at A)
    (.lt_cross_kind_lower .nul (.tuple []) (by decide))
  HasType.binop h_ge h_lt (.and_lit_false_l _ (.sh .boo_some_to_none))

private def h_isstring_at_bool :
    HasType isstring_body
      (.arr (.sh (.boo none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.boo none)
  let h_dot := HasType.inst 0 A (HasType.dot 0)
  let h_ge := HasType.binop h_dot (empty_str_lit_at A)
    (.ge_cross_kind_lower (.boo none) (.str (some "")) (by decide))
  let h_lt := HasType.binop h_dot (empty_arr_lit_at A)
    (.lt_cross_kind_lower (.boo none) (.tuple []) (by decide))
  HasType.binop h_ge h_lt (.and_lit_false_l _ (.sh .boo_some_to_none))

private def h_isstring_at_num :
    HasType isstring_body
      (.arr (.sh (.num none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.num none)
  let h_dot := HasType.inst 0 A (HasType.dot 0)
  let h_ge := HasType.binop h_dot (empty_str_lit_at A)
    (.ge_cross_kind_lower (.num none) (.str (some "")) (by decide))
  let h_lt := HasType.binop h_dot (empty_arr_lit_at A)
    (.lt_cross_kind_lower (.num none) (.tuple []) (by decide))
  HasType.binop h_ge h_lt (.and_lit_false_l _ (.sh .boo_some_to_none))

private def h_isstring_at_array :
    HasType isstring_body
      (.arr (.sh (.array (.sh .top) none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.array (.sh .top) none)
  let h_dot := HasType.inst 0 A (HasType.dot 0)
  -- (array >= "") → bool[true] (array > str cross-kind).
  let h_ge := HasType.binop h_dot (empty_str_lit_at A)
    (.ge_cross_kind_higher (.array (.sh .top) none) (.str (some "")) (by decide))
  -- (array < []) → bool[false] (anchor).
  let h_lt := HasType.binop h_dot (empty_arr_lit_at A)
    (.lt_array_tuple_empty (.sh .top) none)
  HasType.binop h_ge h_lt (.and_lit_false_r _ (.sh .boo_some_to_none))

private def h_isstring_at_object :
    HasType isstring_body
      (.arr (.sh (.object [])) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.object [])
  let h_dot := HasType.inst 0 A (HasType.dot 0)
  let h_ge := HasType.binop h_dot (empty_str_lit_at A)
    (.ge_cross_kind_higher (.object []) (.str (some "")) (by decide))
  let h_lt := HasType.binop h_dot (empty_arr_lit_at A)
    (.lt_cross_kind_higher (.object []) (.tuple []) (by decide))
  HasType.binop h_ge h_lt (.and_lit_false_r _ (.sh .boo_some_to_none))

example :
    HasType isstring_body
      (.inter
        (.arr (.sh (.str none)) (.yld (.sh (.boo (some true)))))
        (.arr (.neg_sh (.str none)) (.yld (.sh (.boo (some false)))))) :=
  let h_45 :=
    HasType.sub (HasType.inter_intro h_isstring_at_array h_isstring_at_object)
                TyLE.arrow_inter_dom
  let h_345 :=
    HasType.sub (HasType.inter_intro h_isstring_at_num h_45) TyLE.arrow_inter_dom
  let h_2345 :=
    HasType.sub (HasType.inter_intro h_isstring_at_bool h_345) TyLE.arrow_inter_dom
  let h_5way :=
    HasType.sub (HasType.inter_intro h_isstring_at_nul h_2345) TyLE.arrow_inter_dom
  let h_neg :=
    HasType.sub h_5way (TyLE.arr TyLE.neg_kind_top_str StreamLE.refl)
  HasType.inter_intro h_isstring_at_str h_neg

/-! ### `isnumber: . > true and . < ""` -/

private def isnumber_body : Filter :=
  .binop .and (.binop .gt .dot (.boolLit true)) (.binop .lt .dot (.strLit ""))

private def true_lit_at (A : Ty) :
    HasType (.boolLit true) (.arr A (.yld (.sh (.boo (some true))))) :=
  HasType.inst 0 A (HasType.boolLit 0 true)

private def h_isnumber_at_num :
    HasType isnumber_body
      (.arr (.sh (.num none)) (.yld (.sh (.boo (some true))))) :=
  let A : Ty := .sh (.num none)
  let h_dot := HasType.inst 0 A (HasType.dot 0)
  let h_gt := HasType.binop h_dot (true_lit_at A)
    (.gt_cross_kind_higher (.num none) (.boo (some true)) (by decide))
  let h_lt := HasType.binop h_dot (empty_str_lit_at A)
    (.lt_cross_kind_lower (.num none) (.str (some "")) (by decide))
  HasType.binop h_gt h_lt .and_true_true

private def h_isnumber_at_nul :
    HasType isnumber_body
      (.arr (.sh .nul) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh .nul
  let h_dot := HasType.inst 0 A (HasType.dot 0)
  let h_gt := HasType.binop h_dot (true_lit_at A)
    (.gt_cross_kind_lower .nul (.boo (some true)) (by decide))
  let h_lt := HasType.binop h_dot (empty_str_lit_at A)
    (.lt_cross_kind_lower .nul (.str (some "")) (by decide))
  HasType.binop h_gt h_lt (.and_lit_false_l _ (.sh .boo_some_to_none))

private def h_isnumber_at_bool :
    HasType isnumber_body
      (.arr (.sh (.boo none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.boo none)
  let h_dot := HasType.inst 0 A (HasType.dot 0)
  -- bool > bool[true] is always false (true is the max bool).
  let h_gt := HasType.binop h_dot (true_lit_at A) (.gt_anything_bool_true none)
  let h_lt := HasType.binop h_dot (empty_str_lit_at A)
    (.lt_cross_kind_lower (.boo none) (.str (some "")) (by decide))
  HasType.binop h_gt h_lt (.and_lit_false_l _ (.sh .boo_some_to_none))

private def h_isnumber_at_str :
    HasType isnumber_body
      (.arr (.sh (.str none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.str none)
  let h_dot := HasType.inst 0 A (HasType.dot 0)
  let h_gt := HasType.binop h_dot (true_lit_at A)
    (.gt_cross_kind_higher (.str none) (.boo (some true)) (by decide))
  -- str s < str "" is always false (anchor).
  let h_lt := HasType.binop h_dot (empty_str_lit_at A) (.lt_str_str_empty none)
  HasType.binop h_gt h_lt (.and_lit_false_r _ (.sh .boo_some_to_none))

private def h_isnumber_at_array :
    HasType isnumber_body
      (.arr (.sh (.array (.sh .top) none)) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.array (.sh .top) none)
  let h_dot := HasType.inst 0 A (HasType.dot 0)
  let h_gt := HasType.binop h_dot (true_lit_at A)
    (.gt_cross_kind_higher (.array (.sh .top) none) (.boo (some true)) (by decide))
  let h_lt := HasType.binop h_dot (empty_str_lit_at A)
    (.lt_cross_kind_higher (.array (.sh .top) none) (.str (some "")) (by decide))
  HasType.binop h_gt h_lt (.and_lit_false_r _ (.sh .boo_some_to_none))

private def h_isnumber_at_object :
    HasType isnumber_body
      (.arr (.sh (.object [])) (.yld (.sh (.boo (some false))))) :=
  let A : Ty := .sh (.object [])
  let h_dot := HasType.inst 0 A (HasType.dot 0)
  let h_gt := HasType.binop h_dot (true_lit_at A)
    (.gt_cross_kind_higher (.object []) (.boo (some true)) (by decide))
  let h_lt := HasType.binop h_dot (empty_str_lit_at A)
    (.lt_cross_kind_higher (.object []) (.str (some "")) (by decide))
  HasType.binop h_gt h_lt (.and_lit_false_r _ (.sh .boo_some_to_none))

example :
    HasType isnumber_body
      (.inter
        (.arr (.sh (.num none)) (.yld (.sh (.boo (some true)))))
        (.arr (.neg_sh (.num none)) (.yld (.sh (.boo (some false)))))) :=
  let h_45 :=
    HasType.sub (HasType.inter_intro h_isnumber_at_array h_isnumber_at_object)
                TyLE.arrow_inter_dom
  let h_345 :=
    HasType.sub (HasType.inter_intro h_isnumber_at_str h_45) TyLE.arrow_inter_dom
  let h_2345 :=
    HasType.sub (HasType.inter_intro h_isnumber_at_bool h_345) TyLE.arrow_inter_dom
  let h_5way :=
    HasType.sub (HasType.inter_intro h_isnumber_at_nul h_2345) TyLE.arrow_inter_dom
  let h_neg :=
    HasType.sub h_5way (TyLE.arr TyLE.neg_kind_top_num StreamLE.refl)
  HasType.inter_intro h_isnumber_at_num h_neg

/-! ## §20. Negation decomposition (Phase 3)

    Demonstrating that `Neg(Sh(object [(a, num)]))` — the type the user
    flagged as awkward — decomposes set-theoretically into:

    1. "Not an object at all"             — `Neg(Sh(object []))`.
    2. "An object missing key `a`"        — `Sh(object []) ∩ Neg(Sh(object [(a, top)]))`.
    3. "An object with `a` of wrong type" — `Sh(object [(a, Neg num)])`.

    The decomposition is complete — every value not in `Sh(object [(a, num)])`
    is in exactly one of the three cases (and they are mutually disjoint).

    This is what "real" set-theoretic negation looks like — it does NOT
    collapse to "non-objects". -/

example :
    TyLE
      (.neg_sh (.object [("a", .sh (.num none))]))
      (.union
         (.neg_sh (.object []))                                         -- not an object
         (.union
           (.inter (.sh (.object []))                                       -- object missing "a"
                   (.neg_sh (.object [("a", .sh .top)])))
           (.sh (.object [("a", .neg_sh (.num none))]))))                -- "a" of wrong type
   :=
  TyLE.neg_object_single_field

/-- A multi-field record negation splits into per-field negations. -/
example :
    TyLE
      (.neg_sh (.object [("a", .sh (.num none)), ("b", .sh (.str none))]))
      (.union
         (.neg_sh (.object [("a", .sh (.num none))]))
         (.neg_sh (.object [("b", .sh (.str none))])))
   :=
  TyLE.neg_object_multi_split

/-- A `Boo(true)` value is *disjoint* from `Boo(false)` (Phase 2). The
    intersection of two distinct singletons can be subtyped to anything. -/
example {t : Ty} :
    TyLE (.inter (.sh (.boo (some true))) (.sh (.boo (some false)))) t :=
  TyLE.singleton_disjoint_boo (by decide)

/-- A `num` and a `str` are disjoint kinds (Phase 1). -/
example {t : Ty} :
    TyLE (.inter (.sh (.num none)) (.sh (.str none))) t :=
  TyLE.kind_disjoint (.num none) (.str none) (by decide)

/-! ## `error` vs `empty`: distinct stream types

    `empty : A → ε` produces no values then *continues* (so that `f, empty`
    yields whatever `f` yields, no abort).
    `error : A → ⊥`  aborts the entire computation.

    `⊥ ⊑ S` for every stream `S`, so an aborting filter can be subsumed to
    fit any expected output. This is what makes
    `if isboolean then 1 else error : bool → ⟨1⟩` derivable: the precise
    if-then-else gives the intersection
    `(bool → ⟨1⟩) ∩ (¬bool → ⊥)`, and `TyLE.inter_intro_l` projects to the
    well-defined branch. -/

/-- `error : α → ⊥` directly. -/
example : HasType .error (.arr (.tvar 0) .bot) :=
  HasType.error 0

/-- `error`'s `⊥` codomain is below every singleton-yield stream. -/
example (T : Ty) : HasType .error (.arr (.tvar 0) (.yld T)) :=
  HasType.sub (HasType.error 0)
    (TyLE.arr TyLE.refl StreamLE.bot_min)

/-- `error`'s `⊥` codomain is below every star stream too. -/
example (T : Ty) : HasType .error (.arr (.tvar 0) (.star (.yld T))) :=
  HasType.sub (HasType.error 0)
    (TyLE.arr TyLE.refl StreamLE.bot_min)

/-- The intersection `(A → S) ∩ (B → ⊥)` projects to `A → S` via
    `inter_intro_l`. This is the *shape* of the simplification used to
    derive `bool → ⟨1⟩` from the precise type of
    `if isboolean then 1 else error`. -/
example {A B : Ty} {S : Stream} :
    TyLE (.inter (.arr A S) (.arr B .bot)) (.arr A S) :=
  TyLE.inter_intro_l TyLE.refl

/-- The full pipeline:

        if (. == true) then 1 else error  :  bool[true] → ⟨1⟩

    `error : ¬bool[true] → ⊥` sits as the false-branch; the precise
    if-then-else combines the branches into an intersection and we
    project to the well-defined side.

    Note that the same derivation with `empty` (`A → ε`) in the false
    branch would yield `(¬bool[true] → ε)`, which is the *wrong* type
    for an aborting branch — `empty` says "no values, then continues"
    while `error` aborts. The `⊥ ⊑ ε` direction is admissible (errors
    can be hidden), but the converse is not. -/
example :
    HasType (.ifThenElse
              (.binop .eq .dot (.boolLit true))
              (.numLit 1)
              .error)
            (.arr (.sh (.boo (some true)))
                  (.yld (.sh (.num (some 1))))) := by
  -- Choose A = ⊤ so `inter ⊤ (refine c true) = inter ⊤ bool[true]`,
  -- which `inter_intro_r` weakens to `bool[true]` if needed.
  let A : Ty := .sh .top
  let St : Stream := .yld (.sh (.num (some 1)))
  -- 1. The condition `. == true` types as `⊤ → ⟨bool⟩` via Overload.eq_any.
  have h_c : HasType (.binop .eq .dot (.boolLit true))
                     (.arr A (.yld (.sh (.boo none)))) :=
    HasType.binop
      (HasType.inst 0 A (HasType.dot 0))
      (HasType.inst 0 A (HasType.boolLit 0 true))
      Overload.eq_any
  -- 2. The true-branch `1 : (⊤ ∩ bool[true]) → ⟨1⟩`.
  have h_t : HasType (.numLit 1)
                     (.arr (.inter A (.sh (.boo (some true)))) St) :=
    HasType.inst 0 (.inter A (.sh (.boo (some true))))
                 (HasType.numLit 0 1)
  -- 3. The false-branch `error : (⊤ ∩ ¬bool[true]) → ⊥`.
  --    The `⊥` codomain is what makes this work — it can sit next to
  --    the true-branch's `⟨1⟩` in the intersection without forcing the
  --    entire result to be wider than `⟨1⟩`.
  have h_e : HasType .error
                     (.arr (.inter A (.neg_sh (.boo (some true)))) .bot) :=
    HasType.inst 0 (.inter A (.neg_sh (.boo (some true))))
                 (HasType.error 0)
  -- 4. ifPrecise produces the intersection of the two arrows.
  have h_full :
      HasType (.ifThenElse (.binop .eq .dot (.boolLit true))
                           (.numLit 1) .error)
              (.inter (.arr (.sh (.boo (some true))) St)
                      (.arr (.neg_sh (.boo (some true))) .bot)) :=
    HasType.ifPrecise h_c (by decide) h_t h_e
  -- 5. Project to the well-defined branch via inter_intro_l.
  exact HasType.sub h_full (TyLE.inter_intro_l TyLE.refl)

end Tjq
