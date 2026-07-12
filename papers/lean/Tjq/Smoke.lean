import Tjq.Syntax
import Tjq.Subtyping
import Tjq.Typing

/-! Quick smoke test: instantiate the typing rules on a few small filters
    to catch obvious gaps. Not a real test suite — just a few example
    derivations. -/

namespace Tjq

open Filter

/-- `.` types as `α → ⟨α⟩`. -/
example : HasType .dot (.arr (.tvar 0) (.yld (.tvar 0))) :=
  .dot 0

/-- `1` types as `α → ⟨1⟩`. -/
example : HasType (.numLit 1)
            (.arr (.tvar 0) (.yld (.sh (.num (some 1))))) :=
  .numLit 0 1

/-- `., .` types as `α → ⟨α⟩;⟨α⟩`. -/
example :
    HasType (.comma .dot .dot)
            (.arr (.tvar 0)
                  (.concat (.yld (.tvar 0)) (.yld (.tvar 0)))) :=
  .comma (.dot 0) (.dot 0)

/-- `.foo`: `(Null ∪ Object({foo: α})) → ⟨α ∪ Null⟩`. -/
example :
    HasType (.objIndex "foo")
            (.arr (.union (.sh .nul)
                          (.sh (.object [("foo", .tvar 0)])))
                  (.yld (.union (.tvar 0) (.sh .nul)))) :=
  .objIndex 0 "foo"

end Tjq
