import Lake
open Lake DSL

package tjq where
  -- Spec for the tjq type system. No mathlib dependency.

@[default_target]
lean_lib Tjq where
  -- Root module is Tjq.lean (which imports Tjq/Syntax, Tjq/Subtyping, Tjq/Typing).
