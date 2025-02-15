# The Ruleset Used by the Shape Inference

Each language construct produces a set of `shapes` for its consumers to use and manipulates the typing context based on the `shapes` it consumes.

## Dot (.) Rule

`[S], <T>, (.) |-- [S], <T>`

- `[S]` is the set of shapes `.` consumes.
- `<T>` is the typing context.
- `.` consumes `[S]` and produces `[S]`.
- `.` does not change the typing context.

## Comma (,) Rule

`[S], <T>, (R1,R2) |-- `
