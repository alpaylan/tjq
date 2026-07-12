# Real-program snapshot corpus

`jq.test` is jq's own test suite (from `jqlang/jq`, `tests/jq.test`): blocks of
`program` / `input` / N expected-output lines, blank-separated. It is a curated
set of real jq programs with expected outputs.

Run the snapshot harness against it:

```
cargo run -q -p tjq_testing --release --example snapcheck -- tjq_testing/corpus/jq.test --show 40
```

`snapcheck` runs each program through tjq's interpreter and categorizes the
outcome against the expected outputs (pass / diverge / tjq-errored / parse
panic). This complements fuzzing — real programs reach constructs and builtins
the generator does not.

## Baseline (first run, 514 considered)

pass 149 (29%), diverge 48, tjq-errored 113 (missing builtins), parse-panic 204.

The 204 parse panics and 113 errors are mostly *unsupported* jq features (the
parser should reject them gracefully rather than panic — a robustness follow-up).
The 48 divergences are the actionable parity gaps this surfaced, none of which
the fuzzer had reached:

- **String interpolation** `"a\(EXPR)b"` — not implemented (treated as a literal).
- **`@format` strings** (`@html`, `@base64`, …) — the format is ignored.
- **Object shorthand** `{a}` ≡ `{a: .a}`, and `{$x}`, `{(expr): v}` computed keys.
- **Postfix `[]`** after any term — `.a[]`, `EXPR[]` (only `.[]` on `.` works).
- **`error` value preservation** — `error` should carry a value (jq: `catch`
  receives the raw value, e.g. the input), tjq only models a message string.
- **`flatten(depth)`** and other builtins that take arguments.
- **Slice with `?`** on non-sliceable inputs (`.[1:3]?` should yield `null` on
  null, not drop it).

These are tracked as parity work; fixing them also widens what the fuzzer and
the bytecode VM can cover.
