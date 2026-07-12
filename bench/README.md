# Benchmarks: tjq bytecode VM vs jq / jaq / gojq

`bench/run.sh [N] [reps]` times tjq's bytecode VM against jq 1.7.1, jaq
(Rust/f64), and gojq (Go) — all as subprocesses reading the **same** input file
and serializing to `/dev/null`, best-of-`reps` via `/usr/bin/time`. Each run's
output md5 is checked against jq, so a tool is never "fast" by producing less
(a `!` in the table flags a mismatch).

## Results (1,000,000-element array, best of 4, ms)

| program | tjq | jq | jaq | gojq |
| --- | --- | --- | --- | --- |
| `.` (parse+serialize only) | 80 | 340 | 70 | 120 |
| `[.[] \| .+1]` | 160 | 510 | 200 | 290 |
| `.[] \| .+1` (stream) | 200 | 530 | 620 | 1210 |
| `[.[] \| (.*2)+1]` | 170 | 600 | 260 | 390 |
| `.[] \| if .%2==0 then . else -. end` | 250 | 720 | 750 | 1320 |

All four produce **byte-identical output** (md5 verified) on every program.

## Honest reading of the numbers

- **Parity first.** tjq matches jq exactly here; it is not faster by cutting
  corners on output.
- **Most of jq's disadvantage is number parsing, not execution.** The `.`
  baseline is jq 340 ms vs 70–120 ms for the others: jq 1.7's decNumber makes
  parsing a million numbers ~4× slower. Subtracting the baseline, the *compute*
  of `[.[]|.+1]` is tjq ≈ 80 ms, jaq ≈ 130 ms, jq ≈ 170 ms, gojq ≈ 170 ms — tjq
  leads but by ~1.6–2×, not the ~3× a naive total-time read suggests.
- **tjq's streaming output path is fast.** On `.[]|.+1` (a million lines) tjq is
  well ahead of jaq/gojq, which pay more per emitted value.
- **The big caveat: tjq compiles only a core subset** (identity, literals, pipe,
  comma, indexing, iteration, arithmetic/comparison, `and`/`or`, negation,
  if/then/else, array/object construction — no builtins, bindings, `reduce`,
  error unwinding, paths, …). Its VM is leaner *because* it does less machinery.
  The comparison is therefore only meaningful on that subset; jq/jaq/gojq are
  complete implementations. As tjq grows to cover more, the gap should narrow.

So: tjq's bytecode VM is genuinely competitive with the fastest jq
implementations on the fragment it supports, and jq 1.7's headline slowness is
dominated by decNumber. Earlier notes in the testing report that quoted a flat
"~3× faster than jq" without this breakdown were overstated; this is the
corrected, reproducible picture.
