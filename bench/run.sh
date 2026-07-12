#!/usr/bin/env bash
# Reproducible, fair benchmark of tjq's bytecode VM against jq, jaq, and gojq.
#
# Fairness: every tool runs as a subprocess reading the SAME input file and
# serializing output to /dev/null (so parsing + serialization are counted
# equally for all), timed best-of-N with `/usr/bin/time -p`. A `.` (identity)
# baseline isolates parse+serialize cost from the actual computation, and each
# run's output md5 is checked so a tool is never "fast" by producing less.
#
# tjq only compiles a core subset (see tjq_exec::bytecode); pick programs in it.
#
# Usage: bench/run.sh [N_elements] [reps]
set -euo pipefail
cd "$(dirname "$0")/.."

N="${1:-1000000}"
REPS="${2:-4}"
INPUT="/tmp/tjq-bench-$N.json"
TJQ="./target/release/examples/tjqrun"
GOJQ="${GOJQ:-$HOME/go/bin/gojq}"

cargo build -q -p tjq_testing --release --example tjqrun
[ -f "$INPUT" ] || python3 -c "import json,sys;print(json.dumps(list(range(int(sys.argv[1])))))" "$N" > "$INPUT"

declare -A TOOL=( [tjq]="$TJQ" [jq]="jq" [jaq]="jaq" [gojq]="$GOJQ" )
PROGS=( "." "[.[]|.+1]" ".[]|.+1" "[.[]|(.*2)+1]" ".[]|if .%2==0 then . else -. end" )

echo "input=$INPUT ($N elements), best of $REPS, output to /dev/null"
printf "%-38s %8s %8s %8s %8s\n" "program" "tjq" "jq" "jaq" "gojq"
for prog in "${PROGS[@]}"; do
  # Parity: all available tools must agree (md5). tjq may not support a program.
  ref=""
  declare -A md5=()
  for name in jq jaq gojq tjq; do
    bin="${TOOL[$name]}"
    [ "$name" = gojq ] && [ ! -x "$bin" ] && continue
    md5[$name]=$("$bin" -c "$prog" "$INPUT" 2>/dev/null | md5 || echo NA)
  done
  ref="${md5[jq]}"
  printf "%-38s" "$prog"
  for name in tjq jq jaq gojq; do
    bin="${TOOL[$name]}"
    if { [ "$name" = gojq ] && [ ! -x "$bin" ]; } || [ "${md5[$name]:-NA}" = NA ]; then
      printf " %8s" "-"
      continue
    fi
    tag=""; [ -n "${md5[$name]:-}" ] && [ "${md5[$name]}" != "$ref" ] && tag="!"
    best=99999
    for _ in $(seq 1 "$REPS"); do
      t=$( { /usr/bin/time -p sh -c "$bin -c '$prog' '$INPUT' >/dev/null"; } 2>&1 | awk '/^real/{print $2*1000}')
      (( $(echo "$t < $best" | bc -l) )) && best=$t
    done
    printf " %7.0f%s" "$best" "${tag:- }"
  done
  echo
done
echo "( ! = output disagrees with jq )"
