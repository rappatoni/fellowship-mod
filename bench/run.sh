#!/bin/zsh
# Time one synthetic document end to end through the CLI (one run).
#
#   bench/run.sh KIND N [nolabel]        (from the repository root)
#
# Prints the wall time, the exit code and the first error-like line.  The
# generated file is kept under bench/out/ for inspection.
root=${0:A:h:h}
out=$root/bench/out
mkdir -p $out
f=$out/$1_$2${3:+_$3}.fspy
$root/.venv/bin/python $root/bench/gen.py $1 $2 $3 > $f || exit 1
s=$(python3 -c 'import time; print(time.time())')
log=$(cd $root && ACDC_NO_RENDER=1 .venv/bin/acdc --script $f 2>&1)
rc=$?
e=$(python3 -c 'import time; print(time.time())')
printf "%s n=%s %s rc=%s %.2fs | %s\n" $1 $2 "$3" $rc $(( e - s )) \
  "$(print -r -- $log | grep -E 'Traceback|RecursionError|[Tt]imeout|refused' | head -1)"
