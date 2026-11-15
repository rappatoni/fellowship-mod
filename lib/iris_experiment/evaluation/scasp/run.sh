#!/bin/sh
# Count the answer sets with and without each scenario's target (s(CASP),
# SCASP points at the binary, default: scasp on PATH). Writes NAME.out.
cd "$(dirname "$0")"
S=${SCASP:-scasp}
for p in *_pos.pl; do
  n=${p%_pos.pl}
  { echo "## $n: with the target"; "$S" -s0 --timeout=60000 "${n}_pos.pl" 2>&1
    echo "## $n: without the target"; "$S" -s0 --timeout=60000 "${n}_neg.pl" 2>&1; } > "$n.out"
  pos=$("$S" -s0 --timeout=60000 "${n}_pos.pl" 2>&1 | grep -c 'ANSWER:')
  neg=$("$S" -s0 --timeout=60000 "${n}_neg.pl" 2>&1 | grep -c 'ANSWER:')
  printf '%-8s answer sets with the target: %s, without: %s\n' "$n" "$pos" "$neg"
done
# RQ 6 processing: the lawful order as a binding
for p in *_order.pl; do
  n=${p%_order.pl}
  printf '%-8s lawful order: ' "$n"
  "$S" -s0 --timeout=60000 "$p" 2>&1 | grep -A1 'BINDINGS' | grep 'O = ' | tr '\n' ' '
  echo
done
