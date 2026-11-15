#!/bin/sh
# Evaluate every encoding with Carneades 4; CARNEADES points at the binary.
# Writes NAME.out (Carneades' YAML) and prints the labels of each file.
cd "$(dirname "$0")"
for f in *.yml; do
  n=${f%.yml}
  "$CARNEADES" eval -t yaml "$f" > "$n.out" 2>&1
  printf '%-8s ' "$n"
  # a statement printed without a label is undecided
  awk 'function flush(){ if (k!="") printf "%s%s\n", k, (l==""?"undec":l); k=""; l="" }
       /^statements:/{s=1;next} /^[a-z]/{if(s)flush(); s=0}
       s&&/^    [^ ]/{flush(); k=$1} s&&/label:/{l=$2} END{if(s)flush()}' "$n.out" | sort | tr '\n' ' '
  echo
done
