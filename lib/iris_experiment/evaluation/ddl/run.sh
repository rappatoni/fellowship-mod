#!/bin/sh
# Run the DDL encodings with clingo; DDL points at the checkout of
# github.com/gvdgdo/Defeasible-Deontic-Logic, CLINGO at a clingo command
# (default: clingo). Both the ambiguity-blocking variant and its
# well-founded counterpart are run. Writes NAME.out.
cd "$(dirname "$0")"
HERE=$(pwd)
C=${CLINGO:-clingo}
run() {  # name, variant suffix ("" or "-wf"), extra args
  ( cd "$DDL/Deontic" && $C 0 --warn=none defeasible-ab$2.asp deontic-comp$2.asp "$HERE/$1" $3 2>&1 )
}
for v in "" "-wf"; do
  echo "== variant defeasible-ab$v / deontic-comp$v"
  run frag_g.lp "$v" "" > "frag_g$v.out"
  printf '%-8s ' frag_g; grep -A1 '^Answer' "frag_g$v.out" | grep -v '^Answer\|^--' | tr ' ' '\n' | grep -E 'registrable|codet|constitutive|need|powerless|excluded' | sort | tr '\n' ' '; echo
  for s in rq4a rq4b rq4c rq4d rq6a rq6b rq6c rq6d rq7a rq7b rq7c; do
    run metanorm.lp "$v" "-c scenario=$s" > "$s$v.out"
    models=$(grep -c '^Answer' "$s$v.out")
    printf '%-8s models=%s ' $s "$models"
    grep -A1 '^Answer' "$s$v.out" | grep -v '^Answer\|^--' | tr ' ' '\n' | grep -E 'obligation\(enter\)|defeasible\((claim|passed|metanorm|limited)\)' | sort | tr '\n' ' '; echo
  done
done
