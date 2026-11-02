"""Label a generated Tweety document and report, per atom X, the labels of
X[t] (X is established) and X[c] (X is refuted, i.e. ~X) in each labelling
of the document graph.

Usage: python doc.py FILE.fspy [SEM ...]   (default: grounded preferred stable)
Prints one line per semantics and labelling:  X=+ (X[t] IN), X=- (X[c] IN),
X=? (undecided), X=o (both OUT), X=! (both IN); atoms absent from the graph
are left out. Statements other than atoms (rules) are listed when not IN.
"""
import os, sys
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
from aida_env import setup_prover, Service
from core.dc.debate_graph import canonical_prop


def atom_of(key):
    k = str(key)
    return k[2:] if k.startswith("S:") else None


def summary(lab):
    atoms, other = {}, []
    for (k, side), v in lab.items():
        a = atom_of(k)
        if a is None:
            if v != "IN": other.append(f"{k}[{side[0]}]={v}")
            continue
        atoms.setdefault(a, {})[side] = v
    cells = []
    for a in sorted(atoms):
        t, c = atoms[a].get("term"), atoms[a].get("context")
        sym = {("IN", "IN"): "!", ("IN", None): "+", ("IN", "OUT"): "+", (None, "IN"): "-",
               ("OUT", "IN"): "-", ("OUT", "OUT"): "o", ("OUT", None): "o", (None, "OUT"): "o"}.get((t, c), "?")
        cells.append(f"{a}={sym}")
    return " ".join(cells) + (("   | " + " ".join(sorted(other))) if other else "")


def main():
    path = sys.argv[1]
    sems = sys.argv[2:] or ["grounded", "preferred", "stable"]
    svc = Service.of(setup_prover())
    rep = svc.check(open(path).read())
    bad = [e for e in rep.entries if e.status in ("refused", "error", "fatal", "reported")]
    print(f"== {os.path.basename(path)}: check ok={rep.ok}, {len(bad)} problem(s)")
    for e in bad:
        print(f"   line {e.span.line if e.span else '?'}: {e.status}: {e.message}")
    for sem in sems:
        try:
            labs = svc.label("document", sem).labellings
        except Exception as ex:
            print(f"  {sem}: label error {ex}"); continue
        if not labs:
            print(f"  {sem}: no labelling")
        for i, l in enumerate(labs):
            print(f"  {sem}[{i}] {summary(l)}")


if __name__ == "__main__":
    main()
