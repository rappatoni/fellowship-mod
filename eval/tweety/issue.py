"""Issue view of a document: per atom, the grounded/preferred codes of X and
~X over the arguments for them (compare.issue_codes), plus adequacy.
Usage: python issue.py FILE.fspy [SEM,...]"""
import sys
sys.argv, files = [sys.argv[0]] + [a for a in sys.argv[1:] if a.startswith("--")], [a for a in sys.argv[1:] if not a.startswith("--")]
import compare
from compare import Service, setup_prover
svc = Service.of(setup_prover())
for f in files:
    rep = svc.check(open(f).read())
    atoms = sorted({str(k)[2:] for x in svc.inventory().arguments for k in [compare.canonical_prop(x.conclusion)] if str(k).startswith("S:")})
    codes, verdicts, problems = compare.issue_codes(svc, atoms)
    print(f"== {f}: check ok={rep.ok}")
    for p in problems: print("   ADEQUACY", p)
    for a in atoms:
        print(f"   {a:16} " + " ".join(f"{compare.ABBR[s]}={codes[a][s][0]}/{codes[a][s][1]}" for s in compare.SEMS))
