"""Adequacy on encoded abstract AFs: for every abstract argument x, the
verdict of `evaluate x` must be a value exactly when the issue X is IN in
some (credulous) / every (skeptical) labelling of the semantics, and the
issue labels must agree with Dung's semantics for x.

Usage: python af_eval_check.py N SEED"""
import random, sys
from aida_env import setup_prover, Service
from core.dc.debate_graph import canonical_prop
import dung

def main():
    n, seed = int(sys.argv[1]), int(sys.argv[2])
    rnd = random.Random(seed)
    svc = Service.of(setup_prover())
    stats = {"checked": 0, "adequacy": 0, "dung": 0, "errors": 0}
    for i in range(n):
        k = rnd.randint(1, 4)
        args = [chr(ord("a") + j) for j in range(k)]
        att = sorted({(rnd.choice(args), rnd.choice(args)) for _ in range(rnd.randint(0, 2 * k))})
        svc.check(dung.to_fspy(args, att))
        for x in args:
            key = (canonical_prop(x.upper()), "term")
            for sem in ("grounded", "preferred", "stable"):
                ref = [L[x] for L in dung.semantics(args, att, sem)]
                try:
                    vals = [l.get(key, "absent") for l in svc.label(x, sem).labellings]
                except Exception as ex:
                    print(f"ERROR label {args} {att} {x} {sem}: {ex}"); stats["errors"] += 1; continue
                if sorted(vals) != sorted(ref):
                    stats["dung"] += 1
                    print(f"DUNG {args} {att} x={x} {sem}: Dung {ref} issue-graph {vals}")
                for mode in ("skeptical", "credulous"):
                    stats["checked"] += 1
                    try:
                        cls = svc.evaluate(x, mode=mode, semantics=sem).nf_class
                    except Exception as ex:
                        if vals:
                            print(f"ERROR eval {args} {att} {x} {mode}/{sem}: {ex}"); stats["errors"] += 1
                        continue
                    want = bool(vals) and (all(v == "IN" for v in vals) if mode == "skeptical"
                                           else any(v == "IN" for v in vals))
                    if want != (cls == "value"):
                        stats["adequacy"] += 1
                        print(f"ADEQ {args} {att} x={x} {mode}/{sem}: labels {vals} verdict {cls}")
    print(stats)

main()
