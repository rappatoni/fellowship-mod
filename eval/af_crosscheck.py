"""Cross-check AIDA's labellings of encoded abstract AFs against Dung.

Usage: python af_crosscheck.py [N_RANDOM] [SEED]
Runs the named literature AFs, then N random ones."""
import random, sys
from aida_env import setup_prover, Service
from core.dc.debate_graph import canonical_prop
import dung

LIT = {
    "unattacked": (["a"], []),
    "self_attack": (["a"], [("a", "a")]),
    "chain_reinstatement": (["a", "b", "c"], [("b", "a"), ("c", "b")]),
    "mutual_attack": (["a", "b"], [("a", "b"), ("b", "a")]),
    "odd_3_cycle": (["a", "b", "c"], [("a", "b"), ("b", "c"), ("c", "a")]),
    "even_4_cycle": (["a", "b", "c", "d"], [("a", "b"), ("b", "c"), ("c", "d"), ("d", "a")]),
    "floating_reinstatement": (["a", "b", "c", "d"], [("a", "b"), ("b", "a"), ("a", "c"), ("b", "c"), ("c", "d")]),
    "self_attacker_attacks_other": (["a", "b"], [("a", "a"), ("a", "b")]),
    "odd_cycle_attacks_out": (["a", "b", "c", "d"], [("a", "b"), ("b", "c"), ("c", "a"), ("c", "d")]),
    "dung95_ex": (["a", "b", "c"], [("a", "b"), ("b", "a"), ("b", "c")]),
    "stable_none_pref_one": (["a", "b", "c", "d"], [("a", "b"), ("b", "c"), ("c", "a"), ("d", "a")]),
}

SEMS = ["grounded", "complete", "preferred", "stable"]

def project(lab, args):
    return tuple(lab.get((canonical_prop(a.upper()), "term"), "absent") for a in args)

def run(svc, name, args, att):
    text = dung.to_fspy(args, att)
    rep = svc.check(text)
    bad = [e for e in rep.entries if e.status in ("refused", "error", "fatal", "reported")]
    if bad:
        return [f"{name}: document refused: {bad[0].message}"]
    problems = []
    for sem in SEMS:
        ref = sorted(tuple(L[a] for a in args) for L in dung.semantics(args, att, sem))
        try:
            got = sorted(project(l, args) for l in svc.label("document", sem).labellings)
        except Exception as ex:
            problems.append(f"{name} {sem}: label error {ex}"); continue
        if ref != got:
            problems.append(f"{name} {sem}: Dung {ref} AIDA {got}")
    return problems

def main():
    n = int(sys.argv[1]) if len(sys.argv) > 1 else 0
    rnd = random.Random(int(sys.argv[2]) if len(sys.argv) > 2 else 1)
    svc = Service.of(setup_prover())
    cases = list(LIT.items())
    for i in range(n):
        k = rnd.randint(1, 5)
        args = [chr(ord("a") + j) for j in range(k)]
        att = sorted({(rnd.choice(args), rnd.choice(args)) for _ in range(rnd.randint(0, 2 * k))})
        cases.append((f"rand{i}", (args, att)))
    total, fails = 0, 0
    for name, (args, att) in cases:
        total += 1
        probs = run(svc, name, args, att)
        if probs:
            fails += 1
            print(f"MISMATCH {name} args={args} att={att}")
            for p in probs: print("   ", p)
        else:
            print(f"ok {name} args={args} att={att}")
    print(f"{total - fails}/{total} frameworks agree on all four semantics")

main()
