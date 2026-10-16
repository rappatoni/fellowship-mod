"""Cross-check AIDA against reference ADF semantics on ADFs whose
acceptance conditions are in disjunctive normal form.

Encoding: one argument per disjunct; a positive literal y is an obligation
on Y, a negative literal ~z the presumed refutation of Z, a disjunct is a
declared rule (L1 -> ... -> Ln -> X); the empty disjunct is a declared fact.

Usage: python adf_crosscheck.py [N_RANDOM] [SEED]"""
import random, sys
from aida_env import setup_prover, Service
from core.dc.debate_graph import canonical_prop
import adf

def to_fspy(conds):
    atoms = sorted(conds)
    up = str.upper
    out = ["lk.", f"declare {', '.join(up(a) for a in atoms)} : bool."]
    n = 0
    for x in atoms:
        for lits in conds[x]:
            n += 1
            if not lits:
                out += [f"declare fact_{x}_{n} : ({up(x)}).",
                        f"argument a_{x}_{n} : ({up(x)}).", f"axiom fact_{x}_{n}.", "dixi."]
                continue
            prem = [up(y) if s else f"({up(y)} -> false)" for s, y in lits]
            rule = " -> ".join(prem + [up(x)])
            out += [f"declare rule_{x}_{n} : ({rule}).",
                    f"argument a_{x}_{n} : ({up(x)}).",
                    f"cut ({rule}) th.", f"axiom rule_{x}_{n}."]
            for i, (s, y) in enumerate(lits):
                out.append("elim.")
                if s:
                    out.append("next.")
                else:
                    out += [f"elim h{i}.", f"cut ({up(y)}) k{i}.", f"axiom h{i}.", "by default.", "next."]
            out += ["axiom th.", "dixi."]
    return "\n".join(out) + "\n"

def to_adf(conds):
    def phi(lits_list):
        return lambda w: any(all(w[y] == s for s, y in lits) for lits in lits_list)
    return {x: phi(conds[x]) for x in conds}

def show(conds):
    def lit(s, y): return y if s else "~" + y
    return "; ".join(f"{x}: " + (" | ".join("&".join(lit(*l) for l in d) or "T" for d in ds) or "F")
                     for x, ds in sorted(conds.items()))

LIT = {
    "support_chain": {"a": [[]], "b": [[(True, "a")]], "c": [[(True, "b")]]},
    "attack_chain": {"a": [[]], "b": [[(False, "a")]], "c": [[(False, "b")]]},
    "joint_attack": {"a": [[]], "b": [[]], "c": [[(False, "a")], [(False, "b")]]},
    "joint_support": {"a": [[]], "b": [[(False, "d")]], "c": [[(True, "a"), (True, "b")]], "d": []},
    "mixed": {"a": [[]], "b": [[(False, "c")]], "c": [[(True, "a"), (False, "b")]]},
    "xor": {"a": [[]], "b": [[(False, "e")]], "e": [], "c": [[(True, "a"), (False, "b")], [(False, "a"), (True, "b")]]},
    "self_support": {"a": [[(True, "a")]]},
    "support_cycle": {"a": [[(True, "b")]], "b": [[(True, "a")]]},
    "support_cycle_with_base": {"a": [[(True, "b")], [(False, "c")]], "b": [[(True, "a")]], "c": []},
    "odd_negative": {"a": [[(False, "a")]]},
    "even_negative": {"a": [[(False, "b")]], "b": [[(False, "a")]]},
}

SEMS = ["grounded", "complete", "preferred", "stable"]

def run(svc, name, conds):
    atoms = sorted(conds)
    rep = svc.check(to_fspy(conds))
    bad = [e for e in rep.entries if e.status in ("refused", "error", "fatal", "reported")]
    if bad:
        return [f"document refused: {bad[0].message}"]
    probs = []
    for sem in SEMS:
        ref = sorted(tuple(v[a] for a in atoms) for v in adf.SEMANTICS[sem](to_adf(conds)))
        try:
            labs = svc.label("document", sem).labellings
        except Exception as ex:
            probs.append(f"{sem}: {type(ex).__name__} {ex}"); continue
        got = sorted(tuple({"absent": "OUT"}.get(l.get((canonical_prop(a.upper()), "term"), "absent"),
                                                   l.get((canonical_prop(a.upper()), "term"), "OUT"))
                           for a in atoms) for l in labs)
        if ref != got:
            probs.append(f"{sem}: reference {ref} AIDA {got}")
    return probs

def main():
    n = int(sys.argv[1]) if len(sys.argv) > 1 else 0
    rnd = random.Random(int(sys.argv[2]) if len(sys.argv) > 2 else 3)
    cases = list(LIT.items())
    for i in range(n):
        k = rnd.randint(1, 4)
        atoms = [chr(ord("a") + j) for j in range(k)]
        conds = {}
        for x in atoms:
            ds = []
            for _ in range(rnd.choice([0, 1, 1, 2])):
                lits = sorted({(rnd.random() < 0.5, rnd.choice(atoms)) for _ in range(rnd.randint(0, 2))})
                if len({y for _, y in lits}) == len(lits):
                    ds.append(lits)
            conds[x] = ds
        cases.append((f"rand{i}", conds))
    svc = Service.of(setup_prover())
    ok = 0
    for name, conds in cases:
        probs = run(svc, name, conds)
        if probs:
            print(f"MISMATCH {name}: {show(conds)}")
            for p in probs: print("   ", p)
        else:
            ok += 1
            print(f"ok {name}: {show(conds)}")
    print(f"{ok}/{len(cases)} ADFs agree on all four semantics")

main()
