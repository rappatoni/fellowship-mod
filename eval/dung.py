"""Reference Dung semantics by brute force over 3-valued labellings, and an
encoding of an abstract AF as an AIDA document.

Encoding: every abstract argument x is a presumed atom X (argument x : (X).
by default.), every attack y -> x a declared strict axiom att_y_x : (Y -> ~X)
and a counterargument refuting X that applies it, leaving Y as an obligation
(the attacker's own presumption meets it).
"""
import itertools

def labellings(args, att):
    """All complete labellings (dict arg -> IN/OUT/UNDEC)."""
    attackers = {a: [b for (b, c) in att if c == a] for a in args}
    out = []
    for combo in itertools.product(("IN", "OUT", "UNDEC"), repeat=len(args)):
        L = dict(zip(args, combo))
        ok = True
        for a in args:
            atk = [L[b] for b in attackers[a]]
            if L[a] == "IN" and not all(x == "OUT" for x in atk): ok = False
            if L[a] == "OUT" and not any(x == "IN" for x in atk): ok = False
            if L[a] == "UNDEC" and (all(x == "OUT" for x in atk) or any(x == "IN" for x in atk)): ok = False
            if not ok: break
        if ok: out.append(L)
    return out

def semantics(args, att, sem):
    comp = labellings(args, att)
    ins = lambda L: {a for a in args if L[a] == "IN"}
    if sem == "complete": return comp
    if sem == "grounded": return [min(comp, key=lambda L: len(ins(L)))]
    if sem == "preferred":
        return [L for L in comp if not any(ins(L) < ins(M) for M in comp)]
    if sem == "stable": return [L for L in comp if "UNDEC" not in L.values()]
    raise ValueError(sem)

def to_fspy(args, att):
    up = lambda a: a.upper()
    lines = ["lk.", f"declare {', '.join(up(a) for a in args)} : bool."]
    for (b, a) in att:
        lines.append(f"declare att_{b}_{a} : ({up(b)} -> ~{up(a)}).")
    for a in args:
        lines += [f"argument {a} : ({up(a)}).", "by default.", "dixi."]
    for (b, a) in att:
        lines += [f"counterargument {b}_on_{a} : ({up(a)}).",
                  f"cut (~{up(a)}) x.", f"cut ({up(b)} -> ~{up(a)}) th.", f"axiom att_{b}_{a}.",
                  "elim.", "next.", "axiom.", "elim.", "axiom.", "dixi."]
    return "\n".join(lines) + "\n"
