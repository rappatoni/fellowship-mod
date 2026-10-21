"""Brute-force ABA semantics for small theories, flat and non-flat
(Bondarenko, Dung, Kowalski, Toni 1997), as a reference where Tweety's
FlatAbaReasoner refuses a non-flat theory.

theory: rules [(head, [body...])], assumptions [a...], contrary {a: x}.
Th(S) closes S under the rules; S attacks a iff contrary(a) in Th(S);
S is closed iff S = Th(S) & A; S is admissible iff closed, not
self-attacking, and S attacks every closed set that attacks S; complete:
admissible and containing every assumption it defends; preferred:
maximal admissible; stable: closed, not self-attacking and attacking
every assumption outside S; grounded: the least complete extension when
there is one (always, for a flat theory).
"""
from itertools import combinations


def th(rules, S):
    have = set(S)
    while True:
        new = {h for h, b in rules if h not in have and all(x in have for x in b)}
        if not new:
            return have
        have |= new


def extensions(rules, A, contrary, sem):
    A = sorted(A)
    subsets = [frozenset(c) for k in range(len(A) + 1) for c in combinations(A, k)]
    T = {S: th(rules, S) for S in subsets}
    attacks = lambda S, a: a in contrary and contrary[a] in T[S]
    attacks_set = lambda S, R: any(attacks(S, a) for a in R)
    closed = [S for S in subsets if T[S] & set(A) == S]
    cf = [S for S in closed if not attacks_set(S, S)]
    defends = lambda S, a: all(attacks_set(S, R) for R in closed if attacks(R, a))
    adm = [S for S in cf if all(attacks_set(S, R) for R in closed if attacks_set(R, S))]
    comp = [S for S in adm if all(a in S for a in A if defends(S, a))]
    if sem == "complete":
        return comp
    if sem == "preferred":
        return [S for S in adm if not any(S < R for R in adm)]
    if sem == "stable":
        return [S for S in cf if all(attacks(S, a) for a in A if a not in S)]
    if sem == "grounded":
        least = [S for S in comp if all(S <= R for R in comp)]
        return least
    raise ValueError(sem)


def accepted(rules, A, contrary, sem):
    """(extensions, {literal: code}) where code is c/s as in compare.py;
    a literal is "+x" (x in Th(E)) or "-a" for an assumption a whose
    contrary is in Th(E)."""
    exts = extensions(rules, A, contrary, sem)
    closures = [th(rules, E) for E in exts]
    lits = {("+", x) for h, b in rules for x in [h, *b]} | {("+", a) for a in A} | {("-", a) for a in contrary}
    codes = {}
    for l in lits:
        ins = [(l[1] in C) if l[0] == "+" else (contrary[l[1]] in C) for C in closures]
        codes[l] = ("c" if any(ins) else "-") + ("s" if ins and all(ins) else "-")
    return exts, codes
