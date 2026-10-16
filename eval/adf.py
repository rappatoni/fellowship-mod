"""Reference ADF semantics by brute force (Brewka, Strass, Ellmauthaler,
Wallner, Woltran, IJCAI 2013): the approximating operator Gamma over
three-valued interpretations; complete = fixpoints, grounded = least,
preferred = <=_i-maximal complete, stable = two-valued models whose true
atoms are the grounded true atoms of the reduct.

An acceptance condition is a Python function of a dict atom -> bool."""
import itertools

T, F, U = "IN", "OUT", "UNDEC"

def completions(v, atoms):
    free = [a for a in atoms if v[a] == U]
    for bits in itertools.product((True, False), repeat=len(free)):
        w = {a: v[a] == T for a in atoms if v[a] != U}
        w.update(zip(free, bits))
        yield w

def gamma(adf, v):
    atoms = list(adf)
    out = {}
    for s, phi in adf.items():
        vals = {phi(w) for w in completions(v, atoms)}
        out[s] = T if vals == {True} else F if vals == {False} else U
    return out

def leq_i(v, w):
    return all(v[a] == U or v[a] == w[a] for a in v)

def complete(adf):
    atoms = list(adf)
    res = []
    for combo in itertools.product((T, F, U), repeat=len(atoms)):
        v = dict(zip(atoms, combo))
        if gamma(adf, v) == v:
            res.append(v)
    return res

def grounded(adf):
    v = {a: U for a in adf}
    while True:
        w = gamma(adf, v)
        if w == v: return v
        v = w

def preferred(adf):
    c = complete(adf)
    return [v for v in c if not any(v != w and leq_i(v, w) for w in c)]

def stable(adf):
    res = []
    atoms = list(adf)
    for v in complete(adf):
        if U in v.values(): continue
        true = {a for a in atoms if v[a] == T}
        red = {a: (lambda phi: (lambda w: phi({**{b: False for b in atoms}, **w})))(adf[a]) for a in true}
        g = grounded(red) if red else {}
        if {a for a, x in g.items() if x == T} == true:
            res.append(v)
    return res

SEMANTICS = {"grounded": lambda d: [grounded(d)], "complete": complete,
             "preferred": preferred, "stable": stable}
