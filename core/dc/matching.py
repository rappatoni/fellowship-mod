"""One-sided matching of statements (tasks.org, aida-referents-instantiation,
decided 2026-10-21).

A generic statement covers an instance when the instance is the generic
statement with terms for its variables: ``beat ?x ?y`` covers
``beat conrad ?u`` and ``beat z z``; ``beat ?x ?x`` does not cover
``beat conrad peter``.  Matching is one-sided: the generic statement's
variables receive terms, the instance's names never do - a referent, a
bound variable or a constant of the instance is fixed.  So there is at
most one way, and no search.

Witness entries (``referents.Entry`` of kind ``witness``) are rigid on
both sides: a statement about one argument's open choice is never an
instance of a statement about another's, and is covered by a generic
statement only as a term the generic variable receives.
"""

from core.ac import referents
from core.ac.prop import (
    Prop, PTrue, PFalse, PSym, PApp, PNeg, PBin, PQuant, TSym, TApp, PropError,
)


def _bound_in(term, bound) -> bool:
    if isinstance(term, TSym):
        return term.name in bound
    if isinstance(term, TApp):
        return _bound_in(term.fun, bound) or _bound_in(term.arg, bound)
    return False


def match(pattern: Prop, target: Prop, variables) -> "dict | None":
    """The substitution of the ``variables`` of ``pattern`` that makes it
    ``target`` up to bound names, or None.  Both are taken with negation
    unfolded (``~A`` is ``A -> false``, as statement keys take it)."""
    from core.dc.debate_graph import _unfold_neg
    variables = frozenset(variables)
    sigma = {}

    def term(p, t, pb, tb):
        if isinstance(p, TSym):
            if p.name in pb:
                return isinstance(t, TSym) and tb.get(t.name) == pb[p.name]
            if p.name in variables:
                if _bound_in(t, tb):
                    return False
                if p.name in sigma:
                    return sigma[p.name] == t
                sigma[p.name] = t
                return True
            return isinstance(t, TSym) and t.name == p.name and t.name not in tb
        if isinstance(p, TApp):
            return (isinstance(t, TApp) and term(p.fun, t.fun, pb, tb)
                    and term(p.arg, t.arg, pb, tb))
        return False

    def prop(p, t, pb, tb, depth):
        if type(p) is not type(t):
            return False
        if isinstance(p, (PTrue, PFalse)):
            return True
        if isinstance(p, PSym):
            return p.name == t.name
        if isinstance(p, PApp):
            return prop(p.pred, t.pred, pb, tb, depth) and term(p.arg, t.arg, pb, tb)
        if isinstance(p, PNeg):
            return prop(p.body, t.body, pb, tb, depth)
        if isinstance(p, PBin):
            return (p.op == t.op and prop(p.left, t.left, pb, tb, depth)
                    and prop(p.right, t.right, pb, tb, depth))
        if isinstance(p, PQuant):
            if (p.quantifier != t.quantifier or len(p.names) != len(t.names)
                    or str(p.sort) != str(t.sort)):
                return False
            pb2, tb2 = dict(pb), dict(tb)
            for i, (pn, tn) in enumerate(zip(p.names, t.names)):
                pb2[pn] = depth + i
                tb2[tn] = depth + i
            return prop(p.body, t.body, pb2, tb2, depth + len(p.names))
        return False

    if prop(_unfold_neg(pattern), _unfold_neg(target), {}, {}, 0):
        return sigma
    return None


def subst_all(p: Prop, sigma) -> Prop:
    """``p`` with every name of ``sigma`` replaced at once (a value may
    mention another key without being substituted again)."""
    if not sigma:
        return p
    holes = {}
    for i, name in enumerate(sigma):
        hole = f"⁣{i}"
        holes[hole] = sigma[name]
        p = p.subst(name, TSym(hole))
    for hole, value in holes.items():
        p = p.subst(hole, value)
    return p


def rigid_scope(p: Prop, scope) -> Prop:
    """``p`` with the witness entries of ``scope`` it mentions made rigid
    (``referents.rigid``)."""
    free = p.free_vars()
    for e in scope or ():
        if referents.is_witness(e) and e[0] in free:
            p = p.subst(e[0], TSym(referents.rigid(e[0], e.owner)))
    return p


def pattern(text, scope=()):
    """(statement, variables) of a statement as a generic one: its
    parameters and the variables of ``scope`` a binder introduced are the
    variables; witness entries are rigid.  None if it does not parse."""
    try:
        p = Prop.parse(referents.plain(text))
    except PropError:
        return None
    p = rigid_scope(p, scope)
    free = p.free_vars()
    bound = {e[0] for e in scope or () if getattr(e, "kind", "bound") == "bound"}
    variables = {n for n in free if referents.is_referent(n) or n in bound}
    return p, variables


def instance(text, scope=()):
    """A statement as an instance: every name fixed, witness entries rigid.
    None if it does not parse."""
    try:
        p = Prop.parse(referents.plain(text))
    except PropError:
        return None
    return rigid_scope(p, scope)


def unrigid(term, scope_out: dict):
    """``term`` with each rigid witness name back as its referent; the
    witness entries met are recorded in ``scope_out`` (name -> entry)."""
    if isinstance(term, TSym):
        if term.name.startswith("w#") and ":" in term.name:
            owner, name = term.name[2:].rsplit(":", 1)
            scope_out.setdefault(name, owner)
            return TSym(name)
        return term
    if isinstance(term, TApp):
        return TApp(unrigid(term.fun, scope_out), unrigid(term.arg, scope_out))
    return term
