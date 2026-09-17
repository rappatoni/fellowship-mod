"""Unfolding: graph -> debate term (debate-graph-spec.org, "Layer 2->4
interface"; fragment-followups-plan.org B3).

Given a document graph and an issue (a statement), build the debate term
for that issue: start from the issue's site, wrap every edge deriving the
statement as a support scaffold and every edge deriving its contrary as
an attack scaffold, and expand each edge's body with its own sites
unfolded the same way.  Rational closure is reachability: whatever the
graph connects to the issue ends up in the term.

Cycles are broken by capture.  While expanding, the binders of the
enclosing edge bodies are in scope - a lambda's hypothesis, a mu's
continuation - each standing for a statement; an OBLIGATION site for a
statement that is bound in scope becomes that variable (the
mu/mu'-capture of the spec: a demand for P inside a proof of P->Q is
the hypothesis h, a demand for a refutation of P inside a proof of P is
the continuation).  A PRESUMPTION site is never captured: it is not a
demand that a hypothesis could meet but a default the arguer takes, and
binding it to the proponent's continuation would turn the opponent's
default into a classical hypothesis and change the argument (author,
2026-09-17; the even loop stays symmetric this way).  Scaffold catch
variables are wiring, not scope.  A statement that is already being
expanded on the current path but has no binder in scope is left as a
bare site: the route through it is circular and stays open.  Each
statement is therefore expanded at most once per path (T6).

Debates are classical: the scaffolds throw to a second conclusion,
which LJ forbids, so there are no lj debates (author, 2026-09-17) and
unfolding always captures continuations.  The CLI refuses the debate
commands while the file is in lj.

Only argument edges attach as supporters and attackers: a lambda
subargument edge is a piece of its parent's body and appears inline
where the parent is unfolded, never as a scion elsewhere.

The term this produces is evaluated by the term-first pipeline
(core/comp/evaluate.py): compiled, labelled, resolved by sigma.
"""

from copy import deepcopy
from itertools import count

from core.ac.ast import (
    ProofTerm, Mu, Mutilde, Lamda, Admal, Cons, Sonc,
    Goal, Laog, Deleg, Geled, ID, DI, Hyp, Pyh,
)
from core.dc.debate_graph import DebateGraph, canonical_prop, _peel_eta


class UnfoldError(ValueError):
    """The graph cannot be unfolded from this issue."""


def contrary(statement):
    key, side = statement
    return (key, "context" if side == "term" else "term")


class Unfolder:
    def __init__(self, graph: DebateGraph):
        self.graph = graph
        self._sites = count(1)
        self._alts = count(1)
        self._used = set()
        self._by_target = {}
        for edge in graph.edges:
            if edge.role == "subargument":
                continue
            self._by_target.setdefault((edge.target_key, edge.target_side), []).append(edge)

    # -- helpers ------------------------------------------------------------

    def _prop(self, key):
        return self.graph.nodes.get(key, key)

    def _site(self, statement):
        key, side = statement
        prop = self._prop(key)
        number = f"u{next(self._sites)}"
        presumed = "presumption" in self.graph.defaults.get(statement, ())
        if side == "term":
            return Deleg(number, prop) if presumed else Goal(number, prop)
        return Geled(number, prop) if presumed else Laog(number, prop)

    def _variable(self, statement, name):
        key, side = statement
        prop = self._prop(key)
        return DI(name, prop) if side == "term" else ID(name, prop)

    def _fresh_alt(self):
        return f"alt{next(self._alts)}"

    # -- scaffolds ----------------------------------------------------------

    def _support(self, statement, orig, scion, alt):
        key, side = statement
        prop = self._prop(key)
        if side == "term":
            return Mu(ID(alt, prop), prop,
                      Mu(ID("_", prop), prop, orig, ID(alt, prop)),
                      Mutilde(DI("_", prop), prop, scion, ID(alt, prop)))
        return Mutilde(DI(alt, prop), prop,
                       Mu(ID("_", prop), prop, DI(alt, prop), scion),
                       Mutilde(DI("_", prop), prop, DI(alt, prop), orig))

    def _attack(self, statement, orig, scion, alt):
        key, side = statement
        prop = self._prop(key)
        stolen = f"g{next(self._sites)}"
        if side == "term":
            return Mu(ID(alt, prop), prop,
                      Mu(ID("_", prop), prop, orig, ID(alt, prop)),
                      Mutilde(DI("_", prop), prop, Goal(stolen, prop), scion))
        return Mutilde(DI(alt, prop), prop,
                       Mu(ID("_", prop), prop, scion, Laog(stolen, prop)),
                       Mutilde(DI("_", prop), prop, DI(alt, prop), orig))

    # -- unfolding ----------------------------------------------------------

    def unfold(self, issue):
        """The debate term for ``issue``.  At the root a lone derivation is
        the term itself rather than a scaffold around an open site."""
        edges = self._by_target.get(issue, [])
        if edges and not self.graph.defaults.get(issue):
            base = self._edge(edges[0], {}, frozenset({issue}))
            term = self._wrap(issue, base, edges[1:], {}, frozenset({issue}))
        else:
            term = self.statement(issue, {}, frozenset())
        if isinstance(term, (Goal, Laog, Deleg, Geled)):
            # a bare site: give it the eta wrapper every argument has, so
            # the term-first pipeline can read the issue off the root
            key, side = issue
            prop = self._prop(key)
            name = f"x{next(self._sites)}"
            term = (Mu(ID(name, prop), prop, term, ID(name, prop)) if side == "term"
                    else Mutilde(DI(name, prop), prop, DI(name, prop), term))
        return term

    def statement(self, statement, env, spine, presumed=False):
        """The term (or context) for a statement in scope ``env``.  A
        presumed site (``presumed``: the edge body had a Deleg/Geled
        there) stays a presumption and is not captured."""
        if statement in env and not presumed:
            return self._variable(statement, env[statement])
        if statement in spine:
            return self._site(statement)        # circular: stays open
        spine = spine | {statement}
        return self._wrap(statement, self._site(statement),
                          self._by_target.get(statement, []), env, spine)

    def _wrap(self, statement, base, supporters, env, spine):
        """Wrap ``base`` in a support scaffold per deriving edge and an
        attack scaffold per edge (or default marker) of the contrary."""
        term = base
        contra = contrary(statement)
        for edge in supporters:
            term = self._support(statement, term, self._edge(edge, env, spine), self._fresh_alt())
        for edge in self._by_target.get(contra, []):
            term = self._attack(statement, term, self._edge(edge, env, spine | {contra}), self._fresh_alt())
        if self.graph.defaults.get(contra):
            # somebody presumes or demands the contrary outright
            term = self._attack(statement, term, self.statement_site_only(contra), self._fresh_alt())
        return term

    def statement_site_only(self, statement):
        """The contrary's own bare site as an attacking scion, in the
        eta-wrapped form the debate operators produce (mu x.<t || x>): a
        presumed contrary stands as its presumption; a merely demanded one
        is the trivial challenge, an identity edge that labels OUT."""
        key, side = statement
        prop = self._prop(key)
        inner = self._site(statement)
        name = f"x{next(self._sites)}"
        if side == "term":
            return Mu(ID(name, prop), prop, inner, ID(name, prop))
        return Mutilde(DI(name, prop), prop, DI(name, prop), inner)

    def _edge(self, edge, env, spine):
        """An edge's body with every site unfolded in scope."""
        if edge.term is None:
            raise UnfoldError(f"Edge '{edge.name}' carries no term; it cannot be unfolded.")
        root_lambda = _peel_eta(edge.term)
        if not isinstance(root_lambda, Lamda):
            root_lambda = None

        # A binder name is kept the first time it appears in the unfolded
        # term and suffixed on reuse: the same body may be unfolded several
        # times, and Fellowship's replay (the type check) refuses a
        # hypothesis introduced twice.  ``names`` maps the body's binder
        # names to their names in this copy; references follow it.
        def fresh(name):
            if name == "_":
                return name
            candidate, n = name, 1
            while candidate in self._used:
                n += 1
                candidate = f"{name}_{n}"
            self._used.add(candidate)
            return candidate

        def walk(node, env, names):
            if isinstance(node, (Goal, Deleg)):
                return self.statement((canonical_prop(node.prop), "term"), env, spine,
                                      presumed=isinstance(node, Deleg))
            if isinstance(node, (Laog, Geled)):
                return self.statement((canonical_prop(node.prop), "context"), env, spine,
                                      presumed=isinstance(node, Geled))
            if isinstance(node, ID):
                return ID(names.get(("context", node.name), node.name), node.prop)
            if isinstance(node, DI):
                return DI(names.get(("term", node.name), node.name), node.prop)
            if isinstance(node, Lamda):
                new = fresh(node.di.di.name)
                inner = {**env, (canonical_prop(node.di.prop), "term"): new}
                hyp = Hyp(DI(new, node.di.di.prop), node.di.prop)
                body = Lamda(hyp, walk(node.term, inner, {**names, ("term", node.di.di.name): new}))
                body.prop = node.prop
                return body
            if isinstance(node, Mu):
                new = fresh(node.id.name)
                inner = {**env, (canonical_prop(node.prop), "context"): new}
                names2 = {**names, ("context", node.id.name): new}
                return Mu(ID(new, node.id.prop), node.prop,
                          walk(node.term, inner, names2), walk(node.context, inner, names2))
            if isinstance(node, Mutilde):
                new = fresh(node.di.name)
                inner = {**env, (canonical_prop(node.prop), "term"): new}
                names2 = {**names, ("term", node.di.name): new}
                return Mutilde(DI(new, node.di.prop), node.prop,
                               walk(node.term, inner, names2), walk(node.context, inner, names2))
            if isinstance(node, Admal):
                new = fresh(node.id.id.name)
                inner = {**env, (canonical_prop(node.id.prop), "context"): new}
                pyh = Pyh(ID(new, node.id.id.prop), node.id.prop)
                out = Admal(pyh, walk(node.context, inner, {**names, ("context", node.id.id.name): new}))
                out.prop = node.prop
                return out
            if isinstance(node, Cons):
                out = Cons(walk(node.term, env, names), walk(node.context, env, names))
                out.prop = node.prop
                return out
            if isinstance(node, Sonc):
                out = Sonc(walk(node.context, env, names), walk(node.term, env, names))
                out.prop = node.prop
                return out
            raise UnfoldError(f"Edge '{edge.name}': cannot unfold node {type(node).__name__}.")

        return walk(edge.term, env, {})


def unfold(graph: DebateGraph, issue) -> ProofTerm:
    """The debate term for ``issue`` = (canonical key, side)."""
    return Unfolder(graph).unfold(issue)
