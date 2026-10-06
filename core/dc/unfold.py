"""Unfolding: graph -> debate term (debate-graph-spec.org, "Layer 2->4
interface"; fragment-followups-plan.org B3).

Given a document graph and an issue (a statement), build the debate term
for that issue: start from the issue's site, wrap every edge deriving the
statement as a support scaffold and every edge deriving its contrary as
an attack scaffold, and expand each edge's body with its own sites
unfolded the same way.  Rational closure is reachability: whatever the
graph connects to the issue ends up in the term.

Order is registration order (author, 2026-09-17): the first-registered
supporter is innermost, attackers are outside all supporters (an attack
contests the statement after all its alternatives), and the site itself
- an obligation unless the statement is presumed somewhere in the
document - is the innermost original everywhere, the root included.
The scaffolds are the paper's shapes (COMMA 2026), see the catalogue in
core/dc/debate_graph.py.

Cycles are broken by capture.  While expanding, the binders of the
enclosing edge bodies are in scope - a lambda's hypothesis, a mu's
continuation - each standing for a statement, and a site for a
statement that is bound in scope becomes that variable (the
mu/mu'-capture of the spec).  Two kinds of site, two meanings:

- an OBLIGATION captured is a demand met by a hypothesis or
  continuation in scope (a demand for P inside a proof of P->Q is h, a
  demand for a refutation of P inside a proof of P is the
  continuation) - the trap sprung, Peirce's law.  The framework still
  records it as an obligation source (it cannot see scope); the term
  can: strictness is read off the unfolded term (core/dc/strict.py),
  where the scion is closed and the scaffold is decided by it;
- a PRESUMPTION captured is the cycle representation (author,
  2026-09-17): the opponent's default "P fails" inside the debate about
  P is P's own continuation, so the loop closes in the term instead of
  being cut with a copy, and the term for the issue is one "model" of
  the loop - the even loop unfolded from P and from Q are the two.  The
  captured variable is marked (``captured_presumption``) and the
  compiler records it as a presumption source of the scion's own edge,
  so the labelling still sees the loop and semantics and mode decide
  whether neither, one or both models are accepted.

Scaffold catch variables are wiring, not scope.  A statement that is
already being expanded on the current path but has no binder in scope
is left as a bare site: the route through it is circular and stays
open.  Each statement is therefore expanded at most once per path (T6).

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

import logging
from copy import deepcopy
from itertools import count

from core.ac.ast import (
    ProofTerm, Mu, Mutilde, Lamda, Admal, Cons, Sonc,
    Goal, Laog, Deleg, Geled, ID, DI, Hyp, Pyh,
)
from core.dc.debate_graph import DebateGraph, canonical_prop, _peel_eta
from core.logging_util import TRACE, artifact

logger = logging.getLogger(__name__)


class UnfoldError(ValueError):
    """The graph cannot be unfolded from this issue."""


def contrary(statement):
    key, side = statement
    return (key, "context" if side == "term" else "term")


class Unfolder:
    #: The term's shape.  True: the stacked shape of aida-unfold-entrypoints
    #: (the root on top, one support scaffold whose scion is the supporter
    #: stack, one attack scaffold whose scion is the contrary's debate).
    #: False: the legacy shape (the site innermost, each supporter and then
    #: each attacker wrapping what came before), which the shared route
    #: still builds (core/dc/share.py) until aida-shared-route-stack-shape.
    stacked = True

    def __init__(self, graph: DebateGraph, *, stacked=None):
        if stacked is not None:
            self.stacked = stacked
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

    def _show(self, statement):
        """A statement for a log message: ``Prop[t]`` / ``Prop[c]``."""
        key, side = statement
        return f"{self._prop(key)}[{side[0]}]"

    def _show_all(self, statements):
        return ", ".join(self._show(s) for s in statements) or "-"

    def _presumed(self, statement):
        """The canonical kind: a presumption if some argument of the
        document presumes the statement."""
        return "presumption" in self.graph.defaults.get(statement, ())

    def _site(self, statement, presumed=None):
        key, side = statement
        prop = self._prop(key)
        number = f"u{next(self._sites)}"
        if presumed is None:
            presumed = self._presumed(statement)
        if side == "term":
            return Deleg(number, prop) if presumed else Goal(number, prop)
        return Geled(number, prop) if presumed else Laog(number, prop)

    def _variable(self, statement, name):
        key, side = statement
        prop = self._prop(key)
        return DI(name, prop) if side == "term" else ID(name, prop)

    def _wiring(self, stem, counter):
        """A name for a binder the unfolder itself introduces (a scaffold's
        catch variable, its inner binder, an eta wrapper): the next one of
        its series that no binder of the term has, recorded like the names
        of the arguments' own binders.  An argument may well have a binder
        called ``alt1`` - a term pasted from an earlier unfolding has - and
        Fellowship's replay refuses a name introduced twice; taking the
        name here makes ``fresh`` rename that binder when its body is
        walked."""
        while True:
            name = f"{stem}{next(counter)}"
            if name not in self._used:
                self._used.add(name)
                return name

    def _fresh_alt(self):
        return self._wiring("alt", self._alts)

    # -- scaffolds ----------------------------------------------------------

    # The paper's shapes (debate_graph.py, scaffold catalogue): the
    # exposure  t -> mu alpha.< t || [A:] >  with the slot filled.
    def _support(self, statement, orig, scion, alt):
        key, side = statement
        prop = self._prop(key)
        beta = self._wiring("b", self._sites)
        if side == "term":
            return Mu(ID(alt, prop), prop, orig,
                      Mutilde(DI(beta, prop), prop,
                              Mu(ID("_", prop), prop, DI(beta, prop), ID(alt, prop)),
                              Mutilde(DI("_", prop), prop, scion, ID(alt, prop))))
        return Mutilde(DI(alt, prop), prop,
                       Mu(ID(beta, prop), prop,
                          Mu(ID("_", prop), prop, DI(alt, prop), scion),
                          Mutilde(DI("_", prop), prop, DI(alt, prop), ID(beta, prop))),
                       orig)

    def _attack(self, statement, orig, scion, alt):
        key, side = statement
        prop = self._prop(key)
        beta = self._wiring("b", self._sites)
        if side == "term":
            return Mu(ID(alt, prop), prop, orig,
                      Mutilde(DI(beta, prop), prop,
                              Mu(ID("_", prop), prop, DI(beta, prop), scion),
                              Mutilde(DI("_", prop), prop, DI(beta, prop), ID(alt, prop))))
        return Mutilde(DI(alt, prop), prop,
                       Mu(ID(beta, prop), prop,
                          Mu(ID("_", prop), prop, DI(alt, prop), ID(beta, prop)),
                          Mutilde(DI("_", prop), prop, scion, ID(beta, prop))),
                       orig)

    # -- unfolding ----------------------------------------------------------

    def unfold(self, issue):
        """The debate term for ``issue``: the issue is a statement like any
        other - its own site (an obligation unless presumed somewhere),
        wrapped by its supporters and attackers in registration order."""
        logger.debug("unfold: issue %s from a document of %d edge(s), %d node(s)",
                     self._show(issue), len(self.graph.edges), len(self.graph.nodes))
        term = self.statement(issue, {}, frozenset())
        return self._eta_if_bare(issue, term)

    def unfold_argument(self, edge):
        """The debate term for the argument whose document edge is
        ``edge`` (aida-unfold-entrypoints): the canonical shape of its
        statement with the argument itself on top of the supporter stack,
        and the sites of its own body keeping the kind the argument wrote."""
        issue = (edge.target_key, edge.target_side)
        if not any(e is edge for e in self._by_target.get(issue, [])):
            raise UnfoldError(f"'{edge.name}' is not an argument edge of the document.")
        logger.debug("unfold: argument '%s' on %s from a document of %d edge(s), %d node(s)",
                     edge.name, self._show(issue), len(self.graph.edges), len(self.graph.nodes))
        term = self._debate(issue, {}, frozenset({issue}), top=edge)
        return self._eta_if_bare(issue, term)

    def _eta_if_bare(self, issue, term):
        if isinstance(term, (Goal, Laog, Deleg, Geled)):
            logger.debug("unfold: the issue is a bare site; adding the eta wrapper")
            # a bare site: give it the eta wrapper every argument has, so
            # the term-first pipeline can read the issue off the root
            key, side = issue
            prop = self._prop(key)
            name = self._wiring("x", self._sites)
            term = (Mu(ID(name, prop), prop, term, ID(name, prop)) if side == "term"
                    else Mutilde(DI(name, prop), prop, DI(name, prop), term))
        return term

    def statement(self, statement, env, spine, presumed=False, own_kind=None):
        """The term (or context) for a statement in scope ``env``.  A
        captured presumption (``presumed``: the edge body had a
        Deleg/Geled there) is marked so the compiler keeps it a
        presumption source.  ``own_kind`` (stacked shape only): the site is
        in the body of the argument the term is unfolded for, and its root
        keeps the kind the argument wrote (True: a presumption)."""
        if logger.isEnabledFor(TRACE):
            logger.log(TRACE, "  unfold: at %s  env={%s}  spine={%s}",
                       self._show(statement),
                       ", ".join(f"{self._show(k)}:{v}" for k, v in env.items()) or "-",
                       self._show_all(spine))
        if statement in env:
            var = self._variable(statement, env[statement])
            if presumed:
                var.captured_presumption = True
            logger.debug("unfold: %s captured as '%s'%s", self._show(statement),
                         env[statement], " (presumption)" if presumed else "")
            return var
        if statement in spine:
            # circular: the route through it stays open (T6)
            site = self._site(statement, own_kind if self.stacked else None)
            logger.debug("unfold: %s already on the spine, left as the bare site %s",
                         self._show(statement), getattr(site, "number", "?"))
            return site
        spine = spine | {statement}
        if self.stacked:
            return self._debate(statement, env, spine, own_kind=own_kind)
        edges = self._by_target.get(statement, [])
        site = self._site(statement)
        logger.debug("unfold: %s expanded, site %s (%s), %d deriving edge(s)",
                     self._show(statement), getattr(site, "number", "?"),
                     type(site).__name__, len(edges))
        return self._wrap(statement, site, edges, env, spine)

    def _wrap(self, statement, base, supporters, env, spine):
        """Wrap ``base`` in a support scaffold per deriving edge and an
        attack scaffold per edge (or default marker) of the contrary."""
        term = base
        contra = contrary(statement)
        # Registration order is the nesting order: supporters innermost, then
        # attackers, each wrapping what came before (author, 2026-09-17).
        for edge in supporters:
            alt = self._fresh_alt()
            logger.debug("  unfold: %s +support '%s' (alt %s)", self._show(statement), edge.name, alt)
            term = self._support(statement, term, self._edge(edge, env, spine), alt)
        for edge in self._by_target.get(contra, []):
            alt = self._fresh_alt()
            logger.debug("  unfold: %s +attack '%s' via %s (alt %s)",
                         self._show(statement), edge.name, self._show(contra), alt)
            term = self._attack(statement, term, self._edge(edge, env, spine | {contra}), alt)
        if self.graph.defaults.get(contra):
            # somebody presumes or demands the contrary outright
            alt = self._fresh_alt()
            logger.debug("  unfold: %s +attack by the bare %s (%s) (alt %s)",
                         self._show(statement), self._show(contra),
                         "/".join(sorted(self.graph.defaults.get(contra))), alt)
            term = self._attack(statement, term, self.statement_site_only(contra, env), alt)
        return term

    # -- the stacked shape (aida-unfold-entrypoints) ------------------------

    def _debate(self, statement, env, spine, *, own_kind=None, top=None):
        """The debate about ``statement`` (already on ``spine``):

            ATT( SUP(ROOT, STACK), SUP~(ROOT_c, STACK_c) )

        ROOT is the statement's site: a presumption if some argument
        presumes the statement, an obligation otherwise - or, in the body
        of the argument being unfolded, the kind it wrote (``own_kind``).
        STACK holds the supporters, last registered outermost, so judged
        first; ``top`` (the argument being unfolded) is moved to its top.
        Where that argument demands a statement somebody presumes, the
        presumption is the bottom of the stack.  The attacker wing is the
        contrary's debate without its attackers (they are this statement's
        supporters): its root is the contrary's site, present whenever the
        statement has attackers or the contrary a default marker, and
        captured by a binder in scope like any site.  Empty parts are left
        out: no supporters, no support scaffold; no attackers and no
        marker on the contrary, no attack scaffold."""
        canonical = self._presumed(statement)
        root_presumed = canonical if own_kind is None else own_kind
        bottom = own_kind is False and canonical
        edges = self._by_target.get(statement, [])
        root = self._site(statement, root_presumed)
        logger.debug("unfold: %s expanded, root %s (%s), %d supporter(s)%s%s",
                     self._show(statement), root.number, type(root).__name__, len(edges),
                     ", '%s' on top" % top.name if top is not None else "",
                     ", its presumption at the bottom" if bottom else "")
        term = self._supported(statement, root, edges, env, spine, top=top, bottom=bottom)
        contra = contrary(statement)
        attackers = self._by_target.get(contra, [])
        if attackers or self.graph.defaults.get(contra):
            alt = self._fresh_alt()
            logger.debug("  unfold: %s attacked by the debate about %s, %d supporter(s) (alt %s)",
                         self._show(statement), self._show(contra), len(attackers), alt)
            term = self._attack(statement, term, self._wing(contra, env, spine | {contra}), alt)
        return term

    def _supported(self, statement, root, edges, env, spine, *, top=None, bottom=False):
        """``root`` under one support scaffold whose scion is the stack, or
        ``root`` alone when there is nothing to stack."""
        if not edges and not bottom:
            return root
        alt = self._fresh_alt()
        stack = self._stack(statement, edges, env, spine, top=top, bottom=bottom)
        return self._support(statement, root, stack, alt)

    def _stack(self, statement, edges, env, spine, *, top=None, bottom=False):
        """The supporters as a stack: SUP(SUP(P1, P2), P3) for P1, P2, P3 in
        registration order - the last registered outermost, judged first
        by sigma (and, the strict phase checking the supporter first, by
        strictness too).  ``top`` goes outermost; a ``bottom`` presumption
        (the bare presumption site) innermost."""
        ordered = [e for e in edges if e is not top] + ([top] if top is not None else [])
        items = ([None] if bottom else []) + ordered

        def element(item):
            if item is None:
                logger.debug("  unfold: %s +the presumption of %s, at the bottom",
                             self._show(statement), self._show(statement))
                return self._site(statement, True)
            logger.debug("  unfold: %s +support '%s'%s", self._show(statement), item.name,
                         " (the argument unfolded, on top)" if item is top else "")
            return self._edge(item, env, spine, own=item is top)

        # The argument unfolded is expanded first, so that its own binders
        # keep their names and its sites the lowest numbers.
        built = {id(top): element(top)} if top is not None else {}

        def get(item):
            return built[id(item)] if item is not None and id(item) in built else element(item)

        term = get(items[0])
        for item in items[1:]:
            alt = self._fresh_alt()
            term = self._support(statement, term, get(item), alt)
        return term

    def _wing(self, contra, env, spine):
        """The attacker wing: the debate about the contrary without its
        own attackers, SUP~(ROOT_c, STACK_c), or the root alone when nobody
        derives the contrary.  A lone site needs no eta wrapper here: the
        compiler gives one to a bare leaf it records (``stack_elements``),
        and none survives into a normal form."""
        root = self._bare(contra, env)
        if isinstance(root, (Goal, Laog, Deleg, Geled)):
            logger.debug("unfold: %s, the attacker wing, root %s (%s)",
                         self._show(contra), root.number, type(root).__name__)
        return self._supported(contra, root, self._by_target.get(contra, []), env, spine)

    def statement_site_only(self, statement, env=None):
        """The contrary's own bare site as an attacking scion, in the
        eta-wrapped form the debate operators produce (mu x.<t || x>): a
        presumed contrary stands as its presumption; a merely demanded one
        is the trivial challenge, an identity edge that labels OUT.  The
        site is looked up in scope like any other (``_bare``)."""
        key, side = statement
        prop = self._prop(key)
        inner = self._bare(statement, env or {})
        name = self._wiring("x", self._sites)
        if side == "term":
            return Mu(ID(name, prop), prop, inner, ID(name, prop))
        return Mutilde(DI(name, prop), prop, DI(name, prop), inner)

    def _bare(self, statement, env):
        """The bare contrary's site, or the binder in scope that captures
        it (author, 2026-10-05).  The bare contrary stands for the default
        somebody holds on that statement, and a default meets a binder in
        scope exactly as a site in an edge body does: the binder takes it.
        It is never expanded - its debate is the attack being built - so
        there is no cut case."""
        if statement in env:
            var = self._variable(statement, env[statement])
            if "presumption" in self.graph.defaults.get(statement, ()):
                var.captured_presumption = True
            logger.debug("unfold: the bare %s captured as '%s'", self._show(statement), env[statement])
            return var
        return self._site(statement)

    def _edge(self, edge, env, spine, own=False):
        """An edge's body with every site unfolded in scope.  ``own``: the
        body of the argument the term is unfolded for, whose sites keep the
        kind it wrote."""
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
            if candidate != name:
                logger.log(TRACE, "  unfold: edge '%s' binder '%s' renamed to '%s' (reused body)",
                           edge.name, name, candidate)
            return candidate

        def own_kind(presumed):
            # only the stacked shape knows the argument being unfolded
            return {"own_kind": presumed} if own else {}

        def walk(node, env, names):
            if isinstance(node, (Goal, Deleg)):
                presumed = isinstance(node, Deleg)
                return self.statement((canonical_prop(node.prop), "term"), env, spine,
                                      presumed=presumed, **own_kind(presumed))
            if isinstance(node, (Laog, Geled)):
                presumed = isinstance(node, Geled)
                return self.statement((canonical_prop(node.prop), "context"), env, spine,
                                      presumed=presumed, **own_kind(presumed))
            if isinstance(node, (ID, DI)) and getattr(node, "cites", None):
                # A citation (core/dc/cite.py) unfolds exactly like an
                # obligation site: the cited argument's edge derives that
                # statement, so the debate brings it in, strict or not.
                side = "context" if isinstance(node, ID) else "term"
                logger.debug("unfold: '%s' cites '%s'; expanded as its statement",
                             edge.name, node.cites)
                return self.statement((canonical_prop(node.prop), side), env, spine,
                                      **own_kind(False))
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
    """The canonical debate term for ``issue`` = (canonical key, side)."""
    return _shown(Unfolder(graph).unfold(issue))


unfold_issue = unfold


def unfold_argument(graph: DebateGraph, edge) -> ProofTerm:
    """The debate term for the argument whose document edge is ``edge``,
    biased towards it (aida-unfold-entrypoints)."""
    return _shown(Unfolder(graph).unfold_argument(edge))


def unfold_legacy(graph: DebateGraph, issue) -> ProofTerm:
    """The issue's term in the legacy shape: the reference the shared
    route (core/dc/share.py) is tested against until
    aida-shared-route-stack-shape."""
    return _shown(Unfolder(graph, stacked=False).unfold(issue))


def argument_edge(graph: DebateGraph, name: str):
    """The document edge of the registered atomic argument ``name``, or
    None if it has none (refused at registration, or composed)."""
    for edge in graph.edges:
        if edge.name == name and edge.role != "subargument":
            return edge
    return None


def report_cached(term, what: str, revision: int):
    """Say that a cached term is used (aida-unfold-entrypoints), with the
    term itself, so that `explain` shows what the pipeline works on."""
    logger.debug("unfold: %s is unchanged since document revision %d; using it", what, revision)
    return _shown(term)


def _shown(term):
    if logger.isEnabledFor(logging.DEBUG):
        from pres.gen import pres_tree
        artifact(logger, "unfold: the debate term unfolded from the document", pres_tree(term))
    return term
