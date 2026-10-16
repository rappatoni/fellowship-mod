"""Unfolding: graph -> debate term (debate-graph-spec.org, "Layer 2->4
interface"; fragment-followups-plan.org B3).

Given a document graph and an issue (a statement), build the debate term
for that issue (aida-unfold-entrypoints): the statement's root under one
support scaffold whose scion is the stack of the edges deriving it, and
one attack scaffold whose scion is the contrary's debate; each edge's
body is expanded with its own sites unfolded the same way.  Rational
closure is reachability: whatever the graph connects to the issue ends
up in the term.

    issue term     ATT( SUP(ROOT, STACK), SUP~(ROOT_c, STACK_c) )
    STACK          SUP( SUP(P1, P2), P3 )      last registered outermost

The root is a presumption if some argument of the document presumes the
statement, an obligation otherwise.  Order is registration order
(author, 2026-09-17), the last registered supporter outermost, judged
first.  An argument entrypoint (``unfold_argument``) puts the argument on
top of its stack, and the sites of its own body keep the kind it wrote.
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

Every binder captures, the scaffolds' own included (author, 2026-10-06;
aida-unfold-scaffold-binders-capture).  On either side a scaffold's
``alt`` binds the contrary of its statement and its ``b`` the statement
itself; the scion is in the scope of both, the supported or attacked
term in that of ``alt`` only.  A supporter that needs the statement it
supports gets ``b`` - after the outer step the supported term, so it
shares the root instead of a copy - and the attacker wing's root, the
contrary, is the attack's ``alt``.  Every statement being expanded has
such a binder in scope below its debate, so capture alone ends the
unfolding: each statement is expanded at most once per path (T6).

Debates are classical: the scaffolds throw to a second conclusion,
which LJ forbids, so there are no lj debates (author, 2026-09-17) and
unfolding always captures continuations.  The CLI refuses the debate
commands while the file is in lj.

Every horn of a body is exposed (author, 2026-10-13/16; tasks.org,
aida-discuss-non-site-attack-points, aida-rational-closure): a declared
axiom applied, a lemma, a lambda - the nodes the compiler gives an edge
of their own (core/dc/debate_graph.py) - become, where they stand, the
debate about their statement, with an obligation root and the node's
own derivation on top iff its argument has primacy (``_exposed``).  So
subargument edges are stack elements like argument edges.  A node
nobody engages stays as written; one whose statement is bound in scope
is the captured variable (``subarguments``).

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
    #: Whether the horns of a body - a lemma, a lambda, an axiom applied -
    #: are unfolded into the debate about their statement where they
    #: stand (tasks.org, aida-rational-closure, 2026-10-16).  False: sites
    #: only, the exposure before that date.
    expose = True

    def __init__(self, graph: DebateGraph, *, order=None,
                 subarguments="default", primacy=(), expose=None):
        """``order`` ({argument name: rank}, a debate's utterance order,
        core/dc/debate.py): at every statement the edges of the ranked
        arguments go above the others, in rank order - the last uttered
        outermost, judged first; the rest keep registration order below.
        ``primacy``: the arguments whose own derivations go first (the
        entry point; a debate's moved arguments), which extends to their
        subarguments.  ``subarguments``: at an exposed node whose
        statement is already bound, "default" keeps the node's own
        derivation (SUP(captured, own)) iff its argument has primacy,
        "share" never (the captured variable alone), "preserve" always,
        with a copy of the statement's attackers where a scaffold binds
        it (tasks.org, aida-circular-support-design).  ``expose`` False:
        the old exposure, sites only (tests of the shape alone)."""
        if expose is not None:
            self.expose = expose
        self.subarguments = subarguments
        self.primacy = set(primacy)
        self._order = dict(order or {})
        self.graph = graph
        self._sites = count(1)
        self._alts = count(1)
        # b has its own series, so that the sites of the argument unfolded
        # keep the lowest numbers although every b is drawn before its
        # scion is built
        self._betas = count(1)
        self._used = set()
        self._by_target = {}
        for edge in graph.edges:
            if edge.role == "subargument" and not self.expose:
                continue
            self._by_target.setdefault((edge.target_key, edge.target_side), []).append(edge)
        #: The exposed nodes, by identity: a subargument edge's term is a
        #: node of the body it was compiled from (core/dc/debate_graph.py).
        self._node_edge = ({id(e.term): e for e in graph.edges
                            if e.role == "subargument" and e.term is not None} if self.expose else {})
        self._scaffold_vars = set()
        if order:
            for edges in self._by_target.values():
                # stable: unranked edges keep registration order
                edges.sort(key=lambda e: (e.name in order, order.get(e.name, 0)))

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
        name = self._wiring("alt", self._alts)
        self._scaffold_vars.add(name)
        return name

    def _fresh_beta(self):
        name = self._wiring("b", self._betas)
        self._scaffold_vars.add(name)
        return name

    def _has_primacy(self, edge) -> bool:
        """Whether ``edge``'s argument has primacy: the entry point or a
        moved argument, their subarguments included (``name.μ1``)."""
        return any(edge.name == p or edge.name.startswith(p + ".")
                   for p in (*self.primacy, *self._order))

    @staticmethod
    def _scaffold_scope(statement, alt, beta, env):
        """The scope inside a scaffold's scion: on either
        side ``alt`` binds the contrary and ``b`` the statement itself, so
        a demand for either in the scion is captured by them like by any
        binder (aida-unfold-scaffold-binders-capture)."""
        return {**env, contrary(statement): alt, statement: beta}

    # -- scaffolds ----------------------------------------------------------

    # The paper's shapes (debate_graph.py, scaffold catalogue): the
    # exposure  t -> mu alpha.< t || [A:] >  with the slot filled.
    def _support(self, statement, orig, scion, alt, beta=None):
        key, side = statement
        prop = self._prop(key)
        if beta is None:
            beta = self._fresh_beta()
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

    def _attack(self, statement, orig, scion, alt, beta=None):
        key, side = statement
        prop = self._prop(key)
        if beta is None:
            beta = self._fresh_beta()
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
        presumption source.  ``own_kind``: the site is in the body of the
        argument the term is unfolded for, and its root
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
            # Every statement being unfolded has a binder in scope below its
            # debate (the scaffold's alt and b), so capture alone ends the
            # unfolding; this is a defensive fallback.
            site = self._site(statement, own_kind)
            logger.warning("unfold: no binder in scope for %s, a statement already being "
                           "unfolded; left as the site %s",
                           self._show(statement), getattr(site, "number", "?"))
            return site
        return self._debate(statement, env, spine | {statement}, own_kind=own_kind)

    # -- the debate about a statement (aida-unfold-entrypoints) --------------

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
        supporters): its root is the contrary, present whenever the
        statement has attackers or the contrary a default marker, and
        always captured by the attack's own ``alt``, which binds the
        contrary - if no attacker succeeds, the proof goes on unattacked.
        A wing that is nothing but that captured root is built like any
        other and left to normalisation.  Empty parts are left out: no
        supporters, no support scaffold; no attackers and no marker on the
        contrary, no attack scaffold."""
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
            alt, beta = self._fresh_alt(), self._fresh_beta()
            logger.debug("  unfold: %s attacked by the debate about %s, %d supporter(s) "
                         "(alt %s, b %s)", self._show(statement), self._show(contra),
                         len(attackers), alt, beta)
            wing = self._wing(contra, self._scaffold_scope(statement, alt, beta, env),
                              spine | {contra})
            term = self._attack(statement, term, wing, alt, beta)
        return term

    def _supported(self, statement, root, edges, env, spine, *, top=None, bottom=False):
        """``root`` under one support scaffold whose scion is the stack, or
        ``root`` alone when there is nothing to stack.  The stack is in
        the scaffold's scope: a supporter demanding the statement itself
        is captured by ``b`` (after the outer step, the supported term)."""
        if not edges and not bottom:
            return root
        alt, beta = self._fresh_alt(), self._fresh_beta()
        stack = self._stack(statement, edges, self._scaffold_scope(statement, alt, beta, env),
                            spine, top=top, bottom=bottom)
        return self._support(statement, root, stack, alt, beta)

    def _stack(self, statement, edges, env, spine, *, top=None, bottom=False):
        """The supporters as a stack: SUP(SUP(P1, P2), P3) for P1, P2, P3 in
        registration order - the last registered outermost, judged first
        by sigma (and, the strict phase checking the supporter first, by
        strictness too).  ``top`` goes outermost; a ``bottom`` presumption
        (the bare presumption site) innermost.  Each element is in the
        scope of the scaffolds around it: P_k (k >= 2), the scion of the
        k-th scaffold, under that scaffold's alt and b; P1, a supported
        term, under the alts only (the statement stays bound by the
        enclosing support scaffold's b, in ``env``)."""
        ordered = [e for e in edges if e is not top] + ([top] if top is not None else [])
        items = ([None] if bottom else []) + ordered
        contra = contrary(statement)

        # Every scaffold's names are drawn when the stack is begun, before
        # any element is built; the innermost scaffold (the second item's)
        # is the first.
        wiring = [(self._fresh_alt(), self._fresh_beta()) for _ in items[1:]]
        scopes = ([{**env, contra: wiring[0][0]} if wiring else env]
                  + [self._scaffold_scope(statement, alt, beta, env) for alt, beta in wiring])

        def element(k):
            item = items[k]
            if item is None:
                logger.debug("  unfold: %s +the presumption of %s, at the bottom",
                             self._show(statement), self._show(statement))
                return self._site(statement, True)
            logger.debug("  unfold: %s +support '%s'%s", self._show(statement), item.name,
                         " (the argument unfolded, on top)" if item is top else "")
            return self._edge(item, scopes[k], spine, own=item is top)

        # The argument unfolded is expanded first, so that its own binders
        # keep their names and its sites the lowest numbers.
        built = {len(items) - 1: element(len(items) - 1)} if top is not None else {}

        def get(k):
            return built[k] if k in built else element(k)

        term = get(0)
        for k in range(1, len(items)):
            alt, beta = wiring[k - 1]
            term = self._support(statement, term, get(k), alt, beta)
        return term

    def _wing(self, contra, env, spine):
        """The attacker wing: the debate about the contrary without its
        own attackers, SUP~(ROOT_c, STACK_c), or the root alone when nobody
        derives the contrary.  ``env`` is the attack's scope, so the root
        is the attack's ``alt``; inside the wing's own support scaffold an
        attacker demanding the contrary is captured by its ``b``, one
        demanding the statement attacked by its ``alt``."""
        root = self._bare(contra, env)
        if isinstance(root, (Goal, Laog, Deleg, Geled)):
            logger.debug("unfold: %s, the attacker wing, root %s (%s)",
                         self._show(contra), root.number, type(root).__name__)
        else:
            logger.debug("unfold: %s, the attacker wing, root '%s'",
                         self._show(contra), root.name)
        return self._supported(contra, root, self._by_target.get(contra, []), env, spine)

    def _bare(self, statement, env):
        """The bare contrary's site, or the binder in scope that captures
        it (author, 2026-10-05).  The bare contrary stands for the default
        somebody holds on that statement, and a default meets a binder in
        scope exactly as a site in an edge body does: the binder takes it.
        It is never expanded - its debate is the attack being built.  The
        attack's own ``alt`` is always in scope."""
        if statement in env:
            var = self._variable(statement, env[statement])
            defaults = self.graph.defaults.get(statement, ())
            if "presumption" in defaults:
                var.captured_presumption = True
            # the default somebody holds on the contrary stays visible to
            # the labelling even where no edge records it (an attack whose
            # wing is nothing but this variable): the compiler marks it
            var.captured_default = tuple(sorted(defaults))
            logger.debug("unfold: the bare %s captured as '%s'", self._show(statement), env[statement])
            return var
        return self._site(statement)

    def _edge(self, edge, env, spine, own=False):
        """An edge's body with every site unfolded in scope.  ``own``: the
        body of the argument the term is unfolded for, whose sites keep the
        kind it wrote.  An exposed node of the body (a lemma, a lambda, an
        axiom applied: ``_node_edge``) becomes the debate about its
        statement (``_exposed``)."""
        if edge.term is None:
            raise UnfoldError(f"Edge '{edge.name}' carries no term; it cannot be unfolded.")
        primary = own or self._has_primacy(edge)
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
            # the sites of the argument unfolded keep the kind it wrote
            return {"own_kind": presumed} if own else {}

        def walk(node, env, names, expose=True):
            """``expose``: whether this node itself may be exposed; its
            children are checked either way."""
            exposed = self._node_edge.get(id(node)) if expose else None
            if exposed is not None and node is not edge.term:
                return self._exposed(node, exposed, env, spine, names, primary, walk)
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

        copy = walk(edge.term, env, {})
        if isinstance(copy, (Mu, Mutilde)):
            # the copy's root binder may be renamed (a1_0_2); it still
            # records the argument a1_0 (aida-fold-occurrences-names)
            copy.origin = edge.name
        return copy

    def _exposed(self, node, sub, env, spine, names, primary, walk):
        """An exposed node where it stands in a body (the horn rule,
        tasks.org aida-discuss-non-site-attack-points):

        - its statement S bound in scope: the captured variable, with the
          node's own derivation kept on top of it, SUP(captured, own), iff
          its argument has primacy ("default"), never ("share") or always
          ("preserve", then also a copy of S's attackers where a scaffold
          binds S); no attack scaffold otherwise - the enclosing one
          covers the node;
        - S not bound, and nobody else derives, contests or presumes S:
          the node as written (empty parts are left out);
        - S not bound otherwise: the debate about S, its root an
          obligation site (a presumption of S elsewhere at the bottom of
          the stack), the node's own derivation on top iff primacy, else
          at its registered position."""
        statement = (sub.target_key, sub.target_side)
        contra = contrary(statement)
        if statement in env:
            keep = (self.subarguments == "preserve"
                    or (self.subarguments == "default" and primary))
            captured = self._variable(statement, env[statement])
            if not keep:
                logger.debug("unfold: node '%s' (%s) captured as '%s'",
                             sub.name, self._show(statement), env[statement])
                return captured
            alt, beta = self._fresh_alt(), self._fresh_beta()
            logger.debug("unfold: node '%s' (%s) kept on top of the captured '%s' (alt %s, b %s)",
                         sub.name, self._show(statement), env[statement], alt, beta)
            own = walk(node, self._scaffold_scope(statement, alt, beta, env), names, False)
            term = self._support(statement, captured, own, alt, beta)
            if (self.subarguments == "preserve" and env[statement] in self._scaffold_vars
                    and (self._by_target.get(contra) or self.graph.defaults.get(contra))):
                alt2, beta2 = self._fresh_alt(), self._fresh_beta()
                logger.debug("unfold: node '%s' also attacked by a copy of the debate about %s "
                             "(alt %s, b %s)", sub.name, self._show(contra), alt2, beta2)
                wing = self._wing(contra, self._scaffold_scope(statement, alt2, beta2, env),
                                  spine | {contra})
                term = self._attack(statement, term, wing, alt2, beta2)
            return term
        if not self._engaged(statement, sub):
            logger.log(TRACE, "  unfold: node '%s' (%s) stays as written: nobody engages it",
                       sub.name, self._show(statement))
            return walk(node, env, names, False)
        logger.debug("unfold: node '%s' exposed as %s%s", sub.name, self._show(statement),
                     " (its derivation on top)" if primary else "")
        return self._debate(statement, env, spine | {statement}, own_kind=False,
                            top=sub if primary else None)

    def _engaged(self, statement, sub) -> bool:
        """Whether anybody but the node's own edge ``sub`` derives, contests
        or presumes ``statement``."""
        contra = contrary(statement)
        others = [e for e in self._by_target.get(statement, []) if e is not sub]
        return bool(others or self._by_target.get(contra) or self.graph.defaults.get(contra))


def unfold(graph: DebateGraph, issue, subarguments="default") -> ProofTerm:
    """The canonical debate term for ``issue`` = (canonical key, side)."""
    return _shown(Unfolder(graph, subarguments=subarguments).unfold(issue))


unfold_issue = unfold


def unfold_argument(graph: DebateGraph, edge, subarguments="default") -> ProofTerm:
    """The debate term for the argument whose document edge is ``edge``,
    biased towards it (aida-unfold-entrypoints): primacy is its, and its
    subarguments'."""
    return _shown(Unfolder(graph, subarguments=subarguments, primacy={edge.name}).unfold_argument(edge))


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
