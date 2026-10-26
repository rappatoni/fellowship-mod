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
import re
from copy import deepcopy
from itertools import count

from core.ac.ast import (
    ProofTerm, Mu, Mutilde, Lamda, Admal, Cons, Sonc,
    Goal, Laog, Deleg, Geled, ID, DI, Hyp, Pyh,
    LamdaFO, ConsFO, TermsPairFO, DestructTermsPairFO,
)
from core.ac.prop import Prop, TSym
from core.ac import referents
from core.ac.referents import plain
from core.dc.debate_graph import (
    DebateGraph, canonical_prop, _peel_eta, _is_affine_binder, _prop_names,
    split_context_key, sub_multiset, contextualise, has_context, sort_text,
)
from core.comp.oracle_terms import uncaught_referents
from core.dc import matching
from core.logging_util import TRACE, artifact

logger = logging.getLogger(__name__)

_NAME = re.compile(r"[A-Za-z_?][A-Za-z0-9_']*")


def _names_in(node) -> set:
    """Every name the propositions and first-order terms of ``node``'s
    subterm spell, bound or free: a fresh name avoids them all."""
    found = set()

    def walk(n):
        for attr in ("prop", "fo_term", "witness", "var"):
            value = getattr(n, attr, None)
            if value is not None:
                found.update(_NAME.findall(str(value)))
        for child in vars(n).values():
            if isinstance(child, ProofTerm):
                walk(child)

    walk(node)
    return found


class UnfoldError(ValueError):
    """The graph cannot be unfolded from this issue."""


def _with_witnesses(text, scope):
    """``text`` carrying the witness entries of ``scope``: what keeps a
    statement about an argument's open choice that argument's wherever it
    stands (tasks.org, aida-referents-instantiation, 2026-10-22)."""
    wit = referents.witnesses_of(scope or ())
    if not wit or not isinstance(text, str) or not text:
        return text
    return referents.InContext(plain(text), wit)


def _witness_part(text):
    """``text`` with its context reduced to its witness entries: the
    statement binders in scope capture, in any context."""
    return _with_witnesses(text, getattr(text, "scope", ()))


def _site_props(term) -> list:
    """The propositions of the open sites of ``term``."""
    found, stack = [], [term]
    while stack:
        node = stack.pop()
        if isinstance(node, (Goal, Laog, Deleg, Geled)) and node.prop:
            found.append(node.prop)
        stack.extend(c for c in vars(node).values() if isinstance(c, ProofTerm))
    return found


def mark_instantiated(term) -> None:
    """Mark every node of ``term``, an argument placed with a substitution
    (or an alternative of a witness scaffold), as an instance."""
    stack = [term]
    while stack:
        node = stack.pop()
        node.instantiated = True
        stack.extend(c for c in vars(node).values() if isinstance(c, ProofTerm))


def _same_edge(edge, other) -> bool:
    """Whether ``edge`` is ``other`` or its instance (``DebateGraph.instantiate``)."""
    if other is None:
        return False
    return edge is other or (edge.generic is not None and edge.term is other.term
                             and edge.name == other.name)


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
            if edge.role == "weakening":
                continue                    # looked up by ``_targeting``
            self._by_target.setdefault((edge.target_key, edge.target_side), []).append(edge)
        #: The keys of each statement by the contexts it is stated in
        #: (tasks.org, aida-discourse-referents): weakening.
        self._bases = {}
        for key in graph.nodes:
            sorts, base = split_context_key(key)
            self._bases.setdefault(base, []).append((key, sorts))
        self._position = {id(e): i for i, e in enumerate(graph.edges)}
        #: The exposed nodes, by identity: a subargument edge's term is a
        #: node of the body it was compiled from (core/dc/debate_graph.py).
        self._node_edge = ({id(e.term): e for e in graph.edges
                            if e.role == "subargument" and e.term is not None} if self.expose else {})
        self._scaffold_vars = set()
        #: Discourse referents and contexts (tasks.org,
        #: aida-discourse-referents): for a statement in a context being
        #: unfolded, its proposition at that use and the context there,
        #: ((name, sort), outermost first); and the context where a
        #: statement is being unfolded.
        self._frames = {}
        self._fo_here = ()
        self._generic = None                # ``_generic_edges``
        self._generic_asserted = None       # ``_generic_markers``
        if order:
            for edges in self._by_target.values():
                # stable: unranked edges keep registration order
                edges.sort(key=lambda e: (e.name in order, order.get(e.name, 0)))

    # -- helpers ------------------------------------------------------------

    def _prop(self, key):
        frame = self._frames.get(key)
        if frame is not None:
            return frame[0]
        return self.graph.nodes.get(key, key)

    # -- discourse referents ------------------------------------------------

    def _framed(self, key, literal, scope, build):
        """``build()`` with the statement ``key`` read, at this use, as
        ``literal`` in the context ``scope``."""
        saved = self._frames.get(key)
        self._frames[key] = (literal, tuple(scope))
        try:
            return build()
        finally:
            if saved is None:
                self._frames.pop(key, None)
            else:
                self._frames[key] = saved

    def _root_frame(self, key):
        """A statement read as the document states it: its referents and
        its context stand for themselves."""
        text = self._prop(key)
        scope = self.graph.contexts.get(key)
        if scope is None:
            scope = tuple((r, None) for r in Prop.parse(text).referent_order())
        return _with_witnesses(text, scope), tuple(scope)

    def _targeting(self, statement):
        """The edges deriving ``statement``: its own and, by weakening,
        those of the same statement in every smaller context, in the order
        of the document (ranked arguments above the rest)."""
        key, side = statement
        sorts, base = split_context_key(key)
        found = []
        for other, other_sorts in self._bases.get(base, [(key, sorts)]):
            if other == key or (len(other_sorts) < len(sorts) and sub_multiset(other_sorts, sorts)):
                found.extend(self._by_target.get((other, side), []))
        if not found and key not in dict(self._bases.get(base, ())):
            found = list(self._by_target.get(statement, []))
        if not found and key not in self.graph.nodes and key in self._frames:
            found = self._covering(statement)
        if len(found) > 1:
            order = self._order
            found.sort(key=lambda e: (e.name in order, order.get(e.name, 0),
                                      self._position.get(id(e), 0)))
        return found

    def _covering(self, statement):
        """The edges of the generic statements covering ``statement``, a
        statement the document does not hold, as written here: one an
        alternative of a witness scaffold states about a candidate
        (tasks.org, aida-referents-anaphora).  The document's instances of
        a generic statement are its nodes (``DebateGraph.instantiate``);
        this finds them for a statement that is no node, without adding it
        to the document.  ``_lineups`` gives each its substitution."""
        literal, scope = self._frames[statement[0]]
        target = matching.instance(literal, scope)
        if target is None:
            return []
        found = []
        for edge, (eprop, evars) in self._generic_edges():
            if edge.target_side == statement[1] and matching.match(eprop, target, evars) is not None:
                found.append(edge)
        if found:
            logger.debug("unfold: %s, no node of the document, is covered by %s",
                         self._show(statement), ", ".join(e.name for e in found))
        return found

    def _generic_edges(self):
        """The edges deriving or contesting a generic statement (one with
        parameters), not their instances, each with the statement and its
        variables (``matching.pattern``): [(edge, (statement, variables))]."""
        if self._generic is None:
            self._generic = []
            for edges in self._by_target.values():
                for edge in edges:
                    if edge.generic is not None or edge.term is None or edge.role == "subargument":
                        continue
                    concl = getattr(edge.term, "prop", None) or ""
                    found = matching.pattern(concl, getattr(concl, "scope", ()))
                    if found is not None and found[1]:
                        self._generic.append((edge, found))
        return self._generic

    def _covering_markers(self, statement):
        """The markers asserted of the generic statements covering
        ``statement``, a statement the document does not hold, as written
        here.  The document copies them to its own instances
        (``DebateGraph.instantiate``); a reading's statement is none
        (tasks.org, aida-referents-anaphora-knowledge)."""
        if self._generic_asserted is None:
            self._generic_asserted = self.graph.generic_asserted()
        literal, scope = self._frames[statement[0]]
        kinds = DebateGraph.covering_kinds(self._generic_asserted, literal, scope, statement[1])
        if kinds:
            logger.debug("unfold: %s, no node of the document, takes %s from a generic "
                         "statement covering it", self._show(statement), ", ".join(sorted(kinds)))
        return kinds

    def _defaults(self, statement):
        """The default markers on ``statement`` or on it in a smaller
        context (weakening); for a statement the document does not hold,
        also those asserted of a generic statement covering it."""
        key, side = statement
        sorts, base = split_context_key(key)
        kinds = set(self.graph.defaults.get(statement, ()))
        for other, other_sorts in self._bases.get(base, ()):
            if other != key and len(other_sorts) < len(sorts) and sub_multiset(other_sorts, sorts):
                kinds |= set(self.graph.defaults.get((other, side), ()))
        if key not in self.graph.nodes and key in self._frames:
            kinds |= self._covering_markers(statement)
        return kinds

    def _literal(self, statement):
        """The statement as stated at this use, by the key binders in scope
        capture it with: a binder of P captures P in any context."""
        key, side = statement
        if (key not in self._frames and not key.startswith("[") and "r#" not in key
                and "w#" not in key):
            return statement
        return (canonical_prop(_witness_part(self._prop(key))), side)

    def _lineups(self, edge, statement):
        """The ways ``edge`` is used where ``statement`` is unfolded: each a
        substitution of its variables by what they stand for here.  Its
        conclusion is matched against the statement as written here
        (``core.dc.matching``, one-sided: the statement's names are fixed),
        which gives each parameter its term - a constant, a variable in
        scope, another argument's open choice (tasks.org,
        aida-referents-instantiation, 2026-10-21).  A witness-only referent
        is not lined up: it stays the argument's own open choice."""
        if edge.term is None:
            return [{}]
        frame = self._frames.get(statement[0])
        concl = getattr(edge.term, "prop", None) or ""
        env = self.graph.referent_envs.get(edge.name) or ()
        if frame is None and edge.generic is None and not env and not referents.mentioned(str(concl)):
            return [{}]
        literal, scope = frame if frame is not None else (self._prop(statement[0]), ())
        found = matching.pattern(concl, getattr(concl, "scope", ()))
        if found is None:
            return [{}]
        eprop, evars = found
        base = {}
        if evars:
            try:
                target = Prop.parse(plain(literal))
            except Exception:                       # noqa: BLE001 - an unreadable statement
                return []
            sigma = matching.match(eprop, target, evars)
            if sigma is None:
                logger.debug("unfold: '%s' does not match %s here", edge.name, literal)
                return []
            base = {n: t for n, t in sigma.items() if not (isinstance(t, TSym) and t.name == n)}
        # A referent of the environment the conclusion does not state is the
        # argument's own open choice: never lined up with what stands here,
        # it gets a witness scaffold where the argument chooses
        # (``_witness_scaffold``; tasks.org, aida-referents-instantiation,
        # 2026-10-22).
        return [base]

    def _own_witnesses(self, edge, inst=()):
        """The witness entries of ``edge``'s argument: the referents of its
        environment its conclusion does not state, but those ``inst``
        gives a value."""
        env = self.graph.referent_envs.get(edge.name) or ()
        if not env or edge.term is None:
            return ()
        stated = referents.mentioned(plain(getattr(edge.term, "prop", "") or ""))
        return tuple(referents.Entry(n, s, "witness", edge.name) for n, s in env
                     if n not in stated and n not in inst)

    def _alternatives(self, body, wname, sort, fo, read, owner=None) -> list:
        """The individuals a witness scaffold tries for the open choice
        ``wname`` of sort ``sort``, bottom of the stack first, each as
        (name in the term, context entry or None):

        - the declared constants some statement of the document is about
          where a site of ``body`` mentioning ``wname`` stands (``read``:
          a site's proposition as written here); for a ``?`` choice every
          constant of the sort;
        - the ``!`` open choices of earlier arguments (``_introduced``),
          those a statement is about likewise, for a ``?`` choice all,
          in document order; each with a witness entry of its owner, so
          that statements about it are its owner's;
        - the variables binders introduced in scope (``fo``), outermost
          first so that the innermost is on top.

        tasks.org, aida-referents-witness-scaffold, aida-referents-anaphora
        (2026-10-22)."""
        asked = not referents.is_presumed(wname)
        introduced = self._introduced(sort, owner)
        constants, borrowed = [], []
        for prop in _site_props(body):
            text = read(prop)
            try:
                site = Prop.parse(plain(text))
            except Exception:                       # noqa: BLE001
                continue
            if wname not in site.free_vars():
                continue
            for key, display in self.graph.nodes.items():
                scope = self.graph.contexts.get(key, ())
                target = matching.instance(display, scope)
                if target is None:
                    continue
                sigma = matching.match(site, target, {wname})
                value = (sigma or {}).get(wname)
                if not isinstance(value, TSym):
                    continue
                if value.name.startswith("w#"):
                    choice = tuple(value.name[2:].rsplit(":", 1))
                    if choice in introduced and choice not in borrowed:
                        borrowed.append(choice)
                elif (not referents.is_referent(value.name)
                        and all(e[0] != value.name for e in scope)
                        and value.name not in constants):
                    constants.append(value.name)
        if asked:
            constants += [c for c in self._constants(sort) if c not in constants]
            borrowed = list(introduced)
        else:
            borrowed = [c for c in introduced if c in borrowed]
        binders = [e[0] for e in fo
                   if not referents.is_witness(e) and e[1] is not None and sort_text(e[1]) == sort]
        taken = {e[0] for e in fo} | {e[0] for e in self._fo_here} | {wname} | set(binders)
        out = [(c, None) for c in constants if c not in binders]
        # an individual borrowed already, by an enclosing scaffold: the
        # same entry, under the same name
        known = {(e.owner, e.base): e for e in (*fo, *self._fo_here)
                 if referents.is_witness(e) and getattr(e, "base", None)}
        for choice_owner, name in borrowed:
            entry = known.get((choice_owner, name))
            if entry is not None:
                out.append((entry[0], entry))
                continue
            shown = name
            n = 2
            while shown in taken:
                shown = f"{name}_{n}"
                n += 1
            taken.add(shown)
            out.append((shown, referents.Entry(shown, sort, "witness", choice_owner, base=name)))
        return out + [(b, None) for b in binders]

    def _introduced(self, sort, owner=None) -> list:
        """The individuals introduced before ``owner``, (argument, name) in
        document order: the ``!`` open choices of sort ``sort`` of the
        arguments registered before it, its antecedents.  An argument
        with parameters introduces none: its open choice is one for all
        its instances (tasks.org, aida-referents-choice-under-parameter)."""
        found = []
        conclusions = {e.name: plain(getattr(e.term, "prop", "") or "")
                       for e in self.graph.edges if e.term is not None}
        for name, env in self.graph.referent_envs.items():
            if name == owner:
                break
            if name not in conclusions:
                continue
            stated = referents.mentioned(conclusions[name])
            if any(n in stated for n, _s in env):
                continue
            found += [(name, n) for n, s in env
                      if referents.is_presumed(n) and s is not None and sort_text(s) == sort]
        return found

    def _constants(self, sort) -> list:
        """The declared constants of ``sort``, as the sort node records
        them (core/dc/debate_graph.py, ``sort_individuals``)."""
        key = self.graph.sorts.get(sort) if sort else None
        if key is None:
            return []
        return [e.term.witness.name for e in self.graph.edges
                if e.target_key == key and e.strict and isinstance(e.term, TermsPairFO)
                and isinstance(e.term.witness, TSym)]

    def _show(self, statement):
        """A statement for a log message: ``Prop[t]`` / ``Prop[c]``."""
        key, side = statement
        return f"{self._prop(key)}[{side[0]}]"

    def _show_all(self, statements):
        return ", ".join(self._show(s) for s in statements) or "-"

    def _presumed(self, statement):
        """The canonical kind: a presumption if some argument of the
        document presumes the statement."""
        return "presumption" in self._defaults(statement)

    def _site(self, statement, presumed=None):
        key, side = statement
        prop = self._prop(key)
        number = f"{next(self._sites)}"
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

    def _scaffold_scope(self, statement, alt, beta, env):
        """The scope inside a scaffold's scion: on either
        side ``alt`` binds the contrary and ``b`` the statement itself, so
        a demand for either in the scion is captured by them like by any
        binder (aida-unfold-scaffold-binders-capture) - by the statement as
        stated at this use, in any context."""
        literal = self._literal(statement)
        return {**env, contrary(literal): alt, literal: beta}

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
        key = issue[0]
        if "r#" in key or "w#" in key or key.startswith("["):
            text, scope = self._root_frame(key)
            self._fo_here = scope
            term = self._framed(key, text, scope, lambda: self.statement(issue, {}, frozenset()))
            return self._in_context(self._eta_if_bare(issue, term), scope)
        term = self.statement(issue, {}, frozenset())
        return self._in_context(self._eta_if_bare(issue, term), ())

    def unfold_argument(self, edge):
        """The debate term for the argument whose document edge is
        ``edge`` (aida-unfold-entrypoints): the canonical shape of its
        statement with the argument itself on top of the supporter stack,
        and the sites of its own body keeping the kind the argument wrote."""
        issue = (edge.target_key, edge.target_side)
        if not any(e is edge for e in self._targeting(issue)):
            raise UnfoldError(f"'{edge.name}' is not an argument edge of the document.")
        logger.debug("unfold: argument '%s' on %s from a document of %d edge(s), %d node(s)",
                     edge.name, self._show(issue), len(self.graph.edges), len(self.graph.nodes))
        def build():
            return self._eta_if_bare(issue, self._debate(issue, {}, frozenset({issue}), top=edge))
        if "r#" in issue[0] or "w#" in issue[0] or issue[0].startswith("["):
            # a statement with referents or a context: read as the
            # document states it, each argument's referents renamed to its
            text, scope = self._root_frame(issue[0])
            self._fo_here = scope
            return self._in_context(self._framed(issue[0], text, scope, build), scope)
        return self._in_context(build(), ())

    def _in_context(self, term, scope):
        """The unfolded term with its propositions in context, so that the
        labelling reads each as the statement it is where it stands."""
        env = tuple(e for e in scope if e[1] is not None)
        return contextualise(term, env) if (has_context(term, env) or self.graph.referent_envs) else term

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
        literal = self._literal(statement)
        if literal in env:
            var = self._variable(statement, env[literal])
            if presumed:
                var.captured_presumption = True
            logger.debug("unfold: %s captured as '%s'%s", self._show(statement),
                         env[literal], " (presumption)" if presumed else "")
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
        edges = self._targeting(statement)
        root = self._site(statement, root_presumed)
        logger.debug("unfold: %s expanded, root %s (%s), %d supporter(s)%s%s",
                     self._show(statement), root.number, type(root).__name__, len(edges),
                     ", '%s' on top" % top.name if top is not None else "",
                     ", its presumption at the bottom" if bottom else "")
        term = self._supported(statement, root, edges, env, spine, top=top, bottom=bottom)
        contra = contrary(statement)
        attackers = self._targeting(contra)
        if attackers or self._defaults(contra):
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
        ordered = ([e for e in edges if not _same_edge(e, top)] + ([top] if top is not None else []))
        # each supporter once per way it lines up here (discourse referents)
        uses = [(e, inst) for e in ordered for inst in self._lineups(e, statement)]
        if top is not None and not any(e is top for e, _inst in uses):
            raise UnfoldError(f"'{top.name}' does not line up with {self._show(statement)}.")
        items = ([None] if bottom else []) + uses
        contra = contrary(statement)

        # Every scaffold's names are drawn when the stack is begun, before
        # any element is built; the innermost scaffold (the second item's)
        # is the first.
        wiring = [(self._fresh_alt(), self._fresh_beta()) for _ in items[1:]]
        scopes = ([{**env, contrary(self._literal(statement)): wiring[0][0]} if wiring else env]
                  + [self._scaffold_scope(statement, alt, beta, env) for alt, beta in wiring])

        def element(k):
            if items[k] is None:
                logger.debug("  unfold: %s +the presumption of %s, at the bottom",
                             self._show(statement), self._show(statement))
                return self._site(statement, True)
            item, inst = items[k]
            logger.debug("  unfold: %s +support '%s'%s", self._show(statement), item.name,
                         " (the argument unfolded, on top)" if item is top else "")
            return self._edge(item, scopes[k], spine, own=item is top, inst=inst)

        # The argument unfolded is expanded first, so that its own binders
        # keep their names and its sites the lowest numbers.
        built = ({len(items) - 1: element(len(items) - 1)}
                 if top is not None and items and items[-1] is not None and items[-1][0] is top
                 else {})

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
        return self._supported(contra, root, self._targeting(contra), env, spine)

    def _bare(self, statement, env):
        """The bare contrary's site, or the binder in scope that captures
        it (author, 2026-10-05).  The bare contrary stands for the default
        somebody holds on that statement, and a default meets a binder in
        scope exactly as a site in an edge body does: the binder takes it.
        It is never expanded - its debate is the attack being built.  The
        attack's own ``alt`` is always in scope."""
        literal = self._literal(statement)
        if literal in env:
            var = self._variable(statement, env[literal])
            defaults = self._defaults(statement)
            if "presumption" in defaults:
                var.captured_presumption = True
            # the default somebody holds on the contrary stays visible to
            # the labelling even where no edge records it (an attack whose
            # wing is nothing but this variable): the compiler marks it
            var.captured_default = tuple(sorted(defaults))
            logger.debug("unfold: the bare %s captured as '%s'", self._show(statement), env[literal])
            return var
        return self._site(statement)

    def _edge(self, edge, env, spine, own=False, inst=None):
        """An edge's body with every site unfolded in scope.  ``own``: the
        body of the argument the term is unfolded for, whose sites keep the
        kind it wrote.  An exposed node of the body (a lemma, a lambda, an
        axiom applied: ``_node_edge``) becomes the debate about its
        statement (``_exposed``)."""
        if edge.term is None:
            raise UnfoldError(f"Edge '{edge.name}' carries no term; it cannot be unfolded.")
        primary = own or self._has_primacy(edge)
        # Discourse referents (tasks.org, aida-discourse-referents): ``inst``
        # gives each referent of the edge's conclusion the term it stands
        # for here - substituted into every proposition of the copy before
        # its sites meet the binders in scope.  ``fo0``: the first-order
        # variables bound around the place the body goes.
        inst = dict(inst or {})
        # the argument's own open choices, the referents of its environment
        # its conclusion does not state, where no lineup gives them a value
        # here: a statement about one is this argument's (tasks.org,
        # aida-referents-instantiation, 2026-10-22)
        inst_vars = set().union(*(t.free_vars() for t in inst.values())) if inst else set()
        choices, ren0 = [], {}
        taken = {e[0] for e in self._fo_here} | inst_vars
        for e in self._own_witnesses(edge, inst):
            name = e[0]
            if name in taken:
                n = 2
                while f"{name}_{n}" in taken:
                    n += 1
                ren0[name] = name = f"{name}_{n}"
                logger.debug("unfold: '%s''s open choice %s renamed to %s (the name is taken here)",
                             edge.name, e[0], name)
            taken.add(name)
            choices.append(e.with_(name=name))
        own_names = {e[0] for e in choices}
        fo0 = tuple(self._fo_here) + tuple(choices)

        # ``ren``: the body's own first-order binders renamed where their
        # names are taken here (a variable a referent stands for, or one
        # bound around the place the body goes), applied before ``inst``.
        def ip(prop, ren=None):
            if not isinstance(prop, str):
                return prop
            if not (inst or ren):
                return plain(prop)
            parsed = Prop.parse(plain(prop))
            free = parsed.free_vars()
            if not any(name in free for name in (*(ren or {}), *inst)):
                return plain(prop)          # nothing to substitute: keep the spelling
            for name, new in (ren or {}).items():
                if name in parsed.free_vars():
                    parsed = parsed.subst(name, TSym(new))
            for name, term in inst.items():
                if name in parsed.free_vars():
                    parsed = parsed.subst(name, term)
            return str(parsed)

        def it(term, ren=None):
            if not (inst or ren) or term is None:
                return term
            for name, new in (ren or {}).items():
                term = term.subst(name, TSym(new))
            for name, value in inst.items():
                term = term.subst(name, value)
            return term

        def renamed(node, var, fo, ren):
            """``ren`` with the binder ``var`` renamed if its name is taken
            here, and the name it binds as."""
            in_scope = {n for n, _s in fo}
            if var not in inst_vars and var not in in_scope:
                return ({k: v for k, v in ren.items() if k != var} if ren and var in ren else ren), var
            taken = inst_vars | in_scope | _names_in(node) | set((ren or {}).values())
            n, new = 2, f"{var}_2"
            while new in taken:
                n += 1
                new = f"{var}_{n}"
            logger.debug("unfold: edge '%s' binder '%s' renamed to '%s' (the name is taken here)",
                         edge.name, var, new)
            return {**(ren or {}), var: new}, new

        def site(prop, side, env, fo, ren=None, **kw):
            """A site, or a variable an enclosing argument binds, met as
            its statement: captured by a binder of that very proposition
            in scope, whatever the context (weakening); else, under
            first-order binders or with discourse referents, the statement
            in the context it stands in - the export of the horn
            (tasks.org, aida-discourse-referents, 2026-10-20)."""
            literal = _with_witnesses(ip(prop, ren), fo)
            here = (canonical_prop(literal), side)
            saved, self._fo_here = self._fo_here, fo
            try:
                if here in env or not fo:
                    if fo or inst or "r#" in here[0] or here[0] not in self.graph.nodes:
                        # read as written here
                        return self._framed(here[0], literal, fo,
                                            lambda: self.statement(here, env, spine, **kw))
                    return self.statement(here, env, spine, **kw)
                key = canonical_prop(referents.InContext(literal, fo))
                return self._framed(key, literal, fo,
                                    lambda: self.statement((key, side), env, spine, **kw))
            finally:
                self._fo_here = saved
        # the variables the edge's body uses that an enclosing argument
        # binds (core/dc/debate_graph.py, ``variable``: a captured source)
        captured = {(s.side, s.site) for s in edge.sources
                    if s.kind in ("obligation", "presumption")}
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

        # how the sites of a witness scaffold's alternative being walked
        # take their kind: None as anywhere else; "written", the kind the
        # argument wrote (a ``?`` choice: its onus is about whoever
        # answers); "document", the kind the document gives the statement
        # (a ``!`` choice: its onus is about its own individual; tasks.org,
        # aida-referents-anaphora, 2026-10-22)
        alt_kind = [None]

        def own_kind(presumed):
            # the sites of the argument unfolded keep the kind it wrote
            if alt_kind[0] == "written":
                return {"own_kind": presumed}
            if alt_kind[0] == "document":
                return {}
            if own:
                return {"own_kind": presumed}
            # an instance keeps a presumption it wrote: the document marks
            # its premise so where it copies the edge to an instance it
            # holds (DebateGraph.add_edge), and a statement it does not
            # hold - a reading's - gets no marker (tasks.org,
            # aida-referents-anaphora-knowledge)
            if inst and presumed:
                return {"own_kind": True}
            return {}

        def scoped(env, statement, new):
            # an affine binder (``_``) binds nothing a site could be met by,
            # as for the compiler (``binder_statements``): a user's
            # ``cut (P) _.`` must not capture P's sites (tasks.org,
            # aida-underscore-binder)
            return env if _is_affine_binder(new) else {**env, statement: new}

        def walk(node, env, names, expose=True, fo=None, ren=None):
            """``expose``: whether this node itself may be exposed; its
            children are checked either way.  ``fo``: the first-order
            variables in scope."""
            fo = fo0 if fo is None else fo

            def p(prop):
                return _with_witnesses(ip(prop, ren), fo)

            exposed = self._node_edge.get(id(node)) if expose else None
            if exposed is not None and (inst or ren) and uncaught_referents(node) & {*inst, *(ren or {})}:
                # a horn about a referent read at this use - a parameter, or
                # an open choice an alternative of a witness scaffold
                # replaces - is a statement about what it stands for here:
                # inlined, as any horn mentioning a variable bound above it
                # (step 0, kept; tasks.org, aida-referents-open-horns,
                # 2026-10-22; aida-referents-anaphora)
                exposed = None
            if exposed is not None and node is not edge.term:
                rewalk = (lambda n, e, nm, expose=True: walk(n, e, nm, expose, fo, ren))
                if not fo:
                    return self._exposed(node, exposed, env, spine, names, primary, rewalk)
                # the horn in the context it stands in here
                literal = p(node.prop)
                key = canonical_prop(referents.InContext(literal, fo))
                statement = (key, exposed.target_side)
                saved, self._fo_here = self._fo_here, fo
                try:
                    return self._framed(key, literal, fo, lambda: self._exposed(
                        node, exposed, env, spine, names, primary, rewalk, statement=statement))
                finally:
                    self._fo_here = saved
            if isinstance(node, (Goal, Deleg)):
                presumed = isinstance(node, Deleg)
                return site(node.prop, "term", env, fo, ren=ren, presumed=presumed, **own_kind(presumed))
            if isinstance(node, (Laog, Geled)):
                presumed = isinstance(node, Geled)
                return site(node.prop, "context", env, fo, ren=ren, presumed=presumed, **own_kind(presumed))
            if isinstance(node, (ID, DI)) and getattr(node, "cites", None):
                # A citation (core/dc/cite.py) unfolds exactly like an
                # obligation site: the cited argument's edge derives that
                # statement, so the debate brings it in, strict or not.
                side = "context" if isinstance(node, ID) else "term"
                logger.debug("unfold: '%s' cites '%s'; expanded as its statement",
                             edge.name, node.cites)
                return site(node.prop, side, env, fo, ren=ren, **own_kind(False))
            if isinstance(node, (ID, DI)):
                vside = "context" if isinstance(node, ID) else "term"
                if (vside, node.name) not in names and (vside, node.name) in captured:
                    # A variable an enclosing argument binds, which the
                    # compiler recorded as a source at its binder's
                    # statement: unfolded as that statement in scope -
                    # captured again where the binder is in scope, the
                    # statement's debate where it is not (aida-lemma-
                    # exposure-scope: a lemma stacked outside its lambda).
                    presumed = getattr(node, "captured_presumption", False)
                    logger.debug("unfold: '%s' uses '%s', bound by an enclosing argument; "
                                 "unfolded as its statement", edge.name, node.name)
                    return site(node.prop, vside, env, fo, ren=ren, presumed=presumed, **own_kind(presumed))
                return type(node)(names.get((vside, node.name), node.name), p(node.prop))
            if isinstance(node, Lamda):
                new = fresh(node.di.di.name)
                inner = scoped(env, (canonical_prop(p(node.di.prop)), "term"), new)
                hyp = Hyp(DI(new, p(node.di.di.prop)), p(node.di.prop))
                body = Lamda(hyp, walk(node.term, inner, {**names, ("term", node.di.di.name): new},
                                       fo=fo, ren=ren))
                body.prop = p(node.prop)
                return body
            if isinstance(node, Mu):
                new = fresh(node.id.name)
                inner = scoped(env, (canonical_prop(p(node.prop)), "context"), new)
                names2 = {**names, ("context", node.id.name): new}
                return Mu(ID(new, p(node.id.prop)), p(node.prop),
                          walk(node.term, inner, names2, fo=fo, ren=ren),
                          walk(node.context, inner, names2, fo=fo, ren=ren))
            if isinstance(node, Mutilde):
                new = fresh(node.di.name)
                inner = scoped(env, (canonical_prop(p(node.prop)), "term"), new)
                names2 = {**names, ("term", node.di.name): new}
                return Mutilde(DI(new, p(node.di.prop)), p(node.prop),
                               walk(node.term, inner, names2, fo=fo, ren=ren),
                               walk(node.context, inner, names2, fo=fo, ren=ren))
            if isinstance(node, Admal):
                new = fresh(node.id.id.name)
                inner = scoped(env, (canonical_prop(p(node.id.prop)), "context"), new)
                pyh = Pyh(ID(new, p(node.id.id.prop)), p(node.id.prop))
                out = Admal(pyh, walk(node.context, inner,
                                      {**names, ("context", node.id.id.name): new}, fo=fo, ren=ren))
                out.prop = p(node.prop)
                return out
            if isinstance(node, Cons):
                out = Cons(walk(node.term, env, names, fo=fo, ren=ren), walk(node.context, env, names, fo=fo, ren=ren))
                out.prop = p(node.prop)
                return out
            if isinstance(node, Sonc):
                out = Sonc(walk(node.context, env, names, fo=fo, ren=ren), walk(node.term, env, names, fo=fo, ren=ren))
                out.prop = p(node.prop)
                return out
            # First order (tasks.org, aida-first-order-reduction): copied,
            # the variable keeping its name.  It joins the scope: a site
            # below that mentions it is a referent statement's instance.
            if isinstance(node, LamdaFO):
                ren2, var = renamed(node, node.var, fo, ren)
                out = LamdaFO(var, node.sort, walk(node.term, env, names,
                                                   fo=fo + ((var, sort_text(node.sort)),), ren=ren2))
            elif isinstance(node, ConsFO):
                chosen = it(node.fo_term, ren)
                if isinstance(chosen, TSym) and chosen.name in own_names:
                    return witness_scaffold(node, chosen.name, "context", env, names, fo, ren)
                out = ConsFO(chosen, walk(node.context, env, names, fo=fo, ren=ren))
            elif isinstance(node, TermsPairFO):
                chosen = it(node.witness, ren)
                if isinstance(chosen, TSym) and chosen.name in own_names:
                    return witness_scaffold(node, chosen.name, "term", env, names, fo, ren)
                out = TermsPairFO(chosen, walk(node.term, env, names, fo=fo, ren=ren))
            elif isinstance(node, DestructTermsPairFO):
                ren2, var = renamed(node, node.var, fo, ren)
                out = DestructTermsPairFO(var, node.sort,
                                          walk(node.context, env, names,
                                               fo=fo + ((var, sort_text(node.sort)),), ren=ren2))
            else:
                raise UnfoldError(f"Edge '{edge.name}': cannot unfold node {type(node).__name__}.")
            out.prop = p(node.prop)
            return out

        def witness_scaffold(node, wname, side, env, names, fo, ren):
            """The witness scaffold (tasks.org, aida-referents-witness-scaffold,
            aida-referents-instantiation, 2026-10-21/22): where the argument
            names its own open choice ``wname`` - an existential right
            (``side`` "term", the pair ``(?w, v)``) or a universal left
            ("context", the cell ``?w * e``) - a support scaffold at the
            quantified statement in its context.  Its root is the open
            choice itself; its alternatives, each a whole pair (cell) with
            the body read for one individual, are those ``_alternatives``
            gives: constants, earlier arguments' ``!`` choices, the
            variables binders introduced in scope, the innermost on top.
            An alternative's sites keep the kind the argument wrote for a
            ``?`` choice (a pronoun) and take the document's for a ``!``
            choice (an indefinite; aida-referents-anaphora, 2026-10-22)."""
            body = node.term if side == "term" else node.context
            prop = _with_witnesses(ip(node.prop, ren), fo)
            try:
                quant = Prop.parse(plain(node.prop))
            except Exception:                   # noqa: BLE001 - no quantified statement
                quant = None
            sort = sort_text(getattr(quant, "sort", None)) if quant is not None else None

            def pair(value, ren2, env2, fo2=None):
                inner = walk(body, env2, names, fo=fo if fo2 is None else fo2, ren=ren2)
                out = (TermsPairFO(TSym(value), inner) if side == "term"
                       else ConsFO(TSym(value), inner))
                # an individual borrowed from an earlier argument: its entry
                # in the pair's context, so that the pair rests on what its
                # owner presumes of it, the sort node (debate_graph.py,
                # ``_choice``; core/comp/standing.py, ``_open_choice``)
                out.prop = prop if fo2 is None else _with_witnesses(ip(node.prop, ren), fo2)
                return out

            candidates = self._alternatives(body, wname, sort, fo, lambda text: ip(text, ren),
                                            owner=edge.name) if sort else []
            alternatives = [c for c, _e in candidates]
            key = (canonical_prop(referents.InContext(plain(prop), fo)) if fo
                   else canonical_prop(prop))
            statement = (key, side)

            def build():
                root = pair(wname, ren, env)
                if not alternatives:
                    return root
                logger.debug("unfold: '%s' chooses %s at %s; alternatives %s",
                             edge.name, wname, self._show(statement), ", ".join(alternatives))
                alt0, beta0 = self._fresh_alt(), self._fresh_beta()
                scope0 = self._scaffold_scope(statement, alt0, beta0, env)
                wiring = [(self._fresh_alt(), self._fresh_beta()) for _ in alternatives[1:]]
                scopes = ([{**scope0, contrary(self._literal(statement)): wiring[0][0]}
                           if wiring else scope0]
                          + [self._scaffold_scope(statement, a, b, scope0) for a, b in wiring])

                def element(k):
                    value, entry = candidates[k]
                    saved_kind = alt_kind[0]
                    alt_kind[0] = "document" if referents.is_presumed(wname) else "written"
                    try:
                        out = pair(value, {**(ren or {}), wname: value}, scopes[k],
                                   fo + (entry,) if entry is not None else None)
                    finally:
                        alt_kind[0] = saved_kind
                    mark_instantiated(out)
                    return out

                stack = element(0)
                for k in range(1, len(alternatives)):
                    stack = self._support(statement, stack, element(k), *wiring[k - 1])
                return self._support(statement, root, stack, alt0, beta0)

            saved, self._fo_here = self._fo_here, fo
            try:
                return self._framed(key, prop, fo, build)
            finally:
                self._fo_here = saved

        copy = walk(edge.term, env, {}, ren=ren0 or None)
        if inst or ren0:
            # an instance placed here, or a placement whose open choice was
            # renamed: the strict phase reads its edges off the term
            # (core/dc/strict.py, ``instance_edges``)
            mark_instantiated(copy)
        if isinstance(copy, (Mu, Mutilde)):
            # the copy's root binder may be renamed (a1_0_2); it still
            # records the argument a1_0 (aida-fold-occurrences-names)
            copy.origin = edge.name
        return copy

    def _exposed(self, node, sub, env, spine, names, primary, walk, statement=None):
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
          at its registered position.

        ``statement``: the node's statement as stated at this use - in the
        context there (tasks.org, aida-discourse-referents) - by default
        the document's."""
        if statement is None:
            statement = (sub.target_key, sub.target_side)
        contra = contrary(statement)
        literal = self._literal(statement)
        if literal in env:
            keep = (self.subarguments == "preserve"
                    or (self.subarguments == "default" and primary))
            captured = self._variable(statement, env[literal])
            if not keep:
                logger.debug("unfold: node '%s' (%s) captured as '%s'",
                             sub.name, self._show(statement), env[literal])
                return captured
            alt, beta = self._fresh_alt(), self._fresh_beta()
            logger.debug("unfold: node '%s' (%s) kept on top of the captured '%s' (alt %s, b %s)",
                         sub.name, self._show(statement), env[literal], alt, beta)
            own = walk(node, self._scaffold_scope(statement, alt, beta, env), names, False)
            term = self._support(statement, captured, own, alt, beta)
            if (self.subarguments == "preserve" and env[literal] in self._scaffold_vars
                    and (self._targeting(contra) or self._defaults(contra))):
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
        others = [e for e in self._targeting(statement) if not _same_edge(e, sub)]
        return bool(others or self._targeting(contra) or self._defaults(contra))


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
