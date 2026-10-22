"""Debate-graph compilation: Layers 2-3 of debate-graph-spec.org.

This module is being built along propositional-fragment-plan.org:

- M2: canonical proposition identity.  Node identity in the debate graph
  is the parsed Prop modulo alpha-equivalence, with negation unfolded:
  ``~A`` is definitionally ``A -> false`` in the calculus, and the graph
  must give both spellings one node (the NAF fixtures write the
  implication form; contrary-detection needs them identified).  Keys are
  internal to this module: they are stable identifiers, not display text.
- M3: the DebateGraph structure and the compiler.  Terms are ground truth:
  the compiler decomposes composed debate terms by matching the four
  support/attack scaffold shapes that the debate operations actually
  produce (locked against generated output, see the shape catalogue at
  ``_match_scaffold``), and refuses what it does not recognise rather
  than guessing.
"""

import logging
import re
from copy import deepcopy
from dataclasses import dataclass, field, replace
from functools import lru_cache
from itertools import count
from typing import Optional

from core.ac.ast import (
    ProofTerm, Mu, Mutilde, Lamda, Admal, Cons, Sonc,
    Goal, Laog, Deleg, Geled, ID, DI,
    LamdaFO, ConsFO, TermsPairFO, DestructTermsPairFO,
    first_order_node, FirstOrderNotSupported,
)
from core.ac import referents
from core.ac.prop import (
    Prop, PTrue, PFalse, PSym, PApp, PNeg, PBin, PQuant, BinOp, PropError, TSym,
)
from core.logging_util import TRACE, artifact
from core.dc.discipline import free_variables, _is_negation_wrapper, _occurrences

logger = logging.getLogger(__name__)


def _unfold_neg(p: Prop) -> Prop:
    """Structurally rewrite every ``PNeg(b)`` into ``b -> false``."""
    if isinstance(p, PNeg):
        return PBin(_unfold_neg(p.body), BinOp.IMP, PFalse())
    if isinstance(p, PBin):
        return PBin(_unfold_neg(p.left), p.op, _unfold_neg(p.right))
    if isinstance(p, PApp):
        return PApp(_unfold_neg(p.pred), p.arg)
    if isinstance(p, PQuant):
        return PQuant(p.quantifier, p.names, p.sort, _unfold_neg(p.body))
    # PTrue, PFalse, PSym carry no subpropositions.
    return p


def canonical_prop(text: str) -> str:
    """The graph-node identity key for a proposition string; for a
    proposition in context (``referents.InContext``) the statement in
    that context (``context_key``).

    Parse (Fellowship's printed syntax), unfold negation, and take the
    alpha-invariant canonical rendering.  Whitespace and parenthesisation
    differences, bound-variable names, and the ``~A`` / ``A -> false``
    spelling all map to one key; genuinely different propositions (e.g.
    ``A-(B->C)`` vs ``(A-B)->C``) map to different keys.

    Raises PropError on unparsable input: an unreadable proposition must
    never silently become its own node.

    Cached: a document spells the same few propositions over and over, and
    each call would otherwise run the Earley parser (the suite's hottest
    path before the cache).  The result is a pure function of ``text``; a
    PropError is not cached and is raised again on the next call.
    """
    scope = getattr(text, "scope", None)
    if scope:
        return context_key(str(text), scope)
    return _canonical_text(text)


@lru_cache(maxsize=None)
def _canonical_text(text: str) -> str:
    return _unfold_neg(Prop.parse(text)).canonical()


#: A context key: the sorts of the context entries the statement does not
#: mention, then the statement (its mentioned entries made referents).
_CONTEXT_SEP = "]⊢"


@lru_cache(maxsize=None)
def context_key(text: str, scope: tuple) -> str:
    """The key of the statement ``text`` in the context ``scope`` (context
    entries, ``referents.Entry``, outermost first) - tasks.org,
    aida-discourse-referents, decisions of 2026-10-20, and
    aida-referents-instantiation, 2026-10-22.  A binder entry the
    statement mentions is a referent of it, so identity up to renaming
    lines it up (a); one it does not mention contributes its sort, kept
    as a multiset for weakening.  A witness entry the statement mentions
    is rigid and its argument's (``referents.rigid``): a statement about
    one argument's open choice is never another's; one it does not
    mention is no part of the statement.  An empty context, or one the
    statement mentions entirely, gives the plain key."""
    parsed = _unfold_neg(Prop.parse(text))
    free = parsed.free_vars()
    unmentioned = []
    for item in scope:
        name, sort = item
        if referents.is_witness(item):
            if name in free:
                parsed = parsed.subst(name, TSym(referents.rigid(name, item.owner)))
            continue
        if name in free:
            if not referents.is_referent(name):
                # a referent for the key only, distinct from any ?name
                parsed = parsed.subst(name, TSym(f"?{name}'"))
        else:
            unmentioned.append(sort_text(sort))
    base = parsed.canonical()
    if not unmentioned:
        return base
    return "[" + ", ".join(sorted(unmentioned)) + _CONTEXT_SEP + base


def sort_text(sort) -> str:
    """A sort as keys and lineups compare it: without spaces."""
    return "".join(str(sort).split()) if sort is not None else ""


def split_context_key(key: str):
    """(the unmentioned sorts, the statement's key) of a key."""
    if key.startswith("[") and _CONTEXT_SEP in key:
        head, base = key[1:].split(_CONTEXT_SEP, 1)
        return tuple(head.split(", ")), base
    return (), key


def sub_multiset(small, big) -> bool:
    """Whether the sorts ``small`` fit injectively into ``big``."""
    rest = list(big)
    for sort in small:
        if sort not in rest:
            return False
        rest.remove(sort)
    return True


def context_form(text: str, scope) -> str:
    """``text`` with each bound variable of ``scope`` it mentions shown as
    the referent standing for it: ``Dr z -> false`` under ``z`` reads
    ``Dr ?z -> false``."""
    names = [name for name, _sort in scope if not referents.is_referent(name)]
    if not names:
        return str(text)
    parsed = Prop.parse(str(text))
    free = parsed.free_vars()
    for name in names:
        if name in free:
            parsed = parsed.subst(name, TSym(referents.constant(name)))
    return str(parsed)


def contextualise(term, env=()):
    """``term`` with every proposition as stated where it stands
    (``referents.InContext``): its context the discourse referents
    ``env`` (context entries) and the first-order variables bound above
    it.  A variable occurrence states its binder's proposition, in its
    binder's context.  A proposition that already carries witness entries
    keeps them (a placed argument's own open choices, core/dc/unfold.py).
    Propositions in an empty context stay plain.  In place; returns
    ``term``."""
    env = tuple(referents.entry(e).with_(sort=sort_text(e[1])) for e in env)

    def at(prop, scope):
        if not isinstance(prop, str) or not prop:
            return prop
        names = {e[0] for e in scope}
        extra = tuple(e for e in getattr(prop, "scope", ())
                      if referents.is_witness(e) and e[0] not in names)
        scope = scope + extra
        return referents.InContext(str(prop), scope) if scope else str(prop)

    def walk(node, scope, bound):
        if isinstance(node, (ID, DI)):
            side = "context" if isinstance(node, ID) else "term"
            found = bound.get((side, node.name))
            if found is not None and isinstance(node.prop, str) and str(found) == str(node.prop):
                node.prop = found
            elif isinstance(node.prop, str):
                # a name the term does not bind - a declared axiom, a
                # citation - is global: its statement is the closed one
                node.prop = str(node.prop)
            return
        if hasattr(node, "prop"):
            node.prop = at(node.prop, scope)
        inner_bound = bound
        if isinstance(node, Mu):
            node.id.prop = at(node.id.prop, scope)
            inner_bound = {**bound, ("context", node.id.name): node.id.prop}
        elif isinstance(node, Mutilde):
            node.di.prop = at(node.di.prop, scope)
            inner_bound = {**bound, ("term", node.di.name): node.di.prop}
        elif isinstance(node, Lamda):
            node.di.prop = at(node.di.prop, scope)
            node.di.di.prop = at(node.di.di.prop, scope)
            inner_bound = {**bound, ("term", node.di.di.name): node.di.di.prop}
        elif isinstance(node, Admal):
            node.id.prop = at(node.id.prop, scope)
            node.id.id.prop = at(node.id.id.prop, scope)
            inner_bound = {**bound, ("context", node.id.id.name): node.id.id.prop}
        inner_scope = scope
        if isinstance(node, (LamdaFO, DestructTermsPairFO)):
            inner_scope = scope + (referents.Entry(node.var, sort_text(node.sort)),)
        for child in vars(node).values():
            if isinstance(child, ProofTerm):
                walk(child, inner_scope, inner_bound)

    walk(term, env, {})
    return term


def referent_scope(term) -> list:
    """[(name, sort)]: every discourse referent the propositions of a
    contextualised ``term`` carry in their contexts, the first sort found
    for each - the referent environment the term replays in (the root's,
    and the open choices of the arguments placed in it)."""
    found = {}
    stack = [term]
    while stack:
        node = stack.pop()
        for value in vars(node).values():
            if isinstance(value, ProofTerm):
                stack.append(value)
            elif isinstance(value, str):
                for e in getattr(value, "scope", ()):
                    if referents.is_referent(e[0]) and e[1]:
                        found.setdefault(e[0], e[1])
    return sorted(found.items())


def has_context(term, env=()) -> bool:
    """Whether some proposition of ``term`` stands in a context: there is
    a referent environment or a first-order binder."""
    if env:
        return True
    stack = [term]
    while stack:
        node = stack.pop()
        if isinstance(node, (LamdaFO, DestructTermsPairFO)):
            return True
        stack.extend(c for c in vars(node).values() if isinstance(c, ProofTerm))
    return False


@lru_cache(maxsize=None)
def display_prop(text: str) -> str:
    """The display normalization: parse and re-render with the printer.

    Unlike ``canonical_prop`` this keeps the author's negation spelling;
    it only normalizes whitespace and parenthesisation.  Cached like
    ``canonical_prop``.
    """
    return str(Prop.parse(text))


# ---------------------------------------------------------------------------
# M3: graph structure
# ---------------------------------------------------------------------------

class DebateCompileError(ValueError):
    """The compiler met a term it does not understand; refuse, never guess."""


class CyclicDebateNotSupported(DebateCompileError):
    """Retained for callers that still import it.  Since the cyclic
    fragment (tasks.org, aida-cyclic-fragment, 2026-09-16) compilation no
    longer raises it: ``DebateGraph.is_acyclic`` classifies a debate, the
    labelling decides it.  Nothing in the tree raises this any more."""
    """The compiled graph has a propositional dependency cycle.

    The cycle-free fragment (propositional-fragment-plan.org) refuses these;
    cyclic debates need the witness-labelling machinery of the full Layer 3.
    """


@dataclass(frozen=True)
class Source:
    """One unwitnessed input of a hyperedge.

    ``kind``: "obligation" (Goal/Laog: burden on the proponent, default
    OUT), "presumption" (Deleg/Geled: burden delegated, default IN),
    "subargument" (a lambda subargument of the same term, compiled as
    its own edge; ``site`` is that edge's name) or "assumption" (a free,
    undeclared variable; unused).  ``side`` is the side the
    site sits on ("term" = a proof of key is needed, "context" = a
    refutation).  ``site`` is the site number / variable name, diagnostic
    only.  ``scope`` records the propositions mu/mu'-bound on the spine
    from the edge's root to the site, with binding side - the capture
    annotation of debate-graph-spec.org (unused by the acyclic fragment,
    recorded for the general Layer 3).
    """

    key: str
    side: str
    kind: str
    site: str
    scope: tuple = ()
    #: The label of the occurrence in a labelled term (core/comp/labelled.py),
    #: which sigma reads; not part of the source's identity.
    label: object = field(default=None, compare=False, repr=False)
    #: The proposition as the body states it, in its context: what an
    #: instance of the edge substitutes into (``DebateGraph.instantiate``).
    prop: object = field(default=None, compare=False, repr=False)


@dataclass(frozen=True)
class Edge:
    """A hyperedge: one subargument deriving its target from its sources.

    ``target_side`` "term" = an argument for the target proposition,
    "context" = an argument against it.  ``strict`` = no obligation or
    presumption among the sources.  ``role`` is diagnostic ("argument",
    "supporter", "attacker"): the graph meaning of attack vs support is
    already carried by target_side.
    """

    name: str
    target_key: str
    target_side: str
    sources: tuple
    strict: bool
    role: str = "argument"
    #: The subargument's body, for unfolding (core/dc/unfold.py); not part
    #: of the edge's identity.
    term: object = field(default=None, compare=False, repr=False)
    #: The label of the body's root in a labelled term; not part of the
    #: edge's identity.
    target_label: object = field(default=None, compare=False, repr=False)
    #: For an instance of an edge (``DebateGraph.instantiate``): the
    #: generic statement it was copied from, and the terms its variables
    #: receive, ((name, term), ...).
    generic: object = field(default=None, compare=False, repr=False)
    inst: tuple = field(default=(), compare=False, repr=False)


@dataclass(frozen=True)
class _Acc:
    """What a body accumulates while compiling: its sources, and whether a
    subargument it applies is defeasible."""

    sources: tuple = ()
    defeasible: bool = False

    def __add__(self, other):
        return _Acc(self.sources + other.sources, self.defeasible or other.defeasible)


_EMPTY = _Acc()


# How to_dot draws an edge of each role, and what the legend says of it.
_DOT_ROLE_COLOUR = {"supporter": "darkgreen", "attacker": "red", "subargument": "gray40",
                    "strict": "black", "weakening": "gray60", "instantiation": "gray60"}
_DOT_ROLE_TEXT = {"argument": "argument", "supporter": "supporter",
                  "attacker": "attacker:\\nderives\\nthe contrary",
                  "subargument": "subargument:\\npart of an\\nargument, or a\\ndeclared axiom",
                  "strict": "strict edge\\nread off the\\ndebate term",
                  "weakening": "weakening:\\na smaller context\\nbears on a\\nlarger one",
                  "instantiation": "instantiation:\\na generic statement\\nbears on its\\ninstances"}
_DOT_FILL = {"IN": "#cdebc5", "OUT": "#f2c4c4", "UNDEC": "#e0e0e0"}


class DebateGraph:
    """Nodes (canonical props), hyperedges, and per-node default markers.

    ``defaults`` maps (key, side) to the set of site kinds ("obligation" /
    "presumption") with which any edge references that node - the data the
    Layer-3 acceptance conditions derive default literals from.  Identity
    edges (an eta argument whose only source is its own target) contribute
    a default marker instead of an edge: they are bureaucratic, not
    logically progressing, and would otherwise fake dependency cycles.
    """

    def __init__(self):
        self.nodes: dict[str, str] = {}
        self.edges: list[Edge] = []
        self.defaults: dict[tuple, set] = {}
        #: The context a node was first stated in, ((name, sort), ...), its
        #: bound variables shown as referents - for nodes with one only
        #: (tasks.org, aida-discourse-referents).
        self.contexts: dict[str, tuple] = {}
        #: The referent environment of each argument, by edge name.
        self.referent_envs: dict[str, tuple] = {}
        #: Whether some node is a statement in a context with unmentioned
        #: entries (``context_key``): only then are there weakening edges.
        self.has_contexts = False
        #: The default markers an argument asserts of its own conclusion (an
        #: argument that presumes or owes its statement and nothing else):
        #: what reaches the instances of a generic statement, unlike a
        #: marker a site sets (tasks.org, aida-referents-instantiation).
        self.asserted: dict[tuple, set] = {}
        #: (generic, instance) for every instance nearest to its generic
        #: statement, for display; the edges themselves are copied.
        self.instantiations: list = []
        #: The sort nodes, {sort: key} (``referents.sort_statement``).
        self.sorts: dict = {}

    # -- construction -------------------------------------------------------

    def add_node(self, prop_text: str) -> str:
        key = canonical_prop(prop_text)
        scope = getattr(prop_text, "scope", None)
        if key.startswith("["):
            self.has_contexts = True
        if scope:
            # a witness entry the statement does not mention is no part of it
            free = _prop_names(str(prop_text))
            scope = tuple(e for e in scope if not (referents.is_witness(e) and e[0] not in free))
        if scope:
            self.nodes.setdefault(key, display_prop(context_form(prop_text, scope)))
            self.contexts.setdefault(key, tuple(referents.entry(e).with_(name=referents.shown(e[0]))
                                                for e in scope))
        else:
            self.nodes.setdefault(key, display_prop(str(prop_text)))
        return key

    def shown(self, key: str, side: Optional[str] = None) -> str:
        """A node as the graph shows it: with its context, if it has one.
        A variable a binder introduced reads ``z?`` or, where the statement
        is presumed, ``z!``; a referent reads as written (tasks.org,
        aida-referents-instantiation, 2026-10-22)."""
        prop = self.nodes.get(key, key)
        scope = self.contexts.get(key)
        if not scope:
            return prop
        sides = (side,) if side else ("term", "context")
        presumed = any("presumption" in self.defaults.get((key, s), ()) for s in sides)
        names = []
        for e in scope:
            disp = referents.display_name(e, presumed)
            if disp != e[0]:
                prop = re.sub(r"(?<![\w?!])" + re.escape(e[0]) + r"(?![\w'])", disp, prop)
            names.append(f"{disp}:{e[1]}")
        return "[" + ", ".join(names) + "] ⊢ " + prop

    def weaken(self) -> None:
        """The weakening edges (tasks.org, aida-discourse-referents,
        2026-10-20): a statement in a smaller context bears on it in a
        larger one, on either side - whatever supports or attacks P supports
        or attacks [Γ] ⊢ P.  One edge from each node to every node whose
        context extends it by the least, recomputed whenever the graph
        grows.  An edge, so that labelling and reachability see it; the
        unfolder looks the smaller contexts up itself."""
        self.edges = [e for e in self.edges if e.role != "weakening"]
        bases = {}
        for key in self.nodes:
            sorts, base = split_context_key(key)
            bases.setdefault(base, []).append((key, sorts))
        materialised = None
        for base, members in bases.items():
            if len(members) < 2:
                continue
            if materialised is None:
                materialised = set(self.statements())
            for big, big_sorts in members:
                smaller = [(k, s) for k, s in members
                           if k != big and len(s) < len(big_sorts) and sub_multiset(s, big_sorts)]
                least = [(k, s) for k, s in smaller
                         if not any(len(s) < len(s2) and sub_multiset(s, s2) for _k2, s2 in smaller)]
                for small, _sorts in least:
                    for side in ("term", "context"):
                        if (small, side) in materialised:
                            self.edges.append(Edge(
                                "weakening", big, side,
                                (Source(small, side, "obligation", "weakening"),),
                                False, role="weakening"))

    def instantiate(self, rounds: int = 6) -> None:
        """The instances of generic statements (tasks.org,
        aida-referents-instantiation, decided 2026-10-21).  A statement with
        variables - parameters, or variables a binder introduced - covers
        every statement of the graph that is it with terms for them
        (``core.dc.matching``), in a context at least as large (with
        weakening).  Influence flows down only: each edge deriving the
        generic statement or its contrary, and each default marker an
        argument asserts of it, is copied to every instance, its sources
        instantiated alike, so that a supporter or attacker of the generic
        statement bears on each instance and an attack on one instance on
        that instance alone (per-instance defeat, question 3).  A marker a
        site sets on the generic statement - an argument's own premise
        about its parameter - stays there.  The copies' new sources are
        nodes whose instances are found in the next round.  Recomputed
        whenever the graph grows."""
        from core.dc import matching
        self.edges = [e for e in self.edges if e.generic is None]
        self.instantiations = []
        originals = {}
        for e in self.edges:
            # an argument's own conclusion; its parts come with it
            if e.role in ("weakening", "presumed", "subargument") or e.term is None:
                continue
            originals.setdefault(e.target_key, []).append(e)
        patterns, instances, matched = {}, {}, set()

        def pattern_of(key):
            if key not in patterns:
                patterns[key] = matching.pattern(self.nodes.get(key, key), self.contexts.get(key, ()))
            return patterns[key]

        def instance_of(key):
            if key not in instances:
                instances[key] = matching.instance(self.nodes.get(key, key), self.contexts.get(key, ()))
            return instances[key]

        copies = 0
        for _round in range(rounds):
            before = len(self.nodes)
            keys = list(self.nodes)
            for g in [k for k in keys if "r#" in k]:
                found = pattern_of(g)
                if found is None:
                    continue
                gprop, gvars = found
                gsorts, gbase = split_context_key(g)
                for i in keys:
                    if i == g or (g, i) in matched:
                        continue
                    isorts, ibase = split_context_key(i)
                    if ibase == gbase or not sub_multiset(gsorts, isorts):
                        continue
                    iprop = instance_of(i)
                    if iprop is None or matching.match(gprop, iprop, gvars) is None:
                        continue
                    matched.add((g, i))
                    for side in ("term", "context"):
                        for kind in self.asserted.get((g, side), ()):
                            self.mark_default(i, side, kind)
                        for e in originals.get(g, ()):
                            if e.target_side != side:
                                continue
                            for copy in self._instance_edge(e, i, iprop):
                                if not self._holds(copy):
                                    self.add_edge(copy)
                                    copies += 1
            if len(self.nodes) == before:
                break
        else:
            logger.warning("compile: instances of generic statements still growing after %d "
                           "rounds; stopped", rounds)
        self.instantiations = [(g, i) for g, i in matched
                               if not any((g, h) in matched and (h, i) in matched
                                          for h in self.nodes if h not in (g, i))]
        if copies:
            logger.debug("compile: %d instance edge(s) copied from generic statements, "
                         "%d instantiation(s)", copies, len(self.instantiations))

    def _instance_edge(self, edge: "Edge", key: str, target) -> list:
        """``edge``, which derives a generic statement, as it derives the
        instance ``key`` (``target``: its statement, names fixed), with the
        parts of its argument (its horns, ``subargument`` sources) placed
        alike - a part bears on an instance only where its argument is
        placed.  [] if its conclusion or a source cannot be instantiated."""
        from core.dc import matching
        concl = getattr(edge.term, "prop", None)
        found = matching.pattern(concl, getattr(concl, "scope", ())) if concl else None
        if found is None:
            return []
        eprop, evars = found
        sigma = matching.match(eprop, target, evars)
        if sigma is None:
            return []
        owners = {}
        values = {n: matching.unrigid(t, owners) for n, t in sigma.items()}
        named = set().union(*(t.free_vars() for t in values.values())) if values else set()
        # the entries of the instance's context the terms mention
        extra = tuple(e for e in self.contexts.get(key, ()) if e[0] in named)
        for name, owner in owners.items():
            if not any(e[0] == name for e in extra):
                extra += (referents.Entry(name, None, "witness", owner),)
        return self._placed(edge, key, values, extra, depth=0)

    def _placed(self, edge, key, values, extra, depth) -> list:
        """``edge`` with its variables given ``values``, deriving ``key``,
        and its parts likewise (``_instance_edge``)."""
        from core.dc import matching
        if depth > 32:
            return []
        out, sources = [], []
        for source in edge.sources:
            if source.prop is None:
                if "r#" in source.key or "w#" in source.key or source.key.startswith("["):
                    return []
                sources.append(source)
                continue
            text = referents.plain(source.prop)
            scope = getattr(source.prop, "scope", ())
            try:
                p = Prop.parse(text)
            except PropError:
                return []
            here = {n: v for n, v in values.items() if n in p.free_vars()}
            kept = tuple(e for e in scope if e[0] not in values)
            if not here and len(kept) == len(scope):
                new = source
            else:
                p = matching.subst_all(p, here)
                new_scope = kept + tuple(e for e in extra if all(e[0] != k[0] for k in kept))
                new_text = referents.InContext(str(p), new_scope) if new_scope else str(p)
                new = replace(source, key=self.add_node(new_text), prop=new_text)
            sources.append(new)
            if source.kind == "subargument" and new.key != source.key:
                for part in self.edges:
                    if (part.generic is None and part.name == source.site
                            and part.target_key == source.key and part.target_side == source.side):
                        out.extend(self._placed(part, new.key, values, extra, depth + 1))
        out.insert(0, replace(edge, target_key=key, sources=tuple(sources), generic=edge.target_key,
                              inst=tuple(values.items())))
        return out

    def mark_default(self, key: str, side: str, kind: str) -> None:
        if kind in ("obligation", "presumption"):
            self.defaults.setdefault((key, side), set()).add(kind)

    def add_edge(self, edge: Edge) -> None:
        for source in edge.sources:
            self.mark_default(source.key, source.side, source.kind)
        if (
            len(edge.sources) == 1
            and edge.sources[0].key == edge.target_key
            and edge.sources[0].side == edge.target_side
        ):
            if edge.generic is None and edge.sources[0].kind in ("obligation", "presumption"):
                self.asserted.setdefault((edge.target_key, edge.target_side), set()).add(
                    edge.sources[0].kind)
            if not getattr(self, "quiet", False):
                logger.debug("compile: '%s' is an identity edge on %s; kept as a default marker only",
                             edge.name, self._show_edge_target(edge))
            return  # identity edge: default marker only
        if self._holds(edge):
            return                              # the same axiom used twice
        if not getattr(self, "quiet", False):
            logger.debug("compile: edge '%s' (%s, %s) %s <- %s", edge.name, edge.role,
                         "strict" if edge.strict else "defeasible",
                         self._show_edge_target(edge), self._show_sources(edge))
        self.edges.append(edge)

    def _holds(self, edge: Edge) -> bool:
        """Whether an edge of the same name, statement, sources and
        strictness is already present: a declared name used in several
        places is one axiom edge."""
        return any(e.name == edge.name and e.target_key == edge.target_key
                   and e.target_side == edge.target_side and e.strict == edge.strict
                   and [(s.key, s.side, s.kind) for s in e.sources]
                   == [(s.key, s.side, s.kind) for s in edge.sources]
                   for e in self.edges)

    # -- rendering for log messages ----------------------------------------

    def _show(self, key, side) -> str:
        return f"{self.shown(key, side)}[{side[0]}]"

    def _show_edge_target(self, edge: Edge) -> str:
        return self._show(edge.target_key, edge.target_side)

    def _show_sources(self, edge: Edge) -> str:
        return ", ".join(
            f"{self._show(s.key, s.side)}:{s.kind[:4]}"
            + (f"@{s.site}" if getattr(s, "site", None) else "")
            for s in edge.sources
        ) or "-"

    def merge(self, other: "DebateGraph") -> None:
        for key, display in other.nodes.items():
            self.nodes.setdefault(key, display)
        for key, scope in getattr(other, "contexts", {}).items():
            self.contexts.setdefault(key, scope)
        # One naming (tasks.org, aida-referents-node-names): a statement with
        # referents an argument concludes is shown as the first argument
        # concluding it states it, whoever registered the node first - its
        # witness then reads as the argument's own environment names it.
        named = getattr(self, "_named", None)
        if named is None:
            named = self._named = set()
        for edge in other.edges:
            key = edge.target_key
            if (edge.role == "argument" and key not in named and key in other.contexts
                    and ("r#" in key or key.startswith("["))):
                self.nodes[key] = other.nodes[key]
                self.contexts[key] = other.contexts[key]
                named.add(key)
        self.referent_envs.update(getattr(other, "referent_envs", {}))
        for (key, side), kinds in other.defaults.items():
            self.defaults.setdefault((key, side), set()).update(kinds)
        for (key, side), kinds in getattr(other, "asserted", {}).items():
            self.asserted.setdefault((key, side), set()).update(kinds)
        self.sorts.update(getattr(other, "sorts", {}))
        self.edges.extend(e for e in other.edges
                          if e.role != "weakening" and e.generic is None and not self._holds(e))
        self.has_contexts = self.has_contexts or getattr(other, "has_contexts", False)
        if any("r#" in key for key in self.nodes):
            self.instantiate()
        if self.has_contexts:
            self.weaken()

    # -- structure ----------------------------------------------------------

    def parents(self, key: str) -> set:
        """Propositions the given one depends on through any edge.

        Sides are dropped on purpose.  Merging the two contrary statements
        of a proposition contracts every contrariness link, so a cycle over
        these parents is exactly a derivation cycle in the author's sense:
        a cycle through derivation edges and contrariness links that
        contains at least one derivation edge.  A contrariness-only cycle
        (a plain rebuttal) contracts to a loop-free node and is admitted.
        Since aida-cyclic-fragment (2026-09-16) ``is_acyclic`` is a
        classifier - which fragment a debate is in - and no longer a
        refusal: cyclic graphs are labelled by adf-bdd like any other.
        """
        out = set()
        for edge in self.edges:
            if edge.target_key == key:
                out |= {s.key for s in edge.sources}
        return out

    def reachable(self, statement) -> set:
        """Statements connected to ``statement``: through the sources of
        the edges deriving each statement and through contrariness - the
        rational closure of the spec, as reachability."""
        materialized = set(self.statements())
        seen, todo = set(), [statement]
        while todo:
            s = todo.pop()
            if s in seen:
                continue
            seen.add(s)
            for edge in self.edges:
                if (edge.target_key, edge.target_side) == s:
                    todo.extend((src.key, src.side) for src in edge.sources)
            c = (s[0], "context" if s[1] == "term" else "term")
            if c in materialized:
                todo.append(c)
        return seen

    def is_acyclic(self) -> bool:
        WHITE, GRAY, BLACK = 0, 1, 2
        color = {k: WHITE for k in self.nodes}

        def visit(k) -> bool:
            color[k] = GRAY
            for p in self.parents(k):
                if color.get(p) == GRAY:
                    return False
                if color.get(p) == WHITE and not visit(p):
                    return False
            color[k] = BLACK
            return True

        acyclic = all(visit(k) for k in self.nodes if color[k] == WHITE)
        logger.debug("compile: fragment is %s (%d node(s), %d edge(s))",
                     "acyclic" if acyclic else "cyclic", len(self.nodes), len(self.edges))
        return acyclic

    def assert_acyclic(self) -> None:
        """Only for callers that want the initial fragment's guard."""
        if not self.is_acyclic():
            raise CyclicDebateNotSupported(
                "The compiled debate graph has a derivation cycle; the "
                "acyclic fragment refuses it."
            )

    # -- presentation -------------------------------------------------------

    def statements(self) -> list:
        """Every (key, side) pair the graph mentions, in a stable order."""
        seen = []

        def add(key, side):
            if (key, side) not in seen:
                seen.append((key, side))

        for edge in self.edges:
            add(edge.target_key, edge.target_side)
            for source in edge.sources:
                add(source.key, source.side)
        for key, side in self.defaults:
            add(key, side)
        return seen

    def _roots(self) -> list:
        """Conclusion statements: edge targets that feed no other edge."""
        targets = [(e.target_key, e.target_side) for e in self.edges]
        used = {(s.key, s.side) for e in self.edges for s in e.sources}
        roots = [t for t in dict.fromkeys(targets) if t not in used]
        if roots:
            return roots
        return list(dict.fromkeys(targets)) or self.statements()

    def to_text(self, labels=None, root=None) -> str:
        """An indented tree view, rooted at the graph's conclusion(s).

        ``labels`` maps (key, side) to IN/OUT/UNDEC and is shown inline
        when given.  Revisited statements are marked rather than expanded,
        so a cyclic graph still renders.
        """
        out = []

        def annotate(statement):
            key, side = statement
            text = f"{self.shown(key)}[{side}]"
            if labels and statement in labels:
                text += f"  {labels[statement]}"
            kinds = self.defaults.get(statement)
            if kinds:
                text += f"  [{', '.join(sorted(kinds))}]"
            return text

        materialized = set(self.statements())
        rendered = set()

        def walk(statement, prefix, path):
            rendered.add(statement)
            if statement in path:
                out.append(f"{prefix}(cycle back to {annotate(statement)})")
                return
            path = path | {statement}
            incoming = [
                e for e in self.edges
                if (e.target_key, e.target_side) == statement
            ]
            contrary = (statement[0], "context" if statement[1] == "term" else "term")
            # Contrariness is symmetric, so descend into it once only: the
            # back-link from the contrary to this statement adds nothing.
            show_contrary = contrary in materialized and contrary not in path
            branches = len(incoming) + (1 if show_contrary else 0)
            drawn = 0
            for edge in incoming:
                drawn += 1
                last = drawn == branches
                branch = "`-- " if last else "|-- "
                strictness = "strict" if edge.strict else "defeasible"
                out.append(f"{prefix}{branch}{edge.name}  ({edge.role}, {strictness})")
                child_prefix = prefix + ("    " if last else "|   ")
                for j, source in enumerate(edge.sources):
                    child = (source.key, source.side)
                    last_source = j == len(edge.sources) - 1
                    sub_branch = "`-- " if last_source else "|-- "
                    out.append(f"{child_prefix}{sub_branch}{annotate(child)}")
                    walk(child, child_prefix + ("    " if last_source else "|   "), path)
            generic = [g for g, i in self.instantiations if i == statement[0]]
            if generic and statement[1] == "term":
                out.append(f"{prefix}    (an instance of {', '.join(self.shown(g) for g in generic)})")
            if show_contrary:
                # The contrary side is a mutual attack: each statement's
                # acceptance condition negates the other's.  Where the two
                # sides face each other directly, say so once instead of
                # descending into the 2-cycle.
                last = drawn + 1 == branches
                branch = "`-- " if last else "|-- "
                out.append(f"{prefix}{branch}contested by {annotate(contrary)}")
                walk(contrary, prefix + ("    " if last else "|   "), path)

        roots = [root] if root else self._roots()
        for statement in roots:
            out.append(annotate(statement))
            walk(statement, "", frozenset())
        orphans = [s for s in materialized if s not in rendered]
        if orphans:
            out.append("")
            out.append("not reachable from the conclusion:")
            for statement in orphans:
                out.append(f"  {annotate(statement)}")
        return "\n".join(out)

    def to_dot(self, labels=None, legend: bool = True, labelling: Optional[str] = None) -> str:
        """Graphviz source.  With ``labels``, nodes are filled by status;
        with ``legend``, a box explains the symbols the drawing uses, and
        ``labelling`` (which labelling ``labels`` is) heads its table."""
        fill = _DOT_FILL
        lines = ["digraph debate {", "  rankdir=BT;",
                 '  node [shape=box, style="rounded,filled", fillcolor=white];']
        ids = {key: f"n{i}" for i, key in enumerate(self.nodes)}
        for key, display in self.nodes.items():
            marks = []
            statuses = []
            for side in ("term", "context"):
                for kind in sorted(self.defaults.get((key, side), ())):
                    marks.append(f"{side[0]}:{kind[0]}")
                if labels and (key, side) in labels:
                    statuses.append(f"{side[0]}={labels[(key, side)]}")
            suffix = f"\\n[{' '.join(marks)}]" if marks else ""
            if statuses:
                suffix += f"\\n{' '.join(statuses)}"
            label = self.shown(key).replace('"', '\\"')
            colour = ""
            if labels:
                own = [labels.get((key, side)) for side in ("term", "context")]
                decided = [s for s in own if s]
                if decided:
                    # Term side drives the fill; it is the proponent's status.
                    colour = f', fillcolor="{fill.get(decided[0], "white")}"'
            lines.append(f'  {ids[key]} [label="{label}{suffix}"{colour}];')
        # Hyperedges.  A single-source edge is one labelled arrow.  An edge
        # with several sources (or none: a strict edge) is drawn through a
        # junction box carrying its name, one unlabelled arrow per source
        # into it and one out to the target - otherwise every source
        # becomes a parallel arrow with the same label, and since nodes are
        # propositions two sources on P[t] and P[c] look like a duplicated
        # edge.  Context-side sources are dashed; the arrowhead at the
        # target is filled for a proof, empty for a refutation.
        for i, edge in enumerate(self.edges):
            colour = _DOT_ROLE_COLOUR.get(edge.role, "black")
            width = " penwidth=2" if edge.strict else ""
            head = "normal" if edge.target_side == "term" else "empty"
            if len(edge.sources) == 1:
                source = edge.sources[0]
                dashed = " style=dashed" if source.side == "context" else ""
                if edge.role == "weakening":
                    dashed = " style=dotted fontcolor=gray50"
                lines.append(
                    f"  {ids[source.key]} -> {ids[edge.target_key]} "
                    f'[color={colour}{width}{dashed} arrowhead={head} label="{edge.name}"];'
                )
                continue
            junction = f"e{i}"
            lines.append(
                f'  {junction} [shape=box, style="rounded", fontsize=9, color={colour}, '
                f'label="{edge.name}"];'
            )
            for source in edge.sources:
                dashed = " style=dashed" if source.side == "context" else ""
                lines.append(
                    f"  {ids[source.key]} -> {junction} [color={colour}{width}{dashed} arrowhead=none];"
                )
            lines.append(
                f"  {junction} -> {ids[edge.target_key]} [color={colour}{width} arrowhead={head}];"
            )
            if not edge.sources:
                lines.append(f"  {ids[edge.target_key]} [peripheries=2];")
        for generic, instance in self.instantiations:
            if generic in ids and instance in ids:
                colour = _DOT_ROLE_COLOUR["instantiation"]
                lines.append(f'  {ids[generic]} -> {ids[instance]} [color={colour} style=dotted '
                             f'arrowhead=onormal fontcolor=gray50 label="instance"];')
        if legend and (self.nodes or self.edges):
            lines.extend(self._dot_legend(labels, labelling))
        lines.append("}")
        return "\n".join(lines)

    def _dot_legend(self, labels=None, labelling: Optional[str] = None) -> list:
        """A cluster explaining the symbols ``to_dot`` drew for this graph,
        and only those: a sample of every edge style in use, drawn as the
        edges are, and the node markings with their meaning."""
        out = ['  subgraph cluster_legend {', '    label="Legend"; fontsize=11;',
               '    style="rounded"; color=gray60;',
               '    node [shape=plaintext, style="", fillcolor=white, fontsize=9];',
               '    edge [fontsize=9];']
        samples = []                          # (attributes, description)
        for role in dict.fromkeys(e.role for e in self.edges):
            colour = _DOT_ROLE_COLOUR.get(role, "black")
            samples.append((f"color={colour}", _DOT_ROLE_TEXT.get(role, role)))
        if self.instantiations:
            samples.append((f'color={_DOT_ROLE_COLOUR["instantiation"]} style=dotted arrowhead=onormal',
                            _DOT_ROLE_TEXT["instantiation"]))
        sources = [s for e in self.edges for s in e.sources]
        if any(e.strict for e in self.edges):
            samples.append(("color=black penwidth=2", "thick:\\nstrict, rests\\non no site"))
        if any(s.side == "context" for s in sources):
            samples.append(("color=black style=dashed", "dashed:\\nthe source is\\na refutation"))
        if any(e.target_side == "term" for e in self.edges):
            samples.append(("color=black arrowhead=normal", "filled head:\\nderives\\na proof"))
        if any(e.target_side == "context" for e in self.edges):
            samples.append(("color=black arrowhead=empty", "empty head:\\nderives\\na refutation"))
        for i, (attributes, text) in enumerate(samples):
            out.append(f'    lg{i}a [label="", shape=point, width=0.05];')
            out.append(f'    lg{i}b [label="", shape=point, width=0.05];')
            out.append(f'    lg{i}a -> lg{i}b [{attributes} label="{text}"];')
        if samples:
            # Every sample on the same two ranks, side by side, and the table
            # of node markings beneath them (rankdir=BT: lower is earlier).
            out.append("    {rank=same; " + " ".join(f"lg{i}a" for i in range(len(samples))) + "}")
            out.append("    {rank=same; " + " ".join(f"lg{i}b" for i in range(len(samples))) + "}")
            out.append("    lgnodes -> lg0a [style=invis];")
        rows = ['<TR><TD ALIGN="LEFT"><B>box</B></TD>'
                '<TD ALIGN="LEFT">a proposition; its term side (proved) and '
                'context side (refuted) share one box</TD></TR>']
        if any(len(e.sources) == 1 for e in self.edges):
            rows.append('<TR><TD ALIGN="LEFT"><B>labelled arrow</B></TD><TD ALIGN="LEFT">'
                        'an argument with one source, named by the label</TD></TR>')
        if any(len(e.sources) != 1 for e in self.edges):
            rows.append('<TR><TD ALIGN="LEFT"><B>small box</B></TD><TD ALIGN="LEFT">'
                        'an argument with several or no sources, named; a line '
                        'without head from each source, one arrow out</TD></TR>')
        if any(not e.sources for e in self.edges):
            rows.append('<TR><TD ALIGN="LEFT"><B>double border</B></TD><TD ALIGN="LEFT">'
                        'derived by an argument that rests on nothing</TD></TR>')
        marks = sorted({f"{side[0]}:{kind[0]}" for (_key, side), kinds in self.defaults.items()
                        for kind in kinds})
        meaning = {"t:o": "the proof is owed (obligation)", "t:p": "the proof is presumed",
                   "c:o": "the refutation is owed (obligation)", "c:p": "the refutation is presumed"}
        for mark in marks:
            rows.append(f'<TR><TD ALIGN="LEFT"><B>[{mark}]</B></TD>'
                        f'<TD ALIGN="LEFT">{meaning.get(mark, mark)}</TD></TR>')
        if labels:
            if labelling:
                from html import escape
                rows.insert(0, f'<TR><TD ALIGN="LEFT"><B>labels</B></TD>'
                               f'<TD ALIGN="LEFT">{escape(labelling)}</TD></TR>')
            rows.append('<TR><TD ALIGN="LEFT"><B>t=, c=</B></TD><TD ALIGN="LEFT">'
                        'the label of the term side, the context side</TD></TR>')
            for status, colour in _DOT_FILL.items():
                rows.append(f'<TR><TD ALIGN="LEFT" BGCOLOR="{colour}"><B>{status}</B></TD>'
                            f'<TD ALIGN="LEFT">fill: the term side is {status} '
                            f'(the context side if the term side has no label)</TD></TR>')
        out.append('    lgnodes [label=<<TABLE BORDER="0" CELLSPACING="2">'
                   + "".join(rows) + '</TABLE>>];')
        out.append("  }")
        return out

# ---------------------------------------------------------------------------
# Scaffold shapes (COMMA 2026 paper, Definitions Support and Attack; the
# context side mirrored so that CBN/CBV are dual across sides - author,
# 2026-09-17).  A = the issue proposition, alpha/x = the site's own
# continuation/value binder, beta = the binder for the original, "_" = an
# affine binder.  Every scaffold is the exposure  t -> mu alpha:A.< t || [A:] >
# (context side: E -> mu'x:A.< [:A] || E >) with the slot filled:
#
#   T-SUP:  mu alpha:A.< t || mu'beta:A.< mu _:A.<beta || alpha> || mu'_:A.<t2 || alpha> > >
#   T-ATT:  mu alpha:A.< t || mu'beta:A.< mu _:A.<beta || e2>    || mu'_:A.<beta || alpha> > >
#   C-SUP:  mu'x:A.< mu beta:A.< mu _:A.<x || e2>    || mu'_:A.<x || beta> > || e >
#   C-ATT:  mu'x:A.< mu beta:A.< mu _:A.<x || beta>  || mu'_:A.<t2 || beta> > || e >
#
# Polarity (inner critical pair): term side, mu< keeps the original at a
# support and defeats it at an attack, >mu the reverse - so on the term
# side credulous is uniformly CBN and skeptical uniformly CBV; the context
# side is the mirror.  A defeated site holds the real clash
# mu alpha.< t || e2 >  /  mu'x.< t2 || e >  under an affine binder: the
# paper's abort.
#
# LEGACY (M3, 2026-08-31): the shapes Argument.support / Argument.attack
# still build by theta-expansion and grafting, recognised only for terms
# the legacy verbs produced; the attack scion's open port is rerouted to
# the catch and needs _restore_scion:
#
#   M3 T-SUP:  mu alt:A.< mu _:A.<ORIG || alt>  ||  mu'_:A.<SCION || alt> >
#   M3 T-ATT:  mu alt:A.< mu _:A.<ORIG || alt>  ||  mu'_:A.<?g:A || SCION_CTX> >
#   M3 C-SUP:  mu'alt:A.< mu _:A.<alt || SCION_CTX> || mu'_:A.<alt || ORIG_CTX> >
#   M3 C-ATT:  mu'alt:A.< mu _:A.<SCION_T || ?l:A>  || mu'_:A.<alt || ORIG_CTX> >
# ---------------------------------------------------------------------------

def _is_affine_binder(name: str) -> bool:
    return name == "_"


class Match(tuple):
    """A matched scaffold: the 7-tuple (role, site_side, prop, orig, scion,
    alt, scion_kind), with ``legacy`` True for an M3 shape (whose attack
    scion carries the alt reroute and needs _restore_scion) and, for a
    paper shape, ``beta`` the name of its b, which binds the statement in
    the scion (aida-unfold-scaffold-binders-capture)."""

    def __new__(cls, role, site_side, prop, orig, scion, alt, scion_kind):
        return super().__new__(cls, (role, site_side, prop, orig, scion, alt, scion_kind))

    legacy = False
    beta = None


def _tag(m, legacy, beta=None):
    m.legacy = legacy
    m.beta = beta
    return m


def scion_scope(match, owner):
    """The scaffold's own binders over its scion, as ``outer`` entries
    (name -> (prop, side, owner)): ``alt`` binds the contrary of the
    statement, ``b`` the statement.  A legacy attack's alt is a reroute,
    not a binder, and a legacy scaffold has no b in its scion."""
    role, site_side, prop = match[0], match[1], match[2]
    other = "context" if site_side == "term" else "term"
    if match.legacy:
        return {} if role == "attacker" else {match[5]: (prop, other, owner)}
    scope = {match[5]: (prop, other, owner)}
    if match.beta is not None:
        scope[match.beta] = (prop, site_side, owner)
    return scope


def _affine_mu(n, prop):
    return isinstance(n, Mu) and _is_affine_binder(n.id.name) and n.prop == prop


def _affine_mutilde(n, prop):
    return isinstance(n, Mutilde) and _is_affine_binder(n.di.name) and n.prop == prop


def _is_id(n, name):
    return isinstance(n, ID) and n.name == name


def _is_di(n, name):
    return isinstance(n, DI) and n.name == name


def _match_paper_scaffold(n):
    """The four paper shapes (see the catalogue above)."""
    if isinstance(n, Mu):
        alpha, prop, t, ctx = n.id.name, n.prop, n.term, n.context
        if not (isinstance(ctx, Mutilde) and ctx.prop == prop
                and not _is_affine_binder(ctx.di.name)):
            return None
        beta, w1, w2 = ctx.di.name, ctx.term, ctx.context
        if not (_affine_mu(w1, prop) and _affine_mutilde(w2, prop)):
            return None
        # T-SUP: < mu_.<beta||alpha> || mu'_.<t2||alpha> >
        if (_is_di(w1.term, beta) and _is_id(w1.context, alpha)
                and _is_id(w2.context, alpha) and not _is_di(w2.term, beta)):
            return _tag(Match("supporter", "term", prop, t, w2.term, alpha, "term"), False, beta)
        # T-ATT: < mu_.<beta||e2> || mu'_.<beta||alpha> >
        if (_is_di(w1.term, beta) and _is_di(w2.term, beta) and _is_id(w2.context, alpha)
                and not _is_id(w1.context, alpha)):
            return _tag(Match("attacker", "term", prop, t, w1.context, alpha, "context"), False, beta)
        return None
    if isinstance(n, Mutilde):
        x, prop, tm, e = n.di.name, n.prop, n.term, n.context
        if not (isinstance(tm, Mu) and tm.prop == prop
                and not _is_affine_binder(tm.id.name)):
            return None
        beta, w1, w2 = tm.id.name, tm.term, tm.context
        if not (_affine_mu(w1, prop) and _affine_mutilde(w2, prop)):
            return None
        # C-SUP: < mu_.<x||e2> || mu'_.<x||beta> >
        if (_is_di(w1.term, x) and _is_di(w2.term, x) and _is_id(w2.context, beta)
                and not _is_id(w1.context, beta)):
            return _tag(Match("supporter", "context", prop, e, w1.context, x, "context"), False, beta)
        # C-ATT: < mu_.<x||beta> || mu'_.<t2||beta> >
        if (_is_di(w1.term, x) and _is_id(w1.context, beta) and _is_id(w2.context, beta)
                and not _is_di(w2.term, x)):
            return _tag(Match("attacker", "context", prop, e, w2.term, x, "term"), False, beta)
    return None


def scaffold_parts(node, match):
    """Where a matched scaffold keeps its original and its scion: two
    (parent, slot) positions.  Everything else in the node is wiring,
    and wiring must never be matched as a scaffold of its own - the
    paper attack's inner context has the legacy C-SUP shape, for one -
    so a rewriter recurses through these positions, never through raw
    children of a scaffold."""
    if match.legacy:
        if isinstance(node, Mu):
            return [(node.term, "term"), (node.context, "term" if match[0] == "supporter" else "context")]
        return [(node.context, "context"), (node.term, "context" if match[0] == "supporter" else "term")]
    if isinstance(node, Mu):
        wiring = node.context                       # mu'beta.< mu_.c1 || mu'_.c2 >
        scion_pos = (wiring.context, "term") if match[0] == "supporter" else (wiring.term, "context")
        return [(node, "term"), scion_pos]
    wiring = node.term                              # mu beta.< mu_.c1 || mu'_.c2 >
    scion_pos = (wiring.term, "context") if match[0] == "supporter" else (wiring.context, "term")
    return [(node, "context"), scion_pos]


def _match_scaffold(n, strict_names=()):
    """Match a scaffold at this node: the paper shapes first, then the
    legacy M3 shapes.  Only meaningful top-down: the wiring inside a
    paper scaffold can itself match a legacy shape (see scaffold_parts).

    Returns a 7-tuple (role, site_side, prop, orig, scion, alt_name,
    scion_kind) with a ``legacy`` attribute, or None.  ``scion_kind`` is
    "term"/"context" (what the scion body is).  Legacy attack scions still
    contain the alt reroute and need ``_restore_scion``.

    The stolen-value slot of a legacy attack shape normally holds an open
    site (the debate operators put a fresh Goal/Laog there).  It may also
    hold a *declared* name: the primitive contrariness term
    mu att.<mu _.<t || att> || mu'_.<t || t'>>.  ``strict_names`` says which
    names are declared.
    """
    found = _match_paper_scaffold(n)
    if found is not None:
        logger.log(TRACE, "  compile: paper %s scaffold on %s (alt '%s')",
                   found[0], found[2], found[5])
        return found
    found = _match_legacy_scaffold(n, strict_names)
    if found is not None:
        logger.log(TRACE, "  compile: legacy %s scaffold on %s (alt '%s')",
                   found[0], found[2], found[5])
    elif isinstance(n, (Mu, Mutilde)):
        # A near-miss is worth a line: "why was my term not recognised" is
        # answered by which shape it failed to be.
        logger.log(TRACE, "  compile: %s on %s matched no scaffold shape",
                   type(n).__name__, getattr(n, "prop", "?"))
    return found


def _match_legacy_scaffold(n, strict_names=()):
    if isinstance(n, Mu):
        alt, prop = n.id.name, n.prop
        w1, w2 = n.term, n.context
        if not (
            isinstance(w1, Mu)
            and _is_affine_binder(w1.id.name)
            and w1.prop == prop
            and isinstance(w1.context, ID)
            and w1.context.name == alt
            and isinstance(w2, Mutilde)
            and _is_affine_binder(w2.di.name)
            and w2.prop == prop
        ):
            return None
        if isinstance(w2.context, ID) and w2.context.name == alt:
            if _same_variable(w1.term, w2.term):
                return None
            return _tag(Match("supporter", "term", prop, w1.term, w2.term, alt, "term"), True)
        if isinstance(w2.term, (Goal, Deleg)) or (
            isinstance(w2.term, DI) and w2.term.name in strict_names
        ):
            return _tag(Match("attacker", "term", prop, w1.term, w2.context, alt, "context"), True)
        return None
    if isinstance(n, Mutilde):
        alt, prop = n.di.name, n.prop
        w1, w2 = n.term, n.context
        if not (
            isinstance(w2, Mutilde)
            and _is_affine_binder(w2.di.name)
            and w2.prop == prop
            and isinstance(w2.term, DI)
            and w2.term.name == alt
            and isinstance(w1, Mu)
            and _is_affine_binder(w1.id.name)
            and w1.prop == prop
        ):
            return None
        if isinstance(w1.term, DI) and w1.term.name == alt:
            if _same_variable(w2.context, w1.context):
                return None
            return _tag(Match("supporter", "context", prop, w2.context, w1.context, alt, "context"), True)
        if isinstance(w1.context, (Laog, Geled)) or (
            isinstance(w1.context, ID) and w1.context.name in strict_names
        ):
            return _tag(Match("attacker", "context", prop, w2.context, w1.term, alt, "term"), True)
        return None
    return None


def _same_variable(a, b) -> bool:
    """Both halves the same variable: the wiring of a paper attack whose
    wing is nothing but its captured fallback, mu b.< mu _.<a||b> ||
    mu'_.<a||b> >.  It has the legacy support shape, but supports a
    variable by itself; it is no scaffold and is left to normalisation
    (aida-unfold-scaffold-binders-capture)."""
    return (type(a) is type(b) and isinstance(a, (ID, DI)) and a.name == b.name
            and not getattr(a, "cites", None))


def _restore_scion(scion, scion_kind: str, alt: str, prop: str, fresh_site):
    """Undo an attack scion's reroute: the free alt occurrence stands where
    the scion's own open site was; put a fresh open site back."""
    replacement = (
        Laog(fresh_site, prop) if scion_kind == "context" else Goal(fresh_site, prop)
    )
    match_type = ID if scion_kind == "context" else DI

    def walk(node):
        if isinstance(node, match_type) and node.name == alt:
            return deepcopy(replacement)
        # Stop at shadowing binders re-binding alt.
        if isinstance(node, Mu) and match_type is ID and node.id.name == alt:
            return node
        if isinstance(node, Mutilde) and match_type is DI and node.di.name == alt:
            return node
        if isinstance(node, Lamda) and match_type is DI and node.di.di.name == alt:
            return node
        if isinstance(node, Admal) and match_type is ID and node.id.id.name == alt:
            return node
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                setattr(node, slot, walk(child))
        return node

    return walk(deepcopy(scion))


# ---------------------------------------------------------------------------
# M3: the compiler
# ---------------------------------------------------------------------------

_LEAF_INFO = {
    Goal: ("term", "obligation"),
    Deleg: ("term", "presumption"),
    Laog: ("context", "obligation"),
    Geled: ("context", "presumption"),
}


#: Prefix of the temporaries Argument._theta_expand mints around an
#: attacked or supported body.  They are bookkeeping, not subarguments,
#: and never name an edge.
SYNTHETIC_PREFIX = "theta_expand_"


def _host_name(body, fallback: str) -> str:
    """The subargument a host body records: the innermost binder of its
    eta-wrapper chain  mu d.< mu theta.< mu cArg.< ... || cArg > || theta > || d >
    that is not synthetic - here ``cArg``, not the debate's own ``d``
    (tasks.org, aida-host-edge-naming)."""
    names = []
    node = body
    while True:
        if isinstance(node, Mu) and isinstance(node.context, ID) and node.context.name == node.id.name:
            if not _is_negation_introduction(node):
                names.append(binder_origin(node))
            node = node.term
        elif isinstance(node, Mutilde) and isinstance(node.term, DI) and node.term.name == node.di.name:
            names.append(binder_origin(node))
            node = node.context
        else:
            break
    for candidate in reversed(names):
        if not candidate.startswith(SYNTHETIC_PREFIX):
            return candidate
    return fallback


def _is_negation_introduction(node) -> bool:
    """Fellowship's wrapper for ``elim h.`` on a goal ~A,
    ``mu H1:~A.< lambda h:A. ... || H1 >``: its binder (H1) is the prover's,
    not the argument's, and names no subargument (tasks.org,
    aida-negation-edge-provenance)."""
    from core.ac.instructions import is_primitive_negation_prop
    return isinstance(node.term, Lamda) and is_primitive_negation_prop(getattr(node, "prop", None))


def binder_statements(node) -> dict:
    """{variable: (prop, side)} for the binders a node introduces.

    An ID bound by mu (or a Pyh under an Admal) stands for a refutation
    of its proposition - side "context"; a DI bound by mu' or lambda for
    a proof - side "term".  Affine binders bind nothing referenceable."""
    out = {}
    if isinstance(node, Mu) and not _is_affine_binder(node.id.name):
        out[node.id.name] = (node.prop, "context")
    elif isinstance(node, Mutilde) and not _is_affine_binder(node.di.name):
        out[node.di.name] = (node.prop, "term")
    elif isinstance(node, Lamda) and not _is_affine_binder(node.di.di.name):
        out[node.di.di.name] = (node.di.prop, "term")
    elif isinstance(node, Admal) and not _is_affine_binder(node.id.id.name):
        out[node.id.id.name] = (node.id.prop, "context")
    return out


def binder_origin(node) -> str:
    """The name a root binder stands for: the argument an unfolded copy of
    its body came from (``origin``, set by the unfolder: a copy's binder is
    renamed ``a1_0_2``, its argument is ``a1_0``), else the binder's own
    name (aida-fold-occurrences-names)."""
    found = getattr(node, "origin", None)
    if found:
        return found
    return node.id.name if isinstance(node, Mu) else node.di.name


def _scion_name(body, fallback: str) -> str:
    """An eta-wrapped scion carries its argument name in the root binder."""
    if isinstance(body, Mu) and isinstance(body.context, ID) and body.context.name == body.id.name:
        return binder_origin(body)
    if isinstance(body, Mutilde) and isinstance(body.term, DI) and body.term.name == body.di.name:
        return binder_origin(body)
    if isinstance(body, (ID, DI)):
        return body.name
    return fallback


def stack_elements(scion, strict_names=()) -> list:
    """The alternatives a scion stands for (aida-unfold-entrypoints).  In
    the stacked shape a scion may be a *stack*: support scaffolds on one
    statement, SUP(SUP(P1, P2), P3), or the contrary's debate
    SUP~(ROOT, STACK) as an attacker wing.  Its elements, bottom first, are
    the alternatives sigma reads disjunctively and the compiler records as
    one edge each.  A scion that is not a support scaffold is its own one
    element; a bare leaf (a site, a captured variable) is given the eta
    wrapper an argument has, so it compiles as an identity edge."""
    return [element for element, _scope in scoped_stack_elements(scion, strict_names)]


def scoped_stack_elements(scion, strict_names=()) -> list:
    """``stack_elements`` with the binders each element is under inside
    the stack: [(element, {name: (prop, side)})].  A stack's scaffolds bind
    too (aida-unfold-scaffold-binders-capture): an element that is a
    supported term is under its scaffold's alt, one that is a scion under
    its alt and b, so a supporter needing the statement it supports is
    bound there, not free."""
    match = _match_paper_scaffold(scion)
    if match is not None and match[0] == "supporter":
        scope = {k: v[:2] for k, v in scion_scope(match, None).items()}
        alt_only = {match[5]: scope[match[5]]}
        return ([(e, {**alt_only, **inner}) for e, inner in scoped_stack_elements(match[3], strict_names)]
                + [(e, {**scope, **inner}) for e, inner in scoped_stack_elements(match[4], strict_names)])
    if isinstance(scion, (Goal, Deleg, DI)) and not (isinstance(scion, DI) and scion.name in strict_names):
        prop = scion.prop
        return [(_same_label(Mu(ID("x", prop), prop, scion, ID("x", prop)), scion), {})]
    if isinstance(scion, (Laog, Geled, ID)) and not (isinstance(scion, ID) and scion.name in strict_names):
        prop = scion.prop
        return [(_same_label(Mutilde(DI("x", prop), prop, DI("x", prop), scion), scion), {})]
    return [(scion, {})]


def _same_label(wrapper, leaf):
    """An eta wrapper stands for the same statement as the leaf it wraps,
    so in a labelled term (core/comp/labelled.py) it has the same label."""
    label = getattr(leaf, "label", None)
    if label is not None:
        wrapper.label = label
    return wrapper


#: Which declaration kind a declared name must have to appear on a side:
#: a ``declare``d proposition is a proof (term side), a ``deny``ed one a
#: refutation (context side).  Sorts never occur as proof-term leaves.
_KIND_FOR_SIDE = {"term": "prop", "context": "moxia"}


def declaration_kinds(declarations) -> dict:
    """{name: kind} from a prover's declarations mapping (values carry a
    ``.kind`` of "sort", "prop" or "moxia"); entries without a kind are
    skipped.  For passing as ``strict_kinds``."""
    out = {}
    for name, decl in (declarations or {}).items():
        kind = getattr(decl, "kind", None)
        if kind:
            out[name] = kind
    return out


#: Fellowship's eliminator leaves.  `_F_` is the canonical refutation of
#: falsum (the tail of a negation elimination  mu' H:~A.< H || a * _F_ >),
#: `_T_` the canonical proof of truth.  They are strict axioms of the
#: base category, on the side their kind fixes, and never sources.
BUILTIN_LEAVES = {"_F_": "moxia", "_T_": "prop"}


def _fo_scopes(body, outer=frozenset()) -> dict:
    """{id(node): the first-order variables bound above it} for every node
    of ``body``, starting from ``outer`` (those bound around the body):
    lambda x (``LamdaFO``) and the existential's witness (``(x).``,
    ``DestructTermsPairFO``) bind one (tasks.org,
    aida-first-order-reduction)."""
    scopes, stack = {}, [(body, frozenset(outer))]
    while stack:
        node, bound = stack.pop()
        if not isinstance(node, ProofTerm):
            continue
        scopes[id(node)] = bound
        inner = bound | {node.var} if isinstance(node, (LamdaFO, DestructTermsPairFO)) else bound
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                stack.append((child, inner))
    return scopes


@lru_cache(maxsize=None)
def _prop_names(prop) -> frozenset:
    """The free names of ``prop``, or none if it does not parse."""
    try:
        return frozenset(Prop.parse(prop).free_vars())
    except PropError:
        return frozenset()


def _fo_mentions(body) -> dict:
    """{id(node): the names free in the propositions and first-order terms
    of ``node``'s whole subterm} - a hypothesis's annotation and a
    witness included, the variables of its own first-order binders not."""
    mentions = {}

    def walk(node):
        names = set(_prop_names(node.prop)) if isinstance(getattr(node, "prop", None), str) else set()
        for attr in ("fo_term", "witness"):
            term = getattr(node, attr, None)
            if term is not None and hasattr(term, "free_vars"):
                names |= set(term.free_vars())
        for child in vars(node).values():
            if isinstance(child, ProofTerm):
                names |= walk(child)
        if isinstance(node, (LamdaFO, DestructTermsPairFO)):
            names.discard(node.var)
        mentions[id(node)] = frozenset(names)
        return names

    walk(body)
    return mentions


def _mentions_referent(body) -> bool:
    """Whether a proposition of ``body`` spells a discourse referent."""
    stack = [body]
    while stack:
        node = stack.pop()
        prop = getattr(node, "prop", None)
        if isinstance(prop, str) and "?" in prop and referents.mentioned(prop):
            return True
        for attr in ("fo_term", "witness"):
            term = getattr(node, attr, None)
            if term is not None and "?" in str(term):
                return True
        stack.extend(c for c in vars(node).values() if isinstance(c, ProofTerm))
    return False


def _referent_form(prop, bound) -> str:
    """``prop`` with each first-order variable of ``bound`` it mentions
    made the referent of that name: ``Dr z -> false`` under ``forall z``
    is the referent statement ``Dr ?z -> false``."""
    parsed = Prop.parse(prop)
    for var in sorted(bound):
        parsed = parsed.subst(var, TSym(referents.constant(var)))
    return str(parsed)


def witness_key(referent: str, owner=None) -> str:
    """The node a referent target owes for ``referent``, or an argument for
    its own open choice (``owner``): a witness."""
    return f"a witness for {referent}" + (f" of {owner}" if owner else "")


def sort_individuals(graph, declarations) -> None:
    """The declared constants of each sort a sort node is about, each a
    strict proof that the sort has an individual (tasks.org,
    aida-referents-individuals, decided 2026-10-21): the edge ``c`` with
    the term ``(c, _T_)``."""
    for sort, key in list(getattr(graph, "sorts", {}).items()):
        if str(declarations.get(sort, "")).strip() != "type":
            continue
        for name, decl in declarations.items():
            if str(decl).strip() != sort or name == sort:
                continue
            prop = referents.sort_statement(sort)
            term = TermsPairFO(TSym(name), DI("_T_", "true"))
            term.prop = prop
            edge = Edge(name=name, target_key=key, target_side="term", sources=(), strict=True,
                        role="subargument", term=term)
            if not graph._holds(edge):
                graph.add_edge(edge)


def require_witnesses(graph, issue) -> None:
    """The issue graph of a target that is itself a referent statement:
    nothing above the root can catch its referents, so each is an unmet
    obligation of the target - a witness nobody supplies - which every
    derivation of the target owes.  The label then agrees with the
    evaluation, which leaves the referent open (tasks.org,
    aida-discourse-referents, decision (c) and the decisions of
    2026-10-20)."""
    key, side = issue
    if "r#" not in key and not key.startswith("["):
        return
    scope = graph.contexts.get(key)
    if scope:
        names = [(name, sort) for name, sort in scope]
    else:
        try:
            names = [(n, None) for n in Prop.parse(graph.nodes.get(key, key)).referent_order()]
        except PropError:
            return
    if not names:
        return
    owed = []
    for name, sort in names:
        if referents.is_presumed(name) and sort:
            # !x: the sort is presumed to have an individual (aida-referents-individuals)
            source = Source(graph.add_node(referents.sort_statement(sort)), "term", "presumption", name)
            graph.sorts.setdefault(str(sort), source.key)
        else:
            source = Source(witness_key(name), "term", "obligation", name)
            graph.nodes.setdefault(source.key, source.key)
        graph.mark_default(source.key, source.side, source.kind)
        owed.append(source)
    owed = tuple(owed)
    names = [n for n, _s in names]
    graph.edges = [replace(edge, sources=edge.sources + owed, strict=False)
                   if (edge.target_key, edge.target_side) == issue else edge
                   for edge in graph.edges]
    kinds = graph.defaults.get(issue, set())
    if "presumption" in kinds:
        # a presumption about nobody: the default holds given a witness
        kinds.discard("presumption")
        if not kinds:
            del graph.defaults[issue]
        graph.edges.append(Edge("presumed", key, side, owed, False, role="supporter"))
    logger.debug("compile: the target %s is a referent statement; its derivations owe "
                  "a witness for %s", graph.nodes.get(key, key), ", ".join(names))


def _open_in(prop, bound) -> frozenset:
    """The first-order variables of ``bound`` that ``prop`` mentions: a
    statement mentioning one is open - it is no statement at all outside
    the binder (tasks.org, aida-first-order-reduction, Q2)."""
    if not prop or not bound:
        return frozenset()
    try:
        return frozenset(Prop.parse(prop).free_vars()) & frozenset(bound)
    except PropError:
        return frozenset()


class _Compiler:
    """Term -> hyperedges: the abstract argumentation framework of a term.

    One edge per subargument: the body of an argument, every scion of a
    scaffold, and every *horn* of a body - a declared axiom it applies, a
    lemma it proves on the way (a mu/mu' node), a lambda or lambda-bar,
    an elimination cell off the spine (``compile_body``, 2026-10-15).  A
    subargument's sources are its horns: its open sites, the statements
    of the subarguments it applies, and every variable it uses that an
    *enclosing* subargument binds - a captured obligation (a demand met
    by a hypothesis or continuation in scope) is an obligation source at
    that binder's statement, a captured presumption a presumption source.  So a cycle a capture closes is a
    cycle here, obligations included: the framework does not know that a
    demand was met inside a scope.  That is the debate term's business -
    strictness is read off the unfolded term, not the framework
    (core/dc/strict.py; author, 2026-09-17: "capturing is a debate
    phenomenon ... strictness ought to be evaluated on the concrete debate
    term, not the abstract argumentation framework").
    """

    def __init__(self, strict_names, strict_kinds=None):
        self.strict_names = set(strict_names or ()) | set(BUILTIN_LEAVES)
        self.strict_kinds = {**BUILTIN_LEAVES, **dict(strict_kinds or {})}
        self.graph = DebateGraph()
        self._fresh = count(1)
        #: Called with a citation leaf and the name of the body it stands in,
        #: before its source is recorded; the issue compiler uses it to
        #: compile the cited instance in place.
        self.on_citation = None

    def _choice(self, node):
        """The source an argument's own open choice adds where it names it
        (``elim [?w]``): a witness for ``?w``, owed, which only an
        alternative of the witness scaffold supplies (core/dc/unfold.py);
        for ``!w`` the sort node, presumed - the sort has an individual
        (tasks.org, aida-referents-individuals).  None elsewhere."""
        chosen = node.witness if isinstance(node, TermsPairFO) else node.fo_term
        if not isinstance(chosen, TSym) or not referents.is_referent(chosen.name):
            return None
        prop = getattr(node, "prop", None)
        entry = next((e for e in getattr(prop, "scope", ())
                      if e[0] == chosen.name and referents.is_witness(e)), None)
        if entry is None:
            return None
        if referents.is_presumed(chosen.name) and entry[1]:
            key = self.graph.add_node(referents.sort_statement(entry[1]))
            self.graph.sorts.setdefault(str(entry[1]), key)
            return _Acc((Source(key, "term", "presumption", chosen.name),))
        key = witness_key(chosen.name, entry.owner)
        self.graph.nodes.setdefault(key, key)
        return _Acc((Source(key, "term", "obligation", chosen.name),))

    def _check_kind(self, declared: str, side: str, edge_name: str) -> None:
        """A declared name on a side must have the kind that side needs."""
        kind = self.strict_kinds.get(declared)
        if kind is None:
            return
        wanted = _KIND_FOR_SIDE[side]
        if kind != wanted:
            raise DebateCompileError(
                f"Edge '{edge_name}': declared name '{declared}' has kind "
                f"'{kind}' but occurs as a {side}-side leaf, which needs "
                f"kind '{wanted}'."
            )

    def compile(self, body: ProofTerm, name: str, role: str = "argument") -> None:
        self.graph.add_edge(self.compile_body(deepcopy(body), _host_name(body, name), role, {}))

    # -- one subargument ----------------------------------------------------

    def compile_body(self, body, name, role, outer, *, expect_side=None, expect_prop=None,
                     fo_outer=frozenset()):
        """Compile one subargument body into its edge, by the horn rule
        (tasks.org, aida-discuss-non-site-attack-points, 2026-10-15).

        The *spine* of the body is what carries its own variable: the
        root binder's variable, and, accumulating, the variables of the
        same kind - continuations of a term-side edge, hypotheses of a
        context-side one - that binders on the spine introduce.  A subterm
        that contains no spine variable is a *horn*: a site is a source
        with its kind, a declared name a strict source-less edge of its
        own and a ``subargument`` source, a progress node (mu/mu',
        lambda/lambda-bar, a cell) an edge of its own by the same rule and
        a ``subargument`` source, a variable a binder of this edge binds
        nothing, one an enclosing edge binds a captured source (the sprung
        trap, shown as a cycle).  A horn with this edge's own statement is
        inlined: the eta chain and the bureaucratic cuts around a body are
        its exposure, not separate ones.  Spine nodes get no edge: their
        statement is mirrored by the horn they are cut against.  A
        scaffold (an unfolded debate term) is decomposed as before: its
        scion is an edge, its original continues this one.

        ``outer`` maps the variables bound by enclosing subarguments to
        (prop, side, owner).  Nested subarguments are added to the graph
        here; the body's own edge is returned.
        """
        side, prop = self._root_statement(body, name)
        if expect_side is not None and side != expect_side:
            raise DebateCompileError(
                f"Edge '{name}': scion is {side}-side where the scaffold "
                f"needs {expect_side}-side."
            )
        if expect_prop is not None and canonical_prop(prop) != canonical_prop(expect_prop):
            raise DebateCompileError(
                f"Edge '{name}': scion concludes '{prop}' but the scaffold "
                f"issue is '{expect_prop}'."
            )
        target_key = self.graph.add_node(prop)
        own = (target_key, side)
        # the kind of variable that carries this edge's proof on: a
        # continuation (ID) for a proof, a hypothesis (DI) for a refutation
        spine_kind = "c" if side == "term" else "t"
        spine_binders = (Mu, Admal) if side == "term" else (Mutilde, Lamda)

        def statement_of(node):
            p = getattr(node, "prop", None)
            if not p:
                return None
            if isinstance(node, (Mu, Goal, Deleg, DI, Lamda, Sonc, LamdaFO, TermsPairFO)):
                return (canonical_prop(p), "term")
            return (canonical_prop(p), "context")

        # The first-order variables bound above each node (``fo_outer``:
        # those bound around this body, for a subargument).
        fo_scope = _fo_scopes(body, fo_outer)
        fo_mentions = (_fo_mentions(body) if any(fo_scope.values()) or _mentions_referent(body)
                       else {})

        def is_open(node) -> bool:
            """A node whose statement - or anything inside it, such as a
            hypothesis it uses - mentions a first-order variable bound by
            the argument is no horn: outside the binder it is no closed
            argument, so it stays part of the argument, like a spine step
            (the author, 2026-10-16; tasks.org, aida-fo-open-source-horn).
            Likewise a node that uses a discourse referent its statement
            does not state.  Kept on 2026-10-22: lifting it (plan step 4b,
            aida-referents-open-horns) makes such a horn rest on the
            hypotheses of its argument as owed sources, which changes the
            verdicts the decision meant to keep (birds_fly is no value)."""
            mentions = fo_mentions.get(id(node), frozenset())
            bound = fo_scope.get(id(node))
            if bound and mentions & bound:
                return True
            used = {n for n in mentions if referents.is_referent(n)}
            return bool(used) and bool(used - _prop_names(getattr(node, "prop", None) or ""))

        def on_spine(node, spine):
            """Whether ``node`` carries this edge's proof on: a spine
            variable occurs in it with no binder of the spine kind between.
            An occurrence under such a binder is a capture the unfolder
            made (a site met by the continuation, the sprung trap), which
            a horn records as a source; the original occurrence has no
            same-kind binder between its binder and itself (the argument
            is linear)."""
            var_type = ID if spine_kind == "c" else DI
            return any(0 in _occurrences(node, vname, var_type, spine_binders, through_affine=True)
                       for _kind, vname in spine)

        def bind(env, node):
            """``env`` plus the binder ``node`` introduces, and whether that
            variable is of the spine kind."""
            stmts = binder_statements(node)
            if not stmts:
                return env, None
            (vname, (vprop, vside)), = stmts.items()
            vkind = "c" if vside == "context" else "t"
            return {**env, vname: (vprop, vside)}, ((vkind, vname) if vkind == spine_kind else None)

        def sub_edge(node, label, env):
            sub_outer = {**outer, **{k: (*v, name) for k, v in env.items()}}
            rec = self.compile_body(node, label, "subargument", sub_outer,
                                    fo_outer=fo_scope.get(id(node), frozenset()))
            self.graph.add_edge(rec)
            return _Acc((Source(rec.target_key, rec.target_side, "subargument", label, (),
                                rec.target_label, prop=getattr(rec.term, "prop", None)),),
                        not rec.strict)

        def axiom_edge(node, vside, env):
            """A declared name as a horn: its own strict edge, named after
            it, and a ``subargument`` source."""
            self._check_kind(node.name, vside, name)
            if node.name in BUILTIN_LEAVES:
                return _EMPTY                       # _F_, _T_: never sources
            key = self.graph.add_node(node.prop) if getattr(node, "prop", None) else None
            if key is None:
                raise DebateCompileError(
                    f"Edge '{name}': the declared name '{node.name}' has no proposition; "
                    f"run enrichment first.")
            self.graph.add_edge(Edge(name=node.name, target_key=key, target_side=vside,
                                     sources=(), strict=True, role="subargument", term=node,
                                     target_label=getattr(node, "label", None)))
            return _Acc((Source(key, vside, "subargument", node.name, (), getattr(node, "label", None),
                                prop=node.prop),))

        def variable(node, env, scope):
            vside = "context" if isinstance(node, ID) else "term"
            bound = env.get(node.name)
            if bound is not None and bound[1] == vside:
                for kind in getattr(node, "captured_default", ()):
                    # The bare contrary captured within the same body (an
                    # attack whose wing is nothing but it): no source, but
                    # the default somebody holds on the contrary is marked.
                    self.graph.mark_default(self.graph.add_node(bound[0]), vside, kind)
                return _EMPTY
            if node.name in self.strict_names:
                if statement_of(node) == own:
                    self._check_kind(node.name, vside, name)
                    return _EMPTY                   # the edge is the axiom's own
                return axiom_edge(node, vside, env)
            if getattr(node, "cites", None):
                # A citation of a registered argument Fellowship does not
                # hold, i.e. a defeasible one: an obligation on the cited
                # conclusion, which the cited argument's own edge derives
                # (core/dc/cite.py).  A strict citation took the branch above.
                if not node.prop:
                    raise DebateCompileError(
                        f"Edge '{name}': the citation of '{node.name}' has no proposition."
                    )
                if self.on_citation is not None:
                    self.on_citation(node, name)
                return _Acc((Source(self.graph.add_node(node.prop), vside,
                                    getattr(node, "source_kind", "obligation"),
                                    node.name, scope, getattr(node, "label", None),
                                    prop=node.prop),))
            enclosing = outer.get(node.name)
            if enclosing is not None and enclosing[1] == vside:
                kind = "presumption" if getattr(node, "captured_presumption", False) else "obligation"
                captured_key = self.graph.add_node(enclosing[0])
                logger.debug("  compile: '%s' captured '%s' -> %s source at %s "
                             "(the sprung trap, shown as a cycle)",
                             name, node.name, kind, self.graph._show(captured_key, vside))
                return _Acc((Source(captured_key, vside, kind, node.name, scope,
                                    getattr(node, "label", None), prop=enclosing[0]),))
            raise DebateCompileError(
                f"Edge '{name}': free {'context' if vside == 'context' else 'term'} "
                f"variable '{node.name}' is neither bound nor a declared axiom; "
                f"express assumptions as open sites."
            )

        def visit(node, env, spine, scope, *, rest=False, parent=None):
            """``rest``: the node is the continuation of the proof whatever
            it contains (a lambda's body).  ``parent``: the node this is a
            half or child of; the same-issue half of a *bureaucratic*
            parent (binder type = cut type: an eta wrapper, a cut on the
            statement itself) is that parent's exposure and is inlined."""
            for leaf_type, (lside, kind) in _LEAF_INFO.items():
                if isinstance(node, leaf_type):
                    if not node.prop:
                        raise DebateCompileError(
                            f"Edge '{name}': open site {node.number!r} has no "
                            f"proposition; run enrichment first."
                        )
                    # A site under first-order binders (or in an argument
                    # with discourse referents) exports its context: its
                    # node is the statement in that context, the bound
                    # variables it mentions made referents (tasks.org,
                    # aida-discourse-referents, 2026-10-20; ``contextualise``).
                    prop = node.prop
                    return _Acc((Source(self.graph.add_node(prop), lside, kind,
                                        str(node.number), scope,
                                        getattr(node, "label", None), prop=prop),))
            if isinstance(node, (ID, DI)):
                return variable(node, env, scope)
            if _is_negation_wrapper(node):
                if rest or node is content or on_spine(node.context, spine) or is_open(node):
                    return visit(node.context, env, spine, scope, rest=rest)
                # Off the spine, the wrapper is the horn, not the bare cell
                # inside it: ``?:A * _F_`` alone is no refutation of ~A
                # Fellowship can rebuild, the wrapper is (tasks.org,
                # aida-negation-cell-horn).  The cell is then the wrapper's
                # own exposure and is inlined there.
                return sub_edge(node, f"{name}.c{next(self._fresh)}", env)
            match = _match_scaffold(node, self.strict_names)
            if match is not None:
                srole, site_side, sprop, orig, scion_raw, alt, scion_kind = match
                if srole == "attacker":
                    scion = (_restore_scion(scion_raw, scion_kind, alt, sprop, f"r{next(self._fresh)}")
                             if match.legacy else scion_raw)
                    target_side = "context" if site_side == "term" else "term"
                else:
                    scion = scion_raw
                    target_side = site_side
                # the scion sees the binders in scope and the scaffold's own
                # (a legacy attack's reroute is wiring, not capture)
                sub_outer = {**outer, **{k: (*v, name) for k, v in env.items()
                                         if not (match.legacy and srole == "attacker" and k == alt)},
                             **scion_scope(match, name)}
                # A stacked scion is one edge per alternative in it.
                for element, inner in ([(scion, {})] if match.legacy
                                       else scoped_stack_elements(scion, self.strict_names)):
                    label = _scion_name(element, f"{name}.{srole}{next(self._fresh)}")
                    element_outer = {**sub_outer, **{k: (*v, name) for k, v in inner.items()}}
                    self.graph.add_edge(self.compile_body(element, label, srole, element_outer,
                                                          expect_side=target_side, expect_prop=sprop,
                                                          fo_outer=fo_scope.get(id(node), frozenset())))
                # the site stays a source of this derivation; the scion is
                # its own edge
                return visit(orig, env, spine, scope, rest=rest, parent=node)
            same_issue = (statement_of(node) == own and isinstance(parent, (Mu, Mutilde))
                          and getattr(parent, "prop", None)
                          and canonical_prop(parent.prop) == own[0])
            spine_node = rest or node is content or on_spine(node, spine) or same_issue or is_open(node)
            if isinstance(node, (Mu, Mutilde)):
                if not spine_node:
                    tag = "\u03bc" if isinstance(node, Mu) else "\u03bc'"
                    return sub_edge(node, f"{name}.{tag}{next(self._fresh)}", env)
                env2, var = bind(env, node)
                spine2 = spine | {var} if var else spine
                vname = node.id.name if isinstance(node, Mu) else node.di.name
                scope2 = scope if _is_affine_binder(vname) else scope + ((node.prop, "context" if isinstance(node, Mu) else "term"),)
                return (visit(node.term, env2, spine2, scope2, parent=node)
                        + visit(node.context, env2, spine2, scope2, parent=node))
            if isinstance(node, (Lamda, Admal)):
                if not spine_node:
                    tag = "\u03bb" if isinstance(node, Lamda) else "\u03bb\u0304"
                    return sub_edge(node, f"{name}.{tag}{next(self._fresh)}", env)
                env2, var = bind(env, node)
                spine2 = spine | {var} if var else spine
                body_ = node.term if isinstance(node, Lamda) else node.context
                return visit(body_, env2, spine2, scope, rest=True, parent=node)
            if isinstance(node, (Cons, Sonc)):
                if not spine_node:
                    return sub_edge(node, f"{name}.c{next(self._fresh)}", env)
                return (visit(node.term, env, spine, scope, parent=node)
                        + visit(node.context, env, spine, scope, parent=node))
            # First order: the horn rule as for every node (tasks.org,
            # aida-referents-witness-scaffold, 2026-10-22, which undoes the
            # exemption of 2026-10-17).  An instantiation ``t * c``, an
            # existential's introduction ``(t, v)`` and its elimination
            # ``(x). c`` off the spine are horns with the quantified
            # statement, in the context they stand in; on the spine (at the
            # root, under a binder of the spine) they are steps of it.
            if isinstance(node, (ConsFO, TermsPairFO)):
                choice = self._choice(node)
                if choice is not None:
                    return choice + visit_fo(node, env, spine, scope, spine_node)
            return visit_fo(node, env, spine, scope, spine_node)

        def visit_fo(node, env, spine, scope, spine_node):
            if isinstance(node, ConsFO):
                if not spine_node and getattr(node, "prop", None):
                    return sub_edge(node, f"{name}.c{next(self._fresh)}", env)
                return visit(node.context, env, spine, scope, rest=True, parent=node)
            if isinstance(node, TermsPairFO):
                if not spine_node and getattr(node, "prop", None):
                    return sub_edge(node, f"{name}.p{next(self._fresh)}", env)
                return visit(node.term, env, spine, scope, rest=True, parent=node)
            if isinstance(node, DestructTermsPairFO):
                if not spine_node and getattr(node, "prop", None):
                    return sub_edge(node, f"{name}.\u03bb\u0304{next(self._fresh)}", env)
                return visit(node.context, env, spine, scope, rest=True, parent=node)
            if isinstance(node, LamdaFO):
                if not spine_node:
                    return sub_edge(node, f"{name}.\u03bb{next(self._fresh)}", env)
                return visit(node.term, env, spine, scope, rest=True, parent=node)
            raise DebateCompileError(
                f"Edge '{name}': unhandled node {type(node).__name__} during "
                f"source collection."
            )

        root = body
        content = _peel_eta(body)                  # the body under its eta chain: inline
        env, spine = {}, frozenset()
        if isinstance(root, (Mu, Mutilde)) and _match_scaffold(root, self.strict_names) is None:
            env, var = bind(env, root)
            if var:
                spine = frozenset({var})
            vname = root.id.name if isinstance(root, Mu) else root.di.name
            scope = () if _is_affine_binder(vname) else ((root.prop, "context" if isinstance(root, Mu) else "term"),)
            acc = (visit(root.term, env, spine, scope, parent=root)
                   + visit(root.context, env, spine, scope, parent=root))
        elif isinstance(root, (Lamda, Admal)):
            env, var = bind(env, root)
            if var:
                spine = frozenset({var})
            body_ = root.term if isinstance(root, Lamda) else root.context
            acc = visit(body_, env, spine, (), rest=True, parent=root)
        elif isinstance(root, (Cons, Sonc)):
            acc = visit(root.term, env, spine, (), parent=root) + visit(root.context, env, spine, (), parent=root)
        elif isinstance(root, (LamdaFO, TermsPairFO)):
            acc = visit(root.term, env, spine, (), rest=True, parent=root)
            if isinstance(root, TermsPairFO):
                acc = (self._choice(root) or _EMPTY) + acc
        elif isinstance(root, (ConsFO, DestructTermsPairFO)):
            acc = visit(root.context, env, spine, (), rest=True, parent=root)
            if isinstance(root, ConsFO):
                acc = (self._choice(root) or _EMPTY) + acc
        else:
            acc = visit(root, env, spine, ())
        # Strict = the derivation uses no indeterminate anywhere: none among
        # its own sites, and none inside the subarguments it applies.
        strict = (not any(s.kind in ("obligation", "presumption") for s in acc.sources)
                  and not acc.defeasible)
        return Edge(name=name, target_key=target_key, target_side=side,
                    sources=acc.sources, strict=strict, role=role, term=body,
                    target_label=getattr(body, "label", None))

    def _root_statement(self, body, name):
        if isinstance(body, Mu):
            side = "term"
        elif isinstance(body, Mutilde):
            side = "context"
        elif isinstance(body, (Goal, Deleg)):
            side = "term"
        elif isinstance(body, (Laog, Geled)):
            side = "context"
        elif isinstance(body, Lamda):
            side = "term"
            if not body.prop and body.di.prop and getattr(body.term, "prop", None):
                body.prop = f"{body.di.prop}->{body.term.prop}"
        elif isinstance(body, Admal):
            side = "context"
            if not body.prop and body.id.prop and getattr(body.context, "prop", None):
                body.prop = f"{body.context.prop}-{body.id.prop}"
        elif isinstance(body, (LamdaFO, TermsPairFO)):
            side = "term"                      # first order (aida-first-order-reduction)
        elif isinstance(body, (ConsFO, DestructTermsPairFO)):
            side = "context"
        elif isinstance(body, Cons):
            side = "context"                   # an elimination cell as a horn
        elif isinstance(body, Sonc):
            side = "term"
        elif isinstance(body, DI) and body.name in self.strict_names:
            # A bare declared proof: a strict, source-less edge for its side.
            side = "term"
            self._check_kind(body.name, side, name)
        elif isinstance(body, ID) and body.name in self.strict_names:
            # A bare declared refutation (a denied proposition): strict,
            # context side.  Dropping these was the defect of
            # aida-strict-refutation-edges.
            side = "context"
            self._check_kind(body.name, side, name)
        else:
            raise DebateCompileError(
                f"Edge '{name}': body root {type(body).__name__} is not a "
                f"mu/mu' binder, an open leaf or a declared name; refusing "
                f"to guess its side."
            )
        prop = getattr(body, "prop", None)
        if not prop:
            raise DebateCompileError(
                f"Edge '{name}': root proposition missing; run enrichment first."
            )
        return side, prop


def _peel_eta(body):
    """The content under a chain of identity (eta) wrappers."""
    node = body
    while True:
        if isinstance(node, Mu) and isinstance(node.context, ID) and node.context.name == node.id.name:
            node = node.term
        elif isinstance(node, Mutilde) and isinstance(node.term, DI) and node.term.name == node.di.name:
            node = node.context
        else:
            return node


def scion_record(match, env, strict_names, strict_kinds=None, owner="host"):
    """For the evaluator: compile a scaffold's scion against the binders in
    scope (``env``: name -> (prop, side)) exactly as the compiler does and
    return its record(s) - a list, one edge per alternative of a stacked
    scion (``stack_elements``), which ``derivation_status`` reads
    disjunctively."""
    role, site_side, prop, orig, scion_raw, alt, scion_kind = match
    compiler = _Compiler(strict_names, strict_kinds)
    compiler.graph.quiet = True          # a probe, not a graph being built
    if role == "attacker":
        scion = _restore_scion(scion_raw, scion_kind, alt, prop, "r0") if match.legacy else scion_raw
        target_side = "context" if site_side == "term" else "term"
    else:
        scion = scion_raw
        target_side = site_side
    outer = {**{k: (*v, owner) for k, v in env.items()
                if not (match.legacy and role == "attacker" and k == alt)},
             **scion_scope(match, owner)}
    elements = [(scion, {})] if match.legacy else scoped_stack_elements(scion, strict_names)
    return [compiler.compile_body(element, _scion_name(element, "scion"), role,
                                  {**outer, **{k: (*v, owner) for k, v in inner.items()}},
                                  expect_side=target_side, expect_prop=prop)
            for element, inner in elements]


def compile_debate(body: ProofTerm, name: str, *, strict_names=None,
                   strict_kinds=None, env=None) -> DebateGraph:
    """Compile one (possibly composed) debate body into a DebateGraph.

    ``strict_names``: declared names (references to them are strict, not
    assumptions) - the prover's declaration keys.  ``strict_kinds``: the
    matching {name: kind} (see ``declaration_kinds``); when given, a
    declared name occurring on the wrong side for its kind is refused.
    Derivation cycles are admitted (the cyclic fragment); ``is_acyclic``
    on the result says which fragment the debate is in.
    """
    logger.debug("compile: framework of '%s'", name)
    if env:
        # parameters and witness entries (tasks.org, aida-referents-instantiation)
        env = referents.entries(env, getattr(body, "prop", None), name)
    if env is not None and has_context(body, env):
        # an argument's body: every proposition in the context it is
        # stated in (``env``: the argument's referent environment)
        body = contextualise(deepcopy(body), env)
    compiler = _Compiler(strict_names, strict_kinds)
    compiler.compile(body, name)
    graph = compiler.graph
    if env:
        graph.referent_envs[name] = tuple((n, str(s)) for n, s in env)
    logger.debug("compile: '%s' gave %d edge(s) over %d node(s), %d default marker(s)",
                 name, len(graph.edges), len(graph.nodes), len(graph.defaults))
    if logger.isEnabledFor(logging.DEBUG):
        artifact(logger, "compile: the argumentation framework of '%s'" % name,
                 graph.to_text())
    return graph


def compile_document(named_bodies, *, strict_names=None, strict_kinds=None) -> DebateGraph:
    """Compile several (name, body) pairs into one merged DebateGraph."""
    graph = DebateGraph()
    for name, body in named_bodies:
        compiler = _Compiler(strict_names, strict_kinds)
        compiler.compile(body, name)
        graph.merge(compiler.graph)
    return graph
