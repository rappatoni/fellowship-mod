"""The issue graph from the shared debate, without unfolding it (tasks.org,
aida-shared-subarguments, stage 3).

``compile_issue`` (core/dc/strict.py) takes the unfolded term: it compiles
its argumentation framework and adds the strict edges the strict phase
finds in it.  Both walk the whole term, which is as large as the number of
paths through the document graph.  Here the same graph is computed from
the definitions of a ``SharedDebate`` (core/dc/share.py), one *instance*
at a time.

An instance is a statement together with the statements captured and cut
in it - what the unfolded copy of that statement's debate at some site
depends on (decision 5 of the task: strictness is read off the concrete
term, so one sub-debate is decided differently under different captures,
and a definition alone cannot be resolved once and for all).  An instance
is compiled and strictness-resolved once, on its own small term:

- the body of the statement's definition, with each citation replaced by
  what unfolding puts there - the capturing variable, the bare site of a
  statement on the path, or a stand-in for the cited instance;
- for the compiler the stand-in is a citation leaf (a source of the cited
  statement, of the kind its site has), and meeting it compiles the cited
  instance first, so edges come out in the order the unfolded term gives;
- for the strict phase the stand-in is a ``Stub`` holding exactly what the
  enclosing scaffolds may ask of the resolved instance: an open site if
  one remains in it, the captured variables still free in it, and whether
  a decision was made inside.  ``strict_resolve`` itself is unchanged.

Strict edges are collected as before - the outermost closed subterms that
owe their closedness to a decision - descending through stubs into the
resolved instances; only such a closed subterm is written out in full
(``adopt`` replays it), and it has no undecided scaffold left in it.

The reference is ``compile_issue(unfold(graph, issue))``: the tests compare
edges (up to the names of copies), default markers and the labellings.
"""

import logging
from copy import copy, deepcopy
from itertools import count

from core.ac.ast import (
    ProofTerm, Term, Context, Mu, Mutilde, Lamda, Admal, Cons, Sonc,
    Goal, Laog, Deleg, Geled, ID, DI, Hyp, Pyh,
    first_order_node, FirstOrderNotSupported,
)
from core.comp.oracle_terms import _free_names, all_names, contains_uncatchable_clash
from core.dc.debate_graph import DebateGraph, Edge, _Compiler, _match_scaffold, binder_origin
from core.dc.share import SharedDebate, is_cite, _same_origin
from core.dc.strict import (
    strict_resolve, strict_in_scope, is_closed, strict_edge_candidate, _contains_assumption, _eta_statement, fold_occurrences, key_of,
)
from core.logging_util import artifact

#: This module performs two stages of the pipeline and speaks as them, so
#: that `explain` groups its account under the stage that is being done.
_compile_log = logging.getLogger("core.dc.debate_graph")
logger = logging.getLogger("core.dc.strict")

_SITES = (Goal, Laog, Deleg, Geled)

#: Prefix of the variables that stand, inside an instance's own term, for
#: the binders of the context that capture a statement in it.  No name a
#: user or the unfolder writes starts with it.
PARAM = "§"


class Stub(Term, Context):
    """A resolved instance, as far as an enclosing scaffold can see it: a
    chain of leaves - an open site if the instance still has one, and the
    captured variables still free in it under the names they have here.
    The strict phase's own predicates (open sites, free names, decisions)
    read it like any other subterm.  It stands where the instance's term
    or context would, so it is both for the node constructors.

    ``captured`` lists the captured statements in the order of the leading
    leaves.  What each stands for here is read off its leaf (``actual``),
    so a renaming or a substitution that reaches the leaves - the outer
    step b := the original when a scaffold is decided - reaches the stub."""

    def __init__(self, instance, leaves, decision, pres, captured=()):
        captured = tuple(captured)
        self.instance = instance
        self.term = leaves[0] if leaves else None
        self.context = Stub(instance, leaves[1:], False, "", captured[1:]) if len(leaves) > 1 else None
        self._strict_decision = decision
        self.pres = pres
        self.prop = None
        self.flag = None
        self._captured = captured

    @property
    def actual(self) -> dict:
        """{captured statement: what it is in the enclosing term} - its
        variable's name, or the term substituted for that variable."""
        found = {}
        if self._captured:
            leaf = self.term
            found[self._captured[0]] = leaf.name if isinstance(leaf, (ID, DI)) else leaf
        if isinstance(self.context, Stub):
            found.update(self.context.actual)
        return found


class _Pending(Stub):
    """A cited instance not chosen yet, while its citing instance is being
    strictness-resolved: which instance it is depends on what the
    variables capturing its statements stand for there, which the strict
    phase knows only where it meets it (``IssueResolver.resolve``)."""

    pending = True

    def __init__(self, child, names, pres):
        super().__init__(child, [], False, pres)
        self.names = names


class _Instance:
    """One statement under a set of captures and cuts.  ``opened``: the
    captured statements whose capturing variable stands, where the instance
    is written out, for something not strict - an attack's alt, or a
    scaffold's b whose original is not strict - so that the strict phase
    reads them as it reads those variables in the unfolded term."""

    def __init__(self, statement, captured, cut, opened=frozenset()):
        self.statement = statement
        self.captured = captured          # frozenset of statements
        self.cut = cut                    # frozenset of statements
        self.opened = opened              # frozenset of captured statements
        self.resolved = None              # the strict-resolved local term
        self.open = False                 # an open site remains in it
        self.free = frozenset()           # captured statements still free in it
        self.decision = False             # a strict decision was made inside
        self.trace = []
        self.edges = None                 # strict edges found inside (memo)

    @property
    def key(self):
        return (self.statement, self.captured, self.cut, self.opened)

    def __deepcopy__(self, memo):
        # Terms are deep-copied freely (the compiler, the strict phase); an
        # instance referred to from a leaf is shared, never copied.
        return self


def _param(statement) -> str:
    return f"{PARAM}{statement[1][0]}:{statement[0]}"


def _rebuild(node, leaf, captured=None):
    """A copy of ``node`` with every citation replaced by ``leaf(citation)``,
    and every unengaged exposed node whose statement the instance captures
    (``captured(statement)``: the variable, or None) by that variable
    (core/dc/share.py, ``exposed_as``)."""
    if is_cite(node):
        return leaf(node)
    if not isinstance(node, ProofTerm):
        return node
    bound = getattr(node, "exposed_as", None)
    if bound is not None and captured is not None:
        var = captured(bound)
        if var is not None:
            return var
    out = copy(node)
    for slot in ("term", "context"):
        child = getattr(node, slot, None)
        if isinstance(child, ProofTerm):
            setattr(out, slot, _rebuild(child, leaf, captured))
    return out


def _renamed(node, names):
    """A copy of ``node`` with the variables named in ``names`` renamed."""
    if isinstance(node, (ID, DI)) and node.name in names:
        out = copy(node)
        out.name = names[node.name]
        return out
    if not isinstance(node, ProofTerm):
        return node
    out = copy(node)
    for slot in ("term", "context"):
        child = getattr(node, slot, None)
        if isinstance(child, ProofTerm):
            setattr(out, slot, _renamed(child, names))
    return out


def _has_decision(node) -> bool:
    if not isinstance(node, ProofTerm):
        return False
    if getattr(node, "_strict_decision", False):
        return True
    return any(_has_decision(getattr(node, slot, None)) for slot in ("term", "context"))


class IssueResolver:
    """Compiles and strictness-resolves the instances of a shared debate."""

    def __init__(self, shared: SharedDebate, strict_names=None, strict_kinds=None):
        self.shared = shared
        self.strict_names = set(strict_names or ())
        self.strict_kinds = strict_kinds
        #: {(statement, captured, cut): instance} - what the compiler reads
        self.instances = {}
        #: the same with some captured statements opened (``_opened``):
        #: variants the strict phase alone tells apart
        self._variants = {}
        self._compiled = set()
        self._stubs = count(1)
        self._sub_arguments = {}

    # -- instances ----------------------------------------------------------------

    def instance(self, statement, captured, cut, opened=()) -> _Instance:
        """The instance of ``statement`` under the given captures and cuts,
        restricted to what the statement can reach; with ``opened``, its
        variant for the strict phase."""
        relevant = self.shared.reach(statement)
        captured = frozenset(s for s in captured if s in relevant)
        key = (statement, captured, frozenset(s for s in cut if s in relevant))
        opened = frozenset(s for s in opened if s in captured)
        table = self._variants if opened else self.instances
        found = table.get((*key, opened))
        if found is None:
            found = table[(*key, opened)] = _Instance(*key, opened)
        if opened:
            self.instance(statement, captured, key[2])      # its instance is counted
        return found

    def root(self) -> _Instance:
        return self.instance(self.shared.issue, (), ())

    def _at(self, instance, citation):
        """What unfolding puts at ``citation`` inside ``instance``:
        ("captured", variable name), ("cut", None) or ("instance", child,
        names) with ``names`` the variable of every statement captured
        there."""
        names = {s: _param(s) for s in instance.captured}
        names.update(citation.captures)
        target = citation.debate
        if target in names:
            return "captured", names[target], names
        path = instance.cut | {instance.statement} | citation.cuts
        if target in path or getattr(citation, "bare", False):
            return "cut", None, names
        return "instance", self.instance(target, names, path), names

    def _capturer(self, instance, compiled=False):
        """For ``_rebuild``: the variable of a statement the instance
        captures, else None; for the compiler marked as a captured
        citation is (an obligation source of the statement)."""
        def captured(statement):
            if statement not in instance.captured:
                return None
            out = self._variable(statement, _param(statement), False)
            if compiled:
                out.cites = out.name
                out.source_kind = "obligation"
            return out
        return captured

    def _variable(self, statement, name, presumed):
        key, side = statement
        leaf = (DI if side == "term" else ID)(name, self.shared._prop(key))
        if presumed:
            leaf.captured_presumption = True
        return leaf

    def _site(self, statement):
        return self.shared._site(statement, f"u{next(self._stubs)}")

    def _kind(self, statement) -> str:
        return ("presumption" if "presumption" in self.shared.graph.defaults.get(statement, ())
                else "obligation")

    # -- the framework ------------------------------------------------------------

    def _compile_term(self, instance):
        """The instance's term as the compiler reads it: a captured
        statement is a source of the kind its site had, a cited instance a
        source of the kind the cited statement's own site has - unless the
        cited instance's root is no scaffold the compiler matches (an attack
        whose wing is only its own alt): the compiler reads such a debate
        as a sub-argument of the body it stands in, so it is written out
        here, one level, under the names the captured statements have."""
        def leaf(citation):
            what, value, names = self._at(instance, citation)
            target = citation.debate
            if what == "cut":
                return self._site(target)
            if what == "captured":
                out = self._variable(target, value, citation.presumed)
                out.cites = out.name
                out.source_kind = "presumption" if citation.presumed else "obligation"
                return out
            if self._sub_argument(target):
                return _renamed(self._compile_term(value),
                                {_param(s): names[s] for s in value.captured})
            out = self._variable(target, f"{PARAM}{next(self._stubs)}", False)
            out.cites = self.shared.defs[target].name or out.name
            out.source_kind = self._kind(target)
            out.instance = value
            return out

        term = _rebuild(self.shared.defs[instance.statement].body, leaf,
                        self._capturer(instance, compiled=True))
        found = first_order_node(term)
        if found is not None:
            raise FirstOrderNotSupported("Debate graph compilation", found)
        return term

    def _sub_argument(self, statement) -> bool:
        """Whether the compiler reads the debate of ``statement``, where a
        body needs it, as a sub-argument of that body rather than as the
        statement's site: its root is a binder but no scaffold it matches.
        The root's shape is the definition's own, whatever its citations
        are written out as."""
        found = self._sub_arguments.get(statement)
        if found is None:
            body = self.shared.defs[statement].body
            found = self._sub_arguments[statement] = (
                isinstance(body, (Mu, Mutilde)) and _match_scaffold(body, self.strict_names) is None)
        return found

    def compile(self, name: str) -> DebateGraph:
        """The argumentation framework of the issue's unfolded term: every
        instance unfolding writes out is compiled once, where it is first
        met."""
        _compile_log.debug("compile: framework of '%s', one instance of a sub-debate at a time", name)
        compiler = _Compiler(self.strict_names, self.strict_kinds)

        def on_citation(leaf, host):
            child = getattr(leaf, "instance", None)
            if child is None or child.key in self._compiled:
                return
            self._compiled.add(child.key)
            # The cited instance's scaffolds, compiled where they stand, under
            # the name of the body they stand in: its scions become edges;
            # its own site is this citation's source.
            compiler.compile_body(self._compile_term(child), host, "argument", {})

        compiler.on_citation = on_citation
        root = self.root()
        self._compiled.add(root.key)
        compiler.compile(self._compile_term(root), name)
        return compiler.graph

    # -- the strict phase ---------------------------------------------------------

    def resolve(self, instance) -> _Instance:
        """Strictness-resolve ``instance`` (memoised): its own scaffolds on
        a term that shows of each cited instance what the decisions depend
        on.  A cited instance is chosen and resolved where the strict phase
        meets it, since what its captured statements stand for depends on
        the scaffold variables in scope there (``_opened``)."""
        if instance.resolved is not None:
            return instance

        def leaf(citation):
            what, value, names = self._at(instance, citation)
            target = citation.debate
            if what == "cut":
                return self._site(target)
            if what == "captured":
                return self._variable(target, value, citation.presumed)
            shown = self.shared.defs[target].name or f"<{self.shared.marker(target)}>"
            return _Pending(value, names, shown)

        def on_pending(node, scope):
            child = node.instance
            opened = self._opened(child, node.names, scope)
            child = self.instance(child.statement, child.captured, child.cut, opened)
            return self._stub(self.resolve(child), node.names)

        # The parameters standing for something not strict enter as an
        # attack's alt does: "the statement fails", an open site.
        scope = {_param(s): ("alt", DI if s[1] == "term" else ID, None) for s in instance.opened}
        term = _rebuild(self.shared.defs[instance.statement].body, leaf, self._capturer(instance))
        instance.resolved, _ = strict_resolve(term, self.strict_names, trace=instance.trace,
                                              edges=False, scope=scope, on_pending=on_pending)
        instance.open = _contains_assumption(instance.resolved)
        params = {_param(s): s for s in instance.captured}
        instance.free = frozenset(params[n] for _kind, n in _free_names(instance.resolved)
                                  if n in params)
        instance.decision = _has_decision(instance.resolved)
        return instance

    def _opened(self, child, names, scope) -> frozenset:
        """The statements captured in ``child`` whose capturing variable
        (``names``) stands for something not strict under ``scope``, the
        scaffold variables in scope where the strict phase meets the
        citation: an attack's alt (an open site), a scaffold's b whose
        original is not strict there, a parameter of the citing instance
        that is opened itself.  An argument's own binder stands for a
        commitment the debate made: strict."""
        opened = set()
        for statement in child.captured:
            name = names.get(statement)
            entry = scope.get(name) if isinstance(name, str) else None
            if entry is None:
                continue
            role, _kind, what = entry
            if role == "alt" or not strict_in_scope(what, scope):
                opened.add(statement)
        return frozenset(opened)

    def _stub(self, child, names) -> Stub:
        """The stand-in for the resolved instance ``child`` where its
        captured statements have the variables ``names`` (a name, or the
        term substituted for the variable)."""
        target = child.statement
        captured = sorted(child.free)
        leaves = [deepcopy(names[s]) if isinstance(names[s], ProofTerm)
                  else self._variable(s, names[s], False) for s in captured]
        if child.open:
            leaves.append(self._site(target))
        shown = self.shared.defs[target].name or f"<{self.shared.marker(target)}>"
        return Stub(child, leaves, child.decision, shown, captured)

    def materialise(self, node, used=None, *, deep=True, names=None) -> ProofTerm:
        """``node`` of a resolved instance with every stub written out: the
        cited instance's resolved term, its captured variables under their
        names here, binders renamed on reuse.  With ``deep=False`` the
        stubs inside stay stubs (under the new names): one level only."""
        used = set() if used is None else used

        def fresh(name):
            if name == "_":
                return name
            candidate, n = name, 1
            while candidate in used:
                n += 1
                candidate = f"{name}_{n}"
            used.add(candidate)
            return candidate

        def walk(n, names):
            if isinstance(n, Stub):
                here = {statement: (names.get((statement[1], actual), actual)
                                    if isinstance(actual, str) else walk(actual, names))
                        for statement, actual in n.actual.items()}
                if not deep:
                    return self._stub(n.instance, here)
                inner = {(statement[1], _param(statement)): actual
                         for statement, actual in here.items()}
                return walk(self.resolve(n.instance).resolved, inner)
            if isinstance(n, _SITES):
                return type(n)(n.number, n.prop)
            if isinstance(n, (ID, DI)):
                value = names.get(("context" if isinstance(n, ID) else "term", n.name), n.name)
                if isinstance(value, ProofTerm):
                    # a captured variable the enclosing term has substituted
                    used.update(all_names(value))
                    return deepcopy(value)
                out = type(n)(value, n.prop)
            else:
                out = None
            if out is not None:
                if getattr(n, "captured_presumption", False):
                    out.captured_presumption = True
                return out
            if isinstance(n, Lamda):
                new = fresh(n.di.di.name)
                out = Lamda(Hyp(DI(new, n.di.di.prop), n.di.prop),
                            walk(n.term, {**names, ("term", n.di.di.name): new}))
            elif isinstance(n, Admal):
                new = fresh(n.id.id.name)
                out = Admal(Pyh(ID(new, n.id.id.prop), n.id.prop),
                            walk(n.context, {**names, ("context", n.id.id.name): new}))
            elif isinstance(n, Mu):
                new = fresh(n.id.name)
                names2 = {**names, ("context", n.id.name): new}
                return _same_origin(n, Mu(ID(new, n.id.prop), n.prop,
                                          walk(n.term, names2), walk(n.context, names2)))
            elif isinstance(n, Mutilde):
                new = fresh(n.di.name)
                names2 = {**names, ("term", n.di.name): new}
                return _same_origin(n, Mutilde(DI(new, n.di.prop), n.prop,
                                               walk(n.term, names2), walk(n.context, names2)))
            elif isinstance(n, Cons):
                out = Cons(walk(n.term, names), walk(n.context, names))
            elif isinstance(n, Sonc):
                out = Sonc(walk(n.context, names), walk(n.term, names))
            else:
                raise ValueError(f"Cannot write out node {type(n).__name__}.")
            out.prop = n.prop
            return out

        return walk(node, dict(names or {}))

    # -- for evaluation (core/comp/evaluate.py) -------------------------------------

    def open(self, stub, used) -> ProofTerm:
        """The resolved instance a stub stands for, written out one level:
        its captured variables under the names they have at the stub, its
        binders renamed where ``used`` already has them, the instances it
        cites still stubs."""
        names = {(statement[1], _param(statement)): actual
                 for statement, actual in stub.actual.items()}
        return self.materialise(self.resolve(stub.instance).resolved, used, deep=False, names=names)

    def reads_as_its_site(self, instance) -> bool:
        """Whether the compiler, walking into the resolved instance from the
        body that cites it, ends at the statement's own bare site: every
        scaffold from the root inwards is still there and the innermost
        original is the site.  Then the instance is, for the citing edge,
        one source at its statement - whatever its scions hold."""
        node = self.resolve(instance).resolved
        while True:
            match = _match_scaffold(node, self.strict_names)
            if match is None:
                return isinstance(node, _SITES)
            node = match[3]

    def for_record(self, node) -> ProofTerm:
        """``node`` as the compiler must read it to give an edge its
        sources (``scion_record``): a stub that reads as its site becomes
        a source of that statement, any other is written out one level and
        read the same way.  Only what the sources depend on is written
        out; the instances behind sites stay closed."""
        used = set()

        def convert(n):
            if isinstance(n, Stub):
                instance = n.instance
                if self.reads_as_its_site(instance):
                    out = self._variable(instance.statement, f"{PARAM}{next(self._stubs)}", False)
                    out.cites = self.shared.defs[instance.statement].name or out.name
                    out.source_kind = self._kind(instance.statement)
                    return out
                return convert(self.open(n, used))
            if not isinstance(n, ProofTerm):
                return n
            out = copy(n)
            for slot in ("term", "context"):
                child = getattr(n, slot, None)
                if isinstance(child, ProofTerm):
                    setattr(out, slot, convert(child))
            return out

        return convert(node)

    def strict_edges(self, instance=None) -> list:
        """The strict edges of the unfolded term: the outermost closed
        eta-wrapped copies of arguments defeasible as registered that hold
        no uncatchable clash (``strict_resolve``'s collector), found
        in the resolved instances.  What is found inside an instance does
        not depend on where it is cited, so it is computed once."""
        instance = self.resolve(instance or self.root())
        if instance.edges is not None:
            return instance.edges
        out = []

        def visit(node):
            if isinstance(node, Stub):
                out.extend(self.strict_edges(node.instance))
                return
            if not isinstance(node, ProofTerm):
                return
            statement = _eta_statement(node)
            if (statement is not None and strict_edge_candidate(node, self.strict_names, _has_decision)
                    and is_closed(node, self.strict_names)):
                full = self.materialise(node)
                name = binder_origin(node)
                if contains_uncatchable_clash(full):
                    logger.debug("strict: no edge for '%s': it holds an uncatchable clash", name)
                else:
                    out.append(Edge(name=f"{name}*", target_key=key_of(statement),
                                    target_side=statement[1], sources=(), strict=True,
                                    role="strict", term=full))
                    logger.debug("strict: edge '%s*' (a closed derivation the framework missed)", name)
                    return                  # maximal: do not descend into it
            for slot in ("term", "context"):
                child = getattr(node, slot, None)
                if isinstance(child, ProofTerm):
                    visit(child)

        visit(instance.resolved)
        instance.edges = out
        return out

    def decisions(self) -> list:
        """(statement, decision) of every strict decision, per instance."""
        self.resolve(self.root())
        return [d for table in (self.instances, self._variants) for i in table.values()
                if i.resolved is not None for d in i.trace]


def issue_graph(resolver: IssueResolver, name: str) -> DebateGraph:
    """The issue graph of a resolver's debate: the framework of the
    instances unfolding writes out, plus the strict edges the strict phase
    finds in them."""
    graph = resolver.compile(name)
    edges = resolver.strict_edges()
    logger.debug("strict: %d decision(s), %d strict edge(s) for '%s' over %d instance(s)",
                 len(resolver.decisions()), len(edges), name, len(resolver.instances))
    if logger.isEnabledFor(logging.DEBUG):
        artifact(logger, "strict: the strict edges it contributes",
                 "\n".join(f"{e.name}: {e.target_key}[{e.target_side[0]}] <- - (source-less, strict)"
                            for e in edges) or "none")
    for edge in edges:
        graph.nodes.setdefault(edge.target_key, graph.nodes.get(edge.target_key, edge.target_key))
        graph.add_edge(edge)
    fold_occurrences(graph)
    if _compile_log.isEnabledFor(logging.DEBUG):
        artifact(_compile_log, "compile: the argumentation framework of '%s'" % name,
                 graph.to_text())
    return graph


def compile_issue_shared(shared: SharedDebate, name: str, *, strict_names=None,
                         strict_kinds=None) -> DebateGraph:
    """The issue graph of a shared debate: what
    ``compile_issue(shared.expand(), name)`` gives, computed one instance at
    a time."""
    return issue_graph(IssueResolver(shared, strict_names, strict_kinds), name)


__all__ = ["IssueResolver", "Stub", "issue_graph", "compile_issue_shared", "PARAM"]
