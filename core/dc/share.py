"""Sharing: the debate of an issue as named sub-debates (tasks.org,
aida-shared-subarguments; decisions of 2026-10-01).

Unfolding (core/dc/unfold.py) copies the debate of a statement into every
place the statement is needed, so the term is as large as the number of
paths through the document graph.  Here every statement reachable from the
issue gets ONE definition - its debate, the root under its supporter
stack and the contrary's attacker wing, exactly as ``Unfolder._debate``
builds it - and a place that needs the statement holds a *citation* of
that definition instead of a copy.

Names are transparent: a citation means its expansion.  ``expand`` writes
the definitions back in and yields the term ``unfold`` yields, up to
alpha-equivalence and site numbering; ``unfold`` is untouched and remains
the reference the tests compare against.

A citation is not context-free.  Unfolding a statement depends on the
binders in scope (a statement bound there is *captured*: it becomes that
variable) and on the statements already being expanded on the path (those
are *cut*: left as a bare site).  A citation therefore records the captures
and cuts of its own site - the ``env`` and ``spine`` that
``Unfolder.statement`` receives there - and expansion accumulates them, the
inner ones overriding, in the order unfolding applies them: captured first,
then cut, then expanded.  Printed, that is the author's notation

    d[alpha -> !:A, B:?]

"d, with alpha capturing its delegation of A, and its obligation B left
open": ``?:A`` / ``!:A`` are term-side sites, ``A:?`` / ``A:!`` context-side
ones, as the term printer writes them.

What is shown by name (decision 3): a definition cited from two or more
sites, or one the author cited by name.  A sub-debate used once is printed
inline, and so is one that is only a bare site.  Whatever the naming, the
definitions are what is shared; naming is presentation.
"""

import logging
from dataclasses import dataclass, field
from itertools import count

from core.ac.ast import (
    ProofTerm, Mu, Mutilde, Lamda, Admal, Cons, Sonc,
    Goal, Laog, Deleg, Geled, ID, DI, Hyp, Pyh,
)
from core.ac.prop import Prop
from core.dc.debate_graph import DebateGraph, canonical_prop
from core.dc.unfold import Unfolder, UnfoldError, contrary

logger = logging.getLogger(__name__)

_SITES = (Goal, Laog, Deleg, Geled)

#: Prefix of the names given to sub-debates no registered argument names.
#: Reserved: the wrapper refuses it for user names (wrap/prover.py).
ANON_PREFIX = "anon_"


def is_cite(node) -> bool:
    """A citation of a statement's debate (as opposed to an author's
    ``cite NAME`` leaf, which carries ``cites``)."""
    return isinstance(node, (ID, DI)) and getattr(node, "debate", None) is not None


def debate_statement(key) -> tuple:
    """The statement a definition's key is about.  The key of a
    statement's canonical debate is the statement itself, (proposition,
    side); that of a variant is (statement, own_kind, top) - the root kind
    and the argument on top that ``Unfolder._debate`` is given (an exposed
    node's debate, an argument entrypoint)."""
    return key if len(key) == 2 else key[0]


def _cite(statement, prop, presumed, captures, cuts, bare=False, debate=None):
    """The citation leaf for ``statement``: a term-side or context-side
    name leaf carrying the site's captures (statement -> binder name) and
    cuts (statements on the path).  ``debate`` is the key of the definition
    cited, the statement's canonical one by default.  A *bare* citation
    stands for the bare contrary of an attack scaffold (``Unfolder._bare``):
    where it is written out it is the capturing variable or a bare site,
    never the statement's debate."""
    leaf = DI("§", prop) if statement[1] == "term" else ID("§", prop)
    leaf.debate = statement if debate is None else debate
    leaf.statement = statement
    leaf.presumed = presumed
    leaf.captures = dict(captures)
    leaf.cuts = frozenset(cuts)
    leaf.bare = bare
    return leaf


def is_bare(node) -> bool:
    return is_cite(node) and getattr(node, "bare", False)


@dataclass
class Definition:
    """The debate of one statement: ``body`` is its root under its
    supporter stack and the contrary's attacker wing, with every site of
    another statement that no binder of the body captures left as a
    citation.  ``key``: the statement, or (statement, own_kind, top) for a
    variant (``debate_statement``)."""

    statement: tuple
    body: ProofTerm
    key: tuple = None
    #: Display name; None while the definition is printed inline.
    name: str = None
    #: Citations of this definition among the reachable definitions.
    cited: int = 0

    @property
    def trivial(self) -> bool:
        """Nothing derives or contests the statement: its debate is its
        bare site."""
        return isinstance(self.body, _SITES)


class _Builder(Unfolder):
    """Builds the definitions with the unfolder's own scaffolding code:
    the only difference is that a statement no binder in scope captures is
    not unfolded in place but cited."""

    def __init__(self, graph: DebateGraph, subarguments="default"):
        super().__init__(graph, subarguments=subarguments)
        self.defs = {}
        self._todo = []

    def _key(self, statement, own_kind=None, top=None):
        """The key of the debate ``Unfolder._debate`` builds for these
        parameters: the statement itself where that is its canonical
        debate."""
        if own_kind == self._presumed(statement):
            own_kind = None                      # the canonical root anyway
        if own_kind is None and top is None:
            return statement
        return (statement, own_kind, top)

    def statement(self, statement, env, spine, presumed=False, own_kind=None):
        if statement in env:
            return super().statement(statement, env, spine, presumed)
        key = self._key(statement, own_kind)
        self._todo.append(key)
        return _cite(statement, self._prop(statement[0]), presumed, env, spine, debate=key)

    def _exposed(self, node, sub, env, spine, names, primary, walk):
        """An exposed node (core/dc/unfold.py, ``Unfolder._exposed``):
        engaged by nobody, as written; otherwise a citation of the debate
        the unfolder builds there - an obligation root, a presumption of the
        statement at the bottom of the stack, the node's own edge on top
        iff its argument has primacy - which is the canonical debate unless
        one of those differs.  A definition is built without the scope of
        its citing sites, so whether the statement is captured is decided
        where the citation is written out, as for a site; a captured node
        is the captured variable alone."""
        statement = (sub.target_key, sub.target_side)
        if statement in env:
            return super()._exposed(node, sub, env, spine, names, primary, walk)
        kept = self._kept(node, sub, statement, env, spine, names, primary, walk)
        if not self._engaged(statement, sub):
            out = super()._exposed(node, sub, env, spine, names, primary, walk)
            # As written - unless a binder of an enclosing definition binds
            # the statement, which only expansion can see.
            out.exposed_as = statement
            out.kept = kept
            return out
        logger.debug("share: node '%s' exposed as a citation of %s", sub.name, self._show(statement))
        key = self._key(statement, False, sub.name if primary else None)
        self._todo.append(key)
        leaf = _cite(statement, self._prop(statement[0]), False, env, spine, debate=key)
        leaf.kept = kept
        return leaf

    def _kept(self, node, sub, statement, env, spine, names, primary, walk):
        """What the node becomes where a binder outside the definition
        captures its statement and the setting keeps its own derivation
        (``Unfolder._exposed``): (SUP(captured, own), the same attacked by
        a copy of the contrary's debate - for a capture by a scaffold's
        variable under "preserve" - or None).  Built here, in the citing
        definition's scope; the captured variable is a bare citation of the
        statement, which is that variable wherever a form is used.  None
        where the setting keeps nothing."""
        keep = (self.subarguments == "preserve"
                or (self.subarguments == "default" and primary))
        if not keep:
            return None
        contra = contrary(statement)
        captured = _cite(statement, self._prop(statement[0]), False, env, spine, bare=True)
        alt, beta = self._fresh_alt(), self._fresh_beta()
        own = walk(node, self._scaffold_scope(statement, alt, beta, env), names, False)
        term = self._support(statement, captured, own, alt, beta)
        attacked = None
        if self.subarguments == "preserve" and (self._by_target.get(contra)
                                                or self.graph.defaults.get(contra)):
            alt2, beta2 = self._fresh_alt(), self._fresh_beta()
            wing = self._wing(contra, self._scaffold_scope(statement, alt2, beta2, env),
                              spine | {contra})
            attacked = self._attack(statement, term, wing, alt2, beta2)
        return term, attacked

    def _bare(self, statement, env):
        # A definition is built without the scope of its citing sites, so
        # whether the bare contrary is captured is decided where it is
        # written out.
        if statement in env:
            return super()._bare(statement, env)
        presumed = "presumption" in self.graph.defaults.get(statement, ())
        return _cite(statement, self._prop(statement[0]), presumed, env, (), bare=True)

    def build(self, issue):
        self._todo = [issue]
        while self._todo:
            key = self._todo.pop(0)
            if key in self.defs:
                continue
            statement = debate_statement(key)
            own_kind, top = (None, None) if key == statement else key[1:]
            edge = (next(e for e in self._by_target.get(statement, []) if e.name == top)
                    if top is not None else None)
            body = self._debate(statement, {}, frozenset({statement}), own_kind=own_kind, top=edge)
            self.defs[key] = Definition(statement, body, key)
        return self.defs


@dataclass
class SharedDebate:
    """The debate of ``issue`` as one definition per reachable statement."""

    graph: DebateGraph
    issue: tuple
    defs: dict
    #: The names the builder gave the scaffolds' alt and b: a capture by one
    #: of them is what "preserve" answers with a copy of the attackers.
    scaffold_vars: frozenset = frozenset()
    #: The ``subarguments`` setting the definitions were built under.
    subarguments: str = "default"
    _reach: dict = field(default_factory=dict, repr=False)

    # -- structure ----------------------------------------------------------

    def reach(self, statement) -> frozenset:
        """Statements unfolding ``statement`` can meet (a superset: the
        graph's rational closure), cached."""
        found = self._reach.get(statement)
        if found is None:
            found = self._reach[statement] = frozenset(self.graph.reachable(statement))
        return found

    def _prop(self, key):
        return self.graph.nodes.get(key, key)

    def _site(self, statement, number):
        key, side = statement
        prop = self._prop(key)
        presumed = "presumption" in self.graph.defaults.get(statement, ())
        if side == "term":
            return Deleg(number, prop) if presumed else Goal(number, prop)
        return Geled(number, prop) if presumed else Laog(number, prop)

    def marker(self, statement) -> str:
        """A statement as the site it is when left open: ``?:A`` / ``!:A``
        on the term side, ``A:?`` / ``A:!`` on the context side."""
        key, side = statement
        mark = "!" if "presumption" in self.graph.defaults.get(statement, ()) else "?"
        return f"{mark}:{self._prop(key)}" if side == "term" else f"{self._prop(key)}:{mark}"

    # -- interpretation -------------------------------------------------------

    def _instantiate(self, key, env, spine, stop, used, sites, marks=None):
        """The body of the definition ``key`` under the captures ``env``
        and the path ``spine`` (which already holds its statement).
        Citations of definitions in ``stop`` stay citations, annotated with
        what is in force at their site; all others are written back in.
        Binders are renamed on reuse (``used``), sites renumbered;
        ``marks`` collects the new names of the scaffolds' variables."""
        definition = self.defs.get(key)
        if definition is None:
            raise UnfoldError(f"No definition for {key}; it is not reachable from the issue.")
        marks = set() if marks is None else marks

        def kept(node, name, names):
            """A node whose own derivation the setting keeps on top of the
            variable ``name`` capturing its statement (``_Builder._kept``),
            or None."""
            forms = getattr(node, "kept", None)
            if forms is None:
                return None
            plain, attacked = forms
            return walk(attacked if attacked is not None and name in marks else plain, names)

        def fresh(name):
            if name == "_":
                return name
            candidate, n = name, 1
            while candidate in used:
                n += 1
                candidate = f"{name}_{n}"
            used.add(candidate)
            return candidate

        def site(node):
            return type(node)(f"u{next(sites)}", node.prop)

        def cite(node, names):
            target, statement = node.debate, node.statement
            env2 = dict(env)
            for captured, var in node.captures.items():
                env2[captured] = names.get((captured[1], var), var)
            spine2 = spine | node.cuts
            if statement in env2:
                found = kept(node, env2[statement], names)
                if found is not None:
                    return found
                prop_key, side = statement
                var = (DI if side == "term" else ID)(env2[statement], self._prop(prop_key))
                if node.presumed:
                    var.captured_presumption = True
                return var
            if statement in spine2 or is_bare(node):
                return self._site(statement, f"u{next(sites)}")
            if target in stop:
                relevant = self.reach(statement)
                return _cite(statement, node.prop, node.presumed,
                             {s: v for s, v in env2.items() if s in relevant},
                             {s for s in spine2 if s in relevant}, debate=target)
            return self._instantiate(target, env2, spine2 | {statement}, stop, used, sites, marks)

        def walk(node, names):
            if isinstance(node, _SITES):
                return site(node)
            if is_cite(node):
                return cite(node, names)
            bound = getattr(node, "exposed_as", None)
            if bound is not None and bound in env:
                # an unengaged exposed node an enclosing binder captures
                found = kept(node, env[bound], names)
                if found is not None:
                    return found
                prop_key, side = bound
                return (DI if side == "term" else ID)(env[bound], self._prop(prop_key))
            if isinstance(node, ID):
                out = ID(names.get(("context", node.name), node.name), node.prop)
            elif isinstance(node, DI):
                out = DI(names.get(("term", node.name), node.name), node.prop)
            else:
                out = None
            if out is not None:
                if getattr(node, "captured_presumption", False):
                    out.captured_presumption = True
                return out
            if isinstance(node, Lamda):
                new = fresh(node.di.di.name)
                out = Lamda(Hyp(DI(new, node.di.di.prop), node.di.prop),
                            walk(node.term, {**names, ("term", node.di.di.name): new}))
                out.prop = node.prop
                return out
            if isinstance(node, Admal):
                new = fresh(node.id.id.name)
                out = Admal(Pyh(ID(new, node.id.id.prop), node.id.prop),
                            walk(node.context, {**names, ("context", node.id.id.name): new}))
                out.prop = node.prop
                return out
            if isinstance(node, Mu):
                new = fresh(node.id.name)
                if node.id.name in self.scaffold_vars:
                    marks.add(new)
                names2 = {**names, ("context", node.id.name): new}
                return _same_origin(node, Mu(ID(new, node.id.prop), node.prop,
                                             walk(node.term, names2), walk(node.context, names2)))
            if isinstance(node, Mutilde):
                new = fresh(node.di.name)
                if node.di.name in self.scaffold_vars:
                    marks.add(new)
                names2 = {**names, ("term", node.di.name): new}
                return _same_origin(node, Mutilde(DI(new, node.di.prop), node.prop,
                                                  walk(node.term, names2), walk(node.context, names2)))
            if isinstance(node, Cons):
                out = Cons(walk(node.term, names), walk(node.context, names))
                out.prop = node.prop
                return out
            if isinstance(node, Sonc):
                out = Sonc(walk(node.context, names), walk(node.term, names))
                out.prop = node.prop
                return out
            raise UnfoldError(f"Cannot share node {type(node).__name__}.")

        return walk(definition.body, {})

    def skeleton(self, key) -> ProofTerm:
        """The definition ``key`` on its own: its body with every
        citation left as the open site of the cited statement.  This is the
        unit the type check replays (core/dc/typecheck.py): a citation's
        expansion, and a variable that captures it, have the type of that
        site, so well-typed skeletons make every expansion well-typed -
        provided each statement has one spelling (``spelling_clashes``)."""
        every = frozenset(d.statement for d in self.defs.values())
        return self._instantiate(key, {}, every, frozenset(), set(), count(1))

    def spelling_clashes(self) -> dict:
        """{display proposition: spellings} for the statements of this
        debate that its arguments spell in more than one way.

        A statement is a proposition up to ``canonical_prop``, which
        identifies ``~A`` with ``A -> false``.  Fellowship does not: where
        a debate spelled one way is written into a site spelled the other,
        its replay refuses the join.  A skeleton never contains that join -
        the site is open there - so the per-definition type check is only
        as strong as the expanded one when no statement is spelled two
        ways.  The caller falls back to the expanded check otherwise.
        """
        statements = {d.statement for d in self.defs.values()}
        keys = {key for key, _side in statements}
        targets = statements | {contrary(s) for s in statements}
        spelled = {key: {_strict_spelling(self._prop(key))} for key in keys}
        for edge in self.graph.edges:
            if edge.term is None or (edge.target_key, edge.target_side) not in targets:
                continue
            for text in _props(edge.term):
                try:
                    key = canonical_prop(text)
                except Exception:
                    continue
                if key in spelled:
                    spelled[key].add(_strict_spelling(text))
        return {self._prop(key): sorted(found) for key, found in spelled.items() if len(found) > 1}

    def expand(self) -> ProofTerm:
        """The debate term with every definition written back in: what
        ``unfold(graph, issue)`` returns, up to alpha-equivalence and site
        numbering."""
        sites = count(1)
        issue = debate_statement(self.issue)
        term = self._instantiate(self.issue, {}, frozenset({issue}), frozenset(), set(), sites)
        if isinstance(term, _SITES):
            # A bare site: the eta wrapper every argument has (as unfold).
            key, side = issue
            prop = self._prop(key)
            name = f"x{next(sites)}"
            term = (Mu(ID(name, prop), prop, term, ID(name, prop)) if side == "term"
                    else Mutilde(DI(name, prop), prop, DI(name, prop), term))
        return term

    # -- names ----------------------------------------------------------------

    def named(self) -> dict:
        """{statement: name} of the definitions shown by name."""
        return {s: d.name for s, d in self.defs.items() if d.name}

    def assign_names(self, user_names=None, anon=None) -> None:
        """Decide which definitions are shown by name, and name them.

        A definition is named when two or more sites cite it, or when the
        author cited it by name; never the issue itself, never a bare
        site.  ``user_names`` maps a statement to the registered argument
        or debate about it (only where there is exactly one); ``anon`` is
        called with a statement and returns its ``anon_k`` name - the
        caller keeps that table so the numbers are stable in a document.
        """
        user_names = dict(user_names or {})
        if anon is None:
            numbers = count(1)
            table = {}

            def anon(statement):
                if statement not in table:
                    table[statement] = f"{ANON_PREFIX}{next(numbers)}"
                return table[statement]

        explicit = _author_citations(self.graph)
        for definition in self.defs.values():
            definition.cited = 0
            definition.name = None
        self._count_occurrences()
        for key, definition in self.defs.items():
            if key == self.issue:
                continue
            # A name the author wrote stays, even for a bare site; without
            # one, a bare site reads better than a name for it.  A variant
            # of a statement's debate is the author's under no name.
            canonical = key == definition.statement
            if (canonical and key in explicit) or (definition.cited >= 2 and not definition.trivial):
                definition.name = ((explicit.get(key) or user_names.get(key)) if canonical
                                   else None) or anon(key)

    def _count_occurrences(self) -> None:
        """How often each definition is written out when the issue is
        expanded with every definition entered once: the citations met on
        the way that are neither captured nor cut where they stand.  A
        citation the context captures is a variable there and one on its
        own path a bare site, so neither is an occurrence of the debate.
        Each body is walked in the first context that reaches it, which
        keeps this linear in the definitions."""
        entered = {self.issue}
        todo = [(self.issue, {}, frozenset({debate_statement(self.issue)}))]
        while todo:
            key, env, spine = todo.pop()
            for leaf in _leaves(self.defs[key].body):
                if not is_cite(leaf) or is_bare(leaf):
                    continue
                target, statement = leaf.debate, leaf.statement
                env2 = {**env, **leaf.captures}
                spine2 = spine | leaf.cuts
                if statement in env2 or statement in spine2 or target not in self.defs:
                    continue
                self.defs[target].cited += 1
                if target not in entered:
                    entered.add(target)
                    todo.append((target, env2, spine2 | {statement}))

    # -- presentation -----------------------------------------------------------

    def _annotate(self, node) -> None:
        """Hang the display name and bracket on the citations left in a
        presented term."""
        for leaf in _leaves(node):
            if not is_cite(leaf):
                continue
            leaf.name = self.defs[leaf.debate].name
            leaf.cites = leaf.name
            parts = [f"{var} -> {self.marker(s)}" for s, var in leaf.captures.items()]
            parts += [self.marker(s) for s in sorted(leaf.cuts) if s not in leaf.captures]
            leaf.bracket = f"[{', '.join(parts)}]" if parts else ""

    def presented(self):
        """(root term, [(name, open sites, body)]): the debate as it is
        printed - the issue's term citing the named definitions, then each
        named definition, sub-debates used once written in place."""
        stop = frozenset(self.named())
        used, sites = set(), count(1)
        root = self._instantiate(self.issue, {}, frozenset({debate_statement(self.issue)}),
                                 stop, used, sites)
        self._annotate(root)
        rows = []
        for key, definition in self.defs.items():
            if key not in stop:
                continue
            body = self._instantiate(key, {}, frozenset({definition.statement}), stop, used, sites)
            self._annotate(body)
            rows.append((definition.name, self.open_sites(key), body))
        return root, rows

    def open_sites(self, key) -> list:
        """The sites the debate ``key`` rests on when nothing
        outside captures them, as markers - a reading aid for the header
        of a definition, not consulted by the pipeline.  It follows
        citations, dropping what the citing site captures."""
        seen, out = set(), []

        def visit(st, captured):
            if st in seen:
                return
            seen.add(st)
            definition = self.defs.get(st)
            if definition is None:
                return
            for leaf in _leaves(definition.body):
                if isinstance(leaf, _SITES):
                    side = "term" if isinstance(leaf, (Goal, Deleg)) else "context"
                    found = (canonical_prop(leaf.prop), side)
                    if found not in captured and found not in out:
                        out.append(found)
                elif is_cite(leaf):
                    if is_bare(leaf) or leaf.statement in leaf.cuts:
                        if leaf.statement not in captured and leaf.statement not in out:
                            out.append(leaf.statement)
                    else:
                        visit(leaf.debate, captured | set(leaf.captures))

        visit(key, frozenset())
        return [self.marker(s) for s in out]

    def to_text(self, tree: bool = False) -> str:
        """The printed shared form: the issue's term, then one
        ``name[open sites] := body`` line per named definition.  With
        ``tree``, each term is laid out as an indented tree
        (``pres.gen.pres_tree``) under its ``name[open sites] :=`` line,
        for the multi-line log artifacts."""
        from pres.gen import pres_str, pres_tree

        root, rows = self.presented()
        if not tree:
            lines = [pres_str(root)]
            for name, sites, body in rows:
                lines.append(f"{name}[{', '.join(sites)}] := {pres_str(body)}")
            return "\n".join(lines)
        lines = [pres_tree(root)]
        for name, sites, body in rows:
            lines.append(f"{name}[{', '.join(sites)}] :=")
            lines.extend("   " + line for line in pres_tree(body).splitlines())
        return "\n".join(lines)


def _same_origin(old, new):
    """A rebuilt binder keeps the argument its body came from."""
    if getattr(old, "origin", None):
        new.origin = old.origin
    return new


def _leaves(node):
    """Every leaf of a term (sites, variables, citations)."""
    if not isinstance(node, ProofTerm):
        return
    children = [getattr(node, slot, None) for slot in ("term", "context")]
    children = [c for c in children if isinstance(c, ProofTerm)]
    if not children:
        yield node
        return
    for child in children:
        yield from _leaves(child)


_SPELLINGS = {}


def _strict_spelling(text: str) -> str:
    """A proposition up to layout and bound names only: unlike
    ``canonical_prop`` it keeps ``~A`` and ``A -> false`` apart, as
    Fellowship does.  Unparsable text is its own spelling."""
    found = _SPELLINGS.get(text)
    if found is None:
        try:
            found = Prop.parse(text).canonical()
        except Exception:
            found = f"unparsed:{text}"
        _SPELLINGS[text] = found
    return found


def _props(node):
    """Every proposition written anywhere in a term: on binders, sites,
    leaves and hypotheses."""
    if not isinstance(node, ProofTerm):
        return
    for holder in (node, getattr(node, "di", None), getattr(node, "id", None)):
        text = getattr(holder, "prop", None)
        if isinstance(text, str) and text:
            yield text
    for slot in ("term", "context"):
        yield from _props(getattr(node, slot, None))


def _author_citations(graph: DebateGraph) -> dict:
    """{statement: name} for the statements an argument of the document
    cites by name (``cite NAME``).  A statement cited under two different
    names is left out: no single name is the author's for it."""
    found, clash = {}, set()
    for edge in graph.edges:
        for leaf in _leaves(edge.term):
            name = getattr(leaf, "cites", None)
            if not name or not isinstance(leaf, (ID, DI)) or not leaf.prop:
                continue
            statement = (canonical_prop(leaf.prop), "context" if isinstance(leaf, ID) else "term")
            if found.setdefault(statement, name) != name:
                clash.add(statement)
    return {s: n for s, n in found.items() if s not in clash}


def share(graph: DebateGraph, issue, user_names=None, anon=None, subarguments="default") -> SharedDebate:
    """The debate of ``issue`` = (canonical key, side) as named sub-debates."""
    builder = _Builder(graph, subarguments)
    defs = builder.build(issue)
    shared = SharedDebate(graph, issue, defs, frozenset(builder._scaffold_vars), subarguments)
    shared.assign_names(user_names, anon)
    logger.debug("share: issue %s has %d definition(s), %d shown by name",
                 issue, len(shared.defs), len(shared.named()))
    return shared


__all__ = ["ANON_PREFIX", "Definition", "SharedDebate", "share", "is_cite", "is_bare", "contrary",
           "debate_statement"]
