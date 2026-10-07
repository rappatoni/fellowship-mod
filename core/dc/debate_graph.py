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
from copy import deepcopy
from dataclasses import dataclass, field
from itertools import count

from core.ac.ast import (
    ProofTerm, Mu, Mutilde, Lamda, Admal, Cons, Sonc,
    Goal, Laog, Deleg, Geled, ID, DI,
    first_order_node, FirstOrderNotSupported,
)
from core.ac.prop import (
    Prop, PTrue, PFalse, PSym, PApp, PNeg, PBin, PQuant, BinOp, PropError,
)
from core.logging_util import TRACE, artifact

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
    """The graph-node identity key for a proposition string.

    Parse (Fellowship's printed syntax), unfold negation, and take the
    alpha-invariant canonical rendering.  Whitespace and parenthesisation
    differences, bound-variable names, and the ``~A`` / ``A -> false``
    spelling all map to one key; genuinely different propositions (e.g.
    ``A-(B->C)`` vs ``(A-B)->C``) map to different keys.

    Raises PropError on unparsable input: an unreadable proposition must
    never silently become its own node.
    """
    return _unfold_neg(Prop.parse(text)).canonical()


def display_prop(text: str) -> str:
    """The display normalization: parse and re-render with the printer.

    Unlike ``canonical_prop`` this keeps the author's negation spelling;
    it only normalizes whitespace and parenthesisation.
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


@dataclass(frozen=True)
class _Acc:
    """What a body accumulates while compiling: its sources, and whether a
    subargument it applies is defeasible."""

    sources: tuple = ()
    defeasible: bool = False

    def __add__(self, other):
        return _Acc(self.sources + other.sources, self.defeasible or other.defeasible)


_EMPTY = _Acc()


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

    # -- construction -------------------------------------------------------

    def add_node(self, prop_text: str) -> str:
        key = canonical_prop(prop_text)
        self.nodes.setdefault(key, display_prop(prop_text))
        return key

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
            if not getattr(self, "quiet", False):
                logger.debug("compile: '%s' is an identity edge on %s; kept as a default marker only",
                             edge.name, self._show_edge_target(edge))
            return  # identity edge: default marker only
        if not getattr(self, "quiet", False):
            logger.debug("compile: edge '%s' (%s, %s) %s <- %s", edge.name, edge.role,
                         "strict" if edge.strict else "defeasible",
                         self._show_edge_target(edge), self._show_sources(edge))
        self.edges.append(edge)

    # -- rendering for log messages ----------------------------------------

    def _show(self, key, side) -> str:
        return f"{self.nodes.get(key, key)}[{side[0]}]"

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
        for (key, side), kinds in other.defaults.items():
            self.defaults.setdefault((key, side), set()).update(kinds)
        self.edges.extend(other.edges)

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
            text = f"{self.nodes.get(key, key)}[{side}]"
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

    def to_dot(self, labels=None) -> str:
        """Graphviz source.  With ``labels``, nodes are filled by status."""
        fill = {"IN": "#cdebc5", "OUT": "#f2c4c4", "UNDEC": "#e0e0e0"}
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
            label = display.replace('"', '\\"')
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
            colour = {
                "supporter": "darkgreen",
                "attacker": "red",
                "subargument": "gray40",
                "strict": "black",
            }.get(edge.role, "black")
            width = " penwidth=2" if edge.strict else ""
            head = "normal" if edge.target_side == "term" else "empty"
            if len(edge.sources) == 1:
                source = edge.sources[0]
                dashed = " style=dashed" if source.side == "context" else ""
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
        lines.append("}")
        return "\n".join(lines)

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
        return [(Mu(ID("x", prop), prop, scion, ID("x", prop)), {})]
    if isinstance(scion, (Laog, Geled, ID)) and not (isinstance(scion, ID) and scion.name in strict_names):
        prop = scion.prop
        return [(Mutilde(DI("x", prop), prop, DI("x", prop), scion), {})]
    return [(scion, {})]


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


class _Compiler:
    """Term -> hyperedges: the abstract argumentation framework of a term.

    One edge per subargument: the body of an argument, every scion of a
    scaffold, and every lambda (a proof of an implication under a
    hypothesis).  A subargument's sources are its open sites and every
    variable it uses that an *enclosing* subargument binds - a captured
    obligation (a demand met by a hypothesis or continuation in scope) is
    an obligation source at that binder's statement, a captured
    presumption a presumption source.  So a cycle a capture closes is a
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
        found = first_order_node(body)
        if found is not None:
            raise FirstOrderNotSupported("Debate graph compilation", found)
        self.graph.add_edge(self.compile_body(deepcopy(body), _host_name(body, name), role, {}))

    # -- one subargument ----------------------------------------------------

    def compile_body(self, body, name, role, outer, *, expect_side=None, expect_prop=None):
        """Compile one subargument body into its edge.

        ``outer`` maps the variables bound by enclosing subarguments to
        (prop, side, owner).  Nested subarguments (scions, lambdas) are
        added to the graph here; the body's own edge is returned.
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
        root_lambda = _peel_eta(body)
        if not isinstance(root_lambda, Lamda):
            root_lambda = None

        def bind(env, names):
            return {**env, **names}

        def walk(node, env, spine):
            for leaf_type, (lside, kind) in _LEAF_INFO.items():
                if isinstance(node, leaf_type):
                    if not node.prop:
                        raise DebateCompileError(
                            f"Edge '{name}': open site {node.number!r} has no "
                            f"proposition; run enrichment first."
                        )
                    return _Acc((Source(self.graph.add_node(node.prop), lside, kind,
                                        str(node.number), spine),))
            if isinstance(node, (ID, DI)):
                vside = "context" if isinstance(node, ID) else "term"
                bound = env.get(node.name)
                if bound is not None and bound[1] == vside:
                    for kind in getattr(node, "captured_default", ()):
                        # The bare contrary captured within the same body
                        # (an attack whose wing is nothing but it): no
                        # source, but the default somebody holds on the
                        # contrary is still marked, as the legacy term's
                        # bare site marked it.
                        self.graph.mark_default(self.graph.add_node(bound[0]), vside, kind)
                    return _EMPTY
                if node.name in self.strict_names:
                    self._check_kind(node.name, vside, name)
                    return _EMPTY
                if getattr(node, "cites", None):
                    # A citation of a registered argument Fellowship does not
                    # hold, i.e. a defeasible one: an obligation on the cited
                    # conclusion, which the cited argument's own edge derives
                    # (core/dc/cite.py).  A strict citation took the branch above.
                    if not node.prop:
                        raise DebateCompileError(
                            f"Edge '{name}': the citation of '{node.name}' has no proposition."
                        )
                    # A citation of a statement's debate (core/dc/instances.py)
                    # stands where unfolding puts that debate: the cited
                    # instance's own edges come first (``on_citation``), and
                    # the source has the kind the site there would have.
                    if self.on_citation is not None:
                        self.on_citation(node, name)
                    return _Acc((Source(self.graph.add_node(node.prop), vside,
                                        getattr(node, "source_kind", "obligation"),
                                        node.name, spine),))
                enclosing = outer.get(node.name)
                if enclosing is not None and enclosing[1] == vside:
                    # Captured from an enclosing subargument: a source at
                    # that binder's statement, of the kind the site had.
                    kind = "presumption" if getattr(node, "captured_presumption", False) else "obligation"
                    captured_key = self.graph.add_node(enclosing[0])
                    logger.debug("  compile: '%s' captured '%s' -> %s source at %s "
                                 "(the sprung trap, shown as a cycle)",
                                 name, node.name, kind, self.graph._show(captured_key, vside))
                    return _Acc((Source(captured_key, vside, kind, node.name, spine),))
                raise DebateCompileError(
                    f"Edge '{name}': free {'context' if vside == 'context' else 'term'} "
                    f"variable '{node.name}' is neither bound nor a declared axiom; "
                    f"express assumptions as open sites."
                )
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
                                                          expect_side=target_side, expect_prop=sprop))
                # the site stays a source of this derivation; the scion is
                # its own edge
                return walk(orig, env, spine)
            if isinstance(node, Lamda) and node is not root_lambda:
                lname = f"{name}.\u03bb{next(self._fresh)}"
                sub_outer = {**outer, **{k: (*v, name) for k, v in env.items()}}
                rec = self.compile_body(node, lname, "subargument", sub_outer)
                self.graph.add_edge(rec)
                return _Acc((Source(rec.target_key, "term", "subargument", lname, spine),),
                            not rec.strict)
            if isinstance(node, Lamda):
                return walk(node.term, bind(env, {node.di.di.name: (node.di.prop, "term")}), spine)
            if isinstance(node, Mu):
                inner = bind(env, {node.id.name: (node.prop, "context")})
                spine2 = spine if _is_affine_binder(node.id.name) else spine + ((node.prop, "context"),)
                return walk(node.term, inner, spine2) + walk(node.context, inner, spine2)
            if isinstance(node, Mutilde):
                inner = bind(env, {node.di.name: (node.prop, "term")})
                spine2 = spine if _is_affine_binder(node.di.name) else spine + ((node.prop, "term"),)
                return walk(node.term, inner, spine2) + walk(node.context, inner, spine2)
            if isinstance(node, Admal):
                return walk(node.context, bind(env, {node.id.id.name: (node.id.prop, "context")}), spine)
            if isinstance(node, (Cons, Sonc)):
                return walk(node.term, env, spine) + walk(node.context, env, spine)
            raise DebateCompileError(
                f"Edge '{name}': unhandled node {type(node).__name__} during "
                f"source collection."
            )

        acc = walk(body, {}, ())
        # Strict = the derivation uses no indeterminate anywhere: none among
        # its own sites, and none inside the subarguments it applies.
        strict = (not any(s.kind in ("obligation", "presumption") for s in acc.sources)
                  and not acc.defeasible)
        return Edge(name=name, target_key=target_key, target_side=side,
                    sources=acc.sources, strict=strict, role=role, term=body)

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
                   strict_kinds=None) -> DebateGraph:
    """Compile one (possibly composed) debate body into a DebateGraph.

    ``strict_names``: declared names (references to them are strict, not
    assumptions) - the prover's declaration keys.  ``strict_kinds``: the
    matching {name: kind} (see ``declaration_kinds``); when given, a
    declared name occurring on the wrong side for its kind is refused.
    Derivation cycles are admitted (the cyclic fragment); ``is_acyclic``
    on the result says which fragment the debate is in.
    """
    logger.debug("compile: framework of '%s'", name)
    compiler = _Compiler(strict_names, strict_kinds)
    compiler.compile(body, name)
    graph = compiler.graph
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
