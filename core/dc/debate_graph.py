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
    """The compiled graph has a propositional dependency cycle.

    The cycle-free fragment (propositional-fragment-plan.org) refuses these;
    cyclic debates need the witness-labelling machinery of the full Layer 3.
    """


@dataclass(frozen=True)
class Source:
    """One unwitnessed input of a hyperedge.

    ``kind``: "obligation" (Goal/Laog: burden on the proponent, default
    OUT), "presumption" (Deleg/Geled: burden delegated, default IN) or
    "assumption" (a free, undeclared variable).  ``side`` is the side the
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
            return  # identity edge: default marker only
        self.edges.append(edge)

    def merge(self, other: "DebateGraph") -> None:
        for key, display in other.nodes.items():
            self.nodes.setdefault(key, display)
        for (key, side), kinds in other.defaults.items():
            self.defaults.setdefault((key, side), set()).update(kinds)
        self.edges.extend(other.edges)

    # -- structure ----------------------------------------------------------

    def parents(self, key: str) -> set:
        """Propositions the given one depends on through any edge."""
        out = set()
        for edge in self.edges:
            if edge.target_key == key:
                out |= {s.key for s in edge.sources}
        return out

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

        return all(visit(k) for k in self.nodes if color[k] == WHITE)

    def assert_acyclic(self) -> None:
        if not self.is_acyclic():
            raise CyclicDebateNotSupported(
                "The compiled debate graph has a propositional dependency "
                "cycle; the cycle-free fragment refuses it."
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
        for edge in self.edges:
            style = {
                "supporter": "color=darkgreen",
                "attacker": "color=red",
            }.get(edge.role, "color=black")
            head = "normal" if edge.target_side == "term" else "empty"
            for source in edge.sources:
                lines.append(
                    f"  {ids[source.key]} -> {ids[edge.target_key]} "
                    f'[{style} arrowhead={head} label="{edge.name}"];'
                )
            if not edge.sources:
                lines.append(
                    f'  {ids[edge.target_key]} [peripheries=2];'
                )
        lines.append("}")
        return "\n".join(lines)


# ---------------------------------------------------------------------------
# M3: scaffold shapes
#
# The four support/attack shapes, locked against terms produced by
# Argument.support / Argument.attack (2026-08-31; see the M3 log in
# propositional-fragment-plan.org).  A = the issue proposition, alt = the
# catch binder, "_" = an affine binder:
#
#   T-SUP:  mu alt:A.< mu _:A.<ORIG_T || alt>  ||  mu'_:A.<SCION_T || alt> >
#   T-ATT:  mu alt:A.< mu _:A.<ORIG_T || alt>  ||  mu'_:A.<?g:A || SCION_CTX> >
#           where SCION_CTX contains alt free (the scion's own open context
#           tail was rerouted to the catch)
#   C-SUP:  mu'alt:A.< mu _:A.<alt || SCION_CTX> || mu'_:A.<alt || ORIG_CTX> >
#   C-ATT:  mu'alt:A.< mu _:A.<SCION_T || ?l:A>  || mu'_:A.<alt || ORIG_CTX> >
#           where SCION_T contains alt free (the scion's open term site was
#           rerouted to the catch)
#
# Reconstruction: ORIG goes back in the host's place; attack scions get
# their rerouted alt occurrence replaced by a fresh open site of A.
# ---------------------------------------------------------------------------

def _is_affine_binder(name: str) -> bool:
    return name == "_"


def _match_scaffold(n):
    """Match one of the four shapes at this node.

    Returns (role, site_side, prop, orig, scion_raw, alt_name, scion_kind)
    or None.  ``scion_kind`` is "term"/"context" (what the scion body is);
    attack scions still contain the alt reroute and need
    ``_restore_scion``.
    """
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
            return ("supporter", "term", prop, w1.term, w2.term, alt, "term")
        if isinstance(w2.term, (Goal, Deleg)):
            return ("attacker", "term", prop, w1.term, w2.context, alt, "context")
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
            return ("supporter", "context", prop, w2.context, w1.context, alt, "context")
        if isinstance(w1.context, (Laog, Geled)):
            return ("attacker", "context", prop, w2.context, w1.term, alt, "term")
        return None
    return None


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


def _scion_name(body, fallback: str) -> str:
    """An eta-wrapped scion carries its argument name in the root binder."""
    if isinstance(body, Mu) and isinstance(body.context, ID) and body.context.name == body.id.name:
        return body.id.name
    if isinstance(body, Mutilde) and isinstance(body.term, DI) and body.term.name == body.di.name:
        return body.di.name
    return fallback


class _Compiler:
    def __init__(self, strict_names):
        self.strict_names = set(strict_names or ())
        self.graph = DebateGraph()
        self._fresh = count(1)

    def compile(self, body: ProofTerm, name: str, role: str = "argument") -> None:
        found = first_order_node(body)
        if found is not None:
            raise FirstOrderNotSupported("Debate graph compilation", found)
        clean = self._decompose(deepcopy(body), name)
        self._add_edge_from_body(clean, name, role)

    # -- scaffold decomposition ---------------------------------------------

    def _decompose(self, node, host: str):
        """Replace every scaffold by its ORIG wing; compile scions."""
        match = _match_scaffold(node)
        if match is not None:
            role, site_side, prop, orig, scion_raw, alt, scion_kind = match
            if role == "attacker":
                scion = _restore_scion(
                    scion_raw, scion_kind, alt, prop, f"r{next(self._fresh)}"
                )
                target_side = "context" if site_side == "term" else "term"
            else:
                scion = scion_raw
                target_side = site_side
            scion_label = _scion_name(scion, f"{host}.{role}{next(self._fresh)}")
            clean_scion = self._decompose(scion, scion_label)
            self._add_edge_from_body(
                clean_scion, scion_label, role, expect_side=target_side, expect_prop=prop
            )
            return self._decompose(orig, host)
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                setattr(node, slot, self._decompose(child, host))
        return node

    # -- one edge from one clean body ---------------------------------------

    def _add_edge_from_body(self, body, name, role, *, expect_side=None, expect_prop=None):
        if isinstance(body, Mu):
            side = "term"
        elif isinstance(body, Mutilde):
            side = "context"
        elif isinstance(body, (Goal, Deleg)):
            side = "term"
        elif isinstance(body, (Laog, Geled)):
            side = "context"
        else:
            raise DebateCompileError(
                f"Edge '{name}': body root {type(body).__name__} is not a "
                f"mu/mu' binder or an open leaf; refusing to guess its side."
            )
        prop = getattr(body, "prop", None)
        if not prop:
            raise DebateCompileError(
                f"Edge '{name}': root proposition missing; run enrichment first."
            )
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
        sources = tuple(self._collect_sources(body, name))
        strict = not any(s.kind in ("obligation", "presumption") for s in sources)
        self.graph.add_edge(
            Edge(
                name=name,
                target_key=target_key,
                target_side=side,
                sources=sources,
                strict=strict,
                role=role,
            )
        )

    def _collect_sources(self, body, name):
        sources = []

        def walk(node, bound_ids, bound_dis, spine):
            for leaf_type, (side, kind) in _LEAF_INFO.items():
                if isinstance(node, leaf_type):
                    if not node.prop:
                        raise DebateCompileError(
                            f"Edge '{name}': open site {node.number!r} has no "
                            f"proposition; run enrichment first."
                        )
                    sources.append(
                        Source(
                            key=self.graph.add_node(node.prop),
                            side=side,
                            kind=kind,
                            site=str(node.number),
                            scope=spine,
                        )
                    )
                    return
            if isinstance(node, ID):
                if node.name not in bound_ids and node.name not in self.strict_names:
                    # Undeclared free variables are refused: the fixture
                    # corpus expresses assumptions as open sites, and the
                    # default status of a bare free-variable assumption in
                    # the ADF layer is an unsettled theory decision (the
                    # enthymemes design replaces them with obligations /
                    # presumptions).  Refuse rather than guess.
                    raise DebateCompileError(
                        f"Edge '{name}': free context variable '{node.name}' "
                        f"is neither bound nor a declared axiom; express "
                        f"assumptions as open sites."
                    )
                return
            if isinstance(node, DI):
                if node.name not in bound_dis and node.name not in self.strict_names:
                    raise DebateCompileError(
                        f"Edge '{name}': free term variable '{node.name}' "
                        f"is neither bound nor a declared axiom; express "
                        f"assumptions as open sites."
                    )
                return
            if isinstance(node, Mu):
                inner = bound_ids | {node.id.name}
                spine2 = spine if _is_affine_binder(node.id.name) else spine + ((node.prop, "context"),)
                walk(node.term, inner, bound_dis, spine2)
                walk(node.context, inner, bound_dis, spine2)
                return
            if isinstance(node, Mutilde):
                inner = bound_dis | {node.di.name}
                spine2 = spine if _is_affine_binder(node.di.name) else spine + ((node.prop, "term"),)
                walk(node.term, bound_ids, inner, spine2)
                walk(node.context, bound_ids, inner, spine2)
                return
            if isinstance(node, Lamda):
                walk(node.term, bound_ids, bound_dis | {node.di.di.name}, spine)
                return
            if isinstance(node, Admal):
                walk(node.context, bound_ids | {node.id.id.name}, bound_dis, spine)
                return
            if isinstance(node, (Cons, Sonc)):
                walk(node.term, bound_ids, bound_dis, spine)
                walk(node.context, bound_ids, bound_dis, spine)
                return
            raise DebateCompileError(
                f"Edge '{name}': unhandled node {type(node).__name__} during "
                f"source collection."
            )

        walk(body, set(), set(), ())
        return sources


def compile_debate(body: ProofTerm, name: str, *, strict_names=None) -> DebateGraph:
    """Compile one (possibly composed) debate body into a DebateGraph.

    ``strict_names``: declared axiom names (references to them are strict,
    not assumptions) - pass the prover's declaration keys.  Acyclicity is
    asserted; cyclic results raise CyclicDebateNotSupported.
    """
    compiler = _Compiler(strict_names)
    compiler.compile(body, name)
    compiler.graph.assert_acyclic()
    return compiler.graph


def compile_document(named_bodies, *, strict_names=None) -> DebateGraph:
    """Compile several (name, body) pairs into one merged DebateGraph."""
    graph = DebateGraph()
    for name, body in named_bodies:
        compiler = _Compiler(strict_names)
        compiler.compile(body, name)
        graph.merge(compiler.graph)
    graph.assert_acyclic()
    return graph
