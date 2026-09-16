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
    #: Variables of enclosing subarguments this one uses (non-empty only on
    #: absorbed records): the loop a capture closes, made visible.
    captures: tuple = ()
    #: Records of subarguments merged into this edge because they captured
    #: one of its binders (see _Compiler): kept for presentation, not
    #: labelled on their own.
    absorbed: tuple = ()
    #: The subargument's body, for unfolding (core/dc/unfold.py); not part
    #: of the edge's identity.
    term: object = field(default=None, compare=False, repr=False)


@dataclass(frozen=True)
class _Alt:
    """One alternative derivation of a body while compiling: the sources
    and captures it accumulates and the records it absorbed."""

    sources: tuple = ()
    captures: tuple = ()
    absorbed: tuple = ()
    defeasible: bool = False   # a subargument it applies is defeasible

    def __add__(self, other):
        return _Alt(self.sources + other.sources, self.captures + other.captures,
                    self.absorbed + other.absorbed, self.defeasible or other.defeasible)


_EMPTY = _Alt()
MAX_ALTERNATIVES = 256


@dataclass(frozen=True)
class Capture:
    """A variable of an enclosing subargument used inside an absorbed one:
    ``name`` bound by ``owner`` (an edge name) for the statement
    (``key``, ``side``)."""

    name: str
    key: str
    side: str
    owner: str


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

    def describe_absorbed(self, edge, indent: str = "") -> list:
        """Lines describing the subarguments absorbed into ``edge``, with
        the captures that caused it - the structure the labelling does
        not see."""
        lines = []
        for rec in edge.absorbed:
            sources = ", ".join(
                f"{self.nodes.get(s.key, s.key)}[{s.side[0]}:{s.kind[:4]}]" for s in rec.sources
            ) or "-"
            uses = ", ".join(
                f"{c.name}:{self.nodes.get(c.key, c.key)}[{c.side[0]}] of {c.owner}" for c in rec.captures
            )
            lines.append(
                f"{indent}~ absorbed {rec.name} ({rec.role}): "
                f"{self.nodes.get(rec.target_key, rec.target_key)}[{rec.target_side[0]}] <- {sources}"
                + (f"   uses {uses}" if uses else "")
            )
            lines.extend(self.describe_absorbed(rec, indent + "    "))
        return lines

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
                for line in self.describe_absorbed(edge):
                    out.append(f"{child_prefix}{line}")
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
                "subargument": "color=gray40",
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


def _match_scaffold(n, strict_names=()):
    """Match one of the four shapes at this node.

    Returns (role, site_side, prop, orig, scion_raw, alt_name, scion_kind)
    or None.  ``scion_kind`` is "term"/"context" (what the scion body is);
    attack scions still contain the alt reroute and need
    ``_restore_scion``.

    The stolen-value slot of an attack shape normally holds an open site
    (the debate operators put a fresh Goal/Laog there).  It may also hold
    a *declared* name: that is the primitive contrariness term
    mu att.<mu _.<t || att> || mu'_.<t || t'>>, a strict proof confronting a
    strict refutation.  ``strict_names`` says which names are declared.
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
        if isinstance(w2.term, (Goal, Deleg)) or (
            isinstance(w2.term, DI) and w2.term.name in strict_names
        ):
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
        if isinstance(w1.context, (Laog, Geled)) or (
            isinstance(w1.context, ID) and w1.context.name in strict_names
        ):
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
            names.append(node.id.name)
            node = node.term
        elif isinstance(node, Mutilde) and isinstance(node.term, DI) and node.term.name == node.di.name:
            names.append(node.di.name)
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


def _scion_name(body, fallback: str) -> str:
    """An eta-wrapped scion carries its argument name in the root binder."""
    if isinstance(body, Mu) and isinstance(body.context, ID) and body.context.name == body.id.name:
        return body.id.name
    if isinstance(body, Mutilde) and isinstance(body.term, DI) and body.term.name == body.di.name:
        return body.di.name
    if isinstance(body, (ID, DI)):
        return body.name
    return fallback


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
    """Term -> hyperedges.

    One edge per *closed* subargument: the body of an argument, every
    scion of a scaffold, and every lambda (a proof of an implication
    under a hypothesis).  A subargument that refers to a binder of an
    enclosing subargument - a hypothesis, a continuation, or a
    scaffold's catch variable - has *captured* it while grafting; it is
    not closed, so it is absorbed into the enclosing subargument that
    binds the variable (cascading outward until closed), its sites
    become that subargument's sources, and it is kept as a record on
    the edge (``Edge.absorbed``, with ``Edge.captures``) so the loop it
    closes stays visible.  The labelling sees closed edges only: a
    captured variable is discharged by the hypothesis it reaches and
    never becomes a global claim (aida-cyclic-fragment, 2026-09-17).
    """

    def __init__(self, strict_names, strict_kinds=None):
        self.strict_names = set(strict_names or ()) | set(BUILTIN_LEAVES)
        self.strict_kinds = {**BUILTIN_LEAVES, **dict(strict_kinds or {})}
        self.graph = DebateGraph()
        self._fresh = count(1)

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
        for record in self.compile_body(deepcopy(body), _host_name(body, name), role, {}):
            assert not record.captures, "a top-level body cannot capture: free variables are refused"
            self.graph.add_edge(record)

    # -- one subargument ----------------------------------------------------

    def compile_body(self, body, name, role, outer, *, expect_side=None, expect_prop=None):
        """Compile one subargument body into its alternative derivations.

        ``outer`` maps the variables bound by enclosing subarguments to
        (prop, side, owner).  Returns a list of Edge records, one per
        alternative: a scaffold whose scion is absorbed offers two ways
        to derive the target (the site's own route and the scion's), and
        alternatives multiply.  A record with ``captures`` is not closed:
        the caller absorbs it.  Records without captures are the edges;
        closed nested subarguments are added to the graph here.
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

        def product(alts_a, alts_b):
            out = [a + b for a in alts_a for b in alts_b]
            if len(out) > MAX_ALTERNATIVES:
                raise DebateCompileError(
                    f"Edge '{name}': more than {MAX_ALTERNATIVES} alternative "
                    f"derivations; refusing to enumerate."
                )
            return out

        def discharged(captures, env):
            """Captures our own binders satisfy are dropped; the rest climb."""
            return tuple(c for c in captures
                         if not (c.name in env and env[c.name][1] == c.side))

        def absorbed_alt(rec, env):
            return _Alt(rec.sources, discharged(rec.captures, env), (rec,), False)

        def walk(node, env, spine):
            for leaf_type, (lside, kind) in _LEAF_INFO.items():
                if isinstance(node, leaf_type):
                    if not node.prop:
                        raise DebateCompileError(
                            f"Edge '{name}': open site {node.number!r} has no "
                            f"proposition; run enrichment first."
                        )
                    return [_Alt((Source(self.graph.add_node(node.prop), lside, kind,
                                         str(node.number), spine),))]
            if isinstance(node, (ID, DI)):
                vside = "context" if isinstance(node, ID) else "term"
                bound = env.get(node.name)
                if bound is not None and bound[1] == vside:
                    return [_EMPTY]
                if node.name in self.strict_names:
                    self._check_kind(node.name, vside, name)
                    return [_EMPTY]
                enclosing = outer.get(node.name)
                if enclosing is not None and enclosing[1] == vside:
                    return [_Alt((), (Capture(node.name, self.graph.add_node(enclosing[0]),
                                              vside, enclosing[2]),))]
                raise DebateCompileError(
                    f"Edge '{name}': free {'context' if vside == 'context' else 'term'} "
                    f"variable '{node.name}' is neither bound nor a declared axiom; "
                    f"express assumptions as open sites."
                )
            match = _match_scaffold(node, self.strict_names)
            if match is not None:
                srole, site_side, sprop, orig, scion_raw, alt, scion_kind = match
                alt_side = "context" if isinstance(node, Mu) else "term"
                if srole == "attacker":
                    scion = _restore_scion(scion_raw, scion_kind, alt, sprop, f"r{next(self._fresh)}")
                    target_side = "context" if site_side == "term" else "term"
                    sub_outer = {**outer, **{k: (*v, name) for k, v in env.items() if k != alt}}
                else:
                    scion = scion_raw
                    target_side = site_side
                    sub_outer = {**outer, **{k: (*v, name) for k, v in env.items()},
                                 alt: (sprop, alt_side, name)}
                label = _scion_name(scion, f"{name}.{srole}{next(self._fresh)}")
                records = self.compile_body(scion, label, srole, sub_outer,
                                            expect_side=target_side, expect_prop=sprop)
                closed = [r for r in records if not r.captures]
                absorbed = [r for r in records if r.captures]
                alts = []
                if closed or not absorbed:
                    # The site stays a source of this derivation and the
                    # closed scions are its own edges.
                    for rec in closed:
                        self.graph.add_edge(rec)
                    alts += walk(orig, env, spine)
                # An absorbed scion is part of this derivation: a supporter
                # fills its site, an attacked site stays open beside it.
                inner = bind(env, {alt: (sprop, alt_side)})
                for rec in absorbed:
                    child = [absorbed_alt(rec, inner)]
                    if srole == "attacker":
                        wing = node.context if isinstance(node, Mu) else node.term
                        child = product(child, walk(wing.term if isinstance(node, Mu) else wing.context, env, spine))
                    alts += child
                return alts
            if isinstance(node, Lamda) and node is not root_lambda:
                lname = f"{name}.\u03bb{next(self._fresh)}"
                sub_outer = {**outer, **{k: (*v, name) for k, v in env.items()}}
                records = self.compile_body(node, lname, "subargument", sub_outer)
                closed = [r for r in records if not r.captures]
                absorbed = [r for r in records if r.captures]
                alts = []
                if closed:
                    for rec in closed:
                        self.graph.add_edge(rec)
                    alts.append(_Alt((Source(closed[0].target_key, "term", "subargument", lname, spine),),
                                     (), (), not any(r.strict for r in closed)))
                for rec in absorbed:
                    alts.append(absorbed_alt(rec, env))
                return alts
            if isinstance(node, Lamda):
                return walk(node.term, bind(env, {node.di.di.name: (node.di.prop, "term")}), spine)
            if isinstance(node, Mu):
                inner = bind(env, {node.id.name: (node.prop, "context")})
                spine2 = spine if _is_affine_binder(node.id.name) else spine + ((node.prop, "context"),)
                return product(walk(node.term, inner, spine2), walk(node.context, inner, spine2))
            if isinstance(node, Mutilde):
                inner = bind(env, {node.di.name: (node.prop, "term")})
                spine2 = spine if _is_affine_binder(node.di.name) else spine + ((node.prop, "term"),)
                return product(walk(node.term, inner, spine2), walk(node.context, inner, spine2))
            if isinstance(node, Admal):
                return walk(node.context, bind(env, {node.id.id.name: (node.id.prop, "context")}), spine)
            if isinstance(node, (Cons, Sonc)):
                return product(walk(node.term, env, spine), walk(node.context, env, spine))
            raise DebateCompileError(
                f"Edge '{name}': unhandled node {type(node).__name__} during "
                f"source collection."
            )

        records = []
        for i, alt in enumerate(walk(body, {}, ())):
            # Strict = the derivation uses no indeterminate anywhere: none
            # among its own sites, and none inside the subarguments it applies.
            strict = (not any(s.kind in ("obligation", "presumption") for s in alt.sources)
                      and not alt.defeasible)
            records.append(Edge(name=name if i == 0 else f"{name}#{i + 1}",
                                target_key=target_key, target_side=side,
                                sources=alt.sources, strict=strict, role=role,
                                captures=alt.captures, absorbed=alt.absorbed, term=body))
        return records

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
    return its alternative records.  Records with captures are absorbed
    into the host and are not a choice sigma makes."""
    role, site_side, prop, orig, scion_raw, alt, scion_kind = match
    compiler = _Compiler(strict_names, strict_kinds)
    if role == "attacker":
        scion = _restore_scion(scion_raw, scion_kind, alt, prop, "r0")
        target_side = "context" if site_side == "term" else "term"
        outer = {k: (*v, owner) for k, v in env.items() if k != alt}
    else:
        scion = scion_raw
        target_side = site_side
        alt_side = "context" if site_side == "term" else "term"   # mu alt on a term site, mu' alt on a context site
        outer = {**{k: (*v, owner) for k, v in env.items()}, alt: (prop, alt_side, owner)}
    return compiler.compile_body(scion, _scion_name(scion, "scion"), role, outer,
                                 expect_side=target_side, expect_prop=prop)


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
    compiler = _Compiler(strict_names, strict_kinds)
    compiler.compile(body, name)
    return compiler.graph


def compile_document(named_bodies, *, strict_names=None, strict_kinds=None) -> DebateGraph:
    """Compile several (name, body) pairs into one merged DebateGraph."""
    graph = DebateGraph()
    for name, body in named_bodies:
        compiler = _Compiler(strict_names, strict_kinds)
        compiler.compile(body, name)
        graph.merge(compiler.graph)
    return graph
