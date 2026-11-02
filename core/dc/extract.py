"""A registered debate's arguments, read back off its term (tasks.org,
aida-debate-reverse-compilation).

A debate registered by its term (core/dc/debate.py, RegisteredDebate) is
compiled into a graph whose edges are the term's arguments: the opening
(role ``argument``, the debate's own edge), and the supporters and
attackers its scaffolds stack (roles ``supporter``, ``attacker``).  Each
is cut out of the term here, as the argument it stands for:

- a scaffold inside it is replaced by its original: its scion is an
  argument of its own (a stacked scion, one per element), so what is left
  is the site, or the term, the scaffold was built on;
- a variable bound outside the edge - a capture the compiler records as
  a source - becomes a site of the kind recorded (``captured_presumption``:
  a presumption, else an obligation), since outside the debate nothing
  binds it;
- the steps of an argument (role ``subargument``: an axiom applied, a
  lemma, a lambda) stay inline: they come back as edges when the argument
  is compiled again;
- sites are numbered afresh, 1, 2, ... in each argument.

The arguments get ranks for the debate's moves: in every stack the
element at the bottom first, the outermost last, as the unfolder stacks
a statement's derivations by rank (core/dc/unfold.py, ``order``); the
opening has rank 0.  A copy of an argument (same name, target and
sources, as ``fold_occurrences`` folds them) is extracted once.

The edges are those of a compilation of the term itself, not of a copy:
an edge's term is then a node of the term, which is how a scion's place
in its stack is known.  (A deprecated M3 attack's scion is a copy the
compiler restores; it gets no rank and keeps registration order.  A
``legacy`` debate is rewritten into the paper shapes first.)
"""

import logging
from copy import deepcopy
from dataclasses import dataclass
from itertools import count
from typing import Dict, List

from core.ac.ast import (
    ProofTerm, Mu, Mutilde, Lamda, Admal, Goal, Laog, Deleg, Geled, ID, DI,
)
from core.dc.debate_graph import (
    BUILTIN_LEAVES, DebateGraph, _Compiler, _host_name, _match_scaffold, scoped_stack_elements,
)
from core.dc.discipline import free_variables

logger = logging.getLogger(__name__)

ROLES = ("argument", "supporter", "attacker")
_SITES = (Goal, Laog, Deleg, Geled)


@dataclass
class Extracted:
    #: the argument's name in the debate: DEBATE__a<k>, k by rank
    name: str
    #: its conclusion, as the graph spells the statement
    conclusion: str
    anti: bool
    body: ProofTerm
    #: the edge of the debate's graph it was cut from
    edge: str
    rank: int


def extract_arguments(given_term: ProofTerm, debate: str, *, strict_names=(),
                      strict_kinds=None, local_name=None):
    """The arguments of a registered debate's term, opening first, then by
    rank; and the graph they were cut from.  ``local_name(debate, k)``
    names the k-th (default ``DEBATE__a<k>``)."""
    name_of = local_name or (lambda d, k: f"{d}__a{k}")
    body = deepcopy(given_term)
    ranks = _stack_ranks(body, strict_names)
    compiler = _Compiler(strict_names, strict_kinds)
    compiler.graph.add_edge(compiler.compile_body(body, _host_name(body, debate), "argument", {}))
    graph: DebateGraph = compiler.graph
    candidates, seen = [], set()
    for edge in graph.edges:
        if edge.role not in ROLES or edge.term is None:
            continue
        identity = (edge.name, edge.target_key, edge.target_side,
                    tuple((s.key, s.side, s.kind) for s in edge.sources))
        if identity in seen:
            continue                        # a copy of an argument already cut
        seen.add(identity)
        rank = 0 if edge.term is body else ranks.get(id(edge.term))
        candidates.append((rank, len(candidates), edge))
    # ranked first by rank, the unranked after them in registration order
    candidates.sort(key=lambda c: (c[0] is None, c[0] if c[0] is not None else 0, c[1]))
    out = []
    if not candidates or candidates[0][0] != 0:
        # The opening is a bare claim - the issue's site under its
        # scaffolds - which compiles to a default marker, not an edge; it
        # is still the debate's opening move (core/dc/debate.py,
        # unfold_debate).
        out.append(Extracted(name_of(debate, 1), body.prop, isinstance(body, Mutilde),
                             cut(body, strict_names), _host_name(body, debate), 0))
    for k, (rank, _, edge) in enumerate(candidates, len(out) + 1):
        out.append(Extracted(name_of(debate, k), graph.nodes.get(edge.target_key, edge.target_key),
                             edge.target_side == "context", cut(edge.term, strict_names), edge.name,
                             rank if rank is not None else k))
    logger.debug("extract: %d argument(s) of '%s' cut from %d edge(s)",
                 len(out), debate, len(graph.edges))
    return out, graph


def cut(term: ProofTerm, strict_names=()) -> ProofTerm:
    """The argument an edge's term stands for (the module docstring)."""
    term = deepcopy(term)

    def originals(node):
        while True:
            match = _match_scaffold(node, strict_names)
            if match is None:
                return node
            node = match[3]                  # the original; the scion is its own argument

    def walk(node):
        node = originals(node)
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                setattr(node, slot, walk(child))
        return node

    term = walk(term)
    term = _open_captures(term, strict_names)
    return _renumber_sites(term)


def _open_captures(term, strict_names):
    """Every variable bound outside ``term`` (not a declared name, nor a
    built-in leaf such as ``_F_``, the refutation of false that ends a
    negation's elimination) becomes a site of the kind the compiler
    records for the capture."""
    free = {name for _kind, name in free_variables(term)} - set(strict_names) - set(BUILTIN_LEAVES)
    if not free:
        return term

    def walk(node, bound_terms, bound_contexts):
        if isinstance(node, DI) and node.name in free and node.name not in bound_terms:
            presumed = getattr(node, "captured_presumption", False)
            return (Deleg if presumed else Goal)("0", node.prop)
        if isinstance(node, ID) and node.name in free and node.name not in bound_contexts:
            presumed = getattr(node, "captured_presumption", False)
            return (Geled if presumed else Laog)("0", node.prop)
        if isinstance(node, Mu):
            bound_contexts = bound_contexts | {node.id.name}
        elif isinstance(node, Mutilde):
            bound_terms = bound_terms | {node.di.name}
        elif isinstance(node, Lamda):
            bound_terms = bound_terms | {node.di.di.name}
        elif isinstance(node, Admal):
            bound_contexts = bound_contexts | {node.id.id.name}
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                setattr(node, slot, walk(child, bound_terms, bound_contexts))
        return node

    return walk(term, frozenset(), frozenset())


def _renumber_sites(term):
    numbers = count(1)

    def walk(node):
        if isinstance(node, _SITES):
            node.number = str(next(numbers))
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                walk(child)

    walk(term)
    return term


def _stack_ranks(body, strict_names) -> Dict[int, int]:
    """{id(scion element): rank}: in every scaffold, what its original
    holds (the lower levels of a nested stack) first, then the elements of
    its scion bottom first, each followed by the debates inside it."""
    ranks: Dict[int, int] = {}
    counter = count(1)

    def visit(node):
        match = _match_scaffold(node, strict_names)
        if match is not None:
            visit(match[3])
            elements = [match[4]] if match.legacy else [e for e, _ in scoped_stack_elements(match[4], strict_names)]
            for element in elements:
                ranks[id(element)] = next(counter)
                visit(element)
            return
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                visit(child)

    visit(body)
    return ranks
