"""Debates: named, ordered selections of a document's arguments
(author, 2026-10-08; tasks.org, aida-debate-objects).

A debate is a *recipe*, recorded in a script as

    debate pro|con open|closed NAME : ISSUE.
    arg1.
    [VERB] arg2 arg1.
    ...
    hora est.

- The **issue** is ISSUE proved (onus ``pro``) or refuted (``con``); the
  opening move must be an argument for that statement.
- The **scope** is the set of arguments the debate is compiled from.
  ``closed``: the arguments of its moves and, transitively, every
  argument they cite; ``open``: the whole document.  The strict store
  (declarations, the theorems Fellowship holds) is always in scope.
  Inside the scope conflicts arise by proposition identity, as in the
  document: a move attacks or supports wherever its conclusion fits.
- The **order** of the moves biases the term towards the debate's own
  arguments: the term is unfolded for the opening argument (on top of
  the issue's stack, its sites as it wrote them), and at every statement
  the moved arguments stand above the others, the last uttered outermost
  (judged first); in an open scope the arguments nobody moved stay
  below, in registration order (core/dc/unfold.py, ``order``).
- A **verb** only checks: the target must be an earlier move and have a
  node the mover's conclusion can attack or support, of the kind the
  verb names (``VERBS``).  A move without a verb is checked for nothing
  - a non sequitur is allowed, and simply joins the scope.

A debate never contributes to the document graph: its term is compiled
on demand from the recipe and the document as they stand.
"""

from dataclasses import dataclass, field
from typing import Callable, Iterable, Optional

from core.dc.debate_graph import DebateGraph, canonical_prop
from core.dc.unfold import Unfolder, contrary

ONUS = ("pro", "con")
SCOPES = ("open", "closed")

#: verb -> (relation, the kind of node it needs on the target; None: either)
VERBS = {
    "attack": ("attack", None),
    "rebut": ("attack", "obligation"),
    "undermine": ("attack", "presumption"),
    "undercut": ("attack", "presumption"),
    "support": ("support", None),
    "buttress": ("support", "obligation"),
    "reinforce": ("support", "obligation"),
    "undergird": ("support", "presumption"),
}


class DebateError(ValueError):
    """A debate or one of its moves is refused."""


@dataclass
class Move:
    argument: str
    verb: Optional[str] = None
    target: Optional[str] = None

    def __str__(self):
        words = ([self.verb] if self.verb else []) + [self.argument] + ([self.target] if self.target else [])
        return " ".join(words) + "."


@dataclass
class Debate:
    name: str
    issue_prop: str
    onus: str
    scope: str
    moves: list = field(default_factory=list)
    #: False while the debate is being recorded (before ``hora est.``)
    finished: bool = False
    #: cache: (document revision, number of moves) -> term
    _term: tuple = field(default=None, repr=False, compare=False)
    labelled_nf: object = field(default=None, repr=False, compare=False)
    labelled_nf_revision: object = field(default=None, repr=False, compare=False)

    def __post_init__(self):
        if self.onus not in ONUS:
            raise DebateError(f"debate '{self.name}': the onus is pro or con, not '{self.onus}'.")
        if self.scope not in SCOPES:
            raise DebateError(f"debate '{self.name}': the scope is open or closed, not '{self.scope}'.")

    @property
    def issue(self):
        """The statement the debate is about: (canonical key, side)."""
        return (canonical_prop(self.issue_prop), "term" if self.onus == "pro" else "context")

    @property
    def arguments(self) -> list:
        """The moved arguments, in utterance order, each once."""
        return list(dict.fromkeys(m.argument for m in self.moves))

    def order(self) -> dict:
        """{argument: rank} for the unfolder; a re-uttered argument takes
        its last rank."""
        return {m.argument: k for k, m in enumerate(self.moves, 1)}

    def header(self) -> str:
        return f"debate {self.onus} {self.scope} {self.name} : {self.issue_prop}."


def scope_names(debate: Debate, cited: Callable[[str], Iterable[str]]) -> list:
    """The closed scope: the moved arguments and, transitively, the ones
    they cite (``cited(name)``: the names that argument cites)."""
    found, todo = [], list(debate.arguments)
    while todo:
        name = todo.pop(0)
        if name in found:
            continue
        found.append(name)
        todo.extend(n for n in cited(name) if n not in found)
    return found


def _own_edges(graph: DebateGraph, name: str) -> list:
    """The edges compiled from the argument ``name``: its own and its
    subarguments' (``name.λk``, ``name.supporterk`` ...)."""
    return [e for e in graph.edges if e.name == name or e.name.startswith(name + ".")]


def argument_statement(graph: DebateGraph, name: str):
    """The statement the argument ``name`` concludes, read off its edge;
    None if it has none (refused at registration)."""
    for edge in graph.edges:
        if edge.name == name and edge.role != "subargument":
            return (edge.target_key, edge.target_side)
    return None


def target_nodes(graph: DebateGraph, name: str, conclusion=None) -> set:
    """{(statement, kind)}: what of the argument ``name`` a move can reach
    - the sites of its body and subarguments (obligations and
    presumptions), and its conclusion with the kind of its statement's
    root (a presumption if something in ``graph`` presumes it).  An
    argument that is a bare claim compiles to no edge, only a default
    marker; its ``conclusion`` is then given by the caller.
    Intermediate conclusions and strict leaves are not nodes yet
    (rational closure; aida-curried-intermediate-conclusions)."""
    nodes = set()
    for edge in _own_edges(graph, name):
        for source in edge.sources:
            if source.kind in ("obligation", "presumption"):
                nodes.add(((source.key, source.side), source.kind))
    conclusion = conclusion or argument_statement(graph, name)
    if conclusion is not None:
        kind = "presumption" if "presumption" in graph.defaults.get(conclusion, ()) else "obligation"
        nodes.add((conclusion, kind))
    return nodes


def check_opening(debate: Debate, graph: DebateGraph, name: str, statement_of) -> None:
    """``statement_of(name)``: the statement a registered argument
    concludes, None for a name that is not one."""
    statement = statement_of(name)
    if statement is None:
        raise DebateError(f"debate '{debate.name}': '{name}' is not a registered argument.")
    if statement != debate.issue:
        want = "an argument for" if debate.onus == "pro" else "a counterargument to"
        raise DebateError(
            f"debate '{debate.name}': the opening move must be {want} {debate.issue_prop}; "
            f"'{name}' concludes {_show(graph, statement)}.")


def check_move(debate: Debate, graph: DebateGraph, move: Move, statement_of) -> None:
    """Refuse a move that the debate cannot take: a name that is not a
    registered argument, a target that was not moved before, or a verb
    whose target lacks a node of the kind it needs."""
    mover = statement_of(move.argument)
    if mover is None:
        raise DebateError(f"debate '{debate.name}': '{move.argument}' is not a registered argument.")
    if move.target is None:
        if move.verb is not None:
            raise DebateError(f"debate '{debate.name}': '{move.verb}' needs a target: "
                              f"{move.verb} {move.argument} TARGET.")
        return
    if move.target not in debate.arguments:
        raise DebateError(f"debate '{debate.name}': '{move.target}' is not a move of the debate yet; "
                          f"a move can only answer an earlier one.")
    if move.verb is None:
        return
    relation, kind = VERBS[move.verb]
    wanted = contrary(mover) if relation == "attack" else mover
    nodes = target_nodes(graph, move.target, statement_of(move.target))
    if not any(s == wanted and (kind is None or k == kind) for s, k in nodes):
        what = {None: "site", "obligation": "obligation", "presumption": "presumption"}[kind]
        raise DebateError(
            f"debate '{debate.name}': {move.verb}: '{move.argument}' concludes "
            f"{_show(graph, mover)}, but '{move.target}' has no {what} on "
            f"{_show(graph, wanted)} to {relation}.")


def unfold_debate(graph: DebateGraph, debate: Debate):
    """The debate's term, unfolded from ``graph`` (the scope's graph) for
    its opening argument: the opening on top of the issue's stack, the
    sites of its body keeping the kinds it wrote (as ``unfold argument``),
    and at every statement the moved arguments above the others, the last
    uttered outermost.  An opening that is a bare claim has no edge (only
    a default marker): the term is then the issue's, ordered alike."""
    from core.dc.unfold import _shown, argument_edge
    unfolder = Unfolder(graph, order=debate.order())
    edge = argument_edge(graph, debate.moves[0].argument) if debate.moves else None
    if edge is not None and (edge.target_key, edge.target_side) == debate.issue:
        return _shown(unfolder.unfold_argument(edge))
    return _shown(unfolder.unfold(debate.issue))


def _show(graph: DebateGraph, statement) -> str:
    key, side = statement
    prop = graph.nodes.get(key, key)
    return f":{prop}" if side == "term" else f"{prop}:"
