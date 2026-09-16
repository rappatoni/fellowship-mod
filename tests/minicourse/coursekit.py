"""Helpers for working the minicourse exercises.

Run it to print the worked examples:

    .venv/bin/python tests/minicourse/coursekit.py

Or use the helpers on your own graphs:

    import sys; sys.path.insert(0, "tests/minicourse")
    from coursekit import show, conditions_of, graph_from_edges

`compile_conditions` returns nested tuples keyed by "<canonical>\\x01<side>",
which is precise but unreadable; `readable` renders one condition in the
notation the course uses, and `show` prints a whole graph's conditions
next to its grounded labels.
"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from core.comp.adf_label import (  # noqa: E402
    compile_conditions, grounded_labels, split_statement,
)
from core.dc.debate_graph import (  # noqa: E402
    DebateGraph, Edge, Source, canonical_prop,
)


def readable(condition, graph=None) -> str:
    """Render one acceptance condition in the course's notation."""
    tag = condition[0]
    if tag == "const":
        return "true" if condition[1] else "false"
    if tag == "var":
        key, side = split_statement(condition[1])
        name = graph.nodes[key] if graph else key
        return f"{name}[{side[0]}]"
    if tag == "not":
        # Meta-negation of the acceptance condition.  Rendered as "not" so
        # it cannot be confused with object negation "~" inside a
        # proposition: the guard on Q[c] prints "not Q[c]", an edge sourced
        # at the node ~Q prints "~Q[c]" (tasks.org,
        # aida-course-negation-notation).
        return f"not {readable(condition[1], graph)}"
    if tag in ("and", "or"):
        empty, glue = ("true", " & ") if tag == "and" else ("false", " | ")
        parts = condition[1]
        if not parts:
            return empty
        if len(parts) == 1:
            return readable(parts[0], graph)
        return "(" + glue.join(readable(p, graph) for p in parts) + ")"
    raise ValueError(f"unknown formula tag: {tag!r}")


def label_of(statement, graph):
    key, side = statement
    return f"{graph.nodes[key]}[{side[0]}]"


def conditions_of(graph) -> dict:
    """{"Q[t]": "(true & not Q[c])", ...} - readable, in graph order."""
    out = {}
    for statement, condition in compile_conditions(graph).items():
        key, side = split_statement(statement)
        out[f"{graph.nodes[key]}[{side[0]}]"] = readable(condition, graph)
    return out


def show(graph, title="graph") -> None:
    """Print a graph's edges, acceptance conditions and grounded labels."""
    print(f"--- {title} ---")
    for edge in graph.edges:
        sources = ", ".join(
            f"{graph.nodes[s.key]}[{s.side[0]}:{s.kind[:4]}]" for s in edge.sources
        ) or "-"
        print(f"  edge {edge.name}: {graph.nodes[edge.target_key]}"
              f"[{edge.target_side[0]}] <- {sources}"
              f"  ({'strict' if edge.strict else 'defeasible'})")
    print("  conditions:")
    for statement, condition in conditions_of(graph).items():
        print(f"    {statement} = {condition}")
    print("  grounded labels:")
    for statement, label in grounded_labels(graph).items():
        print(f"    {label_of(statement, graph)} {label}")


def graph_from_edges(nodes, edges=(), markers=()) -> DebateGraph:
    """Build a graph directly, without going through a proof term.

    nodes   : proposition strings, e.g. ["A", "B", "C"]
    edges   : (name, target, side, [(prop, side, kind), ...], strict, role)
    markers : (prop, side, kind) default markers
    """
    graph = DebateGraph()
    for text in nodes:
        graph.add_node(text)
    for name, target, side, sources, strict, role in edges:
        graph.add_edge(Edge(
            name=name,
            target_key=canonical_prop(target),
            target_side=side,
            sources=tuple(
                Source(canonical_prop(p), s, k, "x") for p, s, k in sources
            ),
            strict=strict,
            role=role,
        ))
    for prop, side, kind in markers:
        graph.mark_default(canonical_prop(prop), side, kind)
    return graph


if __name__ == "__main__":
    # Lesson 4, exercise 1: the lesson-2 d2 graph, built directly.
    d2 = graph_from_edges(
        nodes=["A", "B", "C"],
        edges=[
            ("d2", "C", "term",
             [("A", "term", "obligation"), ("B", "term", "obligation")],
             False, "argument"),
            ("bArg", "B", "term", [], True, "supporter"),
        ],
    )
    show(d2, "lesson 2's d2 graph (lesson 4 exercise 1)")
