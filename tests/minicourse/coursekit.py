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


# --- lessons 9-13: evaluation one step at a time --------------------------

def debate_term(script, name):
    """(prover, term): run ``script`` quietly and return the debate term
    unfolded from the document for the argument ``name`` - biased towards
    it (aida-unfold-entrypoints), the term `explain` prints under
    "unfold".  Close the prover when done."""
    import contextlib
    import io
    import os
    os.environ["ACDC_NO_RENDER"] = "1"
    from wrap.cli import _issue_term, execute_script, setup_prover
    prover = setup_prover()
    with contextlib.redirect_stdout(io.StringIO()):
        execute_script(prover, str(script), isolate=False)
        _arg, _issue, term = _issue_term(prover, name)
    return prover, term


def scaffold_at(term, site):
    """The scaffold whose original is the bare site ``site`` ("u2"): the
    binder whose command has that site on its own side."""
    from core.ac.ast import Deleg, Geled, Goal, Laog, Mu, Mutilde, ProofTerm

    def walk(n):
        if not isinstance(n, ProofTerm):
            return None
        if isinstance(n, Mu) and isinstance(n.term, (Goal, Deleg)) and n.term.number == site:
            return n
        if isinstance(n, Mutilde) and isinstance(n.context, (Laog, Geled)) and n.context.number == site:
            return n
        for slot in ("term", "context"):
            found = walk(getattr(n, slot, None))
            if found is not None:
                return found
        return None

    return walk(term)


def fire(binder, rule):
    """Fire ``rule`` ("mu<" or ">mu") at the command directly under
    ``binder``, in place, with the normaliser's own rules and substitution.
    Where only one rule applies it fires whatever ``rule`` says; the rule
    actually fired is returned."""
    from core.comp.oracle_terms import _step_command
    fired = []
    step = _step_command(binder.term, binder.context,
                         "cbv" if rule == "mu<" else "cbn", fired)
    if step is None:
        raise ValueError("no reduction applies at this command")
    binder.term, binder.context = step
    return fired[-1]


def eta_step(binder):
    """One eta step at ``binder`` itself: mu a.< t || a >  ->  t  when a is
    not free in t (and the mirror).  Not a rule of the normaliser; the
    scaffold rewrites apply it to a scaffold's outer binder."""
    from core.ac.ast import DI, ID, Mu, Mutilde
    from core.comp.oracle_terms import _occurs
    if (isinstance(binder, Mu) and isinstance(binder.context, ID)
            and binder.context.name == binder.id.name and not _occurs(binder.term, ID, binder.id.name)):
        return binder.term
    if (isinstance(binder, Mutilde) and isinstance(binder.term, DI)
            and binder.term.name == binder.di.name and not _occurs(binder.context, DI, binder.di.name)):
        return binder.context
    raise ValueError("eta does not apply here")


def eta_reduce(term):
    """Eta everywhere, the argument's own wrappers included: for comparing
    two terms modulo eta, not for replaying a step."""
    from core.ac.ast import DI, ID, Mu, Mutilde, ProofTerm
    from core.comp.oracle_terms import _occurs
    if (isinstance(term, Mu) and isinstance(term.context, ID)
            and term.context.name == term.id.name and not _occurs(term.term, ID, term.id.name)):
        return eta_reduce(term.term)
    if (isinstance(term, Mutilde) and isinstance(term.term, DI)
            and term.term.name == term.di.name and not _occurs(term.context, DI, term.di.name)):
        return eta_reduce(term.context)
    for slot in ("term", "context"):
        child = getattr(term, slot, None)
        if isinstance(child, ProofTerm):
            setattr(term, slot, eta_reduce(child))
    return term


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
