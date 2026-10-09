"""JSON for the service's results (tasks.org, aida-json-format; Phase 4 of
aida-dui-api).  The formats are versioned (``VERSION``, semver) and
described by wrap/schemas/aida.schema.json; tests validate against it.

Every top-level object is an envelope ``{"aida": VERSION, "kind": ...}``.
The pieces:

- a *statement* is ``{"prop", "side", "key"}``: the displayed proposition,
  ``term`` (proved) or ``context`` (refuted), and the canonical key - a
  deterministic id;
- a *term* is ``{"pres", "tree"}``: the printed term and its tree, every
  node ``{"id", "node", ...fields}``; ``term_from_tree`` reads a tree back,
  losslessly;
- a *graph* has ``nodes`` (statements), ``edges`` (name, argument, role,
  strict, target, sources with their kind) and ``defaults``; each edge
  names the argument it came from and, where the document knows it, the
  span of that argument in the text;
- verdicts and labels keep the implementation's words: value / open /
  exception, IN / OUT / UNDEC;
- an *error* is ``{"code", "stage", "message", "span", "cause",
  "diagnostics"}``; a *span* is ``{"line", "col", "end_line", "end_col"}``.
"""

from __future__ import annotations

import itertools
from typing import Any, Dict, Optional

from core.ac import ast as A

VERSION = "1.0"


def envelope(kind: str, **data) -> Dict[str, Any]:
    return {"aida": VERSION, "kind": kind, **data}


# ---------------------------------------------------------------------------
# Spans, statements, diagnostics, errors
# ---------------------------------------------------------------------------

def span(s) -> Optional[Dict[str, int]]:
    if s is None:
        return None
    return {"line": s.line, "col": s.col, "end_line": s.end_line, "end_col": s.end_col}


def statement(nodes: Dict[str, str], key: str, side: str) -> Dict[str, str]:
    return {"prop": nodes.get(key, key), "side": side, "key": key}


def diagnostic(d) -> Dict[str, str]:
    return {"level": d.level, "logger": d.logger, "message": d.message}


def error(e, at=None) -> Dict[str, Any]:
    """An AidaError (or any exception) as JSON; ``at`` is a span."""
    cause = getattr(e, "cause", None)
    return {
        "code": getattr(e, "code", "error"),
        "stage": getattr(e, "stage", None),
        "message": str(e),
        "span": span(at if at is not None else getattr(e, "span", None)),
        "cause": None if cause is None else {"type": type(cause).__name__, "message": str(cause)},
        "diagnostics": [diagnostic(d) for d in getattr(e, "diagnostics", [])],
    }


# ---------------------------------------------------------------------------
# Terms
# ---------------------------------------------------------------------------

#: node -> (child slots, scalar fields); children are nodes, scalars text.
#: A binder child is stored under "binder" in JSON (the attribute is ``id``
#: or ``di``, and ``id`` is the node's own id there).
_SHAPES = {
    "Mu": (("id", "term", "context"), ("prop",)),
    "Mutilde": (("di", "term", "context"), ("prop",)),
    "Lamda": (("di", "term"), ("prop",)),
    "Admal": (("id", "context"), ("prop",)),
    "Hyp": (("di",), ("prop",)),
    "Pyh": (("id",), ("prop",)),
    "Cons": (("term", "context"), ("prop",)),
    "Sonc": (("context", "term"), ("prop",)),
    "Goal": ((), ("number", "prop")),
    "Laog": ((), ("number", "prop")),
    "Deleg": ((), ("number", "prop")),
    "Geled": ((), ("number", "prop")),
    "ID": ((), ("name", "prop")),
    "DI": ((), ("name", "prop")),
    "LamdaFO": (("term",), ("var", "sort", "prop")),
    "ConsFO": (("context",), ("fo_term", "prop")),
    "TermsPairFO": (("term",), ("witness", "prop")),
    "DestructTermsPairFO": (("context",), ("var", "sort", "prop")),
}
#: Annotations a node may carry; kept when present.
_EXTRAS = ("label", "cites", "origin", "captured_presumption", "source_kind")


def _json_slot(attr: str) -> str:
    return "binder" if attr in ("id", "di") else attr


def term_tree(node, _ids=None) -> Optional[Dict[str, Any]]:
    """The tree of a proof term; every node gets an id unique in the tree."""
    if node is None:
        return None
    ids = _ids if _ids is not None else itertools.count(1)
    kind = type(node).__name__
    if kind not in _SHAPES:
        raise ValueError(f"cannot serialise a {kind} node")
    children, scalars = _SHAPES[kind]
    out: Dict[str, Any] = {"id": next(ids), "node": kind}
    for name in scalars:
        value = getattr(node, name, None)
        out[name] = None if value is None else str(value)
    for name in children:
        out[_json_slot(name)] = term_tree(getattr(node, name, None), ids)
    for name in _EXTRAS:
        value = getattr(node, name, None)
        if value is not None and value is not False:
            out[name] = value
    return out


def term_from_tree(tree: Optional[Dict[str, Any]]):
    """The proof term a ``term_tree`` describes."""
    if tree is None:
        return None
    kind = tree["node"]
    cls = getattr(A, kind)
    children, scalars = _SHAPES[kind]
    node = cls.__new__(cls)
    for name in ("contr", "pres", "flag"):
        setattr(node, name, None)
    for name in scalars:
        value = tree.get(name)
        if name in ("sort", "fo_term", "witness") and value is not None:
            from core.ac.prop import parse_sort, parse_term
            value = parse_sort(value) if name == "sort" else parse_term(value)
        setattr(node, name, value)
    for name in children:
        setattr(node, name, term_from_tree(tree.get(_json_slot(name))))
    for name in _EXTRAS:
        if name in tree:
            setattr(node, name, tree[name])
    return node


def term(node) -> Optional[Dict[str, Any]]:
    """``{"pres", "tree"}`` of a proof term (``pres`` only for text)."""
    if node is None:
        return None
    if isinstance(node, str):
        return {"pres": node, "tree": None}
    from pres.gen import pres_str
    return {"pres": pres_str(node), "tree": term_tree(node)}


# ---------------------------------------------------------------------------
# Graphs and labellings
# ---------------------------------------------------------------------------

def _argument_of(edge_name: str) -> str:
    """The argument an edge was compiled from: ``a``, ``a.λ1``,
    ``a.supporter2`` all come from ``a``; a strict-phase edge ``s1*`` from
    ``s1``."""
    return edge_name.split(".")[0].rstrip("*")


def graph(g, *, labels=None, spans=None) -> Dict[str, Any]:
    """A DebateGraph; ``labels`` ({(key, side): label}) adds the overlay,
    ``spans`` ({argument name: Span}) the argument's place in the text."""
    nodes = []
    for key in g.nodes:
        for side in ("term", "context"):
            st = statement(g.nodes, key, side)
            if labels is not None and (key, side) in labels:
                st["label"] = labels[(key, side)]
            nodes.append(st)
    edges = []
    for e in g.edges:
        argument = _argument_of(e.name)
        edges.append({
            "name": e.name,
            "argument": argument,
            "span": span((spans or {}).get(argument)),
            "role": e.role,
            "strict": e.strict,
            "target": statement(g.nodes, e.target_key, e.target_side),
            "sources": [{**statement(g.nodes, s.key, s.side), "kind": s.kind} for s in e.sources],
        })
    defaults = [{**statement(g.nodes, key, side), "kinds": sorted(kinds)}
                for (key, side), kinds in g.defaults.items()]
    return {"nodes": nodes, "edges": edges, "defaults": defaults}


def labelling(nodes: Dict[str, str], labels: Dict) -> list:
    return [{**statement(nodes, key, side), "label": label} for (key, side), label in labels.items()]


# ---------------------------------------------------------------------------
# Results
# ---------------------------------------------------------------------------

def _target(t) -> Dict[str, Any]:
    out = {"kind": t.kind, "text": t.text}
    if t.issue is not None:
        out["issue"] = {"key": t.issue[0], "side": t.issue[1]}
    return out


def _diags(result) -> list:
    return [diagnostic(d) for d in getattr(result, "diagnostics", [])]


def spans_of(session) -> Dict[str, Any]:
    """{argument name: Span} for the arguments whose block the interpreter
    saw (``Argument.span``)."""
    if session is None:
        return {}
    return {name: arg.span for name, arg in session.arguments.items() if getattr(arg, "span", None)}


def to_json(result, session=None) -> Dict[str, Any]:
    """The JSON of a service result (wrap/service.py), an AidaError, or an
    interpreter Outcome (wrap/interpreter.py)."""
    from wrap import service as S
    spans = spans_of(session)
    if isinstance(result, S.AidaError):
        return envelope("error", **error(result))
    if isinstance(result, S.GraphResult):
        return envelope("graph", target=_target(result.target), whole=result.whole,
                        graph=graph(result.graph, labels=result.labels, spans=spans),
                        labels_unavailable=result.labels_unavailable, diagnostics=_diags(result))
    if isinstance(result, S.LabelResult):
        return envelope("labellings", target=_target(result.target), semantics=result.semantics,
                        labellings=[labelling(result.graph.nodes, l) for l in result.labellings],
                        graph=graph(result.graph, spans=spans), diagnostics=_diags(result))
    if isinstance(result, S.Evaluation):
        return envelope("evaluation", target=_target(result.target), mode=result.mode,
                        semantics=result.semantics, base=result.base,
                        witness=result.witness, favour=result.favour,
                        verdict=result.nf_class, normal_form=term(result.normal_form),
                        sigma=labelling(result.graph.nodes, result.sigma),
                        graph=graph(result.graph, labels=result.sigma, spans=spans),
                        diagnostics=_diags(result))
    if isinstance(result, S.WitnessEvaluations):
        return envelope("evaluations", target=_target(result.target), semantics=result.semantics,
                        base=result.base,
                        results=[{"witness": n, "verdict": cls, "normal_form": term(nf),
                                  "sigma": [{"key": k, "side": s, "label": l}
                                            for (k, s), l in sigma.items()]}
                                 for n, nf, cls, sigma in result.results],
                        diagnostics=_diags(result))
    if isinstance(result, S.TermResult):
        return envelope("term", target=_target(result.target), which=result.which,
                        description=result.description, term=term(result.term),
                        revision=result.revision, diagnostics=_diags(result))
    if isinstance(result, S.Rendering):
        return envelope("rendering", target=_target(result.target), which=result.which,
                        style=result.style, description=result.description, text=result.text,
                        diagnostics=_diags(result))
    if isinstance(result, S.Shared):
        return envelope("shared", target=_target(result.target),
                        issue={"key": result.issue[0], "side": result.issue[1]},
                        display=result.display, text=result.text,
                        named=sorted(str(n) for n in result.named), diagnostics=_diags(result))
    if isinstance(result, S.Tree):
        return envelope("tree", target=_target(result.target), dot=result.dot, labelled=result.labelled,
                        graph_refused=None if result.graph_refused is None else error(result.graph_refused),
                        labels_unavailable=result.labels_unavailable, diagnostics=_diags(result))
    if isinstance(result, S.Inventory):
        return envelope("inventory", logic=result.logic, revision=result.revision,
                        declarations=[{"name": n, "kind": k, "text": t}
                                      for n, (k, t) in result.declarations.items()],
                        arguments=[{"name": a.name, "conclusion": a.conclusion, "anti": a.anti,
                                    "kind": a.kind, "strict": a.strict, "in_document": a.in_document,
                                    "span": span(spans.get(a.name))}
                                   for a in result.arguments],
                        debates=[{"name": d.name, "issue": d.issue, "onus": d.onus, "scope": d.scope,
                                  "moves": d.moves, "finished": d.finished} for d in result.debates],
                        names=[{"name": n, "kind": k} for n, k in result.names.items()],
                        diagnostics=_diags(result))
    if isinstance(result, S.ImportReport):
        report = to_json(result.report, session)
        return envelope("import", language=result.language, text=result.text,
                        sources=result.sources, ok=report["ok"], entries=report["entries"],
                        diagnostics=_diags(result))
    if isinstance(result, S.Report):
        return envelope("report", ok=result.ok, seconds=round(result.seconds, 3),
                        entries=[outcome(o, session) for o in result.entries],
                        diagnostics=_diags(result))
    if isinstance(result, S.Done):
        return envelope("done", message=result.message, diagnostics=_diags(result))
    if hasattr(result, "status") and hasattr(result, "kind") and hasattr(result, "span"):
        return outcome(result, session)
    raise TypeError(f"no JSON for {type(result).__name__}")


def outcome(o, session=None) -> Dict[str, Any]:
    """One entry of a document check (wrap/interpreter.py's Outcome)."""
    value = o.value
    result = None
    if value is not None and o.status == "ok" and not isinstance(value, dict):
        try:
            result = to_json(value, session)
        except TypeError:
            result = None
    return {
        "span": span(o.span),
        "text": o.text,
        "command": o.kind,
        "status": o.status,
        "message": o.message,
        "error": None if o.error is None else error(o.error, o.span),
        "result": result,
        "diagnostics": [diagnostic(d) for d in o.diagnostics],
    }
