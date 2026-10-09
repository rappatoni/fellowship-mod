"""JSON of the service's results (wrap/serialize.py; tasks.org,
aida-json-format): versioned envelopes, valid against
wrap/schemas/aida.schema.json, and term trees that read back losslessly."""

import json
import warnings
from pathlib import Path

import jsonschema
import pytest

from core.dc.debate_graph import canonical_prop as K
from pres.gen import pres_str
from wrap.cli import setup_prover, execute_script
from wrap.service import Service, NotFound
from wrap.serialize import VERSION, to_json, term_tree, term_from_tree

SCHEMA = json.loads(Path("wrap/schemas/aida.schema.json").read_text())


def valid(obj):
    jsonschema.validate(obj, SCHEMA)
    json.dumps(obj)                                   # plain JSON, no objects left
    return obj


@pytest.fixture
def svc():
    p = setup_prover()
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        execute_script(p, "tests/debates.fspy", strict=True, isolate=False)
    yield Service.of(p)
    p.close()


def _same(a, b):
    """Structural equality of two proof terms, annotations included."""
    if type(a) is not type(b):
        return False
    if a is None:
        return True
    for name in ("prop", "number", "name", "label", "cites", "origin"):
        if getattr(a, name, None) != getattr(b, name, None):
            return False
    return all(_same(getattr(a, s, None), getattr(b, s, None)) for s in ("id", "di", "term", "context"))


@pytest.mark.parametrize("script", ["tests/debates.fspy", "tests/peirces_law.fspy",
                                    "tests/rationality/cyclic_undercut.fspy",
                                    "tests/minicourse/lesson15_capture.fspy"])
def test_term_trees_read_back_losslessly(script):
    p = setup_prover()
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            execute_script(p, script, isolate=False)
        svc = Service.of(p)
        terms = [arg.body for arg in p.arguments.values() if getattr(arg, "body", None) is not None]
        for name in list(p.arguments):
            try:
                terms.append(svc.term(name).term)
                terms.append(svc.evaluate(name).normal_form)
            except Exception:
                pass
        assert terms
        for t in terms:
            if isinstance(t, str):
                continue
            tree = term_tree(t)
            json.dumps(tree)
            back = term_from_tree(json.loads(json.dumps(tree)))
            assert _same(t, back) and pres_str(back) == pres_str(t)
    finally:
        p.close()


def test_every_result_kind_is_valid(svc):
    s = svc.session
    for result in (svc.graph("tweety", labels=True), svc.graph("document"),
                   svc.label("tweety", "preferred"), svc.evaluate("tweety"),
                   svc.evaluate("tweety", mode="credulous", witness="all"),
                   svc.term("tweety"), svc.term("tweety", "registered"),
                   svc.render("tweety", "unfolded", "vanilla"), svc.share("tweety"),
                   svc.tree("tweety", which="unfolded"), svc.inventory(),
                   svc.set_typecheck("on")):
        out = valid(to_json(result, s))
        assert out["aida"] == VERSION


def test_an_evaluation(svc):
    out = valid(to_json(svc.evaluate("tweety"), svc.session))
    assert out["kind"] == "evaluation" and out["verdict"] == "value"
    assert {"prop": "Flies", "side": "term", "key": K("Flies"), "label": "IN"} in out["sigma"]
    assert out["normal_form"]["tree"]["node"] == "Mu"


def test_edges_link_to_their_argument_and_its_span(tmp_path):
    p = setup_prover()
    try:
        path = tmp_path / "doc.fspy"
        path.write_text(Path("tests/debates.fspy").read_text())
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            execute_script(p, str(path), isolate=False)
        out = valid(to_json(Service.of(p).graph("tweety"), p))
        edge = next(e for e in out["graph"]["edges"] if e["name"] == "tweety")
        assert edge["argument"] == "tweety"
        text = path.read_text().splitlines()
        assert text[edge["span"]["line"] - 1].startswith("argument tweety")
        assert text[edge["span"]["end_line"] - 1].strip() == "dixi."
    finally:
        p.close()


def test_an_error(svc):
    with pytest.raises(NotFound) as e:
        svc.evaluate("nobody")
    out = valid(to_json(e.value))
    assert out["kind"] == "error" and out["code"] == "not_found" and out["stage"] is None
