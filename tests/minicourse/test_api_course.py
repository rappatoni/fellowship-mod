"""Pins every value quoted in docs/minicourses/minicourse-api.org.  If the
implementation changes, these fail and the lesson text must be updated."""

import json
from pathlib import Path

import jsonschema
import pytest

from wrap.prover import ProverError
from wrap.serialize import term_from_tree, to_json
from wrap.service import AidaError, NotFound, Service
from wrap.syntax import Span, SyntaxRefused, parse, split

ROOT = Path(__file__).resolve().parents[2]
DEBATES = (ROOT / "tests/debates.fspy").read_text()
SCHEMA = json.loads((ROOT / "wrap/schemas/aida.schema.json").read_text())
LESSON4 = DEBATES + """debate pro closed d : Flies.
tweety.
rebut penguin tweety.
rebut photo penguin.
ct.
evaluate d.
evaluate nobody.
argument broken : (Flies).
axiom nothing.
dixi.
"""


@pytest.fixture
def svc(prover):
    return Service.of(prover)


# --- lesson 1 ----------------------------------------------------------

def test_lesson1_logic_at_the_head(prover, svc):
    assert prover.doc.logic_name == "lk"
    svc.new_document("lj", True)
    assert prover.doc.logic_name == "minimal lj"
    prover.send_command("declare A:bool.")
    with pytest.raises(ProverError, match="the logic is chosen when a document starts"):
        prover.send_command("lk.")


def test_lesson1_exercise1_the_logic_in_force_is_a_no_op(prover):
    prover.send_command("declare A:bool.")
    prover.send_command("lk.")
    assert prover.doc.logic_name == "lk" and "A" in prover.doc.declarations


# --- lesson 2 ----------------------------------------------------------

def test_lesson2_units_and_spans():
    text = ('minimal. lk.\nregister t : B := "μ\'x:B.<x||?1.2:B?>".\n'
            "# a note\ndeclare A,\n  B : bool.\n")
    got = [(u.kind, u.text, (u.span.line, u.span.col)) for u in split(text)]
    assert got == [("command", "minimal", (1, 1)), ("command", "lk", (1, 10)),
                   ("command", 'register t : B := "μ\'x:B.<x||?1.2:B?>"', (2, 1)),
                   ("narration", "a note", (3, 1)),
                   ("command", "declare A,\n  B : bool", (4, 1))]
    assert split(text)[-1].span == Span(4, 1, 5, 11)
    assert text.splitlines()[4][10] == "."


def test_lesson2_parse_and_refuse():
    assert parse("evaluate issue :Flies credulous all").args == {
        "target": "issue :Flies", "mode": "credulous", "semantics": "preferred",
        "base": "cbn", "witness": "all", "favour": False}
    with pytest.raises(SyntaxRefused, match=r"`start argument` is gone: write "
                       r"`argument NAME : \(PROPOSITION\)\.` \.\.\. `dixi\.`"):
        parse("start argument a A")
    assert split("register t : B := μx:B.<x||ax>.")[0].text == "register t : B := μx:B"


# --- lesson 3 ----------------------------------------------------------

def test_lesson3_service(svc):
    assert svc.check(DEBATES).ok
    e = svc.evaluate(svc.target("tweety"))
    assert (type(e).__name__, e.nf_class) == ("Evaluation", "value")
    with pytest.raises(NotFound, match="Argument 'nobody' not found.") as err:
        svc.evaluate(svc.target("nobody"))
    assert (err.value.code, err.value.stage) == ("not_found", None)
    assert [(a.name, a.anti, a.in_document) for a in svc.inventory().arguments] == [
        ("tweety", False, True), ("penguin", True, True), ("photo", True, True),
        ("sings", False, True)]


def test_lesson3_exercise1_a_debate_in_lj(svc):
    report = svc.check(DEBATES.replace("lk.", "lj.", 1)
                       + "debate pro closed d : Flies.\ntweety.\nct.\nevaluate d.\n")
    last = report.entries[-1]
    assert last.status == "reported"
    assert (last.error.code, last.error.stage) == ("logic", "graph")


# --- lesson 4 ----------------------------------------------------------

def test_lesson4_check(svc):
    report = svc.check(LESSON4)
    assert (report.ok, len(report.entries)) == (False, 50)
    problems = [(e.span.line, e.kind, e.status, e.message) for e in report.entries
                if e.status not in ("ok", "comment", "skipped")]
    assert problems == [
        (44, "move", "refused", "debate 'd': rebut: 'photo' concludes Penguin:, but "
         "'penguin' has no obligation on :Penguin to attack."),
        (47, "evaluate", "reported", "Argument 'nobody' not found."),
        (50, "dixi", "refused", "nothing is neither in your hypothesis nor in your conclusion."),
    ]
    at46 = next(e for e in report.entries if e.span.line == 46)
    assert at46.value.nf_class == "open"
    assert svc.evaluate(svc.target("tweety")).nf_class == "value"   # Fellowship is clean


def test_lesson4_exercise1_undermine(svc):
    report = svc.check(LESSON4.replace("rebut photo penguin.", "undermine photo penguin."))
    at46 = next(e for e in report.entries if e.span.line == 46)
    assert at46.status == "ok" and at46.value.nf_class == "value"


# --- lesson 5 ----------------------------------------------------------

def test_lesson5_json(prover, svc):
    svc.check(DEBATES)
    out = to_json(svc.evaluate(svc.target("tweety")), prover)
    jsonschema.validate(out, SCHEMA)
    assert (out["aida"], out["kind"], out["verdict"], out["mode"], out["semantics"]) == (
        "1.0", "evaluation", "value", "skeptical", "preferred")
    assert out["sigma"][:2] == [
        {"prop": "Penguin", "side": "context", "key": "S:Penguin", "label": "IN"},
        {"prop": "Photo", "side": "term", "key": "S:Photo", "label": "IN"}]
    nf = out["normal_form"]
    assert nf["pres"] == "μtweety:Flies.<!IN:Bird->Flies||bird:Bird*tweety:Flies>"
    tree = nf["tree"]
    assert (tree["node"], tree["prop"], tree["origin"]) == ("Mu", "Flies", "tweety")
    assert tree["binder"] == {"id": 2, "node": "ID", "name": "tweety", "prop": None}
    assert {k: tree["term"][k] for k in ("node", "number", "prop")} == {
        "node": "Deleg", "number": "IN", "prop": "Bird->Flies"}
    assert (tree["context"]["node"], tree["context"]["prop"]) == ("Cons", "Bird->Flies")
    assert term_from_tree(tree) is not None

    photo = next(e for e in out["graph"]["edges"] if e["name"] == "photo")
    assert photo["argument"] == "photo" and photo["role"] == "attacker" and not photo["strict"]
    assert photo["span"] == {"line": 28, "col": 1, "end_line": 36, "end_col": 5}
    assert photo["target"] == {"prop": "Penguin", "side": "context", "key": "S:Penguin"}
    assert photo["sources"] == [{"prop": "Photo", "side": "term", "key": "S:Photo",
                                 "kind": "presumption"}]


def test_lesson5_error(svc):
    with pytest.raises(AidaError) as err:
        svc.evaluate(svc.target("nobody"))
    assert to_json(err.value) == {
        "aida": "1.0", "kind": "error", "code": "not_found", "stage": None,
        "message": "Argument 'nobody' not found.", "span": None, "cause": None,
        "diagnostics": []}
