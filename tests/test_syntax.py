"""The command syntax (wrap/syntax.py) and the interpreter
(wrap/interpreter.py; tasks.org, aida-command-syntax)."""

import pytest

from wrap.interpreter import Interpreter
from wrap.syntax import Span, SyntaxRefused, Unit, parse, parse_units, split


def kinds(text):
    return [(u.kind if not isinstance(u, SyntaxRefused) else "refused") for u in parse_units(text)]


# -- splitting ---------------------------------------------------------------

def test_every_dot_outside_double_quotes_ends_a_command():
    units = split('minimal. lk.\nregister t : B := "μ\'x:B.<x||?1.2:B?>".\n')
    assert [u.text for u in units] == ["minimal", "lk", 'register t : B := "μ\'x:B.<x||?1.2:B?>"']


def test_a_single_quote_is_an_ordinary_character():
    assert [u.text for u in split("declare A' : bool. lk.")] == ["declare A' : bool", "lk"]


def test_spans():
    units = split("lk.\n# a note\ndeclare A,\n  B : bool.\n")
    assert units[0].span == Span(1, 1, 1, 3)
    assert units[1].kind == "narration" and units[1].text == "a note"
    assert units[2].text.startswith("declare A") and units[2].span == Span(3, 1, 4, 11)


def test_comments_stop_and_incomplete():
    assert kinds("% silent\n%stop\nlk") == ["silent", "stop", "incomplete"]


# -- parsing -----------------------------------------------------------------

@pytest.mark.parametrize("text, kind, args", [
    ("argument a : (A -> B)", "argument", {"name": "a", "conclusion": "A -> B", "anti": False}),
    ("counterargument c : (A)", "argument", {"name": "c", "conclusion": "A", "anti": True}),
    ("dixi", "dixi", {}), ("cr", "dixi", {}), ("case rested", "dixi", {}),
    ("cedat tempus", "close_debate", {}), ("ct", "close_debate", {}),
    ("new document minimal lj", "new_document", {"logic": "lj", "minimal": True}),
    ("Lemma foo : (A)", "statement", {"keyword": "lemma", "name": "foo", "conclusion": "A", "anti": False}),
    ("prove foo", "refine", {"verb": "prove", "name": "foo"}),
    ("rebut b a", "move", {"verb": "rebut", "argument": "b", "target": "a"}),
    ('graph d all "out.dot" show', "graph", {"target": "d", "whole": True, "show": True, "dot_path": "out.dot"}),
    ("evaluate issue :B credulous all", "evaluate",
     {"target": "issue :B", "mode": "credulous", "semantics": "preferred", "base": "cbn",
      "witness": "all", "favour": False}),
    ('load "tests/demo/01.fspy"', "load", {"path": "tests/demo/01.fspy"}),
    ('decorate p : "a. b"', "decorate", {"name": "p", "template": "a. b"}),
    ("declare A : bool", "opaque", {"word": "declare"}),
])
def test_commands(text, kind, args):
    c = parse(text)
    assert c.kind == kind and c.args == args


@pytest.mark.parametrize("text, hint", [
    ("start argument a A", "argument NAME : (PROPOSITION)."),
    ("end argument", "dixi."),
    ("hora est", "cedat tempus."),
    ("undercut u1 arg2 arg1", "cedat tempus."),
    ("register t : B := μx:B", "double-quote the proof term"),
    ("evaluate a sceptical", "unknown option"),
    ("evaluate a 2", "requires credulous"),
    ("tactic pop", "`tactic` is gone"),
])
def test_old_and_wrong_forms_name_their_fix(text, hint):
    with pytest.raises(SyntaxRefused, match=None) as e:
        parse(text)
    assert hint in str(e.value)


# -- the interpreter -----------------------------------------------------------

def test_a_script_through_the_interpreter(prover):
    text = """lk.
declare A, B : bool.
declare r : (A -> B).
argument a : (B).
cut (A -> B) th.
axiom r.
elim.
by default.
next.
axiom th.
dixi.
evaluate a.
dixi.
"""
    out = list(Interpreter(prover).run(text))
    statuses = [(o.kind, o.status) for o in out]
    assert ("dixi", "ok") in statuses and ("evaluate", "ok") in statuses
    assert statuses[-1] == ("dixi", "refused")           # nothing is being recorded
    assert prover.arguments["a"].span == Span(4, 1, 11, 5)
    evaluation = next(o for o in out if o.kind == "evaluate").value
    assert evaluation.nf_class == "value"
