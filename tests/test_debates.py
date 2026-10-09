"""Debates (core/dc/debate.py; tasks.org, aida-debate-objects): a named,
ordered selection of the document's arguments, recorded as

    debate pro|con open|closed NAME : ISSUE.
    ARG.  /  [VERB] ARG TARGET.
    cedat tempus.

compiled on demand, and never part of the document graph."""

import os
import tempfile
import warnings

import pytest

from core.comp.evaluate import evaluate_debate
from core.dc.debate import DebateError
from core.dc.debate_graph import canonical_prop as K, declaration_kinds
from pres.gen import pres_str
from wrap.cli import setup_prover, execute_script, debate_line
from wrap.prover import NameClash

FIXTURE = "tests/debates.fspy"


@pytest.fixture
def prover():
    p = setup_prover()
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        execute_script(p, FIXTURE, strict=True, isolate=False)
    yield p
    p.close()


def script(prover, text):
    with tempfile.NamedTemporaryFile("w", suffix=".fspy", delete=False) as f:
        f.write(text)
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            execute_script(prover, f.name, strict=True, isolate=False)
    finally:
        os.unlink(f.name)


FEATHERS = """declare Feathers : bool.
declare feathered : (Feathers -> Flies).
argument feathers : (Flies).
cut (Feathers -> Flies) rule.
axiom feathered.
elim.
by default.
next.
axiom rule.
dixi.
"""


def record(prover, *lines):
    for line in lines:
        assert debate_line(prover, line), line


def verdict(prover, name, mode="skeptical"):
    debate = prover.debates[name]
    term = prover.debate_term(debate)
    _nf, cls, sigma, _graph = evaluate_debate(
        term, name, strict_names=prover.declarations.keys(),
        strict_kinds=declaration_kinds(prover.declarations), mode=mode)
    return cls, sigma[debate.issue]


# ---------------------------------------------------------------------------
# Scope
# ---------------------------------------------------------------------------

def test_a_closed_debate_hears_only_its_moves(prover):
    record(prover, "debate pro closed d : Flies.", "tweety.")
    assert verdict(prover, "d") == ("value", "IN")          # nobody contradicts tweety
    record(prover, "rebut penguin tweety.")
    assert verdict(prover, "d") == ("open", "UNDEC")        # photo is not in the scope
    record(prover, "undermine photo penguin.")
    assert verdict(prover, "d") == ("value", "IN")
    record(prover, "cedat tempus.")
    assert prover.debates["d"].finished and prover.recording_debate is None


def test_an_open_debate_hears_the_document(prover):
    record(prover, "debate pro open o : Flies.", "tweety.", "cedat tempus.")
    # penguin and photo are registered, so the open debate hears both
    assert verdict(prover, "o") == ("value", "IN")
    graph = prover.debate_graph(prover.debates["o"])
    assert graph is prover.graph


def test_a_debate_adds_nothing_to_the_document(prover):
    before = (list(prover.graph.edges), dict(prover.graph.defaults))
    record(prover, "debate pro closed d : Flies.", "tweety.", "rebut penguin tweety.", "cedat tempus.")
    verdict(prover, "d")
    assert (list(prover.graph.edges), dict(prover.graph.defaults)) == before
    assert "d" not in prover.arguments and prover.names["d"] == "debate"


def test_a_closed_scope_presumes_only_what_its_arguments_presume(prover):
    record(prover, "debate pro closed d : Flies.", "tweety.")
    graph = prover.debate_graph(prover.debates["d"])
    assert {e.name for e in graph.edges} == {"tweety"}
    assert (K("Penguin"), "term") not in graph.defaults
    assert (K("Penguin"), "term") in prover.graph.defaults


def test_a_citation_brings_the_cited_argument_into_the_scope(prover):
    script(prover, "argument citer : (Flies).\ncite tweety.\ndixi.\n")
    record(prover, "debate pro closed c : Flies.", "citer.")
    # citer is the bare citation - an identity edge, a default marker only;
    # the argument it cites is in the scope with its edge
    names = {e.name for e in prover.debate_graph(prover.debates["c"]).edges}
    assert names == {"tweety"}


def test_a_non_sequitur_is_in_the_scope_but_not_in_the_issue_graph(prover):
    record(prover, "debate pro closed d : Flies.", "tweety.", "sings.", "cedat tempus.")
    scope = prover.debate_graph(prover.debates["d"])
    assert (K("Sings"), "term") in scope.defaults
    assert "Sings" not in pres_str(prover.debate_term(prover.debates["d"]))


# ---------------------------------------------------------------------------
# Order
# ---------------------------------------------------------------------------

WINGS = """declare Wings : bool.
declare winged : (Wings -> Flies).
argument wings : (Flies).
cut (Wings -> Flies) rule.
axiom winged.
elim.
by default.
next.
axiom rule.
dixi.
"""


def test_the_opening_is_on_top_and_the_rest_in_utterance_order(prover):
    # In the printed term the outermost supporter of a stack comes last.
    script(prover, FEATHERS + WINGS)
    record(prover, "debate pro closed one : Flies.", "tweety.",
           "support feathers tweety.", "support wings tweety.", "cedat tempus.")
    record(prover, "debate pro closed two : Flies.", "tweety.",
           "support wings tweety.", "support feathers tweety.", "cedat tempus.")
    one = pres_str(prover.debate_term(prover.debates["one"]))
    two = pres_str(prover.debate_term(prover.debates["two"]))
    assert one.rindex("μtweety") > one.rindex("μwings") > one.rindex("μfeathers")
    assert two.rindex("μtweety") > two.rindex("μfeathers") > two.rindex("μwings")


def test_the_opening_keeps_the_sites_it_wrote(prover):
    # tweety presumes Bird->Flies; unfolded for tweety the site stays its own
    record(prover, "debate pro closed d : Flies.", "tweety.")
    assert "!u" in pres_str(prover.debate_term(prover.debates["d"]))


# ---------------------------------------------------------------------------
# Verbs and refusals
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("move, ok", [
    ("rebut penguin tweety.", True),          # tweety's conclusion Flies: an obligation
    ("attack penguin tweety.", True),
    ("undermine penguin tweety.", False),     # ... not a presumption
    ("undercut penguin tweety.", False),
    ("support penguin tweety.", False),       # penguin refutes Flies
])
def test_attack_verbs_check_the_kind_of_the_targeted_node(prover, move, ok):
    record(prover, "debate pro closed d : Flies.", "tweety.")
    if ok:
        record(prover, move)
    else:
        with pytest.raises(DebateError):
            debate_line(prover, move)


@pytest.mark.parametrize("move, ok", [
    ("undermine photo penguin.", True),       # penguin presumes Penguin
    ("undercut photo penguin.", True),
    ("attack photo penguin.", True),
    ("rebut photo penguin.", False),          # ... not an obligation
])
def test_attacks_on_a_presumption(prover, move, ok):
    record(prover, "debate pro closed d : Flies.", "tweety.", "rebut penguin tweety.")
    if ok:
        record(prover, move)
    else:
        with pytest.raises(DebateError):
            debate_line(prover, move)


@pytest.mark.parametrize("verb, ok", [
    ("support", True), ("buttress", True), ("reinforce", True), ("undergird", False),
])
def test_support_verbs(prover, verb, ok):
    # a second argument for Flies supports tweety's conclusion (an obligation)
    script(prover, FEATHERS)
    record(prover, "debate pro closed d : Flies.", "tweety.")
    move = f"{verb} feathers tweety."
    if ok:
        record(prover, move)
    else:
        with pytest.raises(DebateError):
            debate_line(prover, move)


def test_a_target_must_have_been_uttered(prover):
    record(prover, "debate pro closed d : Flies.", "tweety.")
    with pytest.raises(DebateError, match="not a move of the debate yet"):
        debate_line(prover, "undermine photo penguin.")


def test_the_opening_move_must_match_onus_and_issue(prover):
    record(prover, "debate con closed d : Flies.")
    with pytest.raises(DebateError, match="counterargument"):
        debate_line(prover, "tweety.")
    record(prover, "penguin.")


def test_names_are_claimed(prover):
    with pytest.raises(NameClash):
        debate_line(prover, "debate pro closed tweety : Flies.")


def test_syntax_refusals(prover):
    with pytest.raises(DebateError, match="no debate is being recorded"):
        debate_line(prover, "rebut penguin tweety.")
    with pytest.raises(DebateError, match="is gone"):
        debate_line(prover, "undercut u1 penguin tweety")
    with pytest.raises(DebateError, match="share ARG"):
        debate_line(prover, "debate tweety")
    record(prover, "debate pro closed d : Flies.")
    with pytest.raises(DebateError, match="full stop"):
        debate_line(prover, "tweety")
    assert not debate_line(prover, "lk.")           # other lines pass through


def test_the_term_is_cached_until_the_document_or_the_debate_changes(prover):
    record(prover, "debate pro open o : Flies.", "tweety.")
    debate = prover.debates["o"]
    first = prover.debate_term(debate)
    assert prover.debate_term(debate) is first
    record(prover, "rebut penguin tweety.")
    second = prover.debate_term(debate)
    assert second is not first
    prover.bump_revision()
    assert prover.debate_term(debate) is not second
