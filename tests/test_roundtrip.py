"""The round trip of a registered debate (core/dc/roundtrip.py; tasks.org,
aida-debate-round-trip-warning): the given term against the term its
extracted arguments unfold to, literally up to alpha, sites counted as
names; a difference is a warning naming where they first part."""

import sys
from copy import deepcopy

import pytest

from core.ac.ast import Mu, Mutilde, Cons, Goal, Laog, Deleg, ID, DI
from core.comp.oracle_terms import alpha_equal, canonical_form
from core.dc.roundtrip import compare
from pres.gen import ProofTermGenerationVisitor
from wrap.cli import setup_prover
from wrap.debate_session import declaration_command

sys.path.insert(0, "tests")
from test_registered_debate import block, run  # noqa: E402


def text(term):
    return ProofTermGenerationVisitor(verbose=-1).visit(deepcopy(term)).pres


class TestCanonicalForm:
    def test_site_numbers_count_as_names(self):
        def owing(first, second):
            return Mu(ID("a", "A"), "A", DI("f", "B->C->A"),
                      Cons(Goal(first, "B"), Cons(Goal(second, "C"), ID("a", "A"))))
        assert not alpha_equal(owing("1", "2"), owing("5", "7"))
        assert alpha_equal(owing("1", "2"), owing("5", "7"), sites="order")
        assert not alpha_equal(owing("1", "1"), owing("1", "2"), sites="order")

    def test_propositions_as_statements(self):
        left = Mu(ID("a", "A->B"), "A->B", DI("f", "A->B"), ID("a", "A->B"))
        right = Mu(ID("a", "A -> B"), "A -> B", DI("f", "A->B"), ID("a", "A -> B"))
        assert compare(left, right).literal


class TestCompare:
    def test_the_first_place_the_terms_part(self):
        given = Mu(ID("a", "A"), "A", DI("f", "B->A"), Cons(Deleg("1", "B"), ID("a", "A")))
        recompiled = Mu(ID("x", "A"), "A", DI("f", "B->A"), Cons(Goal("4", "B"), ID("x", "A")))
        result = compare(given, recompiled)
        assert not result.literal
        assert (result.path, result.given, result.recompiled) == ("context.term", "!1:B", "?4:B")
        assert "first part at context.term" in result.describe()

    def test_the_same_term(self):
        term = Mutilde(DI("x", "A"), "A", DI("x", "A"), Laog("3", "A"))
        assert compare(term, deepcopy(term)).describe() == "its arguments unfold to the term it was given"


@pytest.fixture
def prover():
    p = setup_prover()
    yield p
    p.close()


def unfolded_again(prover, debate):
    """A `register debate` block for the term ``debate``'s arguments unfold
    to, as it is printed, with its own declarations."""
    child = prover.debates[debate].debate_session.session
    term = child.debate_term(child.debates[debate])
    own = [declaration_command(n, d) for n, d in child.declarations.items()
           if n.startswith(f"{debate}__") and n not in child.arguments]
    from core.ac.prop_render import prop_to_command
    issue = prop_to_command(prover.debates[debate].issue_prop)
    return "\n".join([f"register debate again : {issue}."] + own + [f'term "{text(term)}".', "cedat tempus."])


@pytest.mark.parametrize("fixture", ["simple", "simple_pred", "orphans"])
def test_an_import_parts_at_the_entry_point(prover, fixture):
    """The importer's term is not in the unfolder's shapes: the unfolder
    puts the opening on the issue's stack, its root an obligation site."""
    outcomes = run(prover, block(f"tests/import/{fixture}.fspy", "d"))
    roundtrip = prover.debates["d"].roundtrip
    assert not roundtrip.literal and roundtrip.path == "term"
    assert roundtrip.given.startswith("d__f_H_") and roundtrip.recompiled.startswith("?")
    assert roundtrip.labels_equal
    assert any("unfold to a different term" in str(d) for d in outcomes[-1].diagnostics)
    assert prover.debates["d"].moves                          # still usable


@pytest.mark.parametrize("fixture", ["simple", "simple_pred", "orphans"])
def test_what_the_arguments_unfold_to_comes_back(prover, fixture):
    """Registered in turn, the unfolded term is unfolded again to itself:
    the round trip is a fixed point once the term is in the unfolder's
    shapes."""
    run(prover, block(f"tests/import/{fixture}.fspy", "d"))
    outcomes = run(prover, unfolded_again(prover, "d"))
    assert outcomes[-1].status == "ok", outcomes[-1].message
    roundtrip = prover.debates["again"].roundtrip
    assert roundtrip.literal, roundtrip.describe()
    assert roundtrip.labels_equal
    assert not any("different term" in str(d) for d in outcomes[-1].diagnostics)
