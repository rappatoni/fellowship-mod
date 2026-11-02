"""Extending a registered debate (tasks.org, aida-debate-extends):

    debate NAME extends PARENT.
    ...moves, its own declarations and arguments...
    cedat tempus.

The extension starts where its parent is - its declarations, arguments
and moves, in a session of its own - and what it adds is its own
(NAME__x).  Its term and labels are computed from all of it; the parent
is not changed."""

import sys

import pytest

from core.dc.debate_graph import canonical_prop as K
from wrap.cli import setup_prover
from wrap.service import AidaError, Service
from wrap.syntax import parse

sys.path.insert(0, "tests")
from test_registered_debate import block, run  # noqa: E402

ATTACK_ON_B = """debate s3 extends simple.
deny nb : (B).
register nb_arg : B := "μ'x:B.<x||nb>".
attack nb_arg simple__a5.
cedat tempus."""


def ok(outcomes):
    assert all(o.status in ("ok", "comment") for o in outcomes), [(o.text, o.message) for o in outcomes]
    return outcomes


@pytest.fixture
def prover():
    p = setup_prover()
    ok(run(p, block("tests/import/simple.fspy", "simple")))
    yield p
    p.close()


def labels(prover, name):
    return Service.of(prover).graph(name, labels=True).labels


def test_syntax():
    c = parse("debate s3 extends simple")
    assert (c.kind, c["name"], c["parent"]) == ("extend_debate", "s3", "simple")
    assert parse("debate pro closed d : Flies").kind == "debate"


class TestExtension:
    def test_it_starts_where_its_parent_is(self, prover):
        ok(run(prover, "debate s3 extends simple.\ncedat tempus."))
        s3, simple = prover.debates["s3"], prover.debates["simple"]
        assert (s3.parent, s3.issue_prop, s3.onus, s3.finished) == ("simple", "A", "pro", True)
        assert [str(m) for m in s3.moves] == [str(m) for m in simple.moves]
        child = s3.debate_session.session
        assert set(child.arguments) == set(simple.debate_session.session.arguments)
        assert Service.of(prover).evaluate("s3").nf_class == "value"

    def test_an_attack_changes_the_extension_not_the_parent(self, prover):
        parent_session = prover.debates["simple"].debate_session.session
        parent_arguments = set(parent_session.arguments)
        ok(run(prover, ATTACK_ON_B))
        assert Service.of(prover).evaluate("s3").nf_class != "value"
        assert labels(prover, "s3")[(K("A"), "term")] == "OUT"
        assert Service.of(prover).evaluate("simple").nf_class == "value"
        assert labels(prover, "simple")[(K("A"), "term")] == "IN"
        assert prover.debates["simple"].debate_session.session is parent_session
        assert set(parent_session.arguments) == parent_arguments

    def test_what_it_adds_is_its_own(self, prover):
        ok(run(prover, ATTACK_ON_B))
        s3 = prover.debates["s3"]
        assert s3.local == {"nb": "s3__nb", "nb_arg": "s3__nb_arg"}
        assert str(s3.moves[-1]) == "attack s3__nb_arg simple__a5."
        child = s3.debate_session.session
        assert "s3__nb" in child.declarations and "s3__nb_arg" in child.arguments
        assert not [n for n in prover.names if n.startswith("s3__")]
        assert "s3__" in prover.doc.local_prefixes

    def test_an_argument_block_goes_to_the_extension(self, prover):
        ok(run(prover, "debate s3 extends simple.\ndeny nb : (B).\n"
                       "counterargument nb2 : (B).\nmoxia s3__nb.\ndixi.\n"
                       "attack nb2 simple__a5.\ncedat tempus."))
        s3 = prover.debates["s3"]
        assert "s3__nb2" in s3.debate_session.session.arguments and "nb2" not in prover.arguments
        assert s3.local["nb2"] == "s3__nb2"
        assert any(a.name == "s3__nb2" for a in s3.debate_session.spec.arguments)

    def test_a_document_argument_is_brought_in(self, prover):
        ok(run(prover, "deny nob : (B).\ncounterargument nob_arg : (B).\nmoxia nob.\ndixi."))
        ok(run(prover, "debate s3 extends simple.\nattack nob_arg simple__a5.\ncedat tempus."))
        child = prover.debates["s3"].debate_session.session
        assert "nob_arg" in child.arguments and "nob" in child.declarations
        assert labels(prover, "s3")[(K("A"), "term")] == "OUT"

    def test_an_extension_of_an_extension(self, prover):
        ok(run(prover, ATTACK_ON_B))
        ok(run(prover, "debate s4 extends s3.\ncedat tempus."))
        s4 = prover.debates["s4"]
        assert [str(m) for m in s4.moves] == [str(m) for m in prover.debates["s3"].moves]
        names = Service.of(prover).display_names("s4")
        assert names["simple__a5"] == "a5" and names["s3__nb_arg"] == "nb_arg"

    def test_its_session_restarts_with_its_moves(self, prover):
        ok(run(prover, ATTACK_ON_B))
        before = Service.of(prover).evaluate("s3").nf_class
        session = prover.debates["s3"].debate_session
        session.close()
        assert not session.alive
        assert Service.of(prover).evaluate("s3").nf_class == before
        inner = session.session.debates["s3"]
        assert inner.finished and str(inner.moves[-1]) == "attack s3__nb_arg simple__a5."


class TestRefusals:
    def test_only_a_registered_debate_is_extended(self, prover):
        ok(run(prover, "declare P : bool.\nargument p : (P).\nby default.\ndixi.\n"
                       "debate pro closed d : P.\np.\ncedat tempus."))
        with pytest.raises(AidaError, match="no registered debate 'd'"):
            Service.of(prover).start_extension("e", "d")

    def test_not_while_a_debate_is_recorded(self, prover):
        ok(run(prover, "debate s3 extends simple."))
        with pytest.raises(AidaError, match="still being recorded"):
            Service.of(prover).start_extension("s4", "simple")

    def test_no_refinement_inside_an_extension(self, prover):
        outcomes = run(prover, "debate s3 extends simple.\nprove simple__a1.")
        assert outcomes[-1].status == "refused" and "extension" in outcomes[-1].message

    def test_names(self, prover):
        with pytest.raises(AidaError, match="'__'"):
            Service.of(prover).start_extension("a__b", "simple")
        with pytest.raises(AidaError, match="already"):
            Service.of(prover).start_extension("simple", "simple")
