"""Statements, witnesses and citations (tasks.org, aida-statements-and-witnesses).

The wrapper owns the registry of every statement and argument; Fellowship is
the kernel for strict proofs only.  A theorem is a statement - an enthymeme
the tactics refine - and `qed` demands its witness be strict.  Citing a
registered defeasible argument grafts its body, while the citing argument
contributes its own atomic derivation to the document.  Names are unique per
document.
"""

import logging
from pathlib import Path

import pytest

from core.ac.ast import Goal, ProofTerm
from core.dc.cite import graft_citation, rename_clashing_binders, CitationError
from core.dc.unfold import unfold
from core.ac.ast import Mu, ID, DI, Deleg
from mod import store
from wrap.cli import execute_script, setup_prover, register_argument_cmd
from wrap.prover import NameClash

FIXTURE = "tests/statements_and_citations.fspy"


@pytest.fixture
def fresh():
    """A prover of the test's own: the fixtures here declare overlapping
    names, and names are unique per document."""
    store.arguments.clear()
    store.document.clear()
    pw = setup_prover()
    yield pw
    pw.close()


def run(prover, text, tmp_path, name="script.fspy"):
    path = tmp_path / name
    path.write_text(text)
    execute_script(prover, str(path), strict=False, stop_on_error=False, isolate=False)


def contains(node, predicate):
    if not isinstance(node, ProofTerm):
        return False
    if predicate(node):
        return True
    return any(contains(getattr(node, slot, None), predicate) for slot in ("term", "context"))


HEADER = """lk.
declare A, B : bool.
declare ax : (A).
declare r : (A -> B).
"""


class TestStatements:
    def test_a_theorem_is_refined_into_a_strict_witness(self, fresh):
        execute_script(fresh, FIXTURE, strict=False, stop_on_error=False, isolate=False)
        down = fresh.get_argument("metro_down")
        assert down.citable                                  # Fellowship holds it
        assert "metro_down" in fresh.declarations
        assert not contains(down.body, lambda n: isinstance(n, Goal))
        assert any(e.name == "metro_down" and e.strict for e in fresh.document.edges)

    def test_an_unproved_statement_is_the_enthymeme(self, fresh, tmp_path):
        run(fresh, HEADER + "theorem foo : (B).\n", tmp_path)
        foo = fresh.get_argument("foo")
        assert isinstance(foo.body, Mu) and isinstance(foo.body.term, Goal)
        assert not foo.citable and "foo" not in fresh.declarations
        key = next(k for k, v in fresh.document.nodes.items() if v == "B")
        assert "obligation" in fresh.document.defaults[(key, "term")]
        assert fresh.names["foo"] == "statement"

    def test_qed_on_an_open_witness_is_refused_and_the_claim_stays(self, fresh):
        execute_script(fresh, FIXTURE, strict=False, stop_on_error=False, isolate=False)
        clash = fresh.get_argument("clash")
        assert isinstance(clash.body.term, Goal)             # still the enthymeme
        assert "clash" not in fresh.declarations
        assert fresh.names["clash"] == "statement"

    def test_a_refused_statement_can_be_reopened(self, fresh, tmp_path):
        run(fresh, HEADER + "theorem foo : (A).\nby default.\nqed.\n"
                   "theorem foo : (A).\naxiom ax.\nqed.\n", tmp_path)
        assert fresh.get_argument("foo").citable

    def test_qed_in_an_argument_keeps_the_body(self, fresh, tmp_path):
        """The regression: `qed` used to reset Fellowship's term to ?1 before
        the wrapper read it."""
        run(fresh, HEADER + "start argument lemma B\ncut (A -> B) th.\naxiom r.\nelim.\n"
                   "axiom ax.\naxiom.\nqed.\n", tmp_path)
        lemma = fresh.get_argument("lemma")
        assert lemma.citable and lemma.conclusion == "B"
        assert contains(lemma.body, lambda n: getattr(n, "name", None) == "r")

    def test_end_argument_on_a_closed_body_makes_it_citable(self, fresh, tmp_path):
        run(fresh, HEADER + "start argument closed A\naxiom ax.\nend argument\n"
                   "start argument user A\naxiom closed.\nend argument\n", tmp_path)
        assert fresh.get_argument("closed").citable
        assert fresh.get_argument("user").citable            # cites a strict name


class TestCitation:
    def test_a_defeasible_citation_is_grafted(self, fresh):
        execute_script(fresh, FIXTURE, strict=False, stop_on_error=False, isolate=False)
        use = fresh.get_argument("use")
        assert use.citations == [("1.2.1", "efficient")]
        assert not use.citable
        # the concrete term has efficient inside it ...
        assert contains(use.body, lambda n: isinstance(n, Deleg))
        assert contains(use.body, lambda n: isinstance(n, Mu) and n.id.name == "efficient")
        # ... the atomic one has the site open
        assert contains(use.atomic_body, lambda n: isinstance(n, Goal) and n.number == "1.2.1")

    def test_the_citing_argument_contributes_its_atomic_edge(self, fresh):
        execute_script(fresh, FIXTURE, strict=False, stop_on_error=False, isolate=False)
        g = fresh.document
        edge = next(e for e in g.edges if e.name == "use")
        assert [(g.nodes[s.key], s.kind) for s in edge.sources] == [("EfficientMetro", "obligation")]
        # and unfolding reaches the cited argument's statement
        term = unfold(g, fresh.issue_of(fresh.get_argument("use")))
        assert contains(term, lambda n: isinstance(n, Deleg) and n.prop == "EfficientMetro")

    def test_a_misfit_citation_is_refused_cleanly(self, fresh, tmp_path, caplog):
        caplog.set_level(logging.WARNING)
        run(fresh, HEADER + "start argument guess A\nby default.\nend argument\n"
                   "start argument wrong B\naxiom guess.\nend argument\n", tmp_path)
        assert fresh.get_argument("wrong") is None
        assert "wrong" not in fresh.names                    # the name is free again
        assert any("concludes A" in r.getMessage() for r in caplog.records)
        # and the prover was left clean: a new proof starts normally
        run(fresh, "start argument after A\naxiom ax.\nend argument\n", tmp_path, "more.fspy")
        assert fresh.get_argument("after").citable

    def test_graft_renames_only_clashing_binders(self):
        cited = Mu(ID("th", "A"), "A", DI("ax", "A"), ID("th", "A"))
        host = Mu(ID("th", "B"), "B", Goal("7", "A"), ID("th", "B"))
        renamed = rename_clashing_binders(cited, {"th"})
        assert renamed.id.name == "th_2" and renamed.context.name == "th_2"
        grafted = graft_citation(host, "7", cited, "c")
        assert grafted.term.id.name == "th_2" and grafted.id.name == "th"

    def test_graft_refuses_a_missing_site(self):
        host = Mu(ID("h", "B"), "B", Goal("1", "B"), ID("h", "B"))
        with pytest.raises(CitationError, match="no open site"):
            graft_citation(host, "9", Mu(ID("c", "B"), "B", DI("x", "B"), ID("c", "B")))


class TestNames:
    def test_an_argument_may_not_reuse_a_declared_name(self, fresh, tmp_path):
        run(fresh, HEADER + "start argument ax A\nby default.\nend argument\n", tmp_path)
        assert fresh.get_argument("ax") is None
        assert fresh.names["ax"] == "declaration"

    def test_two_arguments_may_not_share_a_name(self, fresh, tmp_path):
        run(fresh, HEADER + "start argument x A\nby default.\nend argument\n"
                   "start argument x B\nby default.\nend argument\n", tmp_path)
        assert fresh.get_argument("x").conclusion == "A"

    def test_a_declaration_may_not_reuse_an_argument_name(self, fresh, tmp_path):
        run(fresh, HEADER + "start argument y A\nby default.\nend argument\n"
                   "declare y : (B).\n", tmp_path)
        assert fresh.names["y"] == "argument" and "y" not in fresh.declarations

    def test_register_checks_the_name_before_it_replays(self, fresh, tmp_path):
        run(fresh, HEADER, tmp_path)
        with pytest.raises(NameClash):
            register_argument_cmd(fresh, "register ax strict : A := μthesis:A.<ax:A||thesis:A>")

    def test_reserved_prefixes_are_refused(self, fresh):
        with pytest.raises(NameClash, match="reserves"):
            fresh.claim_name("typecheck_mine", "argument")

    def test_lk_starts_a_new_namespace(self, fresh, tmp_path):
        run(fresh, HEADER + "start argument z A\nby default.\nend argument\n", tmp_path)
        run(fresh, HEADER + "start argument z B\nby default.\nend argument\n", tmp_path, "second.fspy")
        assert fresh.get_argument("z").conclusion == "B"


class TestAdopt:
    """A strict edge found by unfolding (Peirce's thesis) becomes a theorem
    only when adopted: queries never change the registry."""

    def peirce(self, prover, tmp_path, extra=""):
        demo = Path("tests/demo/06_peirce.fspy").read_text()
        head = demo.split("%stop")[0]
        run(prover, head + "\ngraph p1\n" + extra, tmp_path)

    def test_a_query_does_not_register_the_edge(self, fresh, tmp_path):
        self.peirce(fresh, tmp_path)
        assert "s1*" in store.document["strict_edges"]
        assert fresh.get_argument("peirce") is None

    def test_adopt_makes_the_thesis_citable(self, fresh, tmp_path):
        self.peirce(fresh, tmp_path, "adopt s1* as peirce\n"
                    "theorem again : (((P -> Q)->P)->P).\naxiom peirce.\nqed.\n")
        assert fresh.get_argument("peirce").citable
        assert "peirce" in fresh.declarations
        assert fresh.get_argument("again").citable

    def test_adopt_refuses_an_unseen_edge_and_a_taken_name(self, fresh, tmp_path, capsys):
        self.peirce(fresh, tmp_path, "adopt nothing* as x\nadopt s1* as p1\n")
        out = capsys.readouterr().out
        assert "no strict edge 'nothing*'" in out
        assert "adopt: refused" in out and "already" in out
        assert fresh.get_argument("x") is None
