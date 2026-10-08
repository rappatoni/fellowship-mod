"""Statements, refinement and citation (tasks.org, aida-statements-and-witnesses).

The wrapper owns the registry of every statement and argument; Fellowship is
the kernel for strict proofs only.  A statement only records a claim, as the
maximally enthymemic argument; `prove` (a synonym of `refine`) reopens it,
and `qed` demands a strict witness.  `cite NAME` uses a registered argument
by name - the term shows the name, as an axiom would - while `axiom` stays
reserved for what Fellowship holds.  Names are unique per document.
"""

import logging
from pathlib import Path

import pytest

from core.ac.ast import Goal, ProofTerm
from core.dc.cite import (
    graft_citation, rename_clashing_binders, CitationError, cited_names, expand_citations,
    mark_citations,
)
from core.dc.unfold import unfold
from core.ac.ast import Mu, ID, DI, Deleg
from wrap.cli import execute_script, setup_prover, register_argument_cmd
from wrap.prover import NameClash

FIXTURE = "tests/statements_and_citations.fspy"


@pytest.fixture
def fresh():
    """A prover of the test's own: the fixtures here declare overlapping
    names, and names are unique per document."""
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
        assert any(e.name == "metro_down" and e.strict for e in fresh.graph.edges)

    def test_an_unproved_statement_is_the_enthymeme(self, fresh, tmp_path):
        run(fresh, HEADER + "theorem foo : (B).\n", tmp_path)
        foo = fresh.get_argument("foo")
        assert isinstance(foo.body, Mu) and isinstance(foo.body.term, Goal)
        assert not foo.citable and "foo" not in fresh.declarations
        key = next(k for k, v in fresh.graph.nodes.items() if v == "B")
        assert "obligation" in fresh.graph.defaults[(key, "term")]
        assert fresh.names["foo"] == "statement"

    def test_qed_on_an_open_witness_is_refused_and_the_claim_stays(self, fresh):
        execute_script(fresh, FIXTURE, strict=False, stop_on_error=False, isolate=False)
        clash = fresh.get_argument("clash")
        assert isinstance(clash.body.term, Goal)             # still the enthymeme
        assert "clash" not in fresh.declarations
        assert fresh.names["clash"] == "statement"

    def test_a_statement_opens_nothing(self, fresh, tmp_path, caplog):
        """A statement records a claim; the next tactic is an ordinary prover
        command, which fails for want of an open proof."""
        caplog.set_level(logging.ERROR)
        run(fresh, HEADER + "theorem foo : (A).\naxiom ax.\n", tmp_path)
        assert isinstance(fresh.get_argument("foo").body.term, Goal)
        assert fresh.names["foo"] == "statement"

    @pytest.mark.parametrize("keyword", ["theorem", "lemma", "proposition", "claim", "Lemma"])
    def test_statement_keywords(self, fresh, tmp_path, keyword):
        run(fresh, HEADER + f"{keyword} foo : (A).\nprove foo\naxiom ax.\nqed.\n", tmp_path)
        foo = fresh.get_argument("foo")
        assert foo.citable and foo.statement_kind == keyword.lower()

    @pytest.mark.parametrize("keyword,verb", [("antitheorem", "refute"), ("antilemma", "dispute"),
                                              ("antiproposition", "prove"), ("anticlaim", "argue")])
    def test_anti_keywords_and_verbs(self, fresh, tmp_path, keyword, verb):
        """Any refinement verb works on either side: the side is the claim's."""
        run(fresh, "lk.\ndeclare A : bool.\ndeclare na : (A -> false).\n"
                   f"{keyword} foo : (A).\n{verb} foo\nmoxia na.\nqed.\n", tmp_path)
        foo = fresh.get_argument("foo")
        assert foo.is_anti and foo.statement_kind == keyword

    def test_a_refused_proof_can_be_retried(self, fresh, tmp_path):
        run(fresh, HEADER + "theorem foo : (A).\nprove foo\nby default.\nqed.\n"
                   "prove foo\naxiom ax.\nqed.\n", tmp_path)
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
    def test_a_citation_shows_the_name(self, fresh):
        execute_script(fresh, FIXTURE, strict=False, stop_on_error=False, isolate=False)
        use = fresh.get_argument("use")
        assert [(site, name, strict) for site, name, strict, _ in use.citations] == \
            [("1.2.1", "efficient", False)]
        assert not use.citable
        assert cited_names(use.body) == {"efficient"}
        leaf = next(n for n in _walk(use.body) if getattr(n, "cites", None))
        assert leaf.name == "efficient" and leaf.prop == "EfficientMetro"
        assert not contains(use.body, lambda n: isinstance(n, Deleg))     # not grafted

    def test_expand_grafts_on_demand(self, fresh):
        execute_script(fresh, FIXTURE, strict=False, stop_on_error=False, isolate=False)
        full = expand_citations(fresh.get_argument("use").body, fresh.get_argument)
        assert contains(full, lambda n: isinstance(n, Deleg))
        assert contains(full, lambda n: isinstance(n, Mu) and n.id.name == "efficient")

    def test_the_citing_edge_has_an_obligation_the_cited_edge_meets(self, fresh):
        execute_script(fresh, FIXTURE, strict=False, stop_on_error=False, isolate=False)
        g = fresh.graph
        edge = next(e for e in g.edges if e.name == "use")
        assert [(g.nodes[s.key], s.kind) for s in edge.sources] == [("EfficientMetro", "obligation")]
        term = unfold(g, fresh.issue_of(fresh.get_argument("use")))
        assert contains(term, lambda n: isinstance(n, Deleg) and n.prop == "EfficientMetro")
        assert not contains(term, lambda n: getattr(n, "cites", None))   # expanded

    def test_axiom_is_reserved_for_strict_content(self, fresh, tmp_path, caplog):
        caplog.set_level(logging.ERROR)
        run(fresh, HEADER + "claim guess : (A).\n"
                   "start argument wrong A\naxiom guess.\nend argument\n", tmp_path)
        assert fresh.get_argument("wrong") is None           # Fellowship refused it

    def test_a_misfit_citation_is_refused_cleanly(self, fresh, tmp_path, caplog):
        caplog.set_level(logging.WARNING)
        run(fresh, HEADER + "claim guess : (A).\n"
                   "start argument wrong B\ncite guess.\nend argument\n", tmp_path)
        assert fresh.get_argument("wrong") is None
        assert "wrong" not in fresh.names
        assert any("concludes A" in r.getMessage() for r in caplog.records)
        run(fresh, "start argument after A\naxiom ax.\nend argument\n", tmp_path, "more.fspy")
        assert fresh.get_argument("after").citable            # the prover was left clean

    def test_citing_the_sole_goal(self, fresh, tmp_path):
        """`next.` cannot leave a sole goal, so the citation just records it."""
        run(fresh, HEADER + "claim guess : (A).\n"
                   "start argument whole A\ncite guess.\nend argument\n", tmp_path)
        whole = fresh.get_argument("whole")
        assert cited_names(whole.body) == {"guess"} and not whole.citable

    def test_a_cited_site_cannot_be_worked_on(self, fresh, tmp_path, caplog):
        """The reported bug: a later tactic reached the cited site and turned
        it into a presumption.  A cited site is closed as far as the author is
        concerned."""
        caplog.set_level(logging.WARNING)
        run(fresh, HEADER + "claim guess : (A).\n"
                   "start argument whole A\ncite guess.\nby default.\nend argument\n", tmp_path)
        assert fresh.get_argument("whole") is None
        assert any("cited goal" in r.getMessage() for r in caplog.records)

    def test_citing_a_strict_argument(self, fresh, tmp_path):
        run(fresh, HEADER + "start argument lem A\naxiom ax.\nend argument\n"
                   "start argument user A\ncite lem.\nend argument\n", tmp_path)
        user = fresh.get_argument("user")
        assert user.citable and cited_names(user.body) == {"lem"}

    def test_parsed_terms_get_their_marks_back(self):
        """Free leaves naming arguments are citations; bound ones never are,
        whatever their name (the even-loop fixture has binders named after
        arguments)."""
        body = Mu(ID("guess", "A"), "A", DI("guess", "A"), ID("guess", "A"))
        mark_citations(body, lambda n: n == "guess")
        assert body.term.cites == "guess"
        assert not getattr(body.context, "cites", None)

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

    def test_a_new_document_starts_a_new_namespace(self, fresh, tmp_path):
        run(fresh, HEADER + "start argument z A\nby default.\nend argument\n", tmp_path)
        run(fresh, "new document.\n" + HEADER + "start argument z B\nby default.\nend argument\n",
            tmp_path, "second.fspy")
        assert fresh.get_argument("z").conclusion == "B"

    def test_lk_does_not_start_a_new_namespace(self, fresh, tmp_path):
        # `lk.` only chooses the logic (a no-op in a classical document)
        run(fresh, HEADER + "start argument z A\nby default.\nend argument\n", tmp_path)
        run(fresh, "lk.\nstart argument z B\nby default.\nend argument\n", tmp_path, "second.fspy")
        assert fresh.get_argument("z").conclusion == "A"


class TestAdopt:
    """A strict edge found by unfolding (Peirce's thesis) becomes a theorem
    only when adopted: queries never change the registry."""

    def peirce(self, prover, tmp_path, extra=""):
        demo = Path("tests/demo/06_peirce.fspy").read_text()
        head = demo.split("%stop")[0]
        run(prover, head + "\ngraph p1\n" + extra, tmp_path)

    def test_a_query_does_not_register_the_edge(self, fresh, tmp_path):
        self.peirce(fresh, tmp_path)
        assert "s1*" in fresh.doc.strict_edges
        assert fresh.get_argument("peirce") is None

    def test_adopt_makes_the_thesis_citable(self, fresh, tmp_path):
        self.peirce(fresh, tmp_path, "adopt s1* as peirce\n"
                    "theorem again : (((P -> Q)->P)->P).\nprove again\naxiom peirce.\nqed.\n")
        assert fresh.get_argument("peirce").citable
        assert "peirce" in fresh.declarations
        assert fresh.get_argument("again").citable

    def test_adopt_refuses_an_unseen_edge_and_a_taken_name(self, fresh, tmp_path, capsys):
        self.peirce(fresh, tmp_path, "adopt nothing* as x\nadopt s1* as p1\n")
        out = capsys.readouterr().out
        assert "no strict edge 'nothing*'" in out
        assert "adopt: refused" in out and "already" in out
        assert fresh.get_argument("x") is None


class TestRefinement:
    def test_refining_a_presumption_away_rebuilds_the_document(self, fresh, tmp_path):
        run(fresh, HEADER + "start argument y A\nby default.\nend argument\n"
                   "refine y\naxiom ax.\nend argument\n", tmp_path)
        y = fresh.get_argument("y")
        assert y.citable
        g = fresh.graph
        key = next(k for k, v in g.nodes.items() if v == "A")
        assert "presumption" not in g.defaults.get((key, "term"), set())   # the old marker is gone
        assert any(e.name == "y" and e.strict for e in g.edges)

    def test_refinement_keeps_the_registration_order(self, fresh, tmp_path):
        run(fresh, HEADER + "claim first : (A).\nclaim second : (B).\n"
                   "prove first\naxiom ax.\nqed.\n", tmp_path)
        assert list(fresh.arguments)[:2] == ["first", "second"]

    def test_strictness_propagates_to_citers(self, fresh, tmp_path):
        run(fresh, HEADER + "claim guess : (A).\n"
                   "start argument uses B\ncut (A -> B) th.\naxiom r.\nelim.\ncite guess.\naxiom.\n"
                   "end argument\n", tmp_path)
        assert not fresh.get_argument("uses").citable
        run(fresh, "prove guess\naxiom ax.\nqed.\n", tmp_path, "later.fspy")
        uses = fresh.get_argument("uses")
        assert uses.citable and "uses" in fresh.declarations

    def test_refine_refuses_what_cannot_be_reopened(self, fresh, tmp_path, caplog):
        caplog.set_level(logging.WARNING)
        run(fresh, HEADER + "start argument done A\naxiom ax.\nend argument\n"
                   "refine done\nrefine nothing\n", tmp_path)
        said = " ".join(r.getMessage() for r in caplog.records)
        assert "already strict" in said and "no argument or statement 'nothing'" in said


class TestLogicDefault:
    def test_a_debate_type_checks_without_lk(self, fresh, tmp_path, caplog):
        """Fellowship starts in LJ; the wrapper assumed LK and the type oracle
        replayed classical scaffolds in LJ ("alt1 is neither in your
        hypothesis nor in your conclusion").  Sessions now start in LK."""
        caplog.set_level(logging.WARNING)
        run(fresh, "declare A, B : bool.\ndeclare rule : (A -> B).\n"
                   "claim test : (A).\n"
                   "start argument testing B\ncut (A -> B) x.\naxiom rule.\nelim.\ncite test.\n"
                   "axiom.\nend argument\nclaim another : (B).\ngraph another\n", tmp_path)
        assert not any("Type check failed" in r.getMessage() for r in caplog.records)
        assert fresh.logic == "lk"


def _walk(node):
    if not isinstance(node, ProofTerm):
        return
    yield node
    for slot in ("term", "context"):
        yield from _walk(getattr(node, slot, None))
