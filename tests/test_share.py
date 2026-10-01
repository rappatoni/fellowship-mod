"""Sharing sub-debates by name (tasks.org, aida-shared-subarguments, stage 1).

``share`` gives every statement reachable from an issue one definition and
puts citations where unfolding would put copies.  Names are transparent,
so the gate is: writing every definition back in (``expand``) yields the
term ``unfold`` yields, up to alpha-equivalence and site numbering.
``unfold`` is the untouched reference.

The rest pins the presentation: what is shown by name (a sub-debate needed
twice, or one the author cited), what a citation's bracket says, and the
`debate` command.
"""

import logging
import random
import warnings
from itertools import count

import pytest

from core.ac.ast import Mu, Mutilde, Lamda, Hyp, Cons, Goal, Deleg, Geled, ID, DI
from core.dc.debate_graph import compile_document, canonical_prop
from core.dc.share import share, is_cite, ANON_PREFIX, _leaves
from core.dc.typecheck import shape
from core.dc.unfold import unfold
from mod import store
from wrap.cli import setup_prover, execute_script
from wrap.prover import NameClash

K = canonical_prop


# -- hand-built documents -------------------------------------------------------

def eta(name, prop, inner):
    return Mu(ID(name, prop), prop, inner, ID(name, prop))


def from_site(name, prop, axiom, premise, site):
    """``name`` derives ``prop`` from an open site for ``premise`` through
    the declared rule ``axiom : premise -> prop``."""
    return eta(name, prop, Mu(ID("rule", prop), prop, DI(axiom, f"{premise}->{prop}"),
                              Cons(site, ID("rule", prop))))


def from_failure(name, prop, axiom, premise, number="1"):
    """``name`` derives ``prop`` from "``premise`` fails", presumed: the
    negation-as-failure shape of the even-loop fixture.  Its hypothesis and
    its continuation are binders in scope of the site, so they capture."""
    refuted = Lamda(Hyp(DI("h", premise), premise),
                    Mu(ID("alpha", "false"), "false", DI("h", premise), Geled(number, premise)))
    refuted.prop = f"{premise}->false"
    return eta(name, prop, Mu(ID("rule", prop), prop, DI(axiom, f"({premise}->false)->{prop}"),
                              Cons(refuted, ID("rule", prop))))


def doubled_chain(levels):
    """Two alternative supporters per level: P_i from P_{i-1}, twice."""
    named, strict = [], set()
    for i in range(1, levels + 1):
        for a in range(2):
            axiom = f"r{i}_{a}"
            strict.add(axiom)
            site = Deleg("1", "P0") if i == 1 else Goal("1", f"P{i-1}")
            named.append((f"a{i}_{a}", from_site(f"a{i}_{a}", f"P{i}", axiom, f"P{i-1}", site)))
    return compile_document(named, strict_names=strict), (K(f"P{levels}"), "term")


def random_document(seed, props=4, arguments=6):
    """A small random document over P0..: supports from obligations and
    presumptions, and derivations from failure, so cycles and captures of
    both kinds occur."""
    rnd = random.Random(seed)
    named, strict, n = [], set(), count(1)
    for _ in range(arguments):
        target, premise = rnd.sample(range(props), 2)
        name, axiom = f"a{next(n)}", f"ax{next(n)}"
        strict.add(axiom)
        kind = rnd.choice(("goal", "deleg", "naf"))
        if kind == "naf":
            body = from_failure(name, f"P{target}", axiom, f"P{premise}")
        else:
            site = (Goal if kind == "goal" else Deleg)("1", f"P{premise}")
            body = from_site(name, f"P{target}", axiom, f"P{premise}", site)
        named.append((name, body))
    return compile_document(named, strict_names=strict)


def same_term(doc, statement):
    return shape(unfold(doc, statement)) == shape(share(doc, statement).expand())


def size(node):
    if node is None:
        return 0
    return 1 + sum(size(getattr(node, slot, None)) for slot in ("term", "context"))


class TestExpansionIsUnfolding:
    @pytest.mark.parametrize("levels", [1, 2, 3, 5])
    def test_doubled_chain(self, levels):
        doc, issue = doubled_chain(levels)
        assert same_term(doc, issue)

    def test_even_loop_closes_by_capture(self):
        doc = compile_document(
            [("pDef", from_failure("pDef", "P", "pRule", "Q")),
             ("qDef", from_failure("qDef", "Q", "qRule", "P"))],
            strict_names={"pRule", "qRule"})
        for statement in doc.statements():
            assert same_term(doc, statement)

    @pytest.mark.parametrize("seed", range(60))
    def test_random_documents(self, seed):
        doc = random_document(seed)
        for statement in doc.statements():
            assert same_term(doc, statement), f"seed {seed}, statement {statement}"

    def test_a_bare_issue_gets_the_eta_wrapper(self):
        doc, _ = doubled_chain(1)
        bare = (K("P0"), "term")
        term = share(doc, bare).expand()
        assert isinstance(term, Mu) and isinstance(term.term, Deleg)
        assert same_term(doc, bare)


class TestSharing:
    def test_the_shared_form_is_linear_where_the_term_is_exponential(self):
        sizes = {}
        for levels in (4, 8):
            doc, issue = doubled_chain(levels)
            shared = share(doc, issue)
            sizes[levels] = (sum(size(d.body) for d in shared.defs.values()),
                             size(shared.expand()))
        (shared4, term4), (shared8, term8) = sizes[4], sizes[8]
        assert shared8 < 2.5 * shared4          # two more definitions per level
        assert term8 > 12 * term4               # the term doubles per level

    def test_a_sub_debate_needed_twice_is_named_once(self):
        doc, issue = doubled_chain(2)
        shared = share(doc, issue)
        assert shared.named() == {(K("P1"), "term"): f"{ANON_PREFIX}1"}
        root, rows = shared.presented()
        cites = [leaf for leaf in _leaves(root) if is_cite(leaf)]
        assert [c.name for c in cites] == [f"{ANON_PREFIX}1", f"{ANON_PREFIX}1"]
        assert [name for name, _sites, _body in rows] == [f"{ANON_PREFIX}1"]
        text = shared.to_text()
        assert text.count(f"{ANON_PREFIX}1:P1") == 2
        assert f"{ANON_PREFIX}1[?:P1, !:P0] := " in text

    def test_a_sub_debate_needed_once_stays_inline(self):
        doc = compile_document(
            [("a1", from_site("a1", "P1", "r1", "P0", Deleg("1", "P0"))),
             ("a2", from_site("a2", "P2", "r2", "P1", Goal("1", "P1")))],
            strict_names={"r1", "r2"})
        shared = share(doc, (K("P2"), "term"))
        assert shared.named() == {}
        assert "\n" not in shared.to_text()
        assert shape(shared.presented()[0]) == shape(unfold(doc, (K("P2"), "term")))

    def test_the_even_loop_names_nothing_and_shows_the_capture(self):
        doc = compile_document(
            [("pDef", from_failure("pDef", "P", "pRule", "Q")),
             ("qDef", from_failure("qDef", "Q", "qRule", "P"))],
            strict_names={"pRule", "qRule"})
        shared = share(doc, (K("P"), "term"))
        assert shared.named() == {}
        # "P fails" inside qDef is pDef's own continuation: the loop closes
        assert "||rule:P>" in shared.to_text()

    def test_a_citation_says_what_its_site_captures(self):
        # Two derivations of P from "Q fails": the debate about "Q fails" is
        # needed twice, each time under a hypothesis for Q and P's
        # continuation, and Q in turn rests on "P fails".
        doc = compile_document(
            [("p1", from_failure("p1", "P", "ax1", "Q")),
             ("p2", from_failure("p2", "P", "ax2", "Q")),
             ("q", from_failure("q", "Q", "ax3", "P"))],
            strict_names={"ax1", "ax2", "ax3"})
        issue = (K("P"), "term")
        shared = share(doc, issue)
        assert shared.named() == {(K("Q"), "context"): f"{ANON_PREFIX}1"}
        root, _rows = shared.presented()
        cites = [leaf for leaf in _leaves(root) if is_cite(leaf)]
        assert len(cites) == 2
        for leaf in cites:
            # the continuation of P captures "P fails", the hypothesis captures Q
            assert leaf.captures[(K("P"), "context")].startswith("rule")
            assert leaf.captures[(K("Q"), "term")].startswith("h")
            assert "rule" in leaf.bracket and " -> P:!" in leaf.bracket
            assert " -> ?:Q" in leaf.bracket
        assert same_term(doc, issue)

    def test_user_names_and_anonymous_numbers(self):
        doc, issue = doubled_chain(3)
        p1, p2 = (K("P1"), "term"), (K("P2"), "term")
        table = {}

        def anon(statement):
            return table.setdefault(statement, f"{ANON_PREFIX}{len(table) + 1}")

        shared = share(doc, issue, user_names={p2: "middle"}, anon=anon)
        assert shared.named() == {p2: "middle", p1: f"{ANON_PREFIX}1"}
        # the same table gives the same number on the next command
        assert share(doc, issue, anon=anon).named()[p1] == f"{ANON_PREFIX}1"


# -- real documents -----------------------------------------------------------

SCRIPTS = [
    "tests/rationality/contested.fspy",
    "tests/rationality/two_witnesses.fspy",
    "tests/rationality/self_attack_lk.fspy",
    "tests/rationality/cyclic_undercut.fspy",
    "tests/rationality/even_loop_lk.fspy",
    "tests/peirces_law.fspy",
    "tests/naf_olon.fspy",
    "tests/multiple_undercuts.fspy",
    "tests/label_evaluate.fspy",
    "tests/statements_and_citations.fspy",
    "tests/demo/02_support_attack.fspy",
    "tests/demo/03_contested.fspy",
    "tests/demo/04_document.fspy",
    "tests/demo/05_even_loop.fspy",
    "tests/demo/06_peirce.fspy",
]


@pytest.fixture
def fresh():
    store.arguments.clear()
    store.document.clear()
    prover = setup_prover()
    yield prover
    prover.close()


def run(prover, script, **options):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        execute_script(prover, str(script), strict=False, stop_on_error=False, isolate=False,
                       render_files=False, **options)


@pytest.mark.parametrize("script", SCRIPTS)
def test_expansion_is_unfolding_on_fixtures(fresh, script, capsys):
    run(fresh, script, stop_marker=False)
    doc = fresh.document
    assert doc.statements(), "the fixture registered no argument"
    for statement in doc.statements():
        shared = share(doc, statement, fresh.debate_names(), fresh.anon_name)
        assert shape(shared.expand()) == shape(unfold(doc, statement)), statement
        shared.to_text()                         # printing never raises


def chain_script(levels):
    lines = ["lk.", "declare " + ", ".join(f"P{i}" for i in range(levels + 1)) + " : bool."]
    for i in range(1, levels + 1):
        for a in range(2):
            lines.append(f"declare r{i}_{a} : (P{i-1} -> P{i}).")
    lines += ["start argument a0 P0", "by default.", "end argument"]
    for i in range(1, levels + 1):
        for a in range(2):
            lines += [f"start argument a{i}_{a} P{i}", f"cut (P{i-1} -> P{i}) th.",
                      f"axiom r{i}_{a}.", "elim.", "next.", "axiom.", "end argument"]
    return "\n".join(lines) + "\n"


class TestDebateCommand:
    def test_debate_prints_the_named_sub_debate(self, fresh, tmp_path, capsys):
        script = tmp_path / "double.fspy"
        script.write_text(chain_script(2) + "debate a2_0\n")
        run(fresh, script)
        out = capsys.readouterr().out
        assert "Debate about 'a2_0' (P2[t]), 1 sub-debate(s) cited by name:" in out
        assert out.count(f"{ANON_PREFIX}1:P1") == 2
        assert f"{ANON_PREFIX}1[?:P1, !:P0] := " in out

    def test_an_authors_citation_keeps_its_name(self, fresh, tmp_path, capsys):
        script = tmp_path / "cited.fspy"
        script.write_text(open("tests/statements_and_citations.fspy").read() + "\ndebate use\n")
        run(fresh, script)
        out = capsys.readouterr().out
        assert "efficient:EfficientMetro*th:UseMetro" in out
        assert "efficient[!:EfficientMetro] := " in out

    def test_anonymous_numbers_are_stable_in_a_document(self, fresh, tmp_path, capsys):
        script = tmp_path / "double.fspy"
        script.write_text(chain_script(3))
        run(fresh, script)
        p1, p2 = (K("P1"), "term"), (K("P2"), "term")
        first = fresh.shared_debate((K("P2"), "term")).named()
        assert first == {p1: f"{ANON_PREFIX}1"}
        # a larger debate later: P1 keeps its number, P2 gets the next one
        assert fresh.shared_debate((K("P3"), "term")).named() == {
            p1: f"{ANON_PREFIX}1", p2: f"{ANON_PREFIX}2"}

    def test_the_anonymous_prefix_is_reserved(self, fresh):
        with pytest.raises(NameClash):
            fresh.claim_name(f"{ANON_PREFIX}1", "argument")

    def test_label_is_unchanged_by_sharing(self, fresh, tmp_path, capsys, caplog):
        script = tmp_path / "double.fspy"
        script.write_text(chain_script(2) + "label a2_0\ndebate a2_0\nlabel a2_0\n")
        with caplog.at_level(logging.INFO):
            run(fresh, script)
        labels = [r.getMessage() for r in caplog.records
                  if r.getMessage().strip().endswith((" IN", " OUT", " UNDEC"))]
        # three statements, labelled before and after the debate command
        assert len(labels) == 6 and labels[:3] == labels[3:]
