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


# -- stage 2: the type check, one definition at a time ----------------------------

from core.dc.typecheck import typecheck, typecheck_shared, TypeCheckFailed   # noqa: E402

SPELLINGS = """lk.
declare P, Q, S : bool.
declare r : (~Q -> P).
declare n : (S -> (Q -> false)).
start argument s S
by default.
end argument
start argument nq (Q -> false)
cut (S -> (Q -> false)) th.
axiom n.
elim.
next.
axiom.
end argument
start argument p P
cut (~Q -> P) th.
axiom r.
elim.
next.
axiom.
end argument
"""


def outcome(check):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        try:
            check()
            return True
        except TypeCheckFailed:
            return False


def both_checks(prover, doc, statement):
    """(expanded, per definition) verdicts, the second as the CLI runs it:
    falling back to the expanded term where a statement is spelled twice."""
    prop, anti = doc.nodes[statement[0]], statement[1] == "context"
    term = unfold(doc, statement)
    shared = share(doc, statement)
    expanded = outcome(lambda: typecheck(prover, term, "x", prop, anti))
    if shared.spelling_clashes():
        return expanded, outcome(lambda: typecheck(prover, term, "y", prop, anti))
    return expanded, outcome(lambda: typecheck_shared(prover, shared, "y"))


class TestTypeCheckPerDefinition:
    @pytest.mark.parametrize("script", SCRIPTS)
    def test_both_checks_agree_on_fixtures(self, fresh, script, capsys):
        run(fresh, script, stop_marker=False)
        try:
            fresh.send_command("discard theorem.")     # a fixture may end inside a proof
        except Exception:
            pass
        doc = fresh.document
        for statement in doc.statements():
            expanded, per_definition = both_checks(fresh, doc, statement)
            assert expanded == per_definition, statement

    def test_one_replay_per_definition_and_none_the_second_time(self, fresh, tmp_path, capsys):
        script = tmp_path / "double.fspy"
        script.write_text(chain_script(3))
        run(fresh, script)
        shared = fresh.shared_debate((K("P3"), "term"))
        checked = {}
        # P3, P2 and P1 have debates; P0 is a bare presumption
        assert typecheck_shared(fresh, shared, "a3_0", checked) == 3
        assert typecheck_shared(fresh, shared, "a3_0", checked) == 0
        # a smaller debate of the same document is already covered
        assert typecheck_shared(fresh, fresh.shared_debate((K("P2"), "term")), "a2_0", checked) == 0

    def test_an_ill_typed_argument_is_rejected_by_both(self, fresh, tmp_path, capsys):
        script = tmp_path / "double.fspy"
        script.write_text(chain_script(2))
        run(fresh, script)
        doc = fresh.document
        edge = next(e for e in doc.edges if e.name == "a1_0")
        rule = next(leaf for leaf in _leaves(edge.term) if getattr(leaf, "name", "") == "r1_0")
        rule.name = "r2_0"                       # P1->P2 where P0->P1 is needed
        issue = (K("P2"), "term")
        assert both_checks(fresh, doc, issue) == (False, False)
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            with pytest.raises(TypeCheckFailed, match=r"in the debate about P1\[t\]"):
                typecheck_shared(fresh, share(doc, issue), "a2_0")

    def test_two_spellings_of_a_statement_fall_back_to_the_expanded_check(self, fresh, tmp_path, capsys):
        # ~Q and Q -> false are one statement for the graph and two
        # propositions for Fellowship.  The join is in no skeleton, so the
        # per-definition check alone would accept what the replay of the
        # expanded term refuses; spelling_clashes makes the CLI replay that.
        script = tmp_path / "spellings.fspy"
        script.write_text(SPELLINGS + "label p\n")
        run(fresh, script)
        out = capsys.readouterr().out
        doc = fresh.document
        issue = (K("P"), "term")
        shared = share(doc, issue)
        assert list(shared.spelling_clashes()) == ["Q->false"]
        term = unfold(doc, issue)
        assert outcome(lambda: typecheck_shared(fresh, shared, "y"))            # the gap
        assert not outcome(lambda: typecheck(fresh, term, "x", "P", False))    # the reference
        assert both_checks(fresh, doc, issue) == (False, False)
        assert "graph: refused" in out                                         # and so the CLI

    def test_the_naf_fixture_spells_each_statement_once(self, fresh, capsys):
        run(fresh, "tests/naf_olon.fspy", stop_marker=False)
        doc = fresh.document
        for statement in doc.statements():
            assert share(doc, statement).spelling_clashes() == {}

    def test_the_typecheck_command_selects_the_mode(self, fresh, tmp_path, capsys):
        script = tmp_path / "modes.fspy"
        script.write_text("lk.\ntypecheck expanded\n")
        run(fresh, script)
        assert fresh.typecheck_enabled and fresh.typecheck_expanded
        script.write_text("typecheck on\n")
        run(fresh, script)
        assert fresh.typecheck_enabled and not fresh.typecheck_expanded
        script.write_text("typecheck off\nlk.\n")
        run(fresh, script)
        assert not fresh.typecheck_enabled        # the switch survives a new document


# -- stage 3: the issue graph, one instance of a sub-debate at a time ------------

import re                                                                    # noqa: E402

from core.comp.adf_label import labellings, referenced_statements            # noqa: E402
from core.dc.debate_graph import declaration_kinds                           # noqa: E402
from core.dc.instances import IssueResolver, compile_issue_shared            # noqa: E402
from core.dc.strict import compile_issue                                     # noqa: E402


def edge_key(edge):
    """An edge up to its name: the unfolded term names copies apart
    (a1_0_2, x.supporter4.attacker19, s3_2*), the shared debate has each
    edge once."""
    return (edge.target_key, edge.target_side, edge.strict, edge.role,
            tuple(sorted((s.key, s.side, s.kind) for s in edge.sources)))


def same_graph(doc, statement, strict_names=(), strict_kinds=None, semantics=("grounded",)):
    """The issue graph from the shared debate against the reference, the
    graph of the unfolded term: edges, default markers, nodes, labellings."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        reference = compile_issue(unfold(doc, statement), "x", strict_names=strict_names,
                                  strict_kinds=strict_kinds)
        shared = compile_issue_shared(share(doc, statement), "x", strict_names=strict_names,
                                      strict_kinds=strict_kinds)
        assert {edge_key(e) for e in shared.edges} == {edge_key(e) for e in reference.edges}
        assert shared.defaults == reference.defaults
        assert set(shared.nodes) == set(reference.nodes)
        if len(referenced_statements(reference)) > 10:
            semantics = ("grounded",)          # the multi-extension solver is slow beyond that
        for chosen in semantics:
            assert labellings(shared, chosen) == labellings(reference, chosen), chosen
    return shared, reference


ALL = ("grounded", "complete", "preferred", "stable")


class TestIssueGraphFromInstances:
    @pytest.mark.parametrize("levels", [1, 2, 3, 5])
    def test_doubled_chain(self, levels):
        doc, issue = doubled_chain(levels)
        strict = {f"r{i}_{a}" for i in range(1, levels + 1) for a in range(2)}
        same_graph(doc, issue, strict, semantics=ALL)

    def test_even_loop(self):
        strict = {"pRule", "qRule"}
        doc = compile_document(
            [("pDef", from_failure("pDef", "P", "pRule", "Q")),
             ("qDef", from_failure("qDef", "Q", "qRule", "P"))], strict_names=strict)
        for statement in doc.statements():
            same_graph(doc, statement, strict, semantics=ALL)

    @pytest.mark.parametrize("seed", range(60))
    def test_random_documents(self, seed):
        doc = random_document(seed)
        strict = {f"ax{n}" for n in range(1, 40)}
        for statement in doc.statements():
            same_graph(doc, statement, strict, semantics=("grounded", "complete"))

    @pytest.mark.parametrize("script", SCRIPTS)
    def test_fixtures(self, fresh, script, capsys):
        run(fresh, script, stop_marker=False)
        doc = fresh.document
        names, kinds = list(fresh.declarations.keys()), declaration_kinds(fresh.declarations)
        for statement in doc.statements():
            same_graph(doc, statement, names, kinds, semantics=("grounded", "complete", "stable"))

    def test_peirce_gets_the_same_strict_edges_with_closed_terms(self, fresh, capsys):
        run(fresh, "tests/peirces_law.fspy", stop_marker=False)
        doc = fresh.document
        names = list(fresh.declarations.keys())
        found = 0
        for statement in doc.statements():
            shared, reference = same_graph(doc, statement, names)

            def strict(graph):
                return {(re.sub(r"_\d+\*$", "*", e.name), e.target_key, e.target_side,
                         shape(e.term)) for e in graph.edges if e.role == "strict"}

            assert strict(shared) == strict(reference)
            found += len(strict(shared))
        assert found, "Peirce's law has a strict edge the framework misses"

    def test_one_instance_per_statement_where_nothing_is_captured(self):
        doc, issue = doubled_chain(12)
        strict = {f"r{i}_{a}" for i in range(1, 13) for a in range(2)}
        resolver = IssueResolver(share(doc, issue), strict)
        resolver.compile("x")
        resolver.strict_edges()
        # P12 .. P1 have debates; P0 is cited as an instance too (a bare site)
        assert len(resolver.instances) == 13
        assert all(not i.captured for i in resolver.instances.values())

    def test_a_capture_makes_its_own_instance(self):
        # "Q fails" is needed under the hypothesis and continuation of p1
        # and of p2; what it reaches is captured there, so the instance is
        # the statement together with those captures - and both sites
        # share it, the binders' names being parameters.
        strict = {"ax1", "ax2", "ax3"}
        doc = compile_document(
            [("p1", from_failure("p1", "P", "ax1", "Q")),
             ("p2", from_failure("p2", "P", "ax2", "Q")),
             ("q", from_failure("q", "Q", "ax3", "P"))], strict_names=strict)
        issue = (K("P"), "term")
        resolver = IssueResolver(share(doc, issue), strict)
        resolver.compile("x")
        q_fails = [i for i in resolver.instances.values() if i.statement == (K("Q"), "context")]
        assert len(q_fails) == 1
        assert q_fails[0].captured == {(K("P"), "context"), (K("Q"), "term")}
        same_graph(doc, issue, strict, semantics=ALL)


class TestLabelDoesNotUnfold:
    def test_label_and_graph_never_call_unfold(self, fresh, tmp_path, capsys, caplog, monkeypatch):
        import core.dc.unfold

        def refuse(*_args, **_kwargs):
            raise AssertionError("label and graph must not unfold the debate")

        script = tmp_path / "double.fspy"
        script.write_text(chain_script(12))
        run(fresh, script)
        monkeypatch.setattr(core.dc.unfold, "unfold", refuse)
        script.write_text("label a12_0\ngraph a12_0\n")
        with caplog.at_level(logging.INFO):
            run(fresh, script)
        labels = [r.getMessage() for r in caplog.records if r.getMessage().strip().endswith(" IN")]
        assert len(labels) == 13                     # P0 .. P12, all accepted

    def test_a_failure_of_the_instance_compiler_falls_back_to_the_unfolded_term(
            self, fresh, tmp_path, capsys, caplog, monkeypatch):
        import core.dc.instances

        def broken(*_args, **_kwargs):
            raise RuntimeError("no instances today")

        script = tmp_path / "double.fspy"
        script.write_text(chain_script(2))
        run(fresh, script)
        monkeypatch.setattr(core.dc.instances, "compile_issue_shared", broken)
        script.write_text("label a2_0\n")
        with caplog.at_level(logging.INFO):
            run(fresh, script)
        said = [r.getMessage() for r in caplog.records]
        assert any("compiling the unfolded term instead" in m for m in said)
        assert len([m for m in said if m.strip().endswith(" IN")]) == 3


# -- stage 4: evaluation, opening a sub-debate only where it is kept ----------------

from core.ac.ast import Laog, Geled                                           # noqa: E402
from core.comp.evaluate import (                                              # noqa: E402
    evaluate_debate, evaluate_shared, evaluate_witnesses, evaluate_witnesses_shared,
)


def verdict(evaluate, subject, **options):
    """(class, normal form up to names and site numbers, sigma), or what
    was raised."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        try:
            nf, nf_class, sigma, _graph = evaluate(subject, "x", **options)
        except Exception as e:
            return ("raised", type(e).__name__)
    return nf_class, shape(nf), tuple(sorted(sigma.items()))


def same_evaluation(doc, statement, strict_names=(), strict_kinds=None,
                    semantics=("grounded", "preferred"), bases=("cbn", "cbv")):
    """``evaluate_shared`` against the reference, ``evaluate_debate`` on the
    unfolded term, in both modes."""
    term = unfold(doc, statement)
    for mode in ("skeptical", "credulous"):
        for chosen in semantics:
            for base in bases:
                options = dict(strict_names=strict_names, strict_kinds=strict_kinds,
                               mode=mode, semantics=chosen, base=base)
                reference = verdict(evaluate_debate, term, **options)
                shared = verdict(evaluate_shared, share(doc, statement), **options)
                assert shared == reference, (statement, mode, chosen, base)


def sites(node):
    return [leaf.number for leaf in _leaves(node) if isinstance(leaf, (Goal, Deleg, Laog, Geled))]


class TestEvaluateFromTheSharedDebate:
    @pytest.mark.parametrize("levels", [1, 2, 3, 5])
    def test_doubled_chain(self, levels):
        doc, issue = doubled_chain(levels)
        strict = {f"r{i}_{a}" for i in range(1, levels + 1) for a in range(2)}
        same_evaluation(doc, issue, strict)

    def test_even_loop(self):
        strict = {"pRule", "qRule"}
        doc = compile_document(
            [("pDef", from_failure("pDef", "P", "pRule", "Q")),
             ("qDef", from_failure("qDef", "Q", "qRule", "P"))], strict_names=strict)
        for statement in doc.statements():
            same_evaluation(doc, statement, strict, semantics=("grounded", "preferred", "stable"))

    @pytest.mark.parametrize("seed", range(30))
    def test_random_documents(self, seed):
        doc = random_document(seed)
        strict = {f"ax{n}" for n in range(1, 40)}
        for statement in doc.statements():
            same_evaluation(doc, statement, strict, semantics=("grounded", "complete"), bases=("cbn",))

    @pytest.mark.parametrize("script", SCRIPTS)
    def test_fixtures(self, fresh, script, capsys):
        run(fresh, script, stop_marker=False)
        doc = fresh.document
        names, kinds = list(fresh.declarations.keys()), declaration_kinds(fresh.declarations)
        for statement in doc.statements():
            small = len(referenced_statements(
                compile_issue(unfold(doc, statement), "x", strict_names=names, strict_kinds=kinds))) <= 10
            same_evaluation(doc, statement, names, kinds,
                            semantics=("grounded", "preferred") if small else ("grounded",),
                            bases=("cbn",))

    def test_every_accepting_witness(self):
        strict = {"pRule", "qRule"}
        doc = compile_document(
            [("pDef", from_failure("pDef", "P", "pRule", "Q")),
             ("qDef", from_failure("qDef", "Q", "qRule", "P"))], strict_names=strict)
        issue = (K("P"), "term")
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            reference, _ = evaluate_witnesses(unfold(doc, issue), "x", strict_names=strict)
            shared, _ = evaluate_witnesses_shared(share(doc, issue), "x", strict_names=strict)
        assert reference, "the even loop has a labelling that accepts P"
        assert [(n, c, shape(nf), s) for n, nf, c, s in shared] == \
               [(n, c, shape(nf), s) for n, nf, c, s in reference]

    def test_only_the_kept_wings_are_written_out(self):
        # 16 levels: the unfolded term has 2^16 copies of the bottom debate.
        # Evaluation keeps one supporter per level, so the answer is a
        # chain of 16 arguments, and nothing else is ever built.
        doc, issue = doubled_chain(16)
        strict = {f"r{i}_{a}" for i in range(1, 17) for a in range(2)}
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            nf, nf_class, sigma, _graph = evaluate_shared(share(doc, issue), "x", strict_names=strict)
        assert nf_class == "value" and sigma[issue] == "IN"
        assert size(nf) < 200
        assert sites(nf) == ["u1"]                    # the presumption of P0, once

    def test_sites_of_a_sub_debate_kept_twice_get_distinct_numbers(self):
        # P needs Q twice (two premises of one rule); the debate about Q is
        # written out in both places, and its sites must not share a number.
        body = eta("p", "P", Mu(ID("rule", "P"), "P", DI("ax", "Q->Q->P"),
                                Cons(Goal("1", "Q"), Cons(Goal("2", "Q"), ID("rule", "P")))))
        doc = compile_document(
            [("q", from_site("q", "Q", "axq", "R", Deleg("1", "R"))), ("p", body)],
            strict_names={"ax", "axq"})
        issue = (K("P"), "term")
        assert share(doc, issue).named() == {(K("Q"), "term"): f"{ANON_PREFIX}1"}
        same_evaluation(doc, issue, {"ax", "axq"})
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            nf, _class, _sigma, _graph = evaluate_shared(share(doc, issue), "x",
                                                        strict_names={"ax", "axq"})
        numbers = sites(nf)
        assert len(numbers) == 2 and len(set(numbers)) == 2


class TestEvaluateCommand:
    def test_evaluate_never_calls_unfold(self, fresh, tmp_path, capsys, caplog, monkeypatch):
        import core.dc.unfold

        def refuse(*_args, **_kwargs):
            raise AssertionError("evaluate must not unfold the debate")

        script = tmp_path / "double.fspy"
        script.write_text(chain_script(12))
        run(fresh, script)
        monkeypatch.setattr(core.dc.unfold, "unfold", refuse)
        script.write_text("evaluate a12_0 skeptical grounded\n")
        with caplog.at_level(logging.INFO):
            run(fresh, script)
        said = [r.getMessage() for r in caplog.records]
        assert any("Evaluated 'a12_0'" in m and m.endswith("VALUE") for m in said)

    def test_the_pipeline_switch_selects_the_unfolded_term(self, fresh, tmp_path, capsys, caplog,
                                                           monkeypatch):
        import core.dc.unfold
        calls = []
        real = core.dc.unfold.unfold

        def counting(graph, issue):
            calls.append(issue)
            return real(graph, issue)

        script = tmp_path / "double.fspy"
        script.write_text(chain_script(2))
        run(fresh, script)
        monkeypatch.setattr(core.dc.unfold, "unfold", counting)

        def evaluated(command):
            caplog.clear()
            script.write_text(command)
            with caplog.at_level(logging.INFO):
                run(fresh, script)
            return [r.getMessage() for r in caplog.records
                    if "Evaluated 'a2_0'" in r.getMessage() or r.getMessage().strip().endswith(" IN")]

        shared = evaluated("evaluate a2_0\nlabel a2_0\n")
        assert calls == []
        unfolded = evaluated("pipeline unfolded\nevaluate a2_0\nlabel a2_0\n")
        assert len(calls) == 2                       # once for evaluate, once for label
        assert fresh.pipeline_unfolded
        assert shared == unfolded and len(shared) == 4      # the verdict and three labels
        evaluated("pipeline shared\nevaluate a2_0\n")
        assert len(calls) == 2 and not fresh.pipeline_unfolded

    def test_a_failure_of_the_shared_evaluator_falls_back_to_the_unfolded_term(
            self, fresh, tmp_path, capsys, caplog, monkeypatch):
        import core.comp.evaluate

        def broken(*_args, **_kwargs):
            raise RuntimeError("no instances today")

        script = tmp_path / "double.fspy"
        script.write_text(chain_script(2))
        run(fresh, script)
        monkeypatch.setattr(core.comp.evaluate, "evaluate_shared", broken)
        script.write_text("evaluate a2_0\n")
        with caplog.at_level(logging.INFO):
            run(fresh, script)
        said = [r.getMessage() for r in caplog.records]
        assert any("evaluating the unfolded term instead" in m for m in said)
        assert any("Evaluated 'a2_0'" in m and m.endswith("VALUE") for m in said)
