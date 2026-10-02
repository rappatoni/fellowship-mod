"""Pins every value quoted in minicourse-sharing.org (lessons 14-18).

The lessons show the shared form of a debate, its citations under
capture, the type check per definition, the strict phase per instance,
and evaluation without unfolding, on four fixtures.  If the implementation
changes, these fail and the lesson text must be updated.
"""

import io
import contextlib
import warnings
from pathlib import Path

import pytest

from core.ac.ast import Mu, Cons, Goal, Laog, Deleg, Geled, ID, DI
from core.comp.adf_label import grounded_labels, labellings
from core.comp.evaluate import evaluate_debate, evaluate_shared
from core.dc.debate_graph import compile_document, declaration_kinds, canonical_prop
from core.dc.instances import IssueResolver
from core.dc.share import share, is_cite, _leaves
from core.dc.strict import strict_resolve
from core.dc.typecheck import shape, typecheck, typecheck_shared, TypeCheckFailed
from core.dc.unfold import unfold
from mod import store
from pres.gen import pres_str
from wrap.cli import execute_script, setup_prover

HERE = Path(__file__).parent
K = canonical_prop

ROOT14 = (
    "μalt2:P2.<μalt1:P2.<?u1:P2||μ'b2:P2.<μ_:P2.<b2:P2||alt1:P2>||μ'_:P2.<μa2_0:P2.<μth:P2.<"
    "r2_0:P1->P2||anon_1:P1*th:P2>||a2_0:P2>||alt1:P2>>>||μ'b3:P2.<μ_:P2.<b3:P2||alt2:P2>||"
    "μ'_:P2.<μa2_1:P2.<μth_2:P2.<r2_1:P1->P2||anon_1:P1*th_2:P2>||a2_1:P2>||alt2:P2>>>"
)
DEF14 = (
    "anon_1[?:P1, !:P0] := μalt4:P1.<μalt3:P1.<?u2:P1||μ'b5:P1.<μ_:P1.<b5:P1||alt3:P1>||"
    "μ'_:P1.<μa1_0:P1.<μth_3:P1.<r1_0:P0->P1||!u3:P0*th_3:P1>||a1_0:P1>||alt3:P1>>>||"
    "μ'b6:P1.<μ_:P1.<b6:P1||alt4:P1>||μ'_:P1.<μa1_1:P1.<μth_4:P1.<r1_1:P0->P1||!u4:P0*th_4:P1>"
    "||a1_1:P1>||alt4:P1>>>"
)
NF14 = "μa2_1:P2.<r2_1:P1->P2||μb1:P1.<r1_1:P0->P1||!u1:P0*b1:P1>*a2_1:P2>"
NF14_UNFOLDED = "μa2_1:P2.<r2_1:P1->P2||μb1:P1.<r1_1:P0->P1||!u11:P0*b1:P1>*a2_1:P2>"
SKELETON14_P2 = ROOT14.replace("anon_1:P1*th:P2", "?u2:P1*th:P2").replace(
    "anon_1:P1*th_2:P2", "?u3:P1*th_2:P2")

ROOT15 = (
    "μalt3:P.<μalt2:P.<μalt1:P.<?u1:P||μ'b2:P.<μ_:P.<b2:P||alt1:P>||μ'_:P.<μp1:P.<μrule:P.<"
    "pRule1:(Q->false)->P||λh:Q.μalpha:false.<h:Q||anon_1[rule -> P:!, h -> ?:Q, ?:P]:Q>"
    "*rule:P>||p1:P>||alt1:P>>>||μ'b3:P.<μ_:P.<b3:P||alt2:P>||μ'_:P.<μp2:P.<μrule_2:P.<"
    "pRule2:(Q->false)->P||λh_2:Q.μalpha_2:false.<h_2:Q||anon_1[rule_2 -> P:!, h_2 -> ?:Q, ?:P]:Q>"
    "*rule_2:P>||p2:P>||alt2:P>>>||μ'b6:P.<μ_:P.<b6:P||μ'x5:P.<x5:P||u2:P!>>||μ'_:P.<b6:P||alt3:P>>>"
)
DEF15_HEAD = "anon_1[P:!, Q:!] := μ'alt4:Q.<μb8:Q.<μ_:Q.<alt4:Q||b8:Q>||μ'_:Q.<μq:Q.<μrule_3:Q.<"

DEF17_A0 = (
    "a0[!:P0] := μalt5:P0.<!u3:P0||μ'b8:P0.<μ_:P0.<b8:P0||μ'k:P0.<k:P0||μ'x:P0.<np0:~P0||"
    "μ'H1:~P0.<H1:~P0||x:P0*_F_>>>>||μ'_:P0.<b8:P0||alt5:P0>>>"
)


# -- helpers ------------------------------------------------------------------------

@pytest.fixture
def fresh():
    store.arguments.clear()
    store.document.clear()
    prover = setup_prover()
    yield prover
    prover.close()


def run(prover, script, capture=True):
    """Replay a fixture; return what the CLI printed."""
    out = io.StringIO()
    with warnings.catch_warnings(), contextlib.redirect_stdout(out):
        warnings.simplefilter("ignore")
        execute_script(prover, str(script), strict=False, stop_on_error=False, isolate=False,
                       render_files=False)
    return out.getvalue()


def load(prover, lesson, prop):
    """(document, issue, strict names, kinds, shared form) for a lesson's fixture."""
    run(prover, HERE / lesson)
    issue = (K(prop), "term")
    return (prover.document, issue, set(prover.declarations.keys()),
            declaration_kinds(prover.declarations), prover.shared_debate(issue))


def labels(graph, semantics="grounded"):
    found = grounded_labels(graph) if semantics == "grounded" else labellings(graph, semantics)[0]
    return {f"{graph.nodes[k]}[{s[0]}]": v for (k, s), v in found.items()}


def size(node):
    if node is None:
        return 0
    return 1 + sum(size(getattr(node, slot, None)) for slot in ("term", "context"))


def sites(node):
    return [leaf.number for leaf in _leaves(node) if isinstance(leaf, (Goal, Laog, Deleg, Geled))]


def eta(name, prop, inner):
    return Mu(ID(name, prop), prop, inner, ID(name, prop))


def from_site(name, prop, axiom, premise, site):
    return eta(name, prop, Mu(ID("rule", prop), prop, DI(axiom, f"{premise}->{prop}"),
                              Cons(site, ID("rule", prop))))


def doubled_chain(levels, supporters=2):
    """The generated documents of tests/test_share.py: ``supporters``
    alternative derivations of P_i from P_{i-1} per level."""
    named, strict = [], set()
    for i in range(1, levels + 1):
        for a in range(supporters):
            axiom = f"r{i}_{a}"
            strict.add(axiom)
            site = Deleg("1", "P0") if i == 1 else Goal("1", f"P{i-1}")
            named.append((f"a{i}_{a}", from_site(f"a{i}_{a}", f"P{i}", axiom, f"P{i-1}", site)))
    return compile_document(named, strict_names=strict), (K(f"P{levels}"), "term"), strict


def measure(levels, supporters=2):
    doc, issue, strict = doubled_chain(levels, supporters)
    shared = share(doc, issue)
    resolver = IssueResolver(shared, strict)
    resolver.compile("x")
    return (len(shared.defs), sum(size(d.body) for d in shared.defs.values()),
            size(unfold(doc, issue)), len(resolver.instances))


# -- About this sequel: the table ----------------------------------------------------

class TestTheSizeTable:
    @pytest.mark.parametrize("levels, expected", [
        (1, (2, 30, 29, 2)), (2, (3, 59, 85, 3)), (3, (4, 88, 197, 4)),
        (4, (5, 117, 421, 5)), (8, (9, 233, 7141, 9)),
    ])
    def test_two_supporters_per_level(self, levels, expected):
        assert measure(levels) == expected

    def test_the_term_follows_the_recurrence(self):
        sizes = [measure(n)[2] for n in (1, 2, 3, 4)]
        assert all(b == 2 * a + 27 for a, b in zip(sizes, sizes[1:]))

    @pytest.mark.parametrize("levels, expected", [
        (1, (2, 44, 43, 2)), (2, (3, 87, 169, 3)), (3, (4, 130, 547, 4)), (4, (5, 173, 1681, 5)),
    ])
    def test_three_supporters_per_level(self, levels, expected):
        assert measure(levels, 3) == expected                 # lesson 14, exercise 2


# -- Lesson 14 ----------------------------------------------------------------------

class TestLesson14Definitions:
    def test_the_debate_as_printed(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson14_sharing.fspy", "P2")
        assert shared.to_text().splitlines() == [ROOT14, DEF14]
        assert shared.named() == {(K("P1"), "term"): "anon_1"}
        assert shared.defs[(K("P0"), "term")].trivial              # P0 is written in place

    def test_the_cli_prints_it(self, fresh):
        out = run(fresh, HERE / "lesson14_sharing.fspy")
        assert "Debate about 'a2_0' (P2[t]), 1 sub-debate(s) cited by name:" in out
        assert f"  {ROOT14}\n  {DEF14}\n" in out

    def test_expansion_is_unfolding(self, fresh):
        doc, issue, *_ , shared = load(fresh, "lesson14_sharing.fspy", "P2")
        assert shape(shared.expand()) == shape(unfold(doc, issue))
        assert size(unfold(doc, issue)) == 85
        assert sum(size(d.body) for d in shared.defs.values()) == 59

    def test_labels_and_normal_form(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson14_sharing.fspy", "P2")
        nf, cls, sigma, graph = evaluate_shared(shared, "a2_0", strict_names=names, strict_kinds=kinds)
        assert labels(graph) == {"P1[t]": "IN", "P0[t]": "IN", "P2[t]": "IN"}
        assert (pres_str(nf), cls) == (NF14, "value")

    def test_the_issue_p1_names_nothing(self, fresh):                # exercise 1
        run(fresh, HERE / "lesson14_sharing.fspy")
        shared = fresh.shared_debate((K("P1"), "term"))
        assert shared.named() == {} and len(shared.to_text().splitlines()) == 1


# -- Lesson 15 ----------------------------------------------------------------------

class TestLesson15Captures:
    def test_the_debate_as_printed(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson15_capture.fspy", "P")
        root, definition = shared.to_text().splitlines()
        assert root == ROOT15
        assert definition.startswith(DEF15_HEAD) and definition.endswith("||b8:Q>>||u4:Q!>")
        assert shared.named() == {(K("Q"), "context"): "anon_1"}

    def test_what_the_citations_record(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson15_capture.fspy", "P")
        root, _rows = shared.presented()
        cites = [leaf for leaf in _leaves(root) if is_cite(leaf)]
        assert [c.bracket for c in cites] == ["[rule -> P:!, h -> ?:Q, ?:P]",
                                              "[rule_2 -> P:!, h_2 -> ?:Q, ?:P]"]
        for c in cites:
            assert set(c.captures) == {(K("P"), "context"), (K("Q"), "term")}
            assert c.cuts == {(K("P"), "term")}

    def test_the_definition_is_larger_than_the_term(self, fresh):
        doc, issue, *_, shared = load(fresh, "lesson15_capture.fspy", "P")
        assert shape(shared.expand()) == shape(unfold(doc, issue))
        assert size(unfold(doc, issue)) == 79
        assert sum(size(d.body) for d in shared.defs.values()) == 98

    def test_the_even_loop_labels(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson15_capture.fspy", "P")
        _nf, _cls, _sigma, graph = evaluate_shared(shared, "p1", strict_names=names, strict_kinds=kinds)
        assert set(labels(graph).values()) == {"UNDEC"} and len(labels(graph)) == 6

    def test_the_issue_q_names_nothing(self, fresh):                 # exercise 1
        run(fresh, HERE / "lesson15_capture.fspy")
        assert fresh.shared_debate((K("Q"), "term")).named() == {}


# -- Lesson 16 ----------------------------------------------------------------------

class TestLesson16TypeCheck:
    def test_the_skeleton_and_the_replay_count(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson14_sharing.fspy", "P2")
        assert pres_str(shared.skeleton(issue)) == SKELETON14_P2
        # a skeleton numbers its sites from u1 again
        assert pres_str(shared.skeleton((K("P1"), "term"))) == DEF14.split(" := ", 1)[1].replace(
            "?u2:P1", "?u1:P1").replace("!u3:P0", "!u2:P0").replace("!u4:P0", "!u3:P0")
        checked = {}
        assert typecheck_shared(fresh, shared, "a2_0", checked) == 2
        assert typecheck_shared(fresh, shared, "a2_0", checked) == 0
        # exercise 1: the smaller debate replays nothing new
        assert typecheck_shared(fresh, fresh.shared_debate((K("P1"), "term")), "a1_0", checked) == 0

    def test_two_spellings_fall_back_to_the_expanded_term(self, fresh):
        out = run(fresh, HERE / "lesson16_spelling.fspy")
        assert "graph: refused: Fellowship rejected the unfolded term for 'p'" in out
        doc = fresh.document
        issue = (K("P"), "term")
        shared = fresh.shared_debate(issue)
        assert list(shared.spelling_clashes()) == ["Q->false"]
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            assert typecheck_shared(fresh, shared, "p") == 2         # the skeletons alone pass
            with pytest.raises(TypeCheckFailed):
                typecheck(fresh, unfold(doc, issue), "p", "P", False)


# -- Lesson 17 ----------------------------------------------------------------------

class TestLesson17Instances:
    def test_the_debate_names_two(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson17_instances.fspy", "P2")
        lines = shared.to_text().splitlines()
        assert lines[0] == ROOT14
        assert lines[1] == DEF14.replace("!u3:P0*th_3:P1", "a0:P0*th_3:P1").replace(
            "!u4:P0*th_4:P1", "a0:P0*th_4:P1")
        assert lines[2] == DEF17_A0
        assert shared.named() == {(K("P1"), "term"): "anon_1", (K("P0"), "term"): "a0"}

    def test_one_decision_over_three_instances(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson17_instances.fspy", "P2")
        resolver = IssueResolver(shared, names, kinds)
        graph = resolver.compile("a2_0")
        assert [(e.name, e.role, e.strict) for e in graph.edges] == [
            ("k", "attacker", True), ("a1_1", "supporter", False), ("a1_0", "supporter", False),
            ("a2_1", "supporter", False), ("a2_0", "supporter", False)]
        assert resolver.strict_edges() == []
        assert len(resolver.instances) == 3
        assert [what for _, what in resolver.decisions()] == ["attacker strict: defeat"]
        p1 = resolver.resolve(resolver.instance((K("P1"), "term"), (), ()))
        # the stubs for P0 carry its open site and its decision flag
        assert "a0*th_3:P1" in pres_str(p1.resolved) and p1.open and p1.decision
        p0 = resolver.resolve(resolver.instance((K("P0"), "term"), (), ()))
        assert p0.open and p0.decision and not p0.trace == []
        # the unfolded term holds the attack four times and decides it four times
        trace = []
        strict_resolve(unfold(doc, issue), names, trace=trace)
        assert [what for _, what in trace].count("attacker strict: defeat") == 4

    def test_labels_and_evaluation(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson17_instances.fspy", "P2")
        nf, cls, sigma, graph = evaluate_shared(shared, "a2_0", strict_names=names, strict_kinds=kinds)
        assert labels(graph, "preferred") == {"P0[c]": "IN", "P1[t]": "OUT", "P0[t]": "OUT", "P2[t]": "OUT"}
        assert (pres_str(nf), cls) == ("?u1:P2", "open")

    def test_lesson15_has_two_instances_and_one_decision(self, fresh):   # exercises 1 and 2
        doc, issue, names, kinds, shared = load(fresh, "lesson15_capture.fspy", "P")
        resolver = IssueResolver(shared, names, kinds)
        resolver.compile("p1")
        resolver.strict_edges()
        assert len(resolver.instances) == 2
        q_fails = next(i for i in resolver.instances.values() if i.statement == (K("Q"), "context"))
        assert q_fails.captured == {(K("P"), "context"), (K("Q"), "term")}
        assert q_fails.cut == {(K("P"), "term")}
        # the trace names the attacker's statement: q proves Q[t] against the presumption "Q fails"
        assert [(doc.nodes[k], s, what) for (k, s), what in resolver.decisions()] == [
            ("Q", "term", "attacker strict: defeat")]


# -- Lesson 18 ----------------------------------------------------------------------

class TestLesson18Evaluation:
    def test_both_routes_agree_up_to_site_numbers(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson14_sharing.fspy", "P2")
        nf_shared, cls_s, *_ = evaluate_shared(shared, "a2_0", strict_names=names, strict_kinds=kinds)
        nf_unfolded, cls_u, *_ = evaluate_debate(unfold(doc, issue), "a2_0", strict_names=names,
                                                 strict_kinds=kinds)
        assert (pres_str(nf_shared), pres_str(nf_unfolded)) == (NF14, NF14_UNFOLDED)
        assert shape(nf_shared) == shape(nf_unfolded) and cls_s == cls_u == "value"

    def test_sixteen_levels_give_a_small_answer(self):
        doc, issue, strict = doubled_chain(16)
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            nf, cls, sigma, _graph = evaluate_shared(share(doc, issue), "x", strict_names=strict)
        assert cls == "value" and sigma[issue] == "IN"
        assert size(nf) == 65 and sites(nf) == ["u1"]
        assert 2 ** 15 * 56 - 27 == 1834981                        # the unfolded term, by the recurrence

    def test_the_pipeline_switch(self, fresh):
        run(fresh, HERE / "lesson14_sharing.fspy")
        assert not fresh.pipeline_unfolded
        fresh.pipeline_unfolded = True
        out = run(fresh, HERE / "lesson14_sharing.fspy")
        assert "Debate about 'a2_0'" in out                        # debate always shows the shared form
        fresh.pipeline_unfolded = False
