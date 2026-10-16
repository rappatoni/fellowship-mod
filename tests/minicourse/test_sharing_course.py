"""Pins every value quoted in docs/minicourses/minicourse-sharing.org (lessons 14-18).

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
from pres.gen import pres_str
from wrap.cli import execute_script, setup_prover

HERE = Path(__file__).parent

#: The pins of the printed shared form, its sizes and the strict phase's
#: decisions are those of the legacy shape.  Since the shared route builds
#: the stacked shape (tasks.org, aida-shared-route-stack-shape) they are
#: stale until the course is regenerated from runs, in the same task.
STALE = pytest.mark.xfail(strict=True, reason="sharing course pins the legacy shape; "
                          "regenerated in aida-shared-route-stack-shape")
K = canonical_prop

# Since 2026-10-16 the shared route exposes every horn (tasks.org,
# aida-rational-closure): the two axioms each statement P_i-1 -> P_i has
# (r1_0, r1_1) engage each other, so the implication is a sub-debate of
# its own, cited by both arguments - anon_1 and anon_3 below.
ROOT14 = (
    "μalt2:P2.<μalt1:P2.<?u1:P2||μ'b2:P2.<μ_:P2.<b2:P2||alt1:P2>||μ'_:P2.<μa2_0:P2.<μth:P2.<anon_"
    "1:P1->P2||anon_2:P1*th:P2>||a2_0:P2>||alt1:P2>>>||μ'b3:P2.<μ_:P2.<b3:P2||alt2:P2>||μ'_:P2.<μ"
    'a2_1:P2.<μth_2:P2.<anon_1:P1->P2||anon_2:P1*th_2:P2>||a2_1:P2>||alt2:P2>>>'
)
DEF14_RULE2 = (
    "anon_1[?:P1->P2] := μalt4:P1->P2.<μalt3:P1->P2.<?u2:P1->P2||μ'b5:P1->P2.<μ_:P1->P2.<b5:P1->P"
    "2||alt3:P1->P2>||μ'_:P1->P2.<r2_0:P1->P2||alt3:P1->P2>>>||μ'b6:P1->P2.<μ_:P1->P2.<b6:P1->P2|"
    "|alt4:P1->P2>||μ'_:P1->P2.<r2_1:P1->P2||alt4:P1->P2>>>"
)
DEF14 = (
    "anon_2[?:P1, ?:P0->P1, !:P0] := μalt6:P1.<μalt5:P1.<?u3:P1||μ'b8:P1.<μ_:P1.<b8:P1||alt5:P1>|"
    "|μ'_:P1.<μa1_0:P1.<μth_3:P1.<anon_3:P0->P1||!u4:P0*th_3:P1>||a1_0:P1>||alt5:P1>>>||μ'b9:P1.<"
    "μ_:P1.<b9:P1||alt6:P1>||μ'_:P1.<μa1_1:P1.<μth_4:P1.<anon_3:P0->P1||!u5:P0*th_4:P1>||a1_1:P1>"
    '||alt6:P1>>>'
)
DEF14_RULE1 = (
    "anon_3[?:P0->P1] := μalt8:P0->P1.<μalt7:P0->P1.<?u6:P0->P1||μ'b11:P0->P1.<μ_:P0->P1.<b11:P0-"
    ">P1||alt7:P0->P1>||μ'_:P0->P1.<r1_0:P0->P1||alt7:P0->P1>>>||μ'b12:P0->P1.<μ_:P0->P1.<b12:P0-"
    ">P1||alt8:P0->P1>||μ'_:P0->P1.<r1_1:P0->P1||alt8:P0->P1>>>"
)
# sites are named by their labels (core/comp/labelled.py), so both routes
# give the same string
NF14 = "μa2_1:P2.<r2_1:P1->P2||μb1:P1.<r1_1:P0->P1||!IN:P0*b1:P1>*a2_1:P2>"
NF14_UNFOLDED = NF14
SKELETON14_P2 = ROOT14.replace("anon_1:P1->P2||anon_2:P1*th:P2", "?u2:P1->P2||?u3:P1*th:P2").replace(
    "anon_1:P1->P2||anon_2:P1*th_2:P2", "?u4:P1->P2||?u5:P1*th_2:P2")

# lesson 15: p1's and p2's lambdas both derive Q->false, so Q->false is a
# sub-debate (anon_2), and so is their rule's statement (anon_1)
ROOT15 = (
    "μalt3:P.<μalt2:P.<μalt1:P.<?u1:P||μ'b2:P.<μ_:P.<b2:P||alt1:P>||μ'_:P.<μp1:P.<μrule:P.<anon_1"
    ":(Q->false)->P||anon_2[rule -> P:!, ?:P]:Q->false*rule:P>||p1:P>||alt1:P>>>||μ'b3:P.<μ_:P.<b"
    "3:P||alt2:P>||μ'_:P.<μp2:P.<μrule_2:P.<anon_1:(Q->false)->P||anon_2[rule_2 -> P:!, ?:P]:Q->f"
    "alse*rule_2:P>||p2:P>||alt2:P>>>||μ'b5:P.<μ_:P.<b5:P||μ'x4:P.<x4:P||u2:P!>>||μ'_:P.<b5:P||al"
    't3:P>>>'
)
DEF15_RULE = (
    "anon_1[?:(Q->false)->P] := μalt5:(Q->false)->P.<μalt4:(Q->false)->P.<?u3:(Q->false)->P||μ'b7"
    ":(Q->false)->P.<μ_:(Q->false)->P.<b7:(Q->false)->P||alt4:(Q->false)->P>||μ'_:(Q->false)->P.<"
    "pRule1:(Q->false)->P||alt4:(Q->false)->P>>>||μ'b8:(Q->false)->P.<μ_:(Q->false)->P.<b8:(Q->fa"
    "lse)->P||alt5:(Q->false)->P>||μ'_:(Q->false)->P.<pRule2:(Q->false)->P||alt5:(Q->false)->P>>>"
)
DEF15_LAMBDA = (
    'anon_2[?:Q->false, ?:(Q->false)->P, P:!, Q:!] := μalt7:Q->false.<μalt6:Q->false.<?u4:Q->fals'
    "e||μ'b10:Q->false.<μ_:Q->false.<b10:Q->false||alt6:Q->false>||μ'_:Q->false.<λh:Q.μalpha:fals"
    "e.<h:Q||anon_3[h -> ?:Q, ?:Q->false]:Q>||alt6:Q->false>>>||μ'b11:Q->false.<μ_:Q->false.<b11:"
    "Q->false||alt7:Q->false>||μ'_:Q->false.<λh_2:Q.μalpha_2:false.<h_2:Q||anon_3[h_2 -> ?:Q, ?:Q"
    '->false]:Q>||alt7:Q->false>>>'
)
DEF15_HEAD = "anon_3[?:(Q->false)->P, ?:Q->false, P:!, Q:!] := μ'alt8:Q.<μb13:Q.<μ_:Q.<alt8:Q||b13:Q>||μ'_:Q.<μq:Q.<μrule_3:Q.<"
DEF15 = (
    "anon_3[?:(Q->false)->P, ?:Q->false, P:!, Q:!] := μ'alt8:Q.<μb13:Q.<μ_:Q.<alt8:Q||b13:Q>||μ'_"
    ":Q.<μq:Q.<μrule_3:Q.<qRule:(P->false)->Q||λh_3:P.μalpha_3:false.<h_3:P||μ'alt10:P.<μb16:P.<μ"
    "_:P.<alt10:P||b16:P>||μ'_:P.<μp2_2:P.<μrule_5:P.<anon_1:(Q->false)->P||anon_2[rule_3 -> Q:!,"
    " h_3 -> ?:P, rule_5 -> P:!, ?:Q]:Q->false*rule_5:P>||p2_2:P>||b16:P>>||μ'alt9:P.<μb15:P.<μ_:"
    "P.<alt9:P||b15:P>||μ'_:P.<μp1_2:P.<μrule_4:P.<anon_1:(Q->false)->P||anon_2[rule_3 -> Q:!, h_"
    '3 -> ?:P, rule_4 -> P:!, ?:Q]:Q->false*rule_4:P>||p1_2:P>||b15:P>>||u5:P!>>>*rule_3:Q>||q:Q>'
    '||b13:Q>>||u6:Q!>'
)

DEF17_A0 = (
    "a0[!:P0] := μalt9:P0.<!u5:P0||μ'b14:P0.<μ_:P0.<b14:P0||μ'k:P0.<k:P0||μ'x:P0.<np0:~P0||μ'H1:~"
    "P0.<H1:~P0||x:P0*_F_>>>>||μ'_:P0.<b14:P0||alt9:P0>>>"
)


# -- helpers ------------------------------------------------------------------------

@pytest.fixture
def fresh():
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
    return (prover.graph, issue, set(prover.declarations.keys()),
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
        (1, (3, 47, 61, 3)), (2, (5, 93, 181, 5)), (3, (7, 139, 421, 7)),
        (4, (9, 185, 901, 9)), (8, (17, 369, 15301, 17)),
    ])
    def test_two_supporters_per_level(self, levels, expected):
        assert measure(levels) == expected

    def test_the_term_follows_the_recurrence(self):
        sizes = [measure(n)[2] for n in (1, 2, 3, 4)]
        assert all(b == 2 * a + 59 for a, b in zip(sizes, sizes[1:]))

    @pytest.mark.parametrize("levels, expected", [
        (1, (3, 69, 115, 3)), (2, (5, 137, 457, 5)), (3, (7, 205, 1483, 7)), (4, (9, 273, 4561, 9)),
    ])
    def test_three_supporters_per_level(self, levels, expected):
        assert measure(levels, 3) == expected                 # lesson 14, exercise 2


# -- Lesson 14 ----------------------------------------------------------------------

class TestLesson14Definitions:
    @STALE
    def test_the_debate_as_printed(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson14_sharing.fspy", "P2")
        assert shared.to_text().splitlines() == [ROOT14, DEF14_RULE2, DEF14, DEF14_RULE1]
        assert shared.named() == {(K("P1->P2"), "term"): "anon_1", (K("P1"), "term"): "anon_2",
                                  (K("P0->P1"), "term"): "anon_3"}
        assert shared.defs[(K("P0"), "term")].trivial              # P0 is written in place

    @STALE
    def test_the_cli_prints_it(self, fresh):
        out = run(fresh, HERE / "lesson14_sharing.fspy")
        assert "Debate about 'a2_0' (P2[t]), 3 sub-debate(s) cited by name:" in out
        assert f"  {ROOT14}\n  {DEF14_RULE2}\n  {DEF14}\n  {DEF14_RULE1}\n" in out

    def test_expansion_is_unfolding(self, fresh):
        doc, issue, *_ , shared = load(fresh, "lesson14_sharing.fspy", "P2")
        assert shape(shared.expand()) == shape(unfold(doc, issue))
        assert size(unfold(doc, issue)) == 181
        assert sum(size(d.body) for d in shared.defs.values()) == 93

    def test_labels_and_normal_form(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson14_sharing.fspy", "P2")
        nf, cls, sigma, graph = evaluate_shared(shared, "a2_0", strict_names=names, strict_kinds=kinds)
        assert labels(graph) == {"P1->P2[t]": "IN", "P0->P1[t]": "IN", "P1[t]": "IN", "P0[t]": "IN", "P2[t]": "IN"}
        assert (pres_str(nf), cls) == (NF14, "value")

    def test_the_issue_p1_names_nothing(self, fresh):                # exercise 1
        run(fresh, HERE / "lesson14_sharing.fspy")
        shared = fresh.shared_debate((K("P1"), "term"))
        # only the implication's debate, cited by both arguments for P1
        assert shared.named() == {(K("P0->P1"), "term"): "anon_3"}
        assert len(shared.to_text().splitlines()) == 2


# -- Lesson 15 ----------------------------------------------------------------------

class TestLesson15Captures:
    @STALE
    def test_the_debate_as_printed(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson15_capture.fspy", "P")
        assert shared.to_text().splitlines() == [ROOT15, DEF15_RULE, DEF15_LAMBDA, DEF15]
        assert DEF15.startswith(DEF15_HEAD)
        assert shared.named() == {(K("(Q->false)->P"), "term"): "anon_1",
                                  (K("Q->false"), "term"): "anon_2", (K("Q"), "context"): "anon_3"}

    @STALE
    def test_what_the_citations_record(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson15_capture.fspy", "P")
        root, _rows = shared.presented()
        cites = [leaf for leaf in _leaves(root) if is_cite(leaf) and leaf.debate == (K("Q->false"), "term")]
        # the lambda is cited now; its hypothesis h is inside the citation
        assert [c.bracket for c in cites] == ["[rule -> P:!, ?:P]", "[rule_2 -> P:!, ?:P]"]
        for c in cites:
            assert set(c.captures) == {(K("P"), "context")}
            assert c.cuts == {(K("P"), "term")}

    @STALE
    def test_the_definition_is_larger_than_the_term(self, fresh):
        doc, issue, *_, shared = load(fresh, "lesson15_capture.fspy", "P")
        assert shape(shared.expand()) == shape(unfold(doc, issue))
        assert size(unfold(doc, issue)) == 183
        assert sum(size(d.body) for d in shared.defs.values()) == 126

    def test_the_even_loop_labels(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson15_capture.fspy", "P")
        _nf, _cls, _sigma, graph = evaluate_shared(shared, "p1", strict_names=names, strict_kinds=kinds)
        found = labels(graph)
        assert len(found) == 8 and {v for k, v in found.items() if "->P" not in k and "->Q" not in k} == {"UNDEC"}
        assert found["(Q->false)->P[t]"] == "IN" and found["(P->false)->Q[t]"] == "IN"     # the axioms

    def test_the_issue_q_names_nothing(self, fresh):                 # exercise 1
        run(fresh, HERE / "lesson15_capture.fspy")
        assert fresh.shared_debate((K("Q"), "term")).named() == {
            (K("(Q->false)->P"), "term"): "anon_1", (K("Q->false"), "term"): "anon_2"}


# -- Lesson 16 ----------------------------------------------------------------------

class TestLesson16TypeCheck:
    @STALE
    def test_the_skeleton_and_the_replay_count(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson14_sharing.fspy", "P2")
        assert pres_str(shared.skeleton(issue)) == SKELETON14_P2
        # a skeleton numbers its sites from u1 again
        assert pres_str(shared.skeleton((K("P1"), "term"))) == DEF14.split(" := ", 1)[1].replace(
            "?u3:P1", "?u1:P1").replace("anon_3:P0->P1||!u4:P0", "?u2:P0->P1||!u3:P0").replace(
            "anon_3:P0->P1||!u5:P0", "?u4:P0->P1||!u5:P0")
        checked = {}
        # P2, P1 and the two implications
        assert typecheck_shared(fresh, shared, "a2_0", checked) == 4
        assert typecheck_shared(fresh, shared, "a2_0", checked) == 0
        # exercise 1: the smaller debate replays nothing new
        assert typecheck_shared(fresh, fresh.shared_debate((K("P1"), "term")), "a1_0", checked) == 0

    def test_two_spellings_fall_back_to_the_expanded_term(self, fresh):
        out = run(fresh, HERE / "lesson16_spelling.fspy")
        assert "graph: refused: Fellowship rejected the unfolded term for 'p'" in out
        doc = fresh.graph
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
    @STALE
    def test_the_debate_names_two(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson17_instances.fspy", "P2")
        lines = shared.to_text().splitlines()
        assert lines[:2] == [ROOT14, DEF14_RULE2]
        assert lines[2] == DEF14.replace("!u4:P0*th_3:P1", "a0:P0*th_3:P1").replace(
            "!u5:P0*th_4:P1", "a0:P0*th_4:P1")
        assert lines[3] == DEF14_RULE1.replace("u6", "u4")
        assert lines[4] == DEF17_A0
        assert shared.named() == {(K("P1->P2"), "term"): "anon_1", (K("P1"), "term"): "anon_2",
                                  (K("P0->P1"), "term"): "anon_3", (K("P0"), "term"): "a0"}

    @STALE
    def test_one_decision_over_three_instances(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson17_instances.fspy", "P2")
        resolver = IssueResolver(shared, names, kinds)
        graph = resolver.compile("a2_0")
        # the axioms are supporters in their implications' debates now
        assert [(e.name, e.role, e.strict) for e in graph.edges if e.role != "subargument"] == [
            ("r2_1", "supporter", True), ("r2_0", "supporter", True),
            ("r1_1", "supporter", True), ("r1_0", "supporter", True),
            ("k", "attacker", True), ("a1_1", "supporter", False), ("a1_0", "supporter", False),
            ("a2_1", "supporter", False), ("a2_0", "supporter", False)]
        assert sorted(e.name for e in graph.edges if e.role == "subargument") == ["np0"]
        assert resolver.strict_edges() == []
        assert len(resolver.instances) == 5
        assert [what for _, what in resolver.decisions()] == ["supporter strict"] * 4 + [
            "attacker strict: defeat"]
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
        assert labels(graph, "preferred") == {"P1->P2[t]": "IN", "P0->P1[t]": "IN", "~P0[t]": "IN",
                                              "P0[c]": "IN", "P1[t]": "OUT", "P0[t]": "OUT", "P2[t]": "OUT"}
        assert (pres_str(nf), cls) == ("?OUT:P2", "open")

    @STALE
    def test_lesson15_has_two_instances_and_one_decision(self, fresh):   # exercises 1 and 2
        doc, issue, names, kinds, shared = load(fresh, "lesson15_capture.fspy", "P")
        resolver = IssueResolver(shared, names, kinds)
        resolver.compile("p1")
        resolver.strict_edges()
        assert len(resolver.instances) == 4
        q_fails = next(i for i in resolver.instances.values() if i.statement == (K("Q"), "context"))
        assert q_fails.captured == {(K("P"), "context"), (K("Q"), "term")}
        assert q_fails.cut == {(K("P"), "term"), (K("Q->false"), "term")}
        # the trace names the attacker's statement: q proves Q[t] against the
        # presumption "Q fails"; before it, the rule's two axioms
        assert [(doc.nodes[k], s, what) for (k, s), what in resolver.decisions()] == [
            ("(Q->false)->P", "term", "supporter strict"), ("(Q->false)->P", "term", "supporter strict"),
            ("Q", "term", "attacker strict: defeat")]


# -- Lesson 18 ----------------------------------------------------------------------

class TestLesson18Evaluation:
    def test_both_routes_agree(self, fresh):
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
        assert size(nf) == 65 and sites(nf) == ["IN"]
        assert 2 ** 16 * 60 - 59 == 3932101                        # the unfolded term, by the recurrence

    def test_the_pipeline_switch(self, fresh):
        run(fresh, HERE / "lesson14_sharing.fspy")
        # the default is the unfolded route since 2026-10-06, until the shared
        # route builds the stacked shape (tasks.org, aida-shared-route-stack-shape)
        assert fresh.pipeline_unfolded
        fresh.pipeline_unfolded = True
        out = run(fresh, HERE / "lesson14_sharing.fspy")
        assert "Debate about 'a2_0'" in out                        # debate always shows the shared form
        fresh.pipeline_unfolded = False
