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
from core.dc.unfold import argument_edge, unfold, unfold_argument
from pres.gen import pres_str
from wrap.cli import execute_script, setup_prover

HERE = Path(__file__).parent

K = canonical_prop

# The shared forms below are those `share ARG.` prints: the debate biased
# towards ARG, as it is evaluated (tasks.org, aida-shared-route-stack-shape).
# Lesson 14: the root is a2_0's, and the debate about P1 -> P2 is written in
# place twice - canonical under a2_1, a variant with r2_0 on top under a2_0.
ROOT14 = (
    "μalt1:P2.<?u1:P2||μ'b1:P2.<μ_:P2.<b1:P2||alt1:P2>||μ'_:P2.<μalt2:P2.<μa2_1:P2.<μth_2:P2.<μal"
    "t8:P1->P2.<?u2:P1->P2||μ'b8:P1->P2.<μ_:P1->P2.<b8:P1->P2||alt8:P1->P2>||μ'_:P1->P2.<μalt9:P1"
    "->P2.<r2_0:P1->P2||μ'b9:P1->P2.<μ_:P1->P2.<b9:P1->P2||alt9:P1->P2>||μ'_:P1->P2.<r2_1:P1->P2|"
    "|alt9:P1->P2>>>||alt8:P1->P2>>>||anon_1:P1*th_2:P2>||a2_1:P2>||μ'b2:P2.<μ_:P2.<b2:P2||alt2:P"
    "2>||μ'_:P2.<μa2_0:P2.<μth:P2.<μalt4:P1->P2.<?u3:P1->P2||μ'b4:P1->P2.<μ_:P1->P2.<b4:P1->P2||a"
    "lt4:P1->P2>||μ'_:P1->P2.<μalt5:P1->P2.<r2_1:P1->P2||μ'b5:P1->P2.<μ_:P1->P2.<b5:P1->P2||alt5:"
    "P1->P2>||μ'_:P1->P2.<r2_0:P1->P2||alt5:P1->P2>>>||alt4:P1->P2>>>||anon_1:P1*th:P2>||a2_0:P2>"
    "||alt2:P2>>>||alt1:P2>>>"
)
DEF14 = (
    "anon_1[?:P1, ?:P0->P1, !:P0] := μalt6:P1.<?u4:P1||μ'b6:P1.<μ_:P1.<b6:P1||alt6:P1>||μ'_:P1.<μ"
    "alt7:P1.<μa1_0:P1.<μth_3:P1.<anon_2:P0->P1||!u5:P0*th_3:P1>||a1_0:P1>||μ'b7:P1.<μ_:P1.<b7:P1"
    "||alt7:P1>||μ'_:P1.<μa1_1:P1.<μth_4:P1.<anon_2:P0->P1||!u6:P0*th_4:P1>||a1_1:P1>||alt7:P1>>>"
    "||alt6:P1>>>"
)
DEF14_RULE1 = (
    "anon_2[?:P0->P1] := μalt10:P0->P1.<?u7:P0->P1||μ'b10:P0->P1.<μ_:P0->P1.<b10:P0->P1||alt10:P0"
    "->P1>||μ'_:P0->P1.<μalt11:P0->P1.<r1_0:P0->P1||μ'b11:P0->P1.<μ_:P0->P1.<b11:P0->P1||alt11:P0"
    "->P1>||μ'_:P0->P1.<r1_1:P0->P1||alt11:P0->P1>>>||alt10:P0->P1>>>"
)
# sites are named by their labels (core/comp/labelled.py), so both routes
# give the same string
NF14 = "μa2_0:P2.<r2_0:P1->P2||μb1:P1.<r1_1:P0->P1||!IN:P0*b1:P1>*a2_0:P2>"
NF14_UNFOLDED = NF14
SKELETON14_P2 = (
    "μalt1:P2.<?u1:P2||μ'b1:P2.<μ_:P2.<b1:P2||alt1:P2>||μ'_:P2.<μalt2:P2.<μa2_1:P2.<μth_2:P2.<?u2"
    ":P1->P2||?u3:P1*th_2:P2>||a2_1:P2>||μ'b2:P2.<μ_:P2.<b2:P2||alt2:P2>||μ'_:P2.<μa2_0:P2.<μth:P"
    "2.<?u4:P1->P2||?u5:P1*th:P2>||a2_0:P2>||alt2:P2>>>||alt1:P2>>>"
)
SKELETON14_P1 = (
    "μalt6:P1.<?u1:P1||μ'b6:P1.<μ_:P1.<b6:P1||alt6:P1>||μ'_:P1.<μalt7:P1.<μa1_0:P1.<μth_3:P1.<?u2"
    ":P0->P1||!u3:P0*th_3:P1>||a1_0:P1>||μ'b7:P1.<μ_:P1.<b7:P1||alt7:P1>||μ'_:P1.<μa1_1:P1.<μth_4"
    ":P1.<?u4:P0->P1||!u5:P0*th_4:P1>||a1_1:P1>||alt7:P1>>>||alt6:P1>>>"
)

# Lesson 15: only the debate about "Q fails" (Q[c]) is named; the rule's
# and the lambda's debates are written in place, variants under p1.
ROOT15 = (
    "μalt5:P.<μalt1:P.<?u1:P||μ'b1:P.<μ_:P.<b1:P||alt1:P>||μ'_:P.<μalt2:P.<μp2:P.<μrule_2:P.<μalt"
    "12:(Q->false)->P.<?u2:(Q->false)->P||μ'b12:(Q->false)->P.<μ_:(Q->false)->P.<b12:(Q->false)->"
    "P||alt12:(Q->false)->P>||μ'_:(Q->false)->P.<μalt13:(Q->false)->P.<pRule1:(Q->false)->P||μ'b1"
    "3:(Q->false)->P.<μ_:(Q->false)->P.<b13:(Q->false)->P||alt13:(Q->false)->P>||μ'_:(Q->false)->"
    "P.<pRule2:(Q->false)->P||alt13:(Q->false)->P>>>||alt12:(Q->false)->P>>>||μalt14:Q->false.<?u"
    "3:Q->false||μ'b14:Q->false.<μ_:Q->false.<b14:Q->false||alt14:Q->false>||μ'_:Q->false.<μalt15"
    ":Q->false.<λh_5:Q.μalpha_5:false.<h_5:Q||anon_1[rule_2 -> P:!, b1 -> ?:P, b14 -> ?:Q->false,"
    " h_5 -> ?:Q]:Q>||μ'b15:Q->false.<μ_:Q->false.<b15:Q->false||alt15:Q->false>||μ'_:Q->false.<λ"
    "h_6:Q.μalpha_6:false.<h_6:Q||anon_1[rule_2 -> P:!, b1 -> ?:P, b15 -> ?:Q->false, h_6 -> ?:Q]"
    ":Q>||alt15:Q->false>>>||alt14:Q->false>>>*rule_2:P>||p2:P>||μ'b2:P.<μ_:P.<b2:P||alt2:P>||μ'_"
    ":P.<μp1:P.<μrule:P.<μalt6:(Q->false)->P.<?u4:(Q->false)->P||μ'b6:(Q->false)->P.<μ_:(Q->false"
    ")->P.<b6:(Q->false)->P||alt6:(Q->false)->P>||μ'_:(Q->false)->P.<μalt7:(Q->false)->P.<pRule2:"
    "(Q->false)->P||μ'b7:(Q->false)->P.<μ_:(Q->false)->P.<b7:(Q->false)->P||alt7:(Q->false)->P>||"
    "μ'_:(Q->false)->P.<pRule1:(Q->false)->P||alt7:(Q->false)->P>>>||alt6:(Q->false)->P>>>||μalt1"
    "0:Q->false.<?u5:Q->false||μ'b10:Q->false.<μ_:Q->false.<b10:Q->false||alt10:Q->false>||μ'_:Q-"
    ">false.<μalt11:Q->false.<λh_4:Q.μalpha_4:false.<h_4:Q||anon_1[rule -> P:!, b2 -> ?:P, b10 ->"
    " ?:Q->false, h_4 -> ?:Q]:Q>||μ'b11:Q->false.<μ_:Q->false.<b11:Q->false||alt11:Q->false>||μ'_"
    ":Q->false.<λh_3:Q.μalpha_3:false.<h_3:Q||anon_1[rule -> P:!, b2 -> ?:P, b11 -> ?:Q->false, h"
    "_3 -> ?:Q]:Q>||alt11:Q->false>>>||alt10:Q->false>>>*rule:P>||p1:P>||alt2:P>>>||alt1:P>>>||μ'"
    "b5:P.<μ_:P.<b5:P||alt5:P>||μ'_:P.<b5:P||alt5:P>>>"
)
DEF15 = (
    "anon_1[?:(Q->false)->P, ?:Q->false, P:!, Q:!] := μ'alt8:Q.<μb8:Q.<μ_:Q.<alt8:Q||b8:Q>||μ'_:Q"
    ".<μalt9:Q.<alt8:Q||μ'b9:Q.<μ_:Q.<b9:Q||alt9:Q>||μ'_:Q.<μq:Q.<μrule_3:Q.<qRule:(P->false)->Q|"
    "|λh_2:P.μalpha_2:false.<h_2:P||μ'alt16:P.<μb16:P.<μ_:P.<alt16:P||b16:P>||μ'_:P.<μalt17:P.<al"
    "t16:P||μ'b17:P.<μ_:P.<b17:P||alt17:P>||μ'_:P.<μalt18:P.<μp1_2:P.<μrule_4:P.<μalt6_2:(Q->fals"
    "e)->P.<?u6:(Q->false)->P||μ'b6_2:(Q->false)->P.<μ_:(Q->false)->P.<b6_2:(Q->false)->P||alt6_2"
    ":(Q->false)->P>||μ'_:(Q->false)->P.<μalt7_2:(Q->false)->P.<pRule2:(Q->false)->P||μ'b7_2:(Q->"
    "false)->P.<μ_:(Q->false)->P.<b7_2:(Q->false)->P||alt7_2:(Q->false)->P>||μ'_:(Q->false)->P.<p"
    "Rule1:(Q->false)->P||alt7_2:(Q->false)->P>>>||alt6_2:(Q->false)->P>>>||μalt10_2:Q->false.<?u"
    "7:Q->false||μ'b10_2:Q->false.<μ_:Q->false.<b10_2:Q->false||alt10_2:Q->false>||μ'_:Q->false.<"
    "μalt11_2:Q->false.<λh_4_2:Q.μalpha_4_2:false.<h_4_2:Q||rule_3:Q>||μ'b11_2:Q->false.<μ_:Q->fa"
    "lse.<b11_2:Q->false||alt11_2:Q->false>||μ'_:Q->false.<λh_3_2:Q.μalpha_3_2:false.<h_3_2:Q||ru"
    "le_3:Q>||alt11_2:Q->false>>>||alt10_2:Q->false>>>*rule_4:P>||p1_2:P>||μ'b18:P.<μ_:P.<b18:P||"
    "alt18:P>||μ'_:P.<μp2_2:P.<μrule_5:P.<μalt12_2:(Q->false)->P.<?u8:(Q->false)->P||μ'b12_2:(Q->"
    "false)->P.<μ_:(Q->false)->P.<b12_2:(Q->false)->P||alt12_2:(Q->false)->P>||μ'_:(Q->false)->P."
    "<μalt13_2:(Q->false)->P.<pRule1:(Q->false)->P||μ'b13_2:(Q->false)->P.<μ_:(Q->false)->P.<b13_"
    "2:(Q->false)->P||alt13_2:(Q->false)->P>||μ'_:(Q->false)->P.<pRule2:(Q->false)->P||alt13_2:(Q"
    "->false)->P>>>||alt12_2:(Q->false)->P>>>||μalt14_2:Q->false.<?u9:Q->false||μ'b14_2:Q->false."
    "<μ_:Q->false.<b14_2:Q->false||alt14_2:Q->false>||μ'_:Q->false.<μalt15_2:Q->false.<λh_5_2:Q.μ"
    "alpha_5_2:false.<h_5_2:Q||rule_3:Q>||μ'b15_2:Q->false.<μ_:Q->false.<b15_2:Q->false||alt15_2:"
    "Q->false>||μ'_:Q->false.<λh_6_2:Q.μalpha_6_2:false.<h_6_2:Q||rule_3:Q>||alt15_2:Q->false>>>|"
    "|alt14_2:Q->false>>>*rule_5:P>||p2_2:P>||alt18:P>>>||alt17:P>>>||b16:P>>||u10:P!>>*rule_3:Q>"
    "||q:Q>||alt9:Q>>>||b8:Q>>||u11:Q!>"
)
DEF15_HEAD = DEF15[:DEF15.index("μq:Q.")]

DEF17_A0 = (
    "a0[!:P0] := μalt12:P0.<!u6:P0||μ'b12:P0.<μ_:P0.<b12:P0||μ'alt13:P0.<μb13:P0.<μ_:P0.<alt13:P0"
    "||μ'k:P0.<k:P0||μ'x:P0.<np0:~P0||μ'H1:~P0.<H1:~P0||x:P0*_F_>>>>||μ'_:P0.<alt13:P0||b13:P0>>|"
    "|alt12:P0>>||μ'_:P0.<b12:P0||alt12:P0>>>"
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


def load(prover, lesson, prop, argument=None):
    """(document, issue, strict names, kinds, shared form) for a lesson's
    fixture; with ``argument``, its debate biased towards that argument, as
    `share ARG.` prints it."""
    run(prover, HERE / lesson)
    issue = (K(prop), "term")
    found = prover.get_argument(argument) if argument is not None else None
    return (prover.graph, issue, set(prover.declarations.keys()),
            declaration_kinds(prover.declarations), prover.shared_debate(issue, found))


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

@pytest.mark.perf
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
    def test_the_debate_as_printed(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson14_sharing.fspy", "P2", "a2_0")
        assert shared.to_text().splitlines() == [ROOT14, DEF14, DEF14_RULE1]
        assert shared.named() == {(K("P1"), "term"): "anon_1", (K("P0->P1"), "term"): "anon_2"}
        assert shared.defs[(K("P0"), "term")].trivial              # P0 is written in place
        # two variants: the root, a2_0 on top, and P1 -> P2 with r2_0 on top
        assert sorted(key for key in shared.defs if len(key) == 3) == [
            ((K("P1->P2"), "term"), None, "r2_0"), ((K("P2"), "term"), None, "a2_0")]

    def test_the_cli_prints_it(self, fresh):
        out = run(fresh, HERE / "lesson14_sharing.fspy")
        assert "Debate about 'a2_0' (P2[t]), 2 sub-debate(s) cited by name:" in out
        assert f"  {ROOT14}\n  {DEF14}\n  {DEF14_RULE1}\n" in out

    def test_expansion_is_unfolding(self, fresh):
        doc, issue, *_, shared = load(fresh, "lesson14_sharing.fspy", "P2", "a2_0")
        term = unfold_argument(doc, argument_edge(doc, "a2_0"))
        assert shape(shared.expand()) == shape(term)
        assert size(term) == 181 and len(shared.defs) == 6
        assert sum(size(d.body) for d in shared.defs.values()) == 110
        canonical = fresh.shared_debate(issue)                         # the issue's own: the table
        assert sum(size(d.body) for d in canonical.defs.values()) == 93

    def test_labels_and_normal_form(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson14_sharing.fspy", "P2", "a2_0")
        nf, cls, sigma, graph = evaluate_shared(shared, "a2_0", strict_names=names, strict_kinds=kinds)
        assert labels(graph) == {"P1->P2[t]": "IN", "P0->P1[t]": "IN", "P1[t]": "IN", "P0[t]": "IN", "P2[t]": "IN"}
        assert (pres_str(nf), cls) == (NF14, "value")

    def test_a1_0_names_nothing(self, fresh):                          # exercise 1
        doc, *_, shared = load(fresh, "lesson14_sharing.fspy", "P1", "a1_0")
        assert shared.named() == {} and len(shared.to_text().splitlines()) == 1
        assert sorted(key for key in shared.defs if len(key) == 3) == [
            ((K("P0->P1"), "term"), None, "r1_0"), ((K("P0"), "term"), False, None),
            ((K("P1"), "term"), None, "a1_0")]


# -- Lesson 15 ----------------------------------------------------------------------

BRACKETS15 = ["[rule_2 -> P:!, b1 -> ?:P, b14 -> ?:Q->false, h_5 -> ?:Q]",
              "[rule_2 -> P:!, b1 -> ?:P, b15 -> ?:Q->false, h_6 -> ?:Q]",
              "[rule -> P:!, b2 -> ?:P, b10 -> ?:Q->false, h_4 -> ?:Q]",
              "[rule -> P:!, b2 -> ?:P, b11 -> ?:Q->false, h_3 -> ?:Q]"]


class TestLesson15Captures:
    def test_the_debate_as_printed(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson15_capture.fspy", "P", "p1")
        assert shared.to_text().splitlines() == [ROOT15, DEF15]
        assert DEF15.startswith(DEF15_HEAD)
        assert shared.named() == {(K("Q"), "context"): "anon_1"}
        # under p1 the rule's and the lambda's debates are variants
        assert sorted(key for key in shared.defs if len(key) == 3) == [
            ((K("(Q->false)->P"), "term"), None, "pRule1"), ((K("Q->false"), "term"), None, "p1.λ1"),
            ((K("P"), "term"), None, "p1")]

    def test_what_the_citations_record(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson15_capture.fspy", "P", "p1")
        root, _rows = shared.presented()
        cites = [leaf for leaf in _leaves(root) if is_cite(leaf) and leaf.debate == (K("Q"), "context")]
        assert [c.bracket for c in cites] == BRACKETS15
        for c in cites:
            assert set(c.captures) == {(K("P"), "context"), (K("P"), "term"), (K("Q->false"), "term"),
                                       (K("Q"), "term")}

    def test_the_definition_is_larger_than_the_term(self, fresh):
        doc, issue, *_, shared = load(fresh, "lesson15_capture.fspy", "P", "p1")
        term = unfold_argument(doc, argument_edge(doc, "p1"))
        assert shape(shared.expand()) == shape(term)
        assert size(term) == 213
        assert sum(size(d.body) for d in shared.defs.values()) == 180

    def test_the_even_loop_labels(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson15_capture.fspy", "P", "p1")
        _nf, _cls, _sigma, graph = evaluate_shared(shared, "p1", strict_names=names, strict_kinds=kinds)
        found = labels(graph)
        assert len(found) == 8 and {v for k, v in found.items() if "->P" not in k and "->Q" not in k} == {"UNDEC"}
        assert found["(Q->false)->P[t]"] == "IN" and found["(P->false)->Q[t]"] == "IN"     # the axioms

    def test_q_names_the_rule_and_the_lambda(self, fresh):             # exercise 1
        doc, *_, shared = load(fresh, "lesson15_capture.fspy", "Q", "q")
        assert shared.named() == {(K("(Q->false)->P"), "term"): "anon_2", (K("Q->false"), "term"): "anon_3"}

    def test_without_p2_nothing_is_named(self, fresh, tmp_path):        # exercise 2
        text = (HERE / "lesson15_capture.fspy").read_text()
        p2 = text[text.index("argument p2"):text.index("argument q")]
        script = tmp_path / "without_p2.fspy"
        script.write_text(text.replace(p2, ""))
        run(fresh, script)
        shared = fresh.shared_debate((K("P"), "term"), fresh.get_argument("p1"))
        assert shared.named() == {} and len(shared.to_text().splitlines()) == 1


# -- Lesson 16 ----------------------------------------------------------------------

class TestLesson16TypeCheck:
    def test_the_skeleton_and_the_replay_count(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson14_sharing.fspy", "P2", "a2_0")
        assert pres_str(shared.skeleton(shared.issue)) == SKELETON14_P2
        # a skeleton numbers its sites from u1 again
        assert pres_str(shared.skeleton((K("P1"), "term"))) == SKELETON14_P1
        checked = {}
        # P2's root, P1, P0 -> P1, and P1 -> P2 twice (its variant and its own)
        assert typecheck_shared(fresh, shared, "a2_0", checked) == 5
        assert typecheck_shared(fresh, shared, "a2_0", checked) == 0
        # exercise 1: a1_0's debate adds the variants of P0 -> P1 and of P0
        a1_0 = fresh.get_argument("a1_0")
        assert typecheck_shared(fresh, fresh.shared_debate(fresh.issue_of(a1_0), a1_0), "a1_0", checked) == 2

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
    def test_the_debate_names_three(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson17_instances.fspy", "P2", "a2_0")
        lines = shared.to_text().splitlines()
        assert lines[0] == ROOT14
        assert lines[1] == DEF14.replace("!u5:P0*th_3:P1", "a0:P0*th_3:P1").replace(
            "!u6:P0*th_4:P1", "a0:P0*th_4:P1")
        assert lines[2] == DEF14_RULE1.replace("u7", "u5")
        assert lines[3] == DEF17_A0
        assert shared.named() == {(K("P1"), "term"): "anon_1", (K("P0->P1"), "term"): "anon_2",
                                  (K("P0"), "term"): "a0"}

    def test_eight_decisions_over_six_instances(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson17_instances.fspy", "P2", "a2_0")
        resolver = IssueResolver(shared, names, kinds)
        graph = resolver.compile("a2_0")
        # the axioms are supporters in their implications' debates
        assert [(e.name, e.role, e.strict) for e in graph.edges if e.role != "subargument"] == [
            ("r2_0", "supporter", True), ("r2_1", "supporter", True),
            ("r1_0", "supporter", True), ("r1_1", "supporter", True),
            ("k", "attacker", True), ("a1_0", "supporter", False), ("a1_1", "supporter", False),
            ("a2_1", "supporter", False), ("a2_0", "supporter", False)]
        assert sorted(e.name for e in graph.edges if e.role == "subargument") == ["np0"]
        assert resolver.strict_edges() == []
        assert len(resolver.instances) == 6
        assert sorted(what for _, what in resolver.decisions()) == (
            ["attacker strict: defeat"] + ["supporter strict"] * 7)
        p1 = resolver.resolve(resolver.instance((K("P1"), "term"), (), ()))
        # the stubs for P0 carry its open site and its decision flag
        assert "a0*th_3:P1" in pres_str(p1.resolved) and p1.open and p1.decision
        p0 = resolver.resolve(resolver.instance((K("P0"), "term"), (), ()))
        assert p0.open and p0.decision and not p0.trace == []
        # the unfolded term holds the attack four times and decides it four times
        trace = []
        strict_resolve(unfold_argument(doc, argument_edge(doc, "a2_0")), names, trace=trace)
        assert [what for _, what in trace].count("attacker strict: defeat") == 4

    def test_labels_and_evaluation(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson17_instances.fspy", "P2", "a2_0")
        nf, cls, sigma, graph = evaluate_shared(shared, "a2_0", strict_names=names, strict_kinds=kinds)
        assert labels(graph, "preferred") == {"P1->P2[t]": "IN", "P0->P1[t]": "IN", "~P0[t]": "IN",
                                              "P0[c]": "IN", "P1[t]": "OUT", "P0[t]": "OUT", "P2[t]": "OUT"}
        assert (pres_str(nf), cls) == ("?OUT:P2", "open")

    def test_lesson15_has_six_instances(self, fresh):                  # exercises 1 and 2
        doc, issue, names, kinds, shared = load(fresh, "lesson15_capture.fspy", "P", "p1")
        resolver = IssueResolver(shared, names, kinds)
        resolver.compile("p1")
        resolver.strict_edges()
        assert len(resolver.instances) == 6
        q_fails = [i for i in resolver.instances.values() if i.statement == (K("Q"), "context")]
        assert len(q_fails) == 1                                   # four citations, one instance
        assert q_fails[0].captured == {(K("P"), "context"), (K("Q"), "term"), (K("P"), "term"),
                                       (K("Q->false"), "term")}
        assert q_fails[0].cut == {(K("P"), "term"), (K("Q->false"), "term")}
        # the rule's debate four times (two debates, two decisions each), then
        # q's stack and q's attack on the presumption "Q fails"
        assert [(doc.nodes[k], s, what) for (k, s), what in resolver.decisions()] == [
            ("(Q->false)->P", "term", "supporter strict")] * 4 + [
            ("Q", "term", "supporter strict"), ("Q", "term", "attacker strict: defeat")]


# -- Lesson 18 ----------------------------------------------------------------------

class TestLesson18Evaluation:
    def test_both_routes_agree(self, fresh):
        doc, issue, names, kinds, shared = load(fresh, "lesson14_sharing.fspy", "P2", "a2_0")
        nf_shared, cls_s, *_ = evaluate_shared(shared, "a2_0", strict_names=names, strict_kinds=kinds)
        nf_unfolded, cls_u, *_ = evaluate_debate(unfold_argument(doc, argument_edge(doc, "a2_0")), "a2_0",
                                                 strict_names=names, strict_kinds=kinds)
        assert (pres_str(nf_shared), pres_str(nf_unfolded)) == (NF14, NF14_UNFOLDED)
        assert shape(nf_shared) == shape(nf_unfolded) and cls_s == cls_u == "value"

    def test_six_strict_and_four_sigma_decisions(self, fresh):         # exercise 1
        doc, issue, names, kinds, shared = load(fresh, "lesson14_sharing.fspy", "P2", "a2_0")
        resolver = IssueResolver(shared, names, kinds)
        resolver.compile("a2_0")
        assert len(resolver.decisions()) == 6 and len(resolver.instances) == 6
        trace = []
        strict_resolve(unfold_argument(doc, argument_edge(doc, "a2_0")), names, trace=trace)
        assert len(trace) == 12

    @pytest.mark.perf
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
        # the shared route is the default again since it builds the stacked
        # shape (tasks.org, aida-shared-route-stack-shape)
        assert not fresh.pipeline_unfolded
        fresh.pipeline_unfolded = True
        out = run(fresh, HERE / "lesson14_sharing.fspy")
        assert "Debate about 'a2_0'" in out                        # debate always shows the shared form
        fresh.pipeline_unfolded = False
