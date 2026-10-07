"""Pins every value quoted in minicourse-evaluation.org (lessons 8a-13).

Regenerated 2026-10-06 for the stacked shape and argument entrypoints
(tasks.org, aida-unfold-entrypoints): the term is the one unfolded for
the argument `pro`.  Regenerated 2026-10-07 for capture by the
scaffolds' own binders (aida-unfold-scaffold-binders-capture).

The lessons walk one labelled debate term through the evaluator phase by
phase, and then replay each scaffold decision as single reduction steps
with the normaliser's own rules.  If the implementation changes, these
fail and the lesson text must be updated.
"""

from copy import deepcopy
from pathlib import Path

import pytest

from core.ac.ast import Deleg
from core.comp.adf_label import labellings
from core.comp.labelled import label_term, discharge_labels
from core.comp.evaluate import evaluate_debate, resolve_scaffolds
from core.comp.oracle_terms import alpha_equal, classify_nf, normalize_strong
from core.dc.debate_graph import declaration_kinds
from core.dc.strict import compile_issue, strict_resolve
from pres.gen import pres_str

from coursekit import debate_term, eta_reduce, eta_step, fire, scaffold_at

HERE = Path(__file__).parent

UNFOLDED = (
    "μalt1:B.<?u1:B||μ'b1:B.<μ_:B.<b1:B||alt1:B>||μ'_:B.<μpro:B.<μth:B.<r1:A->B||"
    "μalt2:A.<!u2:A||μ'b2:A.<μ_:A.<b2:A||μ'alt3:A.<μb3:A.<μ_:A.<alt3:A||μ'con:A.<con:A||"
    "μ'x:A.<μth_2:~A.<r2:C->~A||μalt4:C.<!u3:C||μ'b4:C.<μ_:C.<b4:C||μ'alt5:C.<μb5:C.<"
    "μ_:C.<alt5:C||μ'killC:C.<killC:C||μ'x_2:C.<nc:~C||μ'H1:~C.<H1:~C||x_2:C*_F_>>>>||"
    "μ'_:C.<alt5:C||b5:C>>||alt4:C>>||μ'_:C.<b4:C||alt4:C>>>*th_2:~A>||μ'H1_2:~A.<H1_2:~A||"
    "x:A*_F_>>>>||μ'_:A.<alt3:A||b3:A>>||alt2:A>>||μ'_:A.<b2:A||alt2:A>>>*th:B>||pro:B>||"
    "alt1:B>>>"
)
AFTER_STRICT_C = "μalt4:C.<!u3:C||μ'killC:C.<killC:C||μ'x_2:C.<nc:~C||μ'H1:~C.<H1:~C||x_2:C*_F_>>>>"
# sigma resolves the labelled term (core/comp/labelled.py): B[t] and A[t]
# are IN; B[c] has no node, so pro's continuation is unlabelled
RESOLVED = "μpro:B{IN}.<μth:B{IN}.<r1:A->B||!u2:A{IN}*th:B>||pro:B>"
NORMAL_FORM = "μpro:B.<r1:A->B||!IN:A*pro:B>"   # the site named by its label
# lesson 9: sigma written into the term after phase 1
LABELLED = ("μalt1:B{IN}.<?u1:B{IN}||μ'b1:B.<μ_:B{IN}.<b1:B{IN}||alt1:B>||μ'_:B.<μpro:B{IN}.<μth:B{IN}.<r1:A->B||"
            "μalt2:A{IN}.<!u2:A{IN}||μ'b2:{OUT}A.<μ_:A{IN}.<b2:A{IN}||μ'alt3:{OUT}A.<μb3:A{IN}.<μ_:A{IN}.<"
            "alt3:A{IN}||μ'con:{OUT}A.<con:A{IN}||μ'x:{OUT}A.<μth_2:~A.<r2:C->~A||μalt4:C{OUT}.<!u3:C{OUT}||"
            "μ'killC:{IN}C.<killC:C{OUT}||μ'x_2:{IN}C.<nc:~C||μ'H1:~C.<H1:~C||x_2:C{OUT}*_F_>>>>*th_2:~A>||"
            "μ'H1_2:~A.<H1_2:~A||x:A{IN}*_F_>>>>||μ'_:{OUT}A.<alt3:A{IN}||b3:{OUT}A>>||alt2:{OUT}A>>||"
            "μ'_:{OUT}A.<b2:A{IN}||alt2:{OUT}A>>>*th:B>||pro:B>||alt1:B>>>")


@pytest.fixture(scope="module")
def debate():
    prover, term = debate_term(HERE / "lesson9_evaluation.fspy", "pro")
    names = set(prover.declarations.keys())
    kinds = declaration_kinds(prover.declarations)
    yield term, names, kinds
    prover.close()


def _labels(graph):
    return {f"{graph.nodes[k]}[{s[0]}]": v for (k, s), v in
            labellings(graph, "preferred")[0].items()}


CYCLE_UNFOLDED = (
    "μalt1:B.<?u1:B||μ'b1:B.<μ_:B.<b1:B||alt1:B>||μ'_:B.<μpro:B.<μth:B.<r1:A->B||"
    "μalt2:A.<?u2:A||μ'b2:A.<μ_:A.<b2:A||alt2:A>||μ'_:A.<μback:A.<μth_2:A.<r3:B->A||"
    "b1:B*th_2:A>||back:A>||alt2:A>>>*th:B>||pro:B>||alt1:B>>>"
)
SELF_ATTACK_UNFOLDED = (
    "μalt2:P.<μalt1:P.<?u1:P||μ'b1:P.<μ_:P.<b1:P||alt1:P>||μ'_:P.<μSelfAttack:P.<"
    "μrule:P.<pRule:(P->false)->P||λh:P.μalpha:false.<h:P||rule:P>*rule:P>||"
    "SelfAttack:P>||alt1:P>>>||μ'b2:P.<μ_:P.<b2:P||alt2:P>||μ'_:P.<b2:P||alt2:P>>>"
)

def _unfold_trace(prover, name, caplog):
    """The unfolder's DEBUG lines (what `explain` prints under "unfold") and
    the term unfolded for the argument NAME (aida-unfold-entrypoints)."""
    import logging
    from core.dc.unfold import argument_edge, unfold_argument
    caplog.clear()
    with caplog.at_level(logging.DEBUG, logger="core.dc.unfold"):
        term = unfold_argument(prover.document, argument_edge(prover.document, name))
    lines = [r.getMessage().strip() for r in caplog.records
             if r.name == "core.dc.unfold" and r.levelno == logging.DEBUG]
    skip = ("unfold: argument ", "unfold: the debate term")
    return [l for l in lines if l.startswith("unfold: ") and not l.startswith(skip)], term


class TestLesson8aUnfold:
    def test_document_graph(self):
        prover, _ = debate_term(HERE / "lesson9_evaluation.fspy", "pro")
        try:
            g = prover.document
            show = lambda k, s: f"{g.nodes.get(k, k)}[{s[0]}]"
            assert [(e.name, show(e.target_key, e.target_side), e.strict,
                     [(show(s.key, s.side), s.kind) for s in e.sources]) for e in g.edges] == [
                ("pro", "B[t]", False, [("A[t]", "presumption")]),
                ("con", "A[c]", False, [("C[t]", "presumption")]),
                ("killC", "C[c]", True, []),
                ("attackrule", "A->B[c]", False, [("D[t]", "presumption")]),
            ]
            assert {show(k, s): kinds for (k, s), kinds in g.defaults.items()} == {
                "A[t]": {"presumption"}, "C[t]": {"presumption"}, "D[t]": {"presumption"}}
            assert pres_str(g.edges[0].term) == "μpro:B.<μth:B.<r1:A->B||!1.2.1:A*th:B>||pro:B>"
        finally:
            prover.close()

    def test_attackrule_takes_no_part(self):
        # The limitation the fixture's last argument shows: r1 is a declared
        # axiom, a leaf of pro's body and no site, so nothing in pro's debate
        # stands for A->B[t] and the refutation of A->B is never reached.
        prover, term = debate_term(HERE / "lesson9_evaluation.fspy", "pro")
        try:
            assert "attackrule" not in pres_str(term) and "D" not in pres_str(term)
            assert (prover.document.nodes and
                    ("B(S:A,->,S:B)", "term") not in set(prover.document.statements()))
        finally:
            prover.close()

    def test_trace_and_term(self, caplog):
        prover, _ = debate_term(HERE / "lesson9_evaluation.fspy", "pro")
        try:
            lines, term = _unfold_trace(prover, "pro", caplog)
            assert lines == [
                "unfold: B[t] expanded, root u1 (Goal), 1 supporter(s), 'pro' on top",
                "unfold: B[t] +support 'pro' (the argument unfolded, on top)",
                "unfold: A[t] expanded, root u2 (Deleg), 0 supporter(s)",
                "unfold: A[t] attacked by the debate about A[c], 1 supporter(s) (alt alt2, b b2)",
                "unfold: the bare A[c] captured as 'alt2'",
                "unfold: A[c], the attacker wing, root 'alt2'",
                "unfold: A[c] +support 'con'",
                "unfold: C[t] expanded, root u3 (Deleg), 0 supporter(s)",
                "unfold: C[t] attacked by the debate about C[c], 1 supporter(s) (alt alt4, b b4)",
                "unfold: the bare C[c] captured as 'alt4'",
                "unfold: C[c], the attacker wing, root 'alt4'",
                "unfold: C[c] +support 'killC'",
            ]
            assert pres_str(term) == UNFOLDED
        finally:
            prover.close()

    def test_capture_by_b(self, caplog):
        prover, _ = debate_term(HERE / "lesson8a_cycle.fspy", "pro")
        try:
            lines, term = _unfold_trace(prover, "pro", caplog)
            assert lines == [
                "unfold: B[t] expanded, root u1 (Goal), 1 supporter(s), 'pro' on top",
                "unfold: B[t] +support 'pro' (the argument unfolded, on top)",
                "unfold: A[t] expanded, root u2 (Goal), 1 supporter(s)",
                "unfold: A[t] +support 'back'",
                "unfold: B[t] captured as 'b1'",
            ]
            assert pres_str(term) == CYCLE_UNFOLDED
        finally:
            prover.close()

    def test_capture_and_the_contrary_root(self, caplog):
        prover, _ = debate_term(HERE.parent / "rationality" / "self_attack_lk.fspy",
                                "SelfAttack")
        try:
            lines, term = _unfold_trace(prover, "SelfAttack", caplog)
            assert lines == [
                "unfold: P[t] expanded, root u1 (Goal), 1 supporter(s), 'SelfAttack' on top",
                "unfold: P[t] +support 'SelfAttack' (the argument unfolded, on top)",
                "unfold: P[c] captured as 'rule' (presumption)",
                "unfold: P[t] attacked by the debate about P[c], 0 supporter(s) (alt alt2, b b2)",
                "unfold: the bare P[c] captured as 'alt2'",
                "unfold: P[c], the attacker wing, root 'alt2'",
            ]
            assert pres_str(term) == SELF_ATTACK_UNFOLDED
            # the lambda's own edge is in the document but never a scion
            assert [e.name for e in prover.document.edges] == ["SelfAttack.λ1", "SelfAttack"]
        finally:
            prover.close()


class TestLesson9Input:
    def test_unfolded_term(self, debate):
        term, _, _ = debate
        assert pres_str(term) == UNFOLDED

    def test_sigma(self, debate):
        term, names, kinds = debate
        graph = compile_issue(term, "pro", strict_names=names, strict_kinds=kinds)
        assert _labels(graph) == {"C[c]": "IN", "A[c]": "OUT", "C[t]": "OUT",
                                  "B[t]": "IN", "A[t]": "IN"}
        assert [e.name for e in graph.edges if e.name.endswith("*")] == ["killC*"]

    def test_the_attack_on_a_is_read_disjunctively(self, debate):
        from core.dc.debate_graph import _match_scaffold, scion_record
        term, names, _ = debate
        match = _match_scaffold(scaffold_at(deepcopy(term), "u2"), names)
        records = scion_record(match, {}, names)
        assert [(r.name, r.target_side, [(s.side, s.kind) for s in r.sources]) for r in records] == [
            ("x", "context", [("context", "obligation")]),       # A[c]'s root, alt2
            ("con", "context", [("term", "presumption")]),       # con: A[c] <- C[t]
        ]


class TestLesson10Strict:
    def test_two_decisions_in_the_debate_about_c(self, debate):
        term, names, _ = debate
        trace = []
        out, edges = strict_resolve(term, names, trace=trace)
        assert [(s, what) for (_, s), what in trace] == [
            ("context", "supporter strict"), ("context", "attacker strict: defeat")]
        assert [e.name for e in edges] == ["killC*"]
        assert pres_str(scaffold_at(out, "u3")) == AFTER_STRICT_C

    def test_the_decisions_as_steps(self, debate):
        from core.ac.ast import Mutilde, ProofTerm
        term, names, _ = debate
        t = deepcopy(term)

        def wing(node, parent=None, slot=None):
            if isinstance(node, Mutilde) and node.di.name == "alt5":
                return node, parent, slot
            for s in ("term", "context"):
                child = getattr(node, s, None)
                if isinstance(child, ProofTerm):
                    found = wing(child, node, s)
                    if found:
                        return found
            return None

        w, parent, slot = wing(t)
        assert fire(w, "mu<") == "mu<"          # W1: outer pair, b5 := alt4 (only rule)
        assert fire(w, "mu<") == "mu<"          # W2: the decided pair: the supporter K
        setattr(parent, slot, eta_step(w))      # W3: eta removes alt5
        c = scaffold_at(t, "u3")
        assert fire(c, ">mu") == ">mu"          # C1: outer pair, b4 := !u3
        assert fire(c, "mu<") == "mu<"          # C2: the decided pair: defeat
        macro, _ = strict_resolve(term, names)
        assert alpha_equal(c, scaffold_at(macro, "u3"))

    def test_substitution_renames_every_binder(self, debate):
        term, _, _ = debate
        c = scaffold_at(deepcopy(term), "u3")
        fire(c, ">mu")
        assert pres_str(c) == (
            "μalt4:C.<μ_:C.<!u3:C||μ'b1:C.<μb2:C.<μ_:C.<b1:C||μ'b3:C.<b3:C||μ'b6:C.<nc:~C||"
            "μ'b7:~C.<b7:~C||b6:C*_F_>>>>||μ'_:C.<b1:C||b2:C>>||alt4:C>>||μ'_:C.<!u3:C||"
            "alt4:C>>")

    def test_a_scaffold_survives_a_substitution(self, debate):
        # freshen_binders leaves the placeholder _ alone (2026-10-06): the
        # wing CW inside is still recognised after the real C1 step
        from core.ac.ast import Mutilde, ProofTerm
        from core.dc.debate_graph import _match_scaffold
        term, names, _ = debate
        c = scaffold_at(deepcopy(term), "u3")
        fire(c, ">mu")
        found = []

        def walk(n):
            if isinstance(n, Mutilde) and _match_scaffold(n, names):
                found.append(n)
            for s in ("term", "context"):
                child = getattr(n, s, None)
                if isinstance(child, ProofTerm):
                    walk(child)

        walk(c)
        assert len(found) == 1 and _match_scaffold(found[0], names)[0] == "supporter"


class TestLesson11Sigma:
    def test_the_labelled_term(self, debate):
        term, names, kinds = debate
        graph = compile_issue(term, "pro", strict_names=names, strict_kinds=kinds)
        sigma = labellings(graph, "preferred")[0]
        strict_body, _ = strict_resolve(term, names)
        assert pres_str(label_term(strict_body, sigma)) == LABELLED

    def test_two_decisions_top_down(self, debate):
        term, names, kinds = debate
        graph = compile_issue(term, "pro", strict_names=names, strict_kinds=kinds)
        sigma = labellings(graph, "preferred")[0]
        strict_body, _ = strict_resolve(term, names)
        trace = []
        out = resolve_scaffolds(strict_body, sigma, "skeptical", strict_names=names, trace=trace)
        assert [(graph.nodes[k], s, status) for (k, s), status in trace] == [
            ("B", "term", "IN"), ("A", "context", "OUT")]
        assert pres_str(out) == RESOLVED

    def test_the_decisions_as_steps(self, debate):
        term, names, kinds = debate
        t, _ = strict_resolve(term, names)
        assert fire(t, ">mu") == ">mu"          # B1: outer pair, b1 := ?u1
        assert fire(t, ">mu") == ">mu"          # B2: sigma keeps the supporter
        t = eta_step(t)                         # B3: mu alt1.<pro-body||alt1> -> pro-body
        assert t.id.name != "pro"               # renamed by the substitution in B1 (lesson 13)
        a = scaffold_at(t, "u2")
        assert fire(a, ">mu") == ">mu"          # A1: outer pair, b2 := !u2
        assert fire(a, ">mu") == ">mu"          # A2: sigma keeps the original
        assert isinstance(a.term, Deleg) and a.context.name == a.id.name   # mu alt2.<!u2||alt2>
        assert not alpha_equal(t, _parse_resolved(term, names, kinds))   # A3 not yet taken
        assert alpha_equal(eta_reduce(t), eta_reduce(_parse_resolved(term, names, kinds)))

    def test_without_eta_the_normal_forms_differ(self, debate):
        term, names, kinds = debate
        t, _ = strict_resolve(term, names)
        for rule in (">mu", ">mu", "mu<"):      # B1, B2, then mu< instead of eta
            fire(t, rule)
        a = scaffold_at(t, "u2")
        fire(a, ">mu"); fire(a, ">mu")
        nf = normalize_strong(t, "cbn")
        assert pres_str(nf) == "μalt1:B.<r1:A->B||μb1:A.<!u2:A||b1:A>*alt1:B>"
        assert classify_nf(nf) == "value"
        macro, _, sigma, _ = evaluate_debate(term, "pro", strict_names=names, strict_kinds=kinds)
        # the steps by hand skip the labels; label and discharge to compare
        nf = discharge_labels(label_term(nf, sigma))
        assert not alpha_equal(nf, macro)
        assert alpha_equal(eta_reduce(deepcopy(nf)), eta_reduce(deepcopy(macro)))


def _parse_resolved(term, names, kinds):
    graph = compile_issue(term, "pro", strict_names=names, strict_kinds=kinds)
    sigma = labellings(graph, "preferred")[0]
    strict_body, _ = strict_resolve(term, names)
    return resolve_scaffolds(strict_body, sigma, "skeptical", strict_names=names)


class TestLesson12Normalise:
    def test_one_step_to_a_value(self, debate):
        term, names, kinds = debate
        for mode in ("skeptical", "credulous"):
            for base in ("cbn", "cbv"):
                nf, cls, _, _ = evaluate_debate(term, "pro", strict_names=names,
                                                strict_kinds=kinds, mode=mode, base=base)
                assert (pres_str(nf), cls) == (NORMAL_FORM, "value")

    def test_plain_strategies_without_sigma(self, debate):
        term, _, _ = debate
        assert pres_str(normalize_strong(term, "cbn")) == \
            "μalt1:B.<r1:A->B||μb1:A.<!u2:A||b1:A>*alt1:B>"
        cbv = normalize_strong(term, "cbv")
        assert pres_str(cbv) == "μalt1:B.<?u1:B||alt1:B>"
        assert classify_nf(cbv) == "open"


class TestLesson13Capture:
    def test_self_attack_captures_and_renames(self):
        prover, term = debate_term(HERE.parent / "rationality" / "self_attack_lk.fspy",
                                   "SelfAttack")
        try:
            names = set(prover.declarations.keys())
            text = pres_str(term)
            assert "λh:P.μalpha:false.<h:P||rule:P>" in text      # the site became 'rule'
            trace = []
            out, edges = strict_resolve(term, names, trace=trace)
            # the root attack's wing is the captured contrary alone: no
            # scaffold, left to normalisation (aida-unfold-scaffold-binders-capture)
            assert [what for _, what in trace] == ["supporter strict"]
            assert [e.name for e in edges] == ["SelfAttack*"]
            nf, cls, _, _ = evaluate_debate(term, "SelfAttack", strict_names=names,
                                            strict_kinds=declaration_kinds(prover.declarations),
                                            mode="credulous")
            assert (pres_str(nf), cls) == (
                "μalt2:P.<pRule:(P->false)->P||λb1:P.μb2:false.<b1:P||alt2:P>"
                "*alt2:P>", "value")
        finally:
            prover.close()
