"""Labelled terms (tasks.org, aida-labelled-sites-delegation-rewrite).

After labelling, every statement occurrence of the debate term carries
its label - ``A{L}`` term-sorted, ``{L}A`` context-sorted - and sigma
decides the scaffolds by reading the term.  Before normalisation the
labels are discharged: a site is named by its label and is a delegation
iff it is IN, an obligation otherwise.
"""

import copy
import warnings

import pytest

from core.ac.ast import Mu, Mutilde, Cons, Goal, Laog, Deleg, Geled, ID, DI
import core.comp.evaluate as evaluate_module
from core.comp.evaluate import evaluate_debate, witness_labelling, issue_of
from core.comp.labelled import (
    label_term, discharge_labels, statement_of, contains_label, UnlabelledSite,
)
from core.dc.debate_graph import canonical_prop as K
from core.dc.strict import compile_issue, strict_resolve
from core.dc.unfold import argument_edge, unfold, unfold_argument
from mod import store
from pres.gen import pres_str

from test_scaffold_capture import fresh, run, options, evaluate, LESSON9  # noqa: F401
from test_unfold_entrypoints import FIXTURES

A, P, Q = K("A"), K("P"), K("Q")


def parg(site):
    return Mu(ID("pArg", "P"), "P",
              Mu(ID("rule", "P"), "P", DI("pRule", "Q->P"), Cons(site, ID("rule", "P"))),
              ID("pArg", "P"))


SIGMA = {(P, "term"): "IN", (P, "context"): "OUT", (Q, "term"): "UNDEC", (Q, "context"): "OUT"}


# ---------------------------------------------------------------------------
# label_term
# ---------------------------------------------------------------------------

def test_term_sorted_occurrences_carry_the_term_label():
    out = label_term(parg(Goal("1", "Q")), SIGMA)
    assert out.label == "IN"                              # mu pArg:P, a proof of P
    assert out.term.context.term.label == "UNDEC"         # ?1:Q
    assert pres_str(out) == "μpArg:P{IN}.<μrule:P{IN}.<pRule:Q->P||?1:Q{UNDEC}*rule:{OUT}P>||pArg:{OUT}P>"


def test_context_sorted_occurrences_carry_the_context_label():
    body = Mutilde(DI("x", "Q"), "Q", DI("x", "Q"), Laog("c", "Q"))
    out = label_term(body, SIGMA)
    assert out.label == "OUT" and out.context.label == "OUT"
    assert out.term.label == "UNDEC"                      # x stands for a proof of Q
    assert pres_str(out) == "μ'x:{OUT}Q.<x:Q{UNDEC}||c:{OUT}Q?>"


def test_labelling_is_idempotent_and_leaves_the_input_alone():
    body = parg(Goal("1", "Q"))
    once = label_term(body, SIGMA)
    assert not contains_label(body)
    assert pres_str(label_term(once, SIGMA)) == pres_str(once)


def test_statements_without_a_label_stay_unlabelled():
    out = label_term(parg(Goal("1", "Q")), SIGMA)
    rule = out.term.term                                  # pRule:Q->P, a strict axiom
    assert statement_of(rule) == (K("Q->P"), "term")
    assert not hasattr(rule, "label")
    placeholder = Mu(ID("_", "Q"), "Q", DI("_", "Q"), ID("k", "Q"))
    assert statement_of(placeholder.term) is None         # an affine placeholder


# ---------------------------------------------------------------------------
# discharge_labels
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("site, label, expected", [
    (Goal("u1", "Q"), "IN", "!IN:Q"),
    (Deleg("u1", "Q"), "IN", "!IN:Q"),
    (Goal("u1", "Q"), "OUT", "?OUT:Q"),
    (Deleg("u1", "Q"), "OUT", "?OUT:Q"),
    (Goal("u1", "Q"), "UNDEC", "?UNDEC:Q"),
    (Deleg("u1", "Q"), "UNDEC", "?UNDEC:Q"),
    (Laog("u1", "Q"), "IN", "IN:Q!"),
    (Geled("u1", "Q"), "IN", "IN:Q!"),
    (Laog("u1", "Q"), "OUT", "OUT:Q?"),
    (Geled("u1", "Q"), "OUT", "OUT:Q?"),
    (Laog("u1", "Q"), "UNDEC", "UNDEC:Q?"),
    (Geled("u1", "Q"), "UNDEC", "UNDEC:Q?"),
])
def test_a_site_is_named_by_its_label_and_its_kind_follows_it(site, label, expected):
    side = "term" if isinstance(site, (Goal, Deleg)) else "context"
    out = discharge_labels(label_term(site, {(Q, side): label}))
    assert pres_str(out) == expected and out.prop == "Q"


def test_discharge_drops_every_other_label():
    out = discharge_labels(label_term(parg(Goal("1", "Q")), SIGMA))
    assert not contains_label(out)
    assert pres_str(out) == "μpArg:P.<μrule:P.<pRule:Q->P||?UNDEC:Q*rule:P>||pArg:P>"


def test_an_unlabelled_site_is_refused():
    with pytest.raises(UnlabelledSite):
        discharge_labels(parg(Goal("1", "Q")))


# ---------------------------------------------------------------------------
# sigma reads the term
# ---------------------------------------------------------------------------

def test_the_attackers_conclusion_is_labelled_on_the_captured_alt(fresh):
    # Lesson 9: con's wing root is the attack's alt2, a context variable of
    # type A - it carries A[c]'s label, OUT, which is what sigma reads for
    # the attacker's conclusion.
    run(fresh, LESSON9)
    doc, opts = fresh.document, options(fresh)
    term = unfold_argument(doc, argument_edge(doc, "pro"))
    sigma, _ = witness_labelling(compile_issue(term, "pro", **opts), issue_of(term), "skeptical")
    assert sigma[(A, "context")] == "OUT"
    labelled = pres_str(label_term(strict_resolve(term, opts["strict_names"])[0], sigma))
    assert "μ'_:{OUT}A.<alt3:A{IN}||b3:{OUT}A>>||alt2:{OUT}A>" in labelled
    assert "μalt2:A{IN}.<!u2:A{IN}||" in labelled


def test_labels_read_off_the_term_agree_with_the_labelling(fresh, monkeypatch):
    # The transition check of resolve_scaffolds, over every issue of the
    # fixtures in both modes: every label sigma reads equals the label the
    # labelling gives that statement.
    monkeypatch.setattr(evaluate_module, "CHECK_LABELS", True)
    checked = 0
    for script in FIXTURES:
        store.arguments.clear()
        store.document.clear()
        run(fresh, script)
        doc = fresh.document
        for statement in doc.statements():
            for mode in ("skeptical", "credulous"):
                evaluate(fresh, unfold(doc, statement), mode)
                checked += 1
    assert checked > 100


# ---------------------------------------------------------------------------
# Verdicts
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("script", ["tests/demo/05_even_loop.fspy",
                                    "tests/minicourse/lesson15_capture.fspy"])
@pytest.mark.parametrize("prop", ["P", "Q"])
def test_an_undecided_delegation_is_an_obligation_again(fresh, script, prop):
    # Issue P: - the opponent's onus to refute P, delegated by u1:P!.
    # Skeptically P is UNDEC: the delegation was not established, so it is
    # an obligation again and the debate is open (VALUE before).
    # Credulously the witness has the delegation IN: a value.
    run(fresh, script)
    term = unfold(fresh.document, (K(prop), "context"))
    nf, cls, *_ = evaluate(fresh, term, "skeptical")
    assert (cls, f"UNDEC:{prop}?" in pres_str(nf)) == ("open", True)
    nf, cls, *_ = evaluate(fresh, term, "credulous")
    assert (cls, pres_str(nf)) == ("value", f"IN:{prop}!")
