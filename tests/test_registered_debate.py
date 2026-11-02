"""A debate registered by its term (tasks.org, aida-register-debate-command):

    register debate NAME [legacy] : ISSUE.
    declare ... / deny ...          % its own: all local, NAME__x
    term "...".
    cedat tempus.

The term is type-checked in the debate's own session and is the debate's
record: graph, label and evaluate answer from it."""

import logging
import warnings

import pytest

from core.comp.adf_label import grounded_labels
from core.dc.debate import RegisteredDebate
from core.dc.debate_graph import canonical_prop as K
from wrap.cli import _shown_graph, execute_script, setup_prover
from wrap.interpreter import Interpreter
from wrap.prover import NameClash
from wrap.service import AidaError, ContentRefused, InvalidRequest, LogicRefused, Refused, Service
from wrap.syntax import Command, SyntaxRefused, parse, parse_units


def block(path, name, *, legacy=True):
    """A scasp-aida import (tests/import/) as a registered debate: the
    program's atoms before the block, everything else - the helper atoms
    H_*, the rule and fact constants, the denials - inside it."""
    units = [u for u in parse_units(open(path, encoding="utf-8").read()) if isinstance(u, Command)]
    register = next(u for u in units if u.kind == "register")
    shared, local = [], []
    for u in units:
        if u.kind != "opaque" or u["word"] not in ("declare", "deny"):
            continue
        names, _, typ = u.text[len(u["word"]):].partition(":")
        names = [n.strip() for n in names.split(",")]
        if u["word"] == "declare" and typ.strip() in ("bool", "type"):
            program = [n for n in names if not n.startswith("H_")]
            helpers = [n for n in names if n.startswith("H_")]
            if program:
                shared.append(f"declare {', '.join(program)} : {typ.strip()}.")
            if helpers:
                local.append(f"declare {', '.join(helpers)} : {typ.strip()}.")
        elif u["word"] == "declare" and not typ.strip().startswith("("):
            shared.append(u.text + ".")                  # a sort, a predicate's type
        else:
            local.append(u.text + ".")
    header = f"register debate {name}{' legacy' if legacy else ''} : {register['conclusion']}."
    term = register["proof_term"].replace('"', '\\"')
    return "\n".join(shared + [header] + local + [f'term "{term}".', "cedat tempus."])


def run(prover, text):
    return list(Interpreter(prover).run(text))


@pytest.fixture
def prover():
    p = setup_prover()
    yield p
    p.close()


@pytest.fixture
def simple(prover):
    outcomes = run(prover, block("tests/import/simple.fspy", "simple"))
    assert all(o.status in ("ok", "comment") for o in outcomes), [o.message for o in outcomes]
    return prover


class TestSyntax:
    def test_the_block_commands(self):
        c = parse("register debate simple legacy : A")
        assert (c.kind, c["name"], c["legacy"], c["issue"]) == ("register_debate", "simple", True, "A")
        assert parse("register debate d : Bird_list[x]")["issue"] == "Bird_list[x]"
        assert parse('term "μa:A.<x||a>"')["proof_term"] == "μa:A.<x||a>"

    def test_an_argument_named_debate_is_still_registered_as_before(self):
        c = parse('register debate : A := "μa:A.<x||a>"')
        assert (c.kind, c["name"]) == ("register", "debate")

    @pytest.mark.parametrize("text", ["register debate d", "term μa:A.<x||a>"])
    def test_malformed(self, text):
        with pytest.raises(SyntaxRefused):
            parse(text)


class TestRegistration:
    def test_simple_is_registered_from_its_term(self, simple):
        debate = simple.debates["simple"]
        assert isinstance(debate, RegisteredDebate)
        assert (debate.onus, debate.scope, debate.finished, debate.legacy) == ("pro", "closed", True, True)
        assert debate.local == {n: f"simple__{n}" for n in
                                ("H_A", "b", "c", "f_H_A_A", "h_H_A_B", "h_H_A_C")}
        child = debate.debate_session.session
        assert {"simple__b", "simple__H_A", "A"} <= set(child.declarations)
        assert not any(n.startswith("simple__") for n in simple.declarations)   # nothing local leaks
        assert "simple__" in simple.doc.local_prefixes
        with pytest.raises(NameClash):
            simple.claim_name("simple__x", "argument")

    def test_graph_label_and_evaluate_answer_from_the_record(self, simple):
        service = Service.of(simple)
        graph = service.graph("simple", labels=True)
        assert graph.graph is simple.debates["simple"].record_graph
        assert graph.graph.is_acyclic() and len(graph.graph.edges) == 8
        assert graph.labels[(K("A"), "term")] == "IN"
        assert graph.labels[(K("B"), "context")] == "OUT"
        labelling, = service.label("simple").labellings
        assert labelling == grounded_labels(graph.graph)
        evaluation = service.evaluate("simple")
        assert evaluation.nf_class == "value"
        assert service.evaluate("simple") is not evaluation            # a result each time,
        assert service.evaluate("simple").normal_form is evaluation.normal_form  # one evaluation
        assert service.term("simple", "evaluated").term is evaluation.normal_form

    def test_the_document_does_not_change_the_record(self, simple):
        evaluation = Service.of(simple).evaluate("simple")
        run(simple, "declare seen : (B).")
        assert Service.of(simple).term("simple", "evaluated").term is evaluation.normal_form
        assert Service.of(simple).graph("simple").graph is simple.debates["simple"].record_graph

    def test_without_legacy_the_old_shapes_stay(self, prover):
        run(prover, block("tests/import/simple.fspy", "simple", legacy=False))
        graph = prover.debates["simple"].record_graph
        assert not graph.is_acyclic()               # B[c] among its own sources (test_shapes.py)

    @pytest.mark.parametrize("fixture, conclusion", [
        ("tests/import/simple_pred.fspy", "Bird_list[List_tweety_clumsy]"),
        ("tests/import/orphans.fspy", "Orphan[Bob]"),
    ])
    def test_the_other_imports(self, prover, fixture, conclusion):
        outcomes = run(prover, block(fixture, "d"))
        assert all(o.status in ("ok", "comment") for o in outcomes), [o.message for o in outcomes]
        graph = Service.of(prover).graph("d", labels=True)
        assert graph.graph.is_acyclic()
        assert graph.labels[prover.debates["d"].issue] == "IN"
        assert Service.of(prover).evaluate("d").nf_class == "value"

    def test_a_refutation_is_a_con_debate(self, prover):
        run(prover, "declare B : bool.")
        outcomes = run(prover, 'register debate nb : B.\ndeny nob : (B).\n'
                               'term "μ\'x:B.<x||nob>".\ncedat tempus.')
        assert outcomes[-1].status == "ok", outcomes[-1].message
        assert prover.debates["nb"].onus == "con"


class TestRefusals:
    def test_an_ill_typed_term_is_refused_and_leaves_nothing(self, prover):
        run(prover, "declare A, B : bool.")
        outcomes = run(prover, 'register debate d : A.\ndeclare b : (B).\n'
                               'term "μa:A.<b||a>".\ncedat tempus.')
        assert outcomes[-1].status == "refused" and "Fellowship refused" in outcomes[-1].message
        assert "d" not in prover.debates and "d" not in prover.names
        assert prover.doc.debate_sessions == [] and prover.doc.local_prefixes == set()

    def test_a_term_for_another_issue_is_refused(self, prover):
        run(prover, "declare A, B : bool.")
        outcomes = run(prover, 'register debate d : A.\ndeclare b : (B).\n'
                               'term "μa:B.<b||a>".\ncedat tempus.')
        assert outcomes[-1].status == "refused" and "not the issue" in outcomes[-1].message

    def test_a_block_without_a_term(self, prover):
        outcomes = run(prover, "declare A : bool.\nregister debate d : A.\ncedat tempus.")
        assert outcomes[-1].status == "refused" and "no term" in outcomes[-1].message

    def test_only_declarations_and_the_term_go_in_the_block(self, prover):
        outcomes = run(prover, "declare A : bool.\nregister debate d : A.\ngraph document.")
        assert outcomes[-1].status == "refused"
        outcomes = run(prover, 'register debate d : A.\nterm "μa:A.<x||a>".\nterm "μa:A.<x||a>".')
        assert outcomes[-1].status == "refused" and "already has its term" in outcomes[-1].message

    def test_term_outside_a_block(self, prover):
        assert run(prover, 'term "μa:A.<x||a>".')[-1].status == "refused"

    def test_names(self, simple):
        with pytest.raises(ContentRefused, match="already"):
            Service.of(simple).register_debate("simple", "A", "μa:A.<x||a>")
        with pytest.raises(ContentRefused, match="'__'"):
            Service.of(simple).register_debate("a__b", "A", "μa:A.<x||a>")

    def test_an_intuitionistic_document_refuses_it(self, prover):
        Service.of(prover).new_document("lj")
        with pytest.raises(LogicRefused):
            Service.of(prover).register_debate("d", "A", "μa:A.<x||a>")

    def test_a_strict_edge_of_a_registered_debate_cannot_be_adopted(self, simple):
        strict = next(e for e in simple.debates["simple"].record_graph.edges if e.strict)
        Service.of(simple).graph("simple")
        simple.doc.strict_edges[strict.name] = ("simple", strict, "B")
        with pytest.raises(Refused, match="registered debate"):
            Service.of(simple).adopt(strict.name, "thm")


class TestDisplay:
    def test_the_debates_names_without_its_prefix(self, simple):
        names = Service.of(simple).display_names("simple")
        assert names["simple__H_A"] == "H_A" and names["simple__b"] == "b"
        assert Service.of(simple).display_names("document") == {}

    def test_a_name_the_document_has_keeps_its_prefix(self, simple):
        run(simple, "declare b : (B).")
        names = Service.of(simple).display_names("simple")
        assert "simple__b" not in names and names["simple__c"] == "c"

    def test_the_cli_prints_them_so(self, simple, caplog):
        graph = _shown_graph(simple.debates["simple"].record_graph,
                             Service.of(simple).display_names("simple"))
        assert {"H_A->A", "H_A"} <= set(graph.nodes.values())
        with caplog.at_level(logging.INFO, logger="fsp.wrapper"), warnings.catch_warnings():
            warnings.simplefilter("ignore")
            from wrap.cli import _render
            for o in run(simple, "graph simple.\nlabel simple.\nrender simple."):
                _render(simple, o)
        printed = caplog.text
        assert "H_A->A[t]" in printed and "simple__" not in printed
