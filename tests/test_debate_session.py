"""A debate's own session (wrap/debate_session.py; tasks.org,
aida-debate-sessions): a second Fellowship holding the declarations a
debate needs and its arguments, which every service operation works on
as on any session."""

import warnings

import pytest

from core.ac.signature import Declaration
from core.comp.oracle_terms import alpha_equal
from core.dc.cite import cited_names
from core.dc.debate import scope_names
from wrap.cli import execute_script, setup_prover
from wrap.debate_session import (
    DebateSession, DebateSessionError, DebateSpec, declarations_needed, scope_spec,
)
from wrap.interpreter import Interpreter
from wrap.service import Service


@pytest.fixture
def prover():
    p = setup_prover()
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        execute_script(p, "tests/debates.fspy", strict=True, isolate=False)
    yield p
    p.close()


def run(prover, *lines):
    interpreter = Interpreter(prover)
    for line in lines:
        outcomes = list(interpreter.run(line))
        assert all(o.status in ("ok", "comment") for o in outcomes), (line, [o.message for o in outcomes])


def scope_of(prover, debate):
    if debate.scope == "open":
        return [n for n, a in prover.arguments.items()
                if not getattr(a, "composed", False) and prover.names.get(n) != "statement"]

    def cited(name):
        body = getattr(prover.arguments.get(name), "body", None)
        return [n for n in cited_names(body) if n in prover.arguments] if body is not None else []
    return scope_names(debate, cited)


def in_session(prover, name):
    """The recorded debate ``name`` rebuilt in a session of its own: its
    scope's arguments, and the same moves over them."""
    debate = prover.debates[name]
    debate_session = DebateSession(prover, scope_spec(prover, scope_of(prover, debate)), name=name)
    service = Service.of(debate_session.session)
    service.start_debate(name, debate.issue_prop, debate.onus, "open")
    for move in debate.moves:
        service.move(move.argument, move.verb, move.target)
    service.close_debate()
    return debate_session, service


def outcome(service, name):
    graph = service.graph(name, labels=True)
    evaluation = service.evaluate(name)
    edges = sorted((e.name, e.target_key, e.target_side, e.strict,
                    tuple(sorted((s.key, s.side, s.kind) for s in e.sources)))
                   for e in graph.graph.edges)
    return edges, graph.labels, evaluation.nf_class, evaluation.normal_form


def assert_same(before, after):
    assert before[0] == after[0]                  # the graph
    assert before[1] == after[1]                  # its grounded labels
    assert before[2] == after[2]                  # the class of the normal form
    assert alpha_equal(before[3], after[3])       # the normal form


class TestSpec:
    def test_only_the_declarations_the_scope_needs(self, prover):
        spec = scope_spec(prover, ["tweety", "penguin"])
        assert spec.declarations == [
            "declare Bird : bool.", "declare Flies : bool.", "declare Penguin : bool.",
            "declare bird : (Bird).", "declare penguins_dont_fly : (Penguin -> ~Flies).",
        ]
        assert [a.name for a in spec.arguments] == ["tweety", "penguin"]

    def test_declarations_are_closed_over_the_sorts_they_mention(self):
        declarations = {
            "iota": Declaration("type", "sort"),
            "Bird": Declaration("iota -> bool", "sort"),
            "Tweety": Declaration("iota", "sort"),
            "Unused": Declaration("bool", "sort"),
            "b": Declaration("Bird Tweety", "prop"),
            "nb": Declaration("Bird Tweety", "moxia"),
        }
        assert declarations_needed(declarations, {"b", "nb"}) == [
            "declare iota : type.", "declare Bird : iota -> bool.", "declare Tweety : iota.",
            "declare b : (Bird[Tweety]).", "deny nb : (Bird[Tweety]).",
        ]

    def test_a_first_order_proposition_is_sent_as_fellowship_reads_it(self):
        assert declarations_needed({"b": Declaration("Bird Tweety", "prop"),
                                    "nb": Declaration("Bird Tweety", "moxia")}, {"b", "nb"}) == [
            "declare b : (Bird[Tweety]).", "deny nb : (Bird[Tweety]).",
        ]

    def test_a_statement_is_refused(self, prover):
        run(prover, "theorem st : (Sings).")
        with pytest.raises(DebateSessionError, match="statement"):
            scope_spec(prover, ["st"])


class TestSameResults:
    """A debate rebuilt in its own session gives the graph, labels and
    normal form the document gives it (every finished debate of the
    fixtures agreed, 2026-10-29; a sample is kept here)."""

    def test_a_closed_debate(self, prover):
        run(prover, "debate pro closed d : Flies.", "tweety.", "rebut penguin tweety.", "cedat tempus.")
        before = outcome(Service.of(prover), "d")
        debate_session, service = in_session(prover, "d")
        assert_same(before, outcome(service, "d"))
        assert set(debate_session.session.arguments) == {"tweety", "penguin"}

    @pytest.mark.parametrize("fixture, name", [
        ("tests/peirces_law.fspy", "d3"),
        ("tests/counterarguments_and_undercut.fspy", "u2"),
        ("tests/rationality/cyclic_undercut.fspy", "d"),
        ("tests/rationality/two_witnesses.fspy", "d"),
        ("tests/simple_tautology.fspy", "d1"),
    ])
    def test_fixture_debates(self, fixture, name):
        p = setup_prover()
        try:
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                execute_script(p, fixture, strict=False, isolate=False)
            before = outcome(Service.of(p), name)
            _, service = in_session(p, name)
            assert_same(before, outcome(service, name))
        finally:
            p.close()


class TestRegisteredByTerm:
    """The task's VERIFY: the debates over tests/debates.fspy, registered
    again by their printed term, agree with today's route - graph, labels,
    normal form - and their arguments unfold to that term again."""

    @pytest.mark.parametrize("lines", [
        ["debate pro open o : Flies.", "tweety.", "cedat tempus."],
        ["debate pro closed o : Flies.", "tweety.", "rebut penguin tweety.", "cedat tempus."],
        ["debate pro closed o : Flies.", "tweety.", "rebut penguin tweety.",
         "undermine photo penguin.", "cedat tempus."],
    ])
    def test_the_same_debate(self, prover, lines):
        from copy import deepcopy
        from pres.gen import pres_str
        run(prover, *lines)
        term = pres_str(deepcopy(prover.debate_term(prover.debates["o"])))
        run(prover, f'register debate r : Flies.\nterm "{term}".\ncedat tempus.')
        registered = prover.debates["r"]
        assert registered.refused == {} and registered.moves      # every argument taken
        before, after = outcome(Service.of(prover), "o"), outcome(Service.of(prover), "r")
        assert [e[1:] for e in before[0]] == [e[1:] for e in after[0]]   # edges, up to names
        assert before[1] == after[1]
        assert before[2] == after[2] and alpha_equal(before[3], after[3])
        assert registered.roundtrip.literal


class TestIsolation:
    def test_the_document_does_not_reach_the_session(self, prover):
        run(prover, "debate pro open o : Flies.", "tweety.", "cedat tempus.")
        debate_session, service = in_session(prover, "o")
        assert service.evaluate("o").nf_class == "value"
        run(prover, "declare seen_penguin : (Penguin).", "argument pen : (Penguin).",
            "axiom seen_penguin.", "dixi.")
        # the open debate of the document hears pen: Tweety is a penguin
        assert Service.of(prover).evaluate("o").nf_class == "exception"
        # its copy in the session does not, nor its declaration
        assert service.evaluate("o").nf_class == "value"
        assert "seen_penguin" not in debate_session.session.declarations
        assert "pen" not in debate_session.session.arguments

    def test_the_session_does_not_reach_the_document(self, prover):
        debate_session = DebateSession(prover, scope_spec(prover, ["tweety"]), name="d")
        Service.of(debate_session.session).command("declare only_here : (Bird).")
        assert "only_here" not in prover.declarations


class TestLifecycle:
    def test_close_stops_its_fellowship_and_the_spec_starts_it_again(self, prover):
        debate_session = DebateSession(prover, scope_spec(prover, ["tweety"]), name="d")
        assert not debate_session.alive                  # started on first use
        process = debate_session.session.prover.proc
        assert process.poll() is None
        assert debate_session in prover.doc.debate_sessions
        debate_session.close()
        assert process.poll() is not None
        assert not debate_session.alive and debate_session not in prover.doc.debate_sessions
        assert "tweety" in debate_session.session.arguments  # replayed in a new Fellowship
        assert debate_session.session.prover.proc is not process

    def test_a_new_document_closes_the_sessions_of_the_old_one(self, prover):
        debate_session = DebateSession(prover, scope_spec(prover, ["tweety"]), name="d")
        process = debate_session.session.prover.proc
        Service.of(prover).new_document()
        assert process.poll() is not None and not debate_session.alive
        with pytest.raises(DebateSessionError, match="document is gone"):
            debate_session.session

    def test_closing_the_owner_closes_them(self):
        p = setup_prover()
        p.send_top_level("declare A : bool.")
        debate_session = DebateSession(p, DebateSpec(["declare A : bool."]), name="d")
        process = debate_session.session.prover.proc
        p.close()
        assert process.poll() is not None

    def test_a_refused_declaration_is_named_and_leaves_nothing_running(self, prover):
        debate_session = DebateSession(prover, DebateSpec(["declare x : (Nowhere)."]), name="d")
        with pytest.raises(DebateSessionError, match=r"declare x : \(Nowhere\)\."):
            debate_session.session
        assert not debate_session.alive and prover.doc.debate_sessions == []

    def test_the_session_is_classical_with_the_owners_settings(self, prover):
        prover.subarguments = "share"
        child = DebateSession(prover, DebateSpec(), name="d").session
        assert child.doc.logic == "lk" and not child.doc.minimal
        assert child.subarguments == "share"
