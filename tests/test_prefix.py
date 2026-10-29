"""Debate-local names (wrap/prefix.py; tasks.org,
aida-debate-local-arguments): a registered debate's own declarations and
arguments are named DEBATE__name in its session, the document keeps the
prefix for them, and a debate shows its own names without it where that
is unambiguous."""

import warnings

import pytest

from core.ac.grammar import Grammar, ProofTermTransformer
from core.ac.ast import Mu, Mutilde, ID, DI, ConsFO
from core.ac.prop import TSym
from core.comp.oracle_terms import alpha_equal
from wrap.cli import execute_script, setup_prover
from wrap.debate_session import (
    DebateSession, DebateSessionError, declaration_command, local_spec, scope_spec,
)
from wrap.interpreter import Interpreter
from wrap.prefix import (
    display_names, local_name, localise_declarations, parse_declaration, rename_term,
    rename_text, shown,
)
from wrap.prover import NameClash
from wrap.service import AidaError, Service


def term(text):
    return ProofTermTransformer().transform(Grammar().parser.parse(text))


@pytest.fixture
def prover():
    p = setup_prover()
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        execute_script(p, "tests/debates.fspy", strict=True, isolate=False)
    yield p
    p.close()


class TestDeclarations:
    def test_parse(self):
        assert parse_declaration("declare a, b : (A -> B).") == ("declare", ["a", "b"], "(A -> B)")
        assert parse_declaration("deny n:(P).") == ("deny", ["n"], "(P)")
        assert parse_declaration("declare all : (forall x:iota, Bird x).")[2] == "(forall x:iota, Bird x)"
        with pytest.raises(ValueError):
            parse_declaration("lk.")

    def test_everything_in_the_block_is_local_and_renamed_where_mentioned(self):
        commands, mapping = localise_declarations(
            ["declare H_A, B : bool.", "declare iota : type.", "declare Bird : iota -> bool.",
             "declare f : (H_A -> A).", "deny h : (H_A - B)."], "simple")
        assert commands == [
            "declare simple__H_A, simple__B : bool.", "declare simple__iota : type.",
            "declare simple__Bird : simple__iota -> bool.",
            "declare simple__f : (simple__H_A -> A).", "deny simple__h : (simple__H_A - simple__B).",
        ]
        assert set(mapping) == {"H_A", "B", "iota", "Bird", "f", "h"}

    def test_a_quantifier_binding_a_local_name_is_refused(self):
        with pytest.raises(ValueError, match="binds x"):
            localise_declarations(["declare x : iota.", "declare all : (forall x:iota, P x)."], "d")
        assert rename_text("forall y:iota, P x", {"x": "d__x"}) == "forall y:iota, P d__x"


class TestRenameTerm:
    def test_free_leaves_are_renamed(self):
        body = term("μa:A.<f||μx:B.<b||x>*a>")
        renamed = rename_term(body, {"f": "d__f", "b": "d__b"})
        assert alpha_equal(renamed, term("μa:A.<d__f||μx:B.<d__b||x>*a>"))
        assert alpha_equal(body, term("μa:A.<f||μx:B.<b||x>*a>"))     # the input is kept

    def test_a_binder_shadows_a_declaration(self):
        # mu'b binds a hypothesis b: the term-side leaf under it is the bound one
        body = Mutilde(DI("b", "A"), "A", DI("b", "A"), ID("k", "A"))
        assert alpha_equal(rename_term(body, {"b": "d__b"}), body)

    def test_first_order_constants_and_propositions_are_renamed(self):
        # mu k:Bird tweety.< all || tweety * k >, as resolve leaves it
        body = Mu(ID("k", "Bird tweety"), "Bird tweety", DI("all", "forall x:iota, Bird x"),
                  ConsFO(TSym("tweety"), ID("k", "Bird tweety")))
        renamed = rename_term(body, {"tweety": "d__tweety", "Bird": "d__Bird"})
        assert renamed.prop == "d__Bird d__tweety"
        assert renamed.term.prop == "forall x:iota, d__Bird x"
        assert renamed.context.fo_term == TSym("d__tweety")
        assert renamed.context.context.prop == "d__Bird d__tweety"

    def test_only_the_kind_a_binder_binds_is_shadowed(self):
        # mu b binds a continuation b; the term-side leaf b is the declared one
        body = Mu(ID("b", "A"), "A", DI("b", "A"), ID("b", "A"))
        renamed = rename_term(body, {"b": "d__b"})
        assert renamed.term.name == "d__b" and renamed.context.name == "b"


class TestDisplay:
    def test_the_prefix_is_dropped_unless_the_short_name_is_taken(self):
        names = display_names("d", ["d__a1", "d__b", "e__c"], taken=["b", "tweety"])
        assert names == {"d__a1": "a1"}

    def test_whole_identifiers_only(self):
        assert shown("d__b * d__bb || xd__b", {"d__b": "b"}) == "b * d__bb || xd__b"


class TestClaims:
    def test_a_debate_name_cannot_contain_the_separator(self, prover):
        with pytest.raises(AidaError, match="'__'"):
            Service.of(prover).start_debate("a__b", "Flies")

    def test_a_reserved_prefix_refuses_the_documents_names(self, prover):
        prover.reserve_prefix("simple")
        with pytest.raises(AidaError, match="simple__"):
            Service.of(prover).command("declare simple__x : (Bird).")
        with pytest.raises(NameClash):
            prover.claim_name("simple__a1", "argument")
        prover.claim_name("simplex", "argument")               # the prefix, not the word

    def test_a_prefix_already_in_use_cannot_be_reserved(self, prover):
        Service.of(prover).command("declare simple__x : (Bird).")
        with pytest.raises(NameClash, match="simple__x"):
            prover.reserve_prefix("simple")


class TestLocalSpec:
    def test_locals_are_prefixed_after_the_document_declarations_they_need(self, prover):
        spec, (body,), mapping = local_spec(
            prover, "d", ["declare bird : (Bird).", "deny not_flying : (Flies)."],
            [term("μa:Bird.<bird||a>")])
        assert spec.declarations == ["declare Bird : bool.", "declare Flies : bool.",
                                     "declare d__bird : (Bird).", "deny d__not_flying : (Flies)."]
        assert mapping == {"bird": "d__bird", "not_flying": "d__not_flying"}
        assert body.term.name == "d__bird"

    def test_a_local_sort_is_renamed_in_the_terms_propositions(self, prover):
        spec, (body,), mapping = local_spec(
            prover, "d", ["declare H : bool.", "declare f : (H -> Bird)."],
            [term("μa:Bird.<f||μb:H.<?1:H||b>*a>")])
        assert spec.declarations == ["declare Bird : bool.", "declare d__H : bool.",
                                     "declare d__f : (d__H -> Bird)."]
        assert body.term.name == "d__f" and body.context.term.prop == "d__H"
        child = DebateSession(prover, spec, name="d").session
        Service.of(child).register_body("d__a1", "Bird", body)   # Fellowship accepts it
        assert "d__a1" in child.arguments

    def test_a_document_name_and_a_local_one_coexist(self, prover):
        spec, (body,), mapping = local_spec(prover, "d", ["declare bird : (Bird)."],
                                            [term("μa:Bird.<bird||a>")])
        child = DebateSession(prover, spec, name="d").session
        Service.of(child).command(declaration_command("bird", prover.declarations["bird"]))
        Service.of(child).register_body(local_name("d", "a1"), "Bird", body)
        assert {"bird", "d__bird"} <= set(child.declarations)
        assert "d__a1" in child.arguments
        assert "d__bird" not in prover.declarations and "d__a1" not in prover.names
        local = set(mapping.values()) | {"d__a1"}
        names = display_names("d", local, taken=set(child.declarations) | set(child.arguments) - local)
        assert names == {"d__a1": "a1"}                   # d__bird stays: bird is taken


def test_a_local_counterargument_changes_the_debate_and_not_the_document(prover):
    """The task's VERIFY, with the session filled by hand until
    `register debate` fills it (aida-register-debate-command)."""
    lines = ["debate pro closed d : Flies.", "tweety.", "rebut penguin tweety.", "cedat tempus."]
    interpreter = Interpreter(prover)
    for line in lines:
        list(interpreter.run(line))
    service = Service.of(prover)
    document_before = (service.evaluate("d").nf_class,
                       sorted(e.name for e in service.graph("document").graph.edges))
    prover.reserve_prefix("d")

    spec = scope_spec(prover, ["tweety", "penguin"])
    local, (body,), _ = local_spec(prover, "d", ["declare seen_penguin : (Penguin)."],
                                   [term("μa:Penguin.<seen_penguin||a>")])
    spec.declarations += [c for c in local.declarations if c not in spec.declarations]
    child = Service.of(DebateSession(prover, spec, name="d").session)
    child.start_debate("d", "Flies", "pro", "open")
    child.move("tweety"); child.move("penguin", "rebut", "tweety"); child.close_debate()
    before = child.evaluate("d").nf_class
    child.register_body(local_name("d", "seen"), "Penguin", body)
    assert (before, child.evaluate("d").nf_class) == ("open", "exception")

    assert (service.evaluate("d").nf_class,
            sorted(e.name for e in service.graph("document").graph.edges)) == document_before
    with pytest.raises(NameClash):
        prover.claim_name("d__seen", "argument")       # still kept from the document
