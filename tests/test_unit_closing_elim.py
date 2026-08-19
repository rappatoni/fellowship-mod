"""Discharging the unit that closes an application chain.

Eliminating a primitive negation consumes the falsum in the same step, so its
⊥ must not get a command of its own; eliminating an arrow into ``false``
leaves the falsum as an open goal, so it needs a closing ``elim``.  ⊤ has no
such exception, there being no primitive co-negation.  Every sequence below
was replayed against the prover when this test was written.
"""
from core.ac.grammar import Grammar, ProofTermTransformer
from core.ac.instructions import (
    InstructionsGenerationVisitor,
    is_primitive_negation_prop,
)
from core.comp.enrich import PropEnrichmentVisitor


DECLARATIONS = {
    "A": "bool", "B": "bool", "C": "bool",
    "rule4": "A->C->false", "neg": "~C",
    "n": "~A", "a": "A", "elur1": "(true-A)-B",
    "P": "bool", "Q": "bool", "R": "bool", "qRule": "(R->false)->Q",
}


def _instructions(term, assumptions=None):
    body = ProofTermTransformer().transform(Grammar().parser.parse(term))
    body = PropEnrichmentVisitor(assumptions=assumptions or {},
                                 axiom_props=DECLARATIONS).visit(body)
    return list(InstructionsGenerationVisitor().return_instructions(body))


def test_arrow_into_falsum_gets_a_closing_elim():
    instructions = _instructions("μ'rule:C.<rule4:A->C->false||!1.2.1:A*rule:C*_F_>")

    assert instructions == [
        "cut (A -> C -> false) rule",
        "axiom rule4",
        "elim",
        "by default",
        "next",
        "elim",
        "axiom rule",
        "elim",
    ]


def test_primitive_negation_falsum_needs_no_closing_elim():
    instructions = _instructions("μ'rule:C.<neg:~C||rule:C*_F_>")

    assert instructions == ["cut (~C) rule", "axiom neg", "elim", "axiom rule"]


def test_primitive_negation_is_distinguished_from_arrow_into_falsum():
    # is_negation_prop accepts both shapes, which is why it cannot decide
    # whether a falsum still needs discharging.
    assert is_primitive_negation_prop("~A")
    assert is_primitive_negation_prop("¬A")
    assert not is_primitive_negation_prop("A->false")
    assert not is_primitive_negation_prop("A->⊥")


def test_truth_closing_a_chain_always_gets_an_elim():
    # Dual of the falsum case, and with no exception: there is no primitive
    # co-negation, so ⊤ is never consumed by the elimination that produced it.
    instructions = _instructions(
        "μtester:A.<μelur:A.<1.1.1:B!*elur:A*_T_||elur1:(true-A)-B>||tester:A>"
    )

    assert instructions == [
        "cut (A) tester",
        # `-` is left associative for the command parser, so the redundant
        # parentheses are no longer emitted; the prover reads `true-A-B` and
        # `(true-A)-B` as the same proposition.
        "cut (true-A-B) elur",
        "elim",
        "by default",
        "next",
        "elim",
        "moxia elur",
        "elim",
        "moxia elur1",
        "moxia tester",
    ]


def test_negation_elimination_wrapper_emits_no_cut_of_its_own():
    # Fellowship encodes context-side ¬-elimination as μ'H:¬A.<H||chain*_F_>.
    # The wrapper is representation, not a cut: this term is what the prover
    # produced for `cut (~A) H1. axiom n. elim. axiom a.`, so generation must
    # reproduce exactly that.
    instructions = _instructions(
        "μthesis:B.<μH1:B.<n||μ'H2:¬A.<H2||a*_F_>>||thesis>"
    )

    assert instructions == ["cut (~A) H1", "axiom n", "elim", "axiom a"]


def test_free_standing_negation_proof_keeps_its_own_commands():
    # A proof of R->false built from a context-side default for R has the same
    # shape as Fellowship's ¬-elim scaffold -- λh:R.μ_:⊥.<h||X> -- but here it
    # is a first-class argument rather than a wrapper the prover collapses, so
    # it must generate its introduction, cut and axiom rather than be skipped.
    instructions = _instructions(
        "μ'rule:Q.<qRule:(R->false)->Q||λh:R.μalpha:⊥.<h:R||1.2.1.1.2:R!>*rule:Q>",
        assumptions={},
    )

    assert instructions == [
        "cut ((R -> false) -> Q) rule",
        "axiom qRule",
        "elim",
        "elim h",
        "cut (R) alpha",
        "axiom h",
        "by default",
        "next",
        "moxia rule",
    ]


def test_term_side_negation_elimination_collapses_to_one_elim():
    # Fellowship answers `elim` on a ~A goal with μH2:¬A.<λH3:A.μH4:⊥.<H3||X>||H2>.
    # That wrapper is its encoding of the elimination, not a proof anyone wrote,
    # so the whole block must come back as the single elim that produced it.
    # Replayed against the prover: reaches the same goal state as the original.
    instructions = _instructions(
        "μthesis:B.<μH1:B.<μH2:¬A.<λH3:A.μH4:⊥.<H3||1.1.1?>||H2>||1.2?>||thesis>",
        assumptions={"1.1.1": {"prop": "A"}, "1.2": {"prop": "~A"}},
    )

    assert instructions == ["cut (~A) H1", "elim H2", "next", "next"]
