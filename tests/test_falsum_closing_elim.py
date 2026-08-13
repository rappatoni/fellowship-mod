"""Discharging the falsum that closes an application chain.

Eliminating a primitive negation consumes the falsum in the same step, so its
⊥ must not get a command of its own; eliminating an arrow into ``false``
leaves the falsum as an open goal, so it needs a closing ``elim``.  Both
sequences below were replayed against the prover when this test was written.
"""
from core.ac.grammar import Grammar, ProofTermTransformer
from core.ac.instructions import (
    InstructionsGenerationVisitor,
    is_primitive_negation_prop,
)
from core.comp.enrich import PropEnrichmentVisitor


DECLARATIONS = {"A": "bool", "C": "bool", "rule4": "A->C->false", "neg": "~C"}


def _instructions(term):
    body = ProofTermTransformer().transform(Grammar().parser.parse(term))
    body = PropEnrichmentVisitor(assumptions={}, axiom_props=DECLARATIONS).visit(body)
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
