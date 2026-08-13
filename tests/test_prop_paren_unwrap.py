"""Unwrapping the parentheses Fellowship puts around a declared type.

Only a pair that wraps the whole proposition may be dropped.  Fellowship
reports `deny elur1: (true - A - B)` as `(true-A)-B`, where the parentheses
are structure rather than a wrapper, so removing them yields a proposition
that no longer parses -- and lands, unparseable, in the enriched proof term.
"""
from core.ac.grammar import Grammar, ProofTermTransformer
from core.comp.enrich import PropEnrichmentVisitor
from pres.gen import ProofTermGenerationVisitor


def _unwrap(prop):
    return PropEnrichmentVisitor()._unwrap_outer_parens(prop)


def test_wrapping_parentheses_are_dropped():
    assert _unwrap("(B -> A)") == "B -> A"
    assert _unwrap("(A -> C -> false)") == "A -> C -> false"
    assert _unwrap("((A))") == "A"


def test_structural_parentheses_are_preserved():
    assert _unwrap("(true-A)-B") == "(true-A)-B"
    assert _unwrap("(A-B)-C") == "(A-B)-C"
    assert _unwrap("(A/\\B)->C") == "(A/\\B)->C"
    assert _unwrap("(A)-(B)") == "(A)-(B)"
    assert _unwrap("~(A)") == "~(A)"


def test_unparenthesised_propositions_are_untouched():
    assert _unwrap("A") == "A"
    assert _unwrap("") == ""


def test_enriched_proof_term_still_parses_for_a_structural_type():
    # Regression: the enriched term used to come out as `elur1:true-A)-B`,
    # so AIDA emitted a proof term it could not read back.
    raw = "μthesis:A.<μelur:A.<1.1.1!*elur*_T_||elur1>||thesis>"
    body = ProofTermTransformer().transform(Grammar().parser.parse(raw))
    body = PropEnrichmentVisitor(
        assumptions={"1.1.1": {"prop": "B"}},
        axiom_props={"A": "bool", "B": "bool", "elur1": "(true-A)-B"},
    ).visit(body)
    enriched = ProofTermGenerationVisitor().visit(body).pres

    assert "elur1:(true-A)-B" in enriched
    Grammar().parser.parse(enriched)  # must round-trip
