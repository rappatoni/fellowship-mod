"""M2: canonical proposition identity (propositional-fragment-plan.org)."""

import pytest

from core.ac.prop import Prop, PropError
from core.dc.debate_graph import canonical_prop, display_prop


#: Representative proposition spellings from the fixture corpus: the NAF
#: fixtures, the Peirce examples, the scasp_import ground atoms, and the
#: historical printer-bug pair.
FIXTURE_PROPS = [
    "P",
    "Q->P",
    "(R->false)->Q",
    "((P -> Q)->P)->P",
    "((P->Q)->P)->P",
    "A-(B->C)",
    "(A-B)->C",
    "true",
    "false",
    "Proof Tweety_is_a_bird",
    "Proof_search Prog Tweety_is_a_bird",
]


class TestRoundtrip:
    @pytest.mark.parametrize("text", FIXTURE_PROPS)
    def test_render_is_idempotent(self, text):
        once = str(Prop.parse(text))
        twice = str(Prop.parse(once))
        assert once == twice

    @pytest.mark.parametrize("text", FIXTURE_PROPS)
    def test_canonical_is_stable_under_rendering(self, text):
        assert canonical_prop(text) == canonical_prop(display_prop(text))


class TestIdentity:
    def test_whitespace_insensitive(self):
        assert canonical_prop("((P -> Q)->P)->P") == canonical_prop("((P->Q)->P)->P")

    def test_redundant_parentheses_insensitive(self):
        assert canonical_prop("(P)->Q") == canonical_prop("P->Q")

    def test_negation_unfolds_to_implication(self):
        assert canonical_prop("~A") == canonical_prop("A->false")

    def test_nested_negation(self):
        assert canonical_prop("~~A") == canonical_prop("(A->false)->false")

    def test_negation_of_compound(self):
        assert canonical_prop("~(A->B)") == canonical_prop("(A->B)->false")

    def test_printer_bug_pair_stays_distinct(self):
        # The historical pretty_prop defect printed these two alike; the
        # identity layer must keep them apart.
        assert canonical_prop("A-(B->C)") != canonical_prop("(A-B)->C")

    def test_ground_application_distinct_from_symbol(self):
        assert canonical_prop("Proof Tweety") != canonical_prop("Proof")

    def test_unparsable_raises(self):
        with pytest.raises(PropError):
            canonical_prop("P -> ")


class TestAlphaInvariance:
    def test_bound_variable_names_do_not_matter(self):
        a = canonical_prop("forall x:iota, Q x")
        b = canonical_prop("forall y:iota, Q y")
        assert a == b

    def test_free_symbols_do_matter(self):
        a = canonical_prop("forall x:iota, Q x")
        b = canonical_prop("forall x:iota, R x")
        assert a != b
