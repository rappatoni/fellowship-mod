"""The deterministic proof-term parser.

Replaces a Lark/Earley grammar.  Earley resolved ambiguity by silently picking
a parse, and the syntax is genuinely ambiguous read as one context-free
grammar -- the old grammar found three parses for `λx:A.y*z`.  Fellowship's
syntax is really two mutually recursive grammars, terms and contexts, so
parsing with the target category in hand removes most of the ambiguity rather
than arbitrating it.
"""

import pytest

from core.ac.ast import (
    Admal,
    Cons,
    ConsFO,
    DI,
    Deleg,
    DestructTermsPairFO,
    Geled,
    Goal,
    ID,
    Laog,
    Lamda,
    Mu,
    Mutilde,
    Sonc,
    TermsPairFO,
)
from core.ac.prop import TApp, TSym
from core.ac.proof_parser import ProofTermSyntaxError, parse_proof_term


# ---------------------------------------------------------------------------
#  Slots decide categories
# ---------------------------------------------------------------------------


def test_bare_name_is_a_term_in_a_term_slot():
    node = parse_proof_term("μt:A.<a||b>")
    assert isinstance(node.term, DI) and node.term.name == "a"


def test_bare_name_is_a_context_in_a_context_slot():
    node = parse_proof_term("μt:A.<a||b>")
    assert isinstance(node.context, ID) and node.context.name == "b"


def test_star_is_a_cons_in_a_context_slot():
    node = parse_proof_term("μt:A.<a||x*y>")
    star = node.context
    assert isinstance(star, Cons)
    assert isinstance(star.term, DI) and isinstance(star.context, ID)


def test_star_is_a_sonc_in_a_term_slot():
    node = parse_proof_term("μt:A.<x*y||b>")
    star = node.term
    assert isinstance(star, Sonc)
    assert isinstance(star.context, ID) and isinstance(star.term, DI)


def test_star_chains_nest_to_the_right():
    node = parse_proof_term("μt:A.<a||x*y*z>")
    assert isinstance(node.context, Cons)
    assert isinstance(node.context.context, Cons)


def test_mutilde_opens_a_context_at_the_root():
    assert isinstance(parse_proof_term("μ'r:A.<a||r>"), Mutilde)


def test_mu_opens_a_term_at_the_root():
    assert isinstance(parse_proof_term("μt:A.<a||t>"), Mu)


# ---------------------------------------------------------------------------
#  Placeholders
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "text,slot,expected",
    [
        ("μt:A.<?1||t>", "term", Goal),
        ("μt:A.<!1||t>", "term", Deleg),
        ("μt:A.<a||1?>", "context", Laog),
        ("μt:A.<a||1!>", "context", Geled),
    ],
)
def test_marker_position_encodes_the_category(text, slot, expected):
    """Leading marker for terms, trailing for contexts -- see commit b3ac7ab."""
    assert isinstance(getattr(parse_proof_term(text), slot), expected)


def test_dotted_placeholder_numbers():
    assert parse_proof_term("μt:A.<?1.2.1||t>").term.number == "1.2.1"


def test_typed_placeholders_keep_their_proposition():
    node = parse_proof_term("μthesis:A.<?1:A||1:A?>")
    assert node.term.prop == "A"
    assert node.context.prop == "A"


# ---------------------------------------------------------------------------
#  Annotations
# ---------------------------------------------------------------------------


def test_annotation_may_contain_parentheses():
    """Parentheses are consumed, then dropped where the precedence makes them
    redundant: subtraction is left associative, so (true-A)-B is true-A-B."""
    node = parse_proof_term("μt:(true-A)-B.<a||t>")
    assert node.prop == "true-A-B"


def test_annotation_keeps_parentheses_that_carry_meaning():
    node = parse_proof_term("μt:true-(A-B).<a||t>")
    assert node.prop == "true-(A-B)"


def test_annotation_stops_at_the_command_dot_not_inside_an_arrow():
    """`>` ends a command but also ends `->`; only the first terminates."""
    node = parse_proof_term("μt:(exists y:N,P y)->A.<a||t>")
    assert node.prop == "(exists y:N,P y)->A"


def test_quantifier_commas_stay_inside_the_annotation():
    node = parse_proof_term("μt:forall n,m:N,Eq n m.<a||t>")
    assert node.prop == "forall n,m:N,Eq n m"


def test_malformed_annotation_fails_at_the_parse():
    with pytest.raises(ProofTermSyntaxError):
        parse_proof_term("μt:A->.<a||t>")


# ---------------------------------------------------------------------------
#  First-order constructs
# ---------------------------------------------------------------------------


def test_existential_elimination_binds_a_sorted_variable():
    node = parse_proof_term("μt:A.<a||(y:N).t>")
    destruct = node.context
    assert isinstance(destruct, DestructTermsPairFO)
    assert destruct.var == "y"
    assert str(destruct.sort) == "N"


def test_existential_introduction_carries_its_witness():
    node = parse_proof_term("μt:A.<(y,a)||t>")
    pair = node.term
    assert isinstance(pair, TermsPairFO)
    assert pair.witness == TSym("y")


def test_compound_witness_is_unambiguously_first_order():
    """A proof term is never juxtaposed, so `S (S O)*c` needs no resolution."""
    node = parse_proof_term("μt:A.<a||S (S O)*t>")
    assert isinstance(node.context, ConsFO)
    assert node.context.fo_term == TApp(TSym("S"), TApp(TSym("S"), TSym("O")))


def test_a_bare_head_stays_propositional_until_resolved():
    """`O*t` is indistinguishable from a proof application without a table."""
    assert isinstance(parse_proof_term("μt:A.<a||O*t>").context, Cons)


def test_first_order_term_may_not_stand_alone():
    with pytest.raises(ProofTermSyntaxError):
        parse_proof_term("μt:A.<a||S O>")


# ---------------------------------------------------------------------------
#  The lambda/star ambiguity
# ---------------------------------------------------------------------------


def test_lambda_before_a_star_binds_tightly():
    """Only this reading types: the Cons has the arrow type the slot needs."""
    node = parse_proof_term("μ'rule:Q.<q||λh:R.μa:F.<h||1.2:R!>*rule>")
    assert isinstance(node.context, Cons)
    assert isinstance(node.context.term, Lamda)
    assert isinstance(node.context.context, ID)


def test_lambda_extends_maximally_when_no_star_follows():
    node = parse_proof_term("μt:A.<a||λh:R.b>")
    assert isinstance(node.context, Admal)
    assert isinstance(node.context.context, ID)


def test_a_lambda_binds_tighter_than_a_star():
    node = parse_proof_term("μt:A.<a||λh:R.b*c>")
    assert isinstance(node.context, Cons)
    assert isinstance(node.context.term, Lamda)


def test_parentheses_give_the_other_reading():
    """The shape the printer previously could not express at all.

    Two `elim`s on a subtraction produce it, and it used to be read as
    `Cons(Lamda(h,?1), 2?)` -- a silent mis-parse.
    """
    node = parse_proof_term("μt:A.<a||λh:R.(b*c)>")
    assert isinstance(node.context, Admal)
    assert isinstance(node.context.context, Cons)


def test_the_two_readings_are_distinguishable():
    tight = parse_proof_term("μt:A.<a||λh:R.b*c>")
    grouped = parse_proof_term("μt:A.<a||λh:R.(b*c)>")
    assert type(tight.context) is not type(grouped.context)


def test_a_group_may_stand_alone():
    node = parse_proof_term("μt:A.<a||(b*c)>")
    assert isinstance(node.context, Cons)


def test_grouping_is_told_apart_from_the_two_pair_forms():
    assert isinstance(parse_proof_term("μt:A.<a||(y:N).t>").context, DestructTermsPairFO)
    assert isinstance(parse_proof_term("μt:A.<(y,a)||t>").term, TermsPairFO)
    assert isinstance(parse_proof_term("μt:A.<a||(b*c)>").context, Cons)


def test_lambda_chains_are_read_as_nested_binders():
    node = parse_proof_term("μt:A.<λh:R.λk:S.a||t>")
    assert isinstance(node.term, Lamda)
    assert isinstance(node.term.term, Lamda)


def test_parsing_is_deterministic():
    """The same input always gives the same tree; Earley guaranteed no such thing."""
    text = "μt:A.<λh:R.λk:S.a||λm:P.b*c>"
    first = parse_proof_term(text)
    second = parse_proof_term(text)
    assert type(first.context) is type(second.context)
    assert type(first.term.term) is type(second.term.term)


# ---------------------------------------------------------------------------
#  Errors
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "text",
    [
        "",
        "μt:A.<a||t> trailing",
        "μt:A.<a||>",
        "μt:A.<a|t>",
        "μt.<a||t>",
        "μ't:A.<a||t>*x",   # a mutilde is a context and cannot head a Cons
    ],
)
def test_malformed_proof_terms_raise(text):
    with pytest.raises(ProofTermSyntaxError):
        parse_proof_term(text)


def test_category_violation_is_reported_not_guessed():
    """A mutilde is a context, so it cannot head a Cons; that must not be
    silently re-read as something else."""
    with pytest.raises(ProofTermSyntaxError) as failure:
        parse_proof_term("μt:A.<a||μ'r:A.<a||r>*x>")
    assert "*x" in str(failure.value)      # points at the offending operator


def test_a_term_may_not_stand_where_a_context_is_expected():
    """A goal carries a leading marker, so it is a term wherever it appears."""
    with pytest.raises(ProofTermSyntaxError, match="term.*context"):
        parse_proof_term("μt:A.<a||?1>")
