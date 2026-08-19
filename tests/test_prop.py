"""The structured proposition AST.

Covers the three things strings could not do -- capture-avoiding substitution,
alpha-equivalence, and precedence-correct rendering -- plus the round trip
against Fellowship's printed syntax.
"""

import pytest

from core.ac.prop import (
    BinOp,
    PApp,
    PBin,
    PFalse,
    PNeg,
    PQuant,
    PSym,
    PTrue,
    Prop,
    PropError,
    Quantifier,
    SArr,
    SProp,
    SSet,
    SSym,
    TApp,
    TSym,
    parse_sort,
    parse_term,
    sort_result,
    term_to_command,
)


# Propositions exactly as Fellowship prints them.  Every one of these was
# observed in a real machine payload or verified by feeding the rendered
# command back through fsp.
PRINTED = [
    "A",
    "true",
    "false",
    "~A",
    "A->B",
    "A->B->C",
    "A-B->C",
    "(A-B)->C",
    "A->B-C",
    "A->(B-C)",
    "A-B-C",
    "A-(B-C)",
    "(A->B)->C",
    "Un O",
    "P x y",
    "Even (S O)",
    "forall n:N,Un n",
    "exists y:N,forall x:N,P x y",
    "forall n,m:N,Eq n m->Eq m n",
    "~(forall x:N,Un x)",
    "B-(forall x:N,Un x)",
    "(forall x:N,Un x)->C",
    "forall n:N,Un n->Even n",
    "forall f:termf->form,truef (f x)",
]

# Spellings Fellowship emitted before its printer learned to parenthesise a
# quantifier in subformula position.  They are rejected rather than guessed at:
# `~forall x:N,Un x` and `B-forall x:N,Un x` each have two readings, and the
# printer now writes the parentheses that tell them apart.
LEGACY_UNPARENTHESISED = [
    "~forall x:N,Un x",
    "B-forall x:N,Un x",
]


@pytest.mark.parametrize("text", PRINTED)
def test_parse_is_the_inverse_of_printing(text):
    assert str(Prop.parse(text)) == text


@pytest.mark.parametrize("text", PRINTED)
def test_parse_is_idempotent_through_a_reparse(text):
    once = Prop.parse(text)
    assert Prop.parse(str(once)) == once


# ---------------------------------------------------------------------------
#  Precedence -- printer convention, not parser convention
# ---------------------------------------------------------------------------


def test_minus_is_looser_than_arrow():
    """Subtraction is the loosest binary connective, per parser.mly and core.ml."""
    assert Prop.parse("A-B->C") == PBin(
        PSym("A"), BinOp.MINUS, PBin(PSym("B"), BinOp.IMP, PSym("C"))
    )


def test_the_other_reading_needs_parentheses():
    assert Prop.parse("(A-B)->C") == PBin(
        PBin(PSym("A"), BinOp.MINUS, PSym("B")), BinOp.IMP, PSym("C")
    )


def test_minus_is_left_associative():
    assert Prop.parse("A-B-C") == PBin(
        PBin(PSym("A"), BinOp.MINUS, PSym("B")), BinOp.MINUS, PSym("C")
    )


@pytest.mark.parametrize(
    "left,right",
    [
        ("A-B->C", "(A-B)->C"),
        ("A->B-C", "A->(B-C)"),
        ("A-B-C", "A-(B-C)"),
        ("A->B->C", "(A->B)->C"),
        ("forall x:N,Un x->C", "(forall x:N,Un x)->C"),
    ],
)
def test_every_grouping_is_distinguishable(left, right):
    """Each pair used to collapse to one string, losing the reading.

    The printer's table disagreed with the parser's, so `A-(B->C)` printed as
    `A-B->C` and read back as `(A-B)->C`.  Both spellings must now survive.
    """
    assert Prop.parse(left) != Prop.parse(right)
    assert str(Prop.parse(left)) != str(Prop.parse(right))


def test_arrow_is_right_associative():
    assert Prop.parse("A->B->C") == PBin(
        PSym("A"), BinOp.IMP, PBin(PSym("B"), BinOp.IMP, PSym("C"))
    )


def test_negation_binds_tighter_than_arrow():
    assert Prop.parse("~A->B") == PBin(PNeg(PSym("A")), BinOp.IMP, PSym("B"))


@pytest.mark.parametrize("text", LEGACY_UNPARENTHESISED)
def test_ambiguous_unparenthesised_quantifiers_are_rejected(text):
    with pytest.raises(PropError):
        Prop.parse(text)


def test_quantifier_scope_is_recoverable_in_both_directions():
    """The reading the printer used to lose.

    `(forall x, Un x) -> C` is an implication whose antecedent is quantified;
    `forall x, (Un x -> C)` is a quantified implication.  Different
    propositions, and they must not render alike.
    """
    outer = Prop.parse("(forall x:N,Un x)->C")
    inner = Prop.parse("forall x:N,Un x->C")

    assert isinstance(outer, PBin) and outer.op is BinOp.IMP
    assert isinstance(inner, PQuant)
    assert outer != inner
    assert str(outer) != str(inner)


def test_quantifier_body_extends_maximally():
    parsed = Prop.parse("forall n:N,Un n->Even n")
    assert isinstance(parsed, PQuant)
    assert parsed.body == PBin(
        PApp(PSym("Un"), TSym("n")), BinOp.IMP, PApp(PSym("Even"), TSym("n"))
    )


def test_application_is_left_nested():
    assert Prop.parse("P x y") == PApp(PApp(PSym("P"), TSym("x")), TSym("y"))


def test_parenthesised_argument_is_a_term_not_a_proposition():
    assert Prop.parse("Even (S O)") == PApp(
        PSym("Even"), TApp(TSym("S"), TSym("O"))
    )


# ---------------------------------------------------------------------------
#  Command rendering
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "printed,command",
    [
        ("A", "A"),
        ("Un O", "Un[O]"),
        ("P x y", "P[x][y]"),
        ("Even (S O)", "Even[S O]"),
        ("~A", "~A"),
        ("A->B", "A -> B"),
        ("A->C->false", "A -> C -> false"),
        ("(R->false)->Q", "(R -> false) -> Q"),
        ("A-B->C", "A-B -> C"),
        ("(A-B)->C", "(A-B) -> C"),
        ("A->B-C", "A -> B-C"),
        ("(true-A)-B", "true-A-B"),
        ("forall n:N,Un n", "forall n:N, Un[n]"),
        ("forall n,m:N,Eq n m->Eq m n", "forall n,m:N, Eq[n][m] -> Eq[m][n]"),
        ("(forall x:N,Un x)->A", "(forall x:N, Un[x]) -> A"),
    ],
)
def test_to_command_brackets_application_and_parenthesises_connectives(printed, command):
    assert Prop.parse(printed).to_command() == command


def test_command_and_printed_forms_share_one_precedence():
    """They differ only in application syntax and spacing, not in grouping."""
    for text in ("A-B->C", "(A-B)->C", "A->B-C", "A->(B-C)"):
        parsed = Prop.parse(text)
        # Strip the cosmetic differences and the two spellings coincide.
        assert parsed.to_command().replace(" -> ", "->") == str(parsed)


# ---------------------------------------------------------------------------
#  Alpha-equivalence
# ---------------------------------------------------------------------------


def test_bound_variable_renaming_preserves_equality():
    assert Prop.parse("forall x:N,Un x") == Prop.parse("forall y:N,Un y")


def test_alpha_equivalent_propositions_hash_alike():
    left = Prop.parse("forall x:N,Un x")
    right = Prop.parse("forall y:N,Un y")
    assert hash(left) == hash(right)
    assert {left: "kept"}[right] == "kept"


def test_free_variables_are_not_renamed_away():
    assert Prop.parse("forall x:N,P x y") != Prop.parse("forall x:N,P x z")


def test_binder_position_is_significant():
    assert Prop.parse("forall x:N,P x y") != Prop.parse("forall y:N,P x y")


def test_quantifier_kind_is_significant():
    assert Prop.parse("forall x:N,Un x") != Prop.parse("exists x:N,Un x")


def test_sort_is_significant():
    assert Prop.parse("forall x:N,Un x") != Prop.parse("forall x:M,Un x")


def test_nested_binders_distinguish_their_depths():
    assert Prop.parse("forall x:N,forall y:N,P x y") != Prop.parse(
        "forall x:N,forall y:N,P y x"
    )


def test_propositional_atoms_are_not_first_order_variables():
    assert Prop.parse("A").free_vars() == set()
    assert Prop.parse("P x y").free_vars() == {"x", "y"}
    assert Prop.parse("forall x:N,P x y").free_vars() == {"y"}


# ---------------------------------------------------------------------------
#  Substitution -- what forall-elimination needs
# ---------------------------------------------------------------------------


def test_instantiate_replaces_the_bound_variable():
    assert Prop.parse("forall n:N,Un n").instantiate(TSym("O")) == Prop.parse("Un O")


def test_instantiate_accepts_a_compound_term():
    assert Prop.parse("forall n:N,Un n").instantiate(
        TApp(TSym("S"), TSym("O"))
    ) == Prop.parse("Un (S O)")


def test_instantiate_peels_one_variable_of_a_shared_binder():
    peeled = Prop.parse("forall n,m:N,Eq n m").instantiate(TSym("O"))
    assert peeled == Prop.parse("forall m:N,Eq O m")


def test_substitution_stops_at_a_shadowing_binder():
    # The inner binder rebinds x, so the outer substitution must not reach it.
    prop = Prop.parse("forall x:N,P x y")
    assert prop.subst("x", TSym("O")) == prop


def test_substitution_avoids_capture():
    """Substituting a term whose variable the binder would capture renames it."""
    prop = Prop.parse("forall y:N,P x y")
    result = prop.subst("x", TSym("y"))

    assert isinstance(result, PQuant)
    # The binder was renamed away from y, so the substituted y stays free.
    assert result.names != ("y",)
    assert "y" in result.free_vars()
    assert result == Prop.parse("forall y2:N,P y y2")


def test_substitution_leaves_predicate_symbols_alone():
    assert Prop.parse("P x").subst("P", TSym("Q")) == Prop.parse("P x")


# ---------------------------------------------------------------------------
#  Sorts and terms
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "text,expected",
    [
        ("type", SSet()),
        ("bool", SProp()),
        ("N", SSym("N")),
        ("N->bool", SArr(SSym("N"), SProp())),
        ("N->N->bool", SArr(SSym("N"), SArr(SSym("N"), SProp()))),
        ("(N->N)->N", SArr(SArr(SSym("N"), SSym("N")), SSym("N"))),
        ("[termf->form]->form", SArr(SArr(SSym("termf"), SSym("form")), SSym("form"))),
    ],
)
def test_sort_parsing(text, expected):
    assert parse_sort(text) == expected


@pytest.mark.parametrize("text", ["type", "bool", "N", "N->bool", "N->N->bool", "(N->N)->N"])
def test_sort_round_trip(text):
    assert str(parse_sort(text)) == text


@pytest.mark.parametrize(
    "text,expected",
    [
        ("bool", SProp()),
        ("N->bool", SProp()),
        ("N->N->bool", SProp()),
        ("N", SSym("N")),
        ("type", SSet()),
    ],
)
def test_sort_result_walks_the_arrow_chain(text, expected):
    assert sort_result(parse_sort(text)) == expected


@pytest.mark.parametrize("text", ["O", "S O", "S (S O)", "Plus (S n) m"])
def test_term_round_trip(text):
    assert str(parse_term(text)) == text


def test_term_to_command_brackets():
    assert term_to_command(parse_term("S O")) == "[S O]"


# ---------------------------------------------------------------------------
#  Errors
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "text",
    [
        "",
        "->B",
        "A->",
        "forall n,P n",        # missing sort
        "forall :N,P n",       # missing name
        "(A->B",               # unbalanced
        "A->B)",
    ],
)
def test_malformed_propositions_raise(text):
    with pytest.raises(PropError):
        Prop.parse(text)


def test_quantifier_must_bind_something():
    with pytest.raises(PropError):
        PQuant(Quantifier.FORALL, (), SSym("N"), PSym("A"))


def test_units_are_distinct():
    assert PTrue() != PFalse()
    assert Prop.parse("true") == PTrue()
    assert Prop.parse("false") == PFalse()
