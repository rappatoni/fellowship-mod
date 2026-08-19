"""Fellowship's printed propositions must be valid input meaning the same thing.

The printer and the parser used to disagree about precedence, so the prover
emitted propositions it could not read back:

    A - (B->C)      printed as  A-B->C     reparsed as  (A-B)->C
    (A->B) - C      printed as  A->B-C     reparsed as  A->(B-C)
    A \\/ (B /\\ C)   printed as  A\\/B/\\C    reparsed as  (A\\/B)/\\C
    A /\\ (B /\\ C)   printed as  A/\\B/\\C    reparsed as  (A/\\B)/\\C

Both sides now use one ordering, the conventional one:
``/\\`` > ``\\/`` > ``->`` > ``-``.  Nothing else in the suite exercises the
OCaml side of that, so these tests drive the real prover.

Conjunction and disjunction are outside AIDA's own fragment, but the prover
supports them and the precedence fix covers them, so they are checked here.
"""

import pytest

from wrap.cli import setup_prover
from conftest import *  # noqa: F401,F403


SIGNATURE = ["lk.", "declare A,B,C:bool."]

# Each pair is two genuinely different propositions.  Before the fix, several
# of these pairs printed to the same string.
DISTINCT_PAIRS = [
    ("A-(B->C)", "(A-B)->C"),
    ("(A->B)-C", "A->(B-C)"),
    ("A-(B-C)", "(A-B)-C"),
    ("A->(B->C)", "(A->B)->C"),
    (r"A/\(B/\C)", r"(A/\B)/\C"),
    (r"A\/(B/\C)", r"(A\/B)/\C"),
    (r"A/\(B\/C)", r"(A/\B)\/C"),
]

ALL_SHAPES = [p for pair in DISTINCT_PAIRS for p in pair]


@pytest.fixture(scope="module")
def printed():
    """Map each written proposition to how the prover prints it back."""
    prover = setup_prover()
    for command in SIGNATURE:
        prover.send_command(command)
    for index, shape in enumerate(ALL_SHAPES):
        prover.send_command(f"declare d{index} : ({shape}).")
    return {shape: str(prover.declarations[f"d{index}"]) for index, shape in enumerate(ALL_SHAPES)}


@pytest.mark.parametrize("left,right", DISTINCT_PAIRS, ids=lambda s: s.replace("/", ""))
def test_different_groupings_print_differently(printed, left, right):
    assert printed[left] != printed[right], (
        f"{left} and {right} are different propositions but both print as "
        f"{printed[left]!r}; the grouping is unrecoverable"
    )


def test_printed_propositions_are_valid_input_meaning_the_same(printed):
    """Feeding the printer's output back in must be a fixpoint."""
    prover = setup_prover()
    for command in SIGNATURE:
        prover.send_command(command)

    once = [printed[shape] for shape in ALL_SHAPES]
    for index, text in enumerate(once):
        prover.send_command(f"declare e{index} : ({text}).")
    twice = [str(prover.declarations[f"e{index}"]) for index in range(len(once))]

    assert twice == once


@pytest.mark.parametrize(
    "written,grouping",
    [
        # Conjunction binds tighter than disjunction, disjunction tighter than
        # implication, and all of them tighter than subtraction.
        (r"A\/B/\C", r"A\/(B/\C)"),
        (r"A/\B\/C", r"(A/\B)\/C"),
        ("A-B->C", "A-(B->C)"),
        ("A->B-C", "(A->B)-C"),
        (r"A->B/\C", r"A->(B/\C)"),
    ],
)
def test_conventional_precedence(printed, written, grouping):
    """An unparenthesised formula groups the conventional way.

    Checked by declaring the explicit grouping and confirming the prover prints
    it the same as the bare spelling -- which it only does if they agree.
    """
    prover = setup_prover()
    for command in SIGNATURE:
        prover.send_command(command)
    prover.send_command(f"declare bare : ({written}).")
    prover.send_command(f"declare grouped : ({grouping}).")

    assert str(prover.declarations["bare"]) == str(prover.declarations["grouped"])
