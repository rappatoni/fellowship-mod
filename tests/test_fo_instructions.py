"""Turning first-order proof terms back into tactics.

Each first-order node came from one `elim` (tactics.ml:502-607): the two that
bind a variable name it, the two that supply a witness bracket it.  The
bracketing is not cosmetic -- Fellowship *prints* a first-order term juxtaposed
(`S (S O)`) but only *accepts* it bracketed (`[S (S O)]`), so the witness has to
be re-serialised on the way back out.

The last test is the end-to-end one this whole milestone exists for: a proof
term the prover printed, read back and replayed to qed.
"""

import warnings

import pytest

from core.ac.instructions import InstructionsGenerationVisitor
from core.ac.resolve import resolve
from core.ac.signature import Declaration
from core.ac.syntax import parse_proof_term
from core.comp.enrich import PropEnrichmentVisitor
from core.dc.argument import Argument
from wrap.cli import setup_prover
from conftest import *  # noqa: F401,F403


FORALL_DECLS = {
    "N": Declaration("type", "sort"),
    "O": Declaration("N", "sort"),
    "S": Declaration("N->N", "sort"),
    "P": Declaration("N->bool", "sort"),
    "ax": Declaration("forall n:N,P n", "prop"),
}

FORALL_TERM = ";:thesis:P O.<;:th:P O.<ax||O*th>||thesis>"

EXISTS_FORALL_DECLS = {
    "N": Declaration("type", "sort"),
    "P": Declaration("N->N->bool", "sort"),
}

EXISTS_FORALL_TERM = (
    ";:thesis:(exists y:N,forall x:N,P x y)->(forall x:N,exists y:N,P x y)."
    "<\\H:exists y:N,forall x:N,P x y.\\x:N."
    ";:th:exists y:N,P x y.<H||(y:N)."
    ";:'K:forall x:N,P x y.<(y,;:K2:P x y.<K||x*K2>)||th>>||thesis>"
)


def instructions(term, declarations, axiom_props=None):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        node = resolve(parse_proof_term(Argument._normalize_pt_to_unicode(term)), declarations)
        node = PropEnrichmentVisitor(axiom_props=axiom_props or {}).visit(node)
        return list(InstructionsGenerationVisitor(root_name="thesis").return_instructions(node))


# ---------------------------------------------------------------------------
#  One elim per first-order node
# ---------------------------------------------------------------------------


def test_universal_instantiation_carries_its_witness():
    """The bug this milestone opened with: the witness used to vanish, leaving
    a bare `elim` and a spurious `axiom O`."""
    assert "elim [O]" in instructions(FORALL_TERM, FORALL_DECLS, {"ax": "forall n:N,P n"})


def test_a_compound_witness_is_rebracketed():
    """Printed as `S (S O)`, accepted only as `[S (S O)]`."""
    term = ";:thesis:P (S (S O)).<;:th:P (S (S O)).<ax||S (S O)*th>||thesis>"
    assert "elim [S (S O)]" in instructions(term, FORALL_DECLS, {"ax": "forall n:N,P n"})


def test_universal_introduction_names_its_variable():
    assert "elim x" in instructions(EXISTS_FORALL_TERM, EXISTS_FORALL_DECLS)


def test_existential_elimination_names_its_variable():
    assert "elim y" in instructions(EXISTS_FORALL_TERM, EXISTS_FORALL_DECLS)


def test_existential_introduction_carries_its_witness():
    assert "elim [y]" in instructions(EXISTS_FORALL_TERM, EXISTS_FORALL_DECLS)


def test_the_quantified_cut_is_rendered_in_command_syntax():
    """`forall n:N,P n` is printed juxtaposed and must go out bracketed."""
    generated = instructions(FORALL_TERM, FORALL_DECLS, {"ax": "forall n:N,P n"})
    assert generated[0] == "cut (forall n:N, P[n]) th"


# ---------------------------------------------------------------------------
#  End to end, against the prover
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "header,term,declarations,axiom_props",
    [
        (
            ["lj.", "minimal.", "declare N: type.", "declare O: N.",
             "declare P: N -> bool.", "declare ax: (forall n:N, P [n]).",
             "theorem t: (P[O])."],
            FORALL_TERM, FORALL_DECLS, {"ax": "forall n:N,P n"},
        ),
        (
            ["lj.", "minimal.", "declare N: type.", "declare P: N -> N -> bool.",
             "theorem t : ((exists y:N, forall x:N, P [x] [y]) -> "
             "(forall x:N, exists y:N, P [x] [y]))."],
            EXISTS_FORALL_TERM, EXISTS_FORALL_DECLS, {},
        ),
    ],
    ids=["forall", "exists_forall"],
)
def test_generated_instructions_replay_to_qed(header, term, declarations, axiom_props):
    """Parse what the prover printed, and hand it back as tactics.

    This is the round trip the milestone is for: prover -> parse -> resolve ->
    enrich -> instructions -> prover, ending at qed.
    """
    prover = setup_prover()
    for command in header:
        prover.send_command(command)
    for instruction in instructions(term, declarations, axiom_props):
        prover.send_command(instruction + ".")
    prover.send_command("qed.")

    assert "t" in prover.declarations
