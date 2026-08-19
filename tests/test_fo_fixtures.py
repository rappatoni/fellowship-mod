"""End-to-end replay of the first-order fixtures against the real prover.

These establish ground truth for the first-order work: they prove that
Fellowship's first-order path is sound and that the wrapper forwards
quantified declarations, sorts and bracketed instantiation intact.  They also
pin the shape of the proof terms the Python layer has to learn to read.

The proofs deliberately avoid ``focus``: it aborts the prover on a first-order
goal (see tasks.org, aida-fellowship-focus-firstorder-crash) and is only sugar
for ``cut`` + ``axiom`` (tactics.ml:713-735).
"""

from pathlib import Path

import pytest

from wrap.cli import execute_script, setup_prover
from conftest import *  # noqa: F401,F403  -- provides the session `prover` fixture


FIXTURES = Path(__file__).parent

FO_FORALL = FIXTURES / "fo_forall.fspy"
FO_EXISTS_FORALL = FIXTURES / "fo_exists_forall.fspy"


@pytest.fixture
def own_prover():
    """A prover session this test owns outright.

    `execute_script` defaults to isolate=True, which spawns its own session and
    leaves the caller's prover untouched.  To assert on declarations we need
    isolate=False, and that in turn needs a session no other test has declared
    into -- Fellowship rejects a redeclaration.
    """
    return setup_prover()


@pytest.mark.parametrize("script", [FO_FORALL, FO_EXISTS_FORALL], ids=lambda p: p.stem)
def test_fo_fixture_replays(prover, script):
    """The fixture reaches qed without the prover raising."""
    assert script.exists(), f"missing fixture: {script}"
    execute_script(prover, str(script), strict=True)


def test_forall_fixture_declares_a_sort_and_a_quantified_axiom(own_prover):
    execute_script(own_prover, str(FO_FORALL), strict=True, isolate=False)

    assert own_prover.declarations["N"] == "type"
    assert own_prover.declarations["N"].kind == "sort"

    # The quantifier survives the round trip through the machine payload.
    assert own_prover.declarations["ax"] == "forall n:N,P n"
    assert own_prover.declarations["ax"].kind == "prop"


def test_exists_forall_fixture_declares_a_binary_predicate(own_prover):
    execute_script(own_prover, str(FO_EXISTS_FORALL), strict=True, isolate=False)

    assert own_prover.declarations["P"] == "N->N->bool"
    assert own_prover.declarations["P"].kind == "sort"
    # The outer parentheses matter: this is an implication between two
    # quantified propositions, not a proposition quantified over an
    # implication.  The printer used to drop them and lose the distinction.
    assert (
        own_prover.declarations["fo_exists_forall"]
        == "(exists y:N,forall x:N,P x y)->(forall x:N,exists y:N,P x y)"
    )


def test_exists_forall_conclusion_reads_back_as_an_implication(own_prover):
    """The regression the printer fix exists to prevent."""
    from core.ac.prop import BinOp, PBin, Prop

    execute_script(own_prover, str(FO_EXISTS_FORALL), strict=True, isolate=False)

    conclusion = Prop.parse(own_prover.declarations["fo_exists_forall"])
    assert isinstance(conclusion, PBin)
    assert conclusion.op is BinOp.IMP
