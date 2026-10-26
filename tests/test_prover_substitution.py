"""Fellowship's substitution must avoid capture.

Instantiating a quantifier (``elim [t]`` on an existential on the right or a
universal on the left) substitutes ``t`` into the body.  The original
substitution went under every quantifier unchecked: with ``y`` in scope,
``elim [y]`` on ``exists x, forall y, R x y`` gave ``forall y, R y y``, and
the prover accepted false theorems.  The bug is in Fellowship 0.1.0 as
distributed (tasks.org, aida-fellowship-capture).

A renamed binder gets the name AIDA's own substitution gives it
(core/ac/prop.py), so that AIDA's replayed statements match the prover's.
"""

import pytest

from core.ac.prop import Prop
from wrap.cli import setup_prover
from wrap.prover import ProverError
from conftest import *  # noqa: F401,F403


SIGNATURE = ["lk.", "declare iota : type.", "declare c : iota.",
             "declare R : iota -> iota -> bool."]

FALSE = ("theorem bad : ((forall z:iota, R [z] [z]) -> "
         "forall y:iota, exists x:iota, forall y:iota, R [x] [y]).")


@pytest.fixture
def prover():
    prover = setup_prover()
    for command in SIGNATURE:
        prover.send_command(command)
    yield prover
    prover.close()


def active(state):
    goal = state["goals"]
    return dict(item for item in goal[1:] if len(item) == 2)["active-prop"]


def test_a_captured_binder_is_renamed(prover):
    for command in (FALSE, "elim h.", "elim y."):
        prover.send_command(command)
    assert active(prover.send_command("elim [y].")) == "forall y2:iota,R y y2"


def test_the_false_theorem_is_refused(prover):
    for command in (FALSE, "elim h.", "elim y.", "elim [y].", "elim w.",
                    "cut (forall z:iota, R [z] [z]) k.", "axiom h.", "elim [w]."):
        prover.send_command(command)
    with pytest.raises(ProverError, match="not trivial"):
        prover.send_command("axiom.")


def test_substitution_stops_at_a_binder_of_the_same_name(prover):
    prover.send_command("theorem t : (exists x:iota, forall x:iota, R [x] [x]).")
    assert active(prover.send_command("elim [c].")) == "forall x:iota,R x x"


def test_a_quantifier_the_substitution_does_not_reach_keeps_its_name(prover):
    """Renaming it anyway broke later axiom steps, which compare literally
    (tests/first_order/hypothesis_horn.fspy, drinker_referent.fspy)."""
    prover.send_command("theorem t : (forall y:iota, exists x:iota, R [x] [x] - ~(forall y:iota, R [y] [y])).")
    prover.send_command("elim y.")
    assert active(prover.send_command("elim [y].")) == "R y y-~(forall y:iota,R y y)"


def test_a_renaming_past_a_taken_name_agrees_with_aida(prover):
    stated = "exists x:iota, forall y:iota, forall y2:iota, R [x] [y] - ~R [y] [y2]"
    prover.send_command(f"theorem t : (forall y:iota, {stated}).")
    prover.send_command("elim y.")
    got = active(prover.send_command("elim [y]."))
    expected = Prop.parse(stated.replace("[", "").replace("]", "")).instantiate(
        Prop.parse("R y y").arg)
    assert got == str(expected)
