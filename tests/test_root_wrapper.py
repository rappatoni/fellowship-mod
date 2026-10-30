"""A binder named `thesis` is a binder like any other (tasks.org,
aida-typecheck-nested-thesis-binder): `theorem` wraps a proof in
μthesis.<t||thesis>, which a proof cannot name, and the instruction
generator knows that wrapper by its place and shape, not by its name."""
import pytest

from pres.gen import pres_tree
from wrap.cli import setup_prover
from wrap.interpreter import Interpreter

DOCUMENT = """lk.
typecheck {mode}.
declare A, B : bool.
declare b : (B).
declare beleg : (B).
declare r : (B -> A).
argument main : (A).
cut (B -> A) f.
axiom r.
elim.
axiom b.
axiom f.
dixi.
register st : B := "μ{binder}:B.<beleg:B||{binder}:B>".
debate pro closed d : A.
main.
st.
cedat tempus.
evaluate d.
"""


def _evaluate(text):
    prover = setup_prover()
    try:
        out = list(Interpreter(prover).run(text))
    finally:
        prover.close()
    return out[-1]


@pytest.mark.parametrize("mode", ["on", "expanded", "off"])
def test_a_debate_gives_one_verdict_whatever_a_binder_is_called(mode):
    named = _evaluate(DOCUMENT.format(mode=mode, binder="thesis"))
    renamed = _evaluate(DOCUMENT.format(mode=mode, binder="t"))
    assert named.status == renamed.status == "ok", (named.message, renamed.message)
    assert named.value.nf_class == renamed.value.nf_class == "value"
    assert pres_tree(named.value.normal_form) == pres_tree(renamed.value.normal_form)


REGISTERED = """lk.
declare A, B : bool.
declare b : (B).
declare r : (B -> A).
register debate reg : A.
term "μthesis:A.<μf:A.<r:B->A||b:B*f:A>||thesis:A>".
cedat tempus.
"""


def test_a_registered_debate_with_a_thesis_binder_extends():
    prover = setup_prover()
    try:
        out = list(Interpreter(prover).run(
            REGISTERED + "debate d2 extends reg.\ncedat tempus.\nevaluate d2.\n"))
    finally:
        prover.close()
    assert [o.status for o in out if o.status not in ("ok", "comment")] == [], [o.message for o in out]
