"""A user binder named ``_`` (tasks.org, aida-underscore-binder; the
literature evaluation's bug 5).  ``cut (P) _.`` is accepted by Fellowship;
``_`` is affine for the compiler (it binds nothing), but the unfolder let
it capture P's sites in its scope, writing ``_`` as a variable that
Fellowship refuses.  Now the unfolder, like the compiler, never captures
with an affine binder."""

import warnings
from pathlib import Path

import pytest

from wrap.cli import setup_prover
from wrap.service import Service

TESTS = Path(__file__).parent


@pytest.mark.parametrize("typecheck", ["on", "off"])
@pytest.mark.parametrize("script, expected", [
    ("exposure/underscore_binder.fspy", {"a": "value", "b": "value"}),
    ("thetaexpander_bureaucratic_cases.fspy", None),
])
def test_every_argument_can_be_graphed_and_evaluated(script, expected, typecheck):
    prover = setup_prover()
    svc = Service.of(prover)
    try:
        svc.set_typecheck(typecheck)
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            svc.check((TESTS / script).read_text())
            classes = {}
            for name in prover.arguments:
                svc.graph(svc.target(name))
                classes[name] = svc.evaluate(svc.target(name)).nf_class
        assert len(classes) == len(prover.arguments) > 0
        if expected is not None:
            assert classes == expected
    finally:
        prover.close()
