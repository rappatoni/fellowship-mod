# fsp_machine_mode_tests.py – high‑level integration smoke tests
"""Run with `pytest -q fsp_machine_mode_tests.py` once:

1.  *machine mode* is compiled into fsp (env var `FSP_MACHINE=1`).
2.  The Python wrapper (`wrapper.py`) is importable from PYTHONPATH.

We stub the bare minimum – the tests will *xfail* until each stage of the
OCaml side emits the corresponding payload fields.
"""

import os
import sys
import json
import subprocess
from pathlib import Path

import pytest
from wrapper import *

# ---------------------------------------------------------------------------
#  Fixtures
# ---------------------------------------------------------------------------
@pytest.fixture(scope="session")
def prover():
    """Launch a *single* FSP process for the whole test session."""
    env = os.environ.copy()
    env.setdefault("FSP_MACHINE", "1")   # force machine mode
    pw = ProverWrapper('./fsp')
    yield pw
    pw.close()
# ---------------------------------------------------------------------------
#  Tests – will be tightened as payload grows
# ---------------------------------------------------------------------------

def test_snapshot_minimal(prover):
    """`lj.` should succeed and leave prover in *idle* mode (no goals)."""
    state = prover.send_command("lj.", silent=1)
    assert state["mode"] in {"idle", "success"}


def test_declare_roundtrip(prover):
    """Ensure `declare` is still recorded in wrapper bookkeeping."""
    prover.send_command("declare A:bool.")
    assert "A" in prover.declarations
    assert prover.declarations["A"] == "bool"


#@pytest.mark.xfail(reason="proof-term-hash field not yet emitted")
def test_proof_term_hash(prover):
    state = prover.last_state or {}
    assert "proof-term-hash" in state, "payload missing proof-term-hash"
