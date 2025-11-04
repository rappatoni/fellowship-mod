import os
from pathlib import Path
import pytest
import logging

from wrap.cli import setup_prover, execute_script
from wrap.prover import ProverError, MachinePayloadError
logger = logging.getLogger(__name__)

def test_moxia_antitheorem_script(monkeypatch):
    # Run from the tag directory so './fsp' is found
    tests_dir = Path(__file__).resolve().parent
    tag_root = tests_dir.parent
    monkeypatch.chdir(tag_root)

    script = tests_dir / "moxia_antitheorem.fspy"
    assert script.is_file(), "missing tests/moxia_antitheorem.fspy"

    prover = setup_prover()
    try:
        # run the batch script using the existing prover (isolate=False)
        execute_script(
            prover,
            str(script),
            strict=True,
            stop_on_error=True,
            echo_notes=False,
            isolate=False
        )

        # Basic sanity of final state
        st = prover.last_state or {}
        assert st.get('mode') == 'idle'
        assert st.get('errors') == []

        # Decls harvested by wrapper
        decls = prover.declarations
        assert decls.get('A') == 'bool'
        # moxia declarations are recorded with kind 'moxia' and their prop
        assert decls.get('mA') == 'A'
        # anti-theorem becomes a named moxia after qed
        assert decls.get('notA') == 'A'
        logger.info("decls %r", decls)

    finally:
        try:
            prover.close()
        except Exception:
            pass
