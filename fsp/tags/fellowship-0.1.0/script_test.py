# tests/test_execute_script.py
from pathlib import Path
import pytest
from wrapper import *
from conftest import *

# ⬇️ Adjust this import to where `execute_script` actually lives
from wrapper import execute_script  # e.g. from fellowship.wrapper import execute_script


def test_execute_script_runs_all(script_paths, prover):
    """
    Runs .fspy scripts through `execute_script`.

    Convention:
      - Scripts whose filename starts with 'error' or 'warning' are expected to fail parsing
        and must raise ProverError (e.g., error_warning_test.fspy).
      - All other scripts must complete without raising exceptions.

    We run scripts in strict mode to surface parse errors with file:line context.
    """
    assert script_paths, "No .fspy scripts resolved to run."

    for script in script_paths:
        # Sanity checks for clarity in failures
        assert isinstance(script, Path), "script_paths must contain Path objects."
        assert script.suffix == ".fspy", f"Unexpected script extension: {script}"
        assert script.exists(), f"Script not found: {script}"

        # Convention: negative scripts start with 'error' or 'warning'
        stem = script.stem.lower()
        is_negative = stem.startswith("error") or stem.startswith("warning")

        if is_negative:
            with pytest.raises(ProverError) as ei:
                execute_script(prover, str(script), strict=True)
            msg = str(ei.value)
            assert f"{script}:" in msg, f"Error message should include script path: {msg}"
            assert ("Parse error" in msg) or ("syntactical analysis" in msg), f"Expected parse error text in: {msg}"
        else:
            # Should complete without exceptions
            execute_script(prover, str(script), strict=True)
