# tests/test_execute_script.py
from pathlib import Path
import pytest
from wrapper import *
from conftest import *

# ⬇️ Adjust this import to where `execute_script` actually lives
from wrapper import execute_script  # e.g. from fellowship.wrapper import execute_script


def test_execute_script_runs_all(script_paths, prover):
    """
    Runs the .fspy scripts through `execute_script` and asserts they complete
    without raising exceptions. If you want per-file isolation, keep the loop
    but rely on pytest's traceback to show which file failed.
    """
    assert script_paths, "No .fspy scripts resolved to run."

    for script in script_paths:
        # Sanity checks for clarity in failures
        assert isinstance(script, Path), "script_paths must contain Path objects."
        assert script.suffix == ".fspy", f"Unexpected script extension: {script}"
        assert script.exists(), f"Script not found: {script}"

        # The call under test: should not raise
        execute_script(prover, str(script))
