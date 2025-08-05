# tests/conftest.py
from pathlib import Path
import pytest
import os
from wrapper import *


DEFAULT_SCRIPTS = [
    "tests/normalize_render.fspy",
    "tests/tactics.fspy",
    "tests/test1.fspy",
    #"tests/test2.fspy",
]

def pytest_addoption(parser: pytest.Parser) -> None:
    parser.addoption(
        "--script",
        action="append",
        default=None,
        help="Path to a .fspy script to run (can be passed multiple times). "
             "If omitted, the default suite is run."
    )

@pytest.fixture(scope="session")
def prover():
    """Launch a *single* FSP process for the whole test session."""
    env = os.environ.copy()
    env.setdefault("FSP_MACHINE", "1")   # force machine mode
    pw = ProverWrapper('./fsp')
    yield pw
    pw.close()

@pytest.fixture(scope="session")
def script_paths(request) -> list[Path]:
    """Resolve the list of .fspy scripts to run."""
    provided = request.config.getoption("--script")
    if provided:
        candidates = [Path(p) for p in provided]
    else:
        candidates = [Path(__file__).parent / s for s in DEFAULT_SCRIPTS]

    resolved = []
    for c in candidates:
        if c.exists():
            resolved.append(c)
            continue
        # Fallback to CWD if not next to the test file
        alt = Path.cwd() / c.name
        if alt.exists():
            resolved.append(alt)
        else:
            raise FileNotFoundError(
                f"Could not find script '{c}'. Tried: {c} and {alt}"
            )
    return resolved


# --- OPTIONAL: If you do NOT already have a `prover` fixture, you can adapt this:
# @pytest.fixture
# def prover():
#     \"\"\"Provide a real prover from your codebase, or adapt the stub below.\"\"\"
#     try:
#         # Example: from your_package.prover import Prover
#         # return Prover(...)
#         raise ImportError  # Remove this once you wire in your real prover
#     except Exception:
#         pytest.skip(
#             "No `prover` fixture available. Provide one in your project that "
#             "returns an initialized prover compatible with execute_script()."
#         )
