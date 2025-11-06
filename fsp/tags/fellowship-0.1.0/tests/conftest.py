# tests/conftest.py
from pathlib import Path
import pytest
import os
import importlib.util, sys
import logging
from mod import store


from wrap.cli import (
    configure_logging_cli,
    pop,
    execute_script,
    interactive_mode,
    setup_prover,
    reduce_argument_cmd,
    render_argument_cmd,
    color_argument_cmd,
    tree_argument_cmd,
    main,
)
from wrap.prover import ProverWrapper, ProverError, MachinePayloadError
from core.dc.argument import Argument
from core.ac.grammar import Grammar, ProofTermTransformer
import builtins as _builtins
# Expose parser-era globals for legacy tests that reference them without import
_builtins.Grammar = Grammar
_builtins.ProofTermTransformer = ProofTermTransformer

PYTEST_TODO = ("Add version of trees with natural language renderings; update/tweak natural language renderings; implement multiple undercuts, support, and rebut; add support for the '-' connective; "
               "then implement scenarios.")

DEFAULT_SCRIPTS = [
    "normalize_render.fspy",
    "tactics.fspy",
    "test1.fspy",
    "counterarguments_and_undercut.fspy",
    "multiple_undercuts.fspy"
    #"tests/test2.fspy",
    # "tests/negation_expanded.fspy",  # uncomment when ready
]

def make_assert_log(logger: logging.Logger):
    """Return an assertion helper bound to the given logger."""
    def _assert_log(cond: bool, msg: str) -> None:
        if cond:
            logger.debug("assertion passed: %s", msg)
        else:
            logger.error("assertion failed: %s", msg)
        assert cond, msg
    return _assert_log

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
    os.environ.setdefault("FSP_MACHINE", "1")   # force machine mode
    pw = setup_prover()
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

def load_monolith():
    p = Path(__file__).parent / "tests.py"
    spec = importlib.util.spec_from_file_location("fsp_monolith_tests", p)
    mod = importlib.util.module_from_spec(spec)
    sys.modules["fsp_monolith_tests"] = mod
    assert spec.loader is not None
    spec.loader.exec_module(mod)
    return mod

def pytest_ignore_collect(collection_path: Path, config):
    # Prevent legacy tests.py (manual runner) from being auto‑collected by pytest
    try:
        return collection_path.name == "tests.py"
    except Exception:
        return False

def pytest_report_header(config):
    return f"TODO: {PYTEST_TODO}"

@pytest.fixture(autouse=True)
def reset_global_store():
    # Keep declarations session-scoped (per ProverWrapper instance).
    # Arguments are global via mod.store; clear between tests to avoid bleed-through.
    store.arguments.clear()
