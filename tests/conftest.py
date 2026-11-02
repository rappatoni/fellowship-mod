# tests/conftest.py
from pathlib import Path
import pytest
import os
import logging
import subprocess
from types import SimpleNamespace


from wrap.cli import (
    configure_logging_cli,
    execute_script,
    interactive_mode,
    setup_prover,
    reduce_argument_cmd,
    render_argument_cmd,
    tree_argument_cmd,
    main,
)
from wrap.prover import ProverWrapper, ProverError, MachinePayloadError
from core.dc.argument import Argument

PYTEST_TODO = ("Add version of trees with natural language renderings; update/tweak natural language renderings; implement multiple undercuts, support, and rebut; add support for the '-' connective; "
               "then implement scenarios.")

DEFAULT_SCRIPTS = [
    "normalize_render.fspy",
    "tactics.fspy",
    "test1.fspy",
    "counterarguments_and_undercut.fspy",
    "multiple_undercuts.fspy",
    "multiple_supports_stack_vs_queue.fspy",
    # The first-order fixtures.
    "fo_forall.fspy",
    "fo_exists_forall.fspy",
    # The runnable rationality-postulate fixtures (tests/rationality/).
    "rationality/self_attack.fspy",
    "rationality/even_loop.fspy",
    "rationality/contested.fspy",
    "rationality/two_witnesses.fspy",
    "rationality/cyclic_undercut.fspy",
    "rationality/document_attack.fspy",
    "rationality/even_loop_lk.fspy",
    "rationality/self_attack_lk.fspy",
    "rationality/modus_tollens.fspy",
    # CLI surface of the fragment pipeline: graph / label / evaluate.
    "label_evaluate.fspy",
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

@pytest.fixture(scope="session", autouse=True)
def no_graph_files():
    """Ten fixtures under tests/ carry `graph ... show` or `tree`.  Without
    this the suite writes a PNG per fixture into the working directory and
    opens a viewer for each.  ACDC_NO_RENDER stops the writing; the commands
    log an indented text view of the graph instead."""
    os.environ.setdefault("ACDC_NO_RENDER", "1")


@pytest.fixture(scope="session", autouse=True)
def require_adf_bdd():
    """The debate labeller is adf-bdd, with no fallback (tasks.org,
    aida-adf-bdd-primary).  Fail the session up front, once and clearly,
    rather than letting every labelling test fail with the same message."""
    from core.comp.oracle import resolve_adf_bdd
    resolve_adf_bdd()


@pytest.fixture(scope="session", autouse=True)
def solver_answers_once():
    """adf-bdd is asked the same question over and over: 93% of its calls in
    a run of the suite repeat one already made.  Answer a repeat from memory;
    the first call of each input still runs the solver.  Only the solver
    module's view of `subprocess` changes."""
    from core.comp import oracle
    answers = {}

    def run(command, **options):
        key = (tuple(command[:-1]), Path(command[-1]).read_text())
        if key not in answers:
            answer = subprocess.run(command, **options)
            if answer.returncode:
                return answer
            answers[key] = answer
        return answers[key]

    patch = pytest.MonkeyPatch()
    patch.setattr(oracle, "subprocess", SimpleNamespace(run=run))
    yield
    patch.undo()


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


def pytest_collection_modifyitems(config, items):
    """Mark the tests of slow_tests.txt (made by tools/list_slow_tests.py)
    `slow`.  A test renamed since just runs by default, which is the safe way
    for the list to go stale."""
    listed = (Path(__file__).parent / "slow_tests.txt").read_text().splitlines()
    slow = {line for line in listed if line and not line.startswith("#")}
    for item in items:
        if item.nodeid in slow:
            item.add_marker(pytest.mark.slow)


def pytest_report_header(config):
    return f"TODO: {PYTEST_TODO}"

@pytest.fixture(autouse=True)
def fresh_document(request):
    """Every test that uses the session-wide `prover` starts on a fresh
    document (wrap/document.py): nothing a test registers or declares leaks
    into the next.  Tests with a prover of their own are fresh anyway."""
    defs = request.node._fixtureinfo.name2fixturedefs.get("prover")
    if defs and defs[-1].scope == "session":       # not a module's own `prover`
        request.getfixturevalue("prover").new_document()
    yield
