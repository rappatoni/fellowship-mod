"""An argument the document graph cannot take is refused, not registered.

It used to be registered with a warning ("not added to the document
graph") and simply left out of the labelling, so a counterargument could
silently vanish from a verdict (tasks.org, aida-refuse-dropped-argument).
The refusal happens in `Argument.execute`'s ``accept`` hook, before
Fellowship is asked to hold the argument, so a strict argument refused
there is not left behind as a theorem either.
"""

import io
import warnings
from contextlib import redirect_stdout

import pytest

import wrap.prover
from core.dc.debate_graph import DebateCompileError
from wrap.cli import execute_script, setup_prover
from wrap.prover import GraphRefused, ProverError

REFUSED = "refused_one"


@pytest.fixture
def prover(monkeypatch):
    """A session of its own, whose graph compiler refuses REFUSED."""
    real = wrap.prover.compile_debate

    def compile_debate(body, name, **kwargs):
        if name == REFUSED:
            raise DebateCompileError(f"Edge '{name}': refused by the test.")
        return real(body, name, **kwargs)

    monkeypatch.setattr(wrap.prover, "compile_debate", compile_debate)
    prover = setup_prover()
    yield prover
    prover.close()


def _run(prover, tmp_path, text, *, strict=True):
    script = tmp_path / "s.fspy"
    script.write_text(text)
    with warnings.catch_warnings(), redirect_stdout(io.StringIO()):
        warnings.simplefilter("ignore")
        execute_script(prover, str(script), strict=strict, isolate=False,
                       render_files=False, stop_marker=False)


HEADER = "lk.\ndeclare A, B : bool.\ndeclare ab : (A -> B).\ndeclare a : (A).\n"


@pytest.mark.parametrize("steps", [
    "cut (A -> B) r.\naxiom ab.\nelim.\nby default.\nnext.\naxiom r.\n",            # defeasible
    "cut (A -> B) r.\naxiom ab.\nelim.\naxiom a.\naxiom r.\n",               # strict
])
def test_the_refused_argument_is_not_registered(prover, tmp_path, steps):
    with pytest.raises(ProverError, match="cannot be added to the document graph"):
        _run(prover, tmp_path, HEADER + f"argument {REFUSED} : (B).\n" + steps + "dixi.\n")
    assert REFUSED not in prover.arguments
    assert REFUSED not in prover.names
    assert REFUSED not in {e.name for e in prover.graph.edges}
    assert REFUSED not in prover.declarations          # Fellowship holds no theorem


def test_the_prover_is_left_clean(prover, tmp_path):
    """After the refusal the next argument records as usual, and the
    refused name is free again."""
    _run(prover, tmp_path, HEADER
         + f"argument {REFUSED} : (B).\ncut (A -> B) r.\naxiom ab.\nelim.\naxiom a.\naxiom r.\ndixi.\n"
         + "argument fine : (B).\ncut (A -> B) r.\naxiom ab.\nelim.\nby default.\nnext.\naxiom r.\ndixi.\n",
         strict=False)
    assert "fine" in prover.arguments
    assert "fine" in {e.name for e in prover.graph.edges}
    assert REFUSED not in prover.arguments


def test_register_argument_refuses_too(prover, tmp_path):
    """The backstop for paths without the accept hook."""
    _run(prover, tmp_path, HEADER
         + "argument other : (B).\ncut (A -> B) r.\naxiom ab.\nelim.\nby default.\nnext.\naxiom r.\ndixi.\n")
    argument = prover.arguments["other"]
    argument.name = REFUSED
    with pytest.raises(GraphRefused):
        prover.register_argument(argument)
    assert REFUSED not in prover.arguments and REFUSED not in prover.names
