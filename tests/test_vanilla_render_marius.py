from pathlib import Path

import pytest

from pres.nl import pretty_natural, vanilla_rendering
from pres.gen import ProofTermGenerationVisitor
from wrap.cli import setup_prover, execute_script


def test_vanilla_render_marius_example_matches_generated_pres(prover, tmp_path):
    # Run the real script to produce the argument in the prover session.
    # This ensures we test the exact pipeline the user cares about.  Without
    # arg0, which the pure level refuses (tests/test_discipline.py).
    text = (Path(__file__).parent / "marius_example.fspy").read_text()
    script = tmp_path / "marius_example.fspy"
    script.write_text(text[:text.index("argument arg0")] + text[text.index("#Then via argument operations"):])
    execute_script(prover, str(script), strict=True, stop_on_error=True, echo_notes=False, isolate=False)

    arg = prover.get_argument("arg1")
    assert arg is not None
    assert arg.executed

    # Ensure presentation exists on the AST
    ProofTermGenerationVisitor().visit(arg.body)
    pres = arg.body.pres

    out = pretty_natural(arg.body, vanilla_rendering)

    # Vanilla: should preserve the full syntax while adding tree-guide characters.
    without_guides = out.translate(str.maketrans("", "", "├└─│"))
    ws_stripped = "".join(without_guides.split())
    assert ws_stripped == pres
