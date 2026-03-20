from pathlib import Path

from wrap.cli import execute_script


def test_mirror_renderer_minimal_renders_mutilde_and_admal(prover):
    script = Path(__file__).with_name("mirror_minimal.fspy")
    execute_script(prover, str(script), strict=True, stop_on_error=True, echo_notes=False, isolate=False)

    arg = prover.get_argument("m")
    assert arg is not None and arg.executed

    from pres.mirror import render_mirror_linear

    s = render_mirror_linear(arg.body)

    # 1) μ' must not appear in mirror rendering.
    assert "μ'" not in s

    # 2) We should have at least one mirrored context-μ suffix: <.NAME:PROPμ
    # (NAME often: stash/aff/alt/..., PROP some proposition)
    assert "<." in s
    assert s.count("μ") >= 1

    # 3) We should have a mirrored context-λ suffix from Admal: ...PROP:NAMEλ
    assert "λ" in s
    assert ":" in s
    assert "A:" in s
