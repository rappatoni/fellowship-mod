from core.ac.ast import Goal, Laog, ID, DI, Mu, Mutilde
from core.comp.reduce import ThetaExpander


def test_default_eta_exposes_atomic_term_once():
    goal = Goal("1", "P")

    expander = ThetaExpander("P", mode="term")
    out = expander.visit(goal)

    assert expander.found_target is True
    assert expander.changed is True
    assert isinstance(out, Mu)
    assert out.id.name == "alt"
    assert isinstance(out.term, Mu)
    assert out.term.id.name == "_"
    assert isinstance(out.context, Laog)
    assert out.context.prop == "P"


def test_default_eta_does_not_reexpand_exposed_term_target():
    exposed = Mu(
        ID("alt", "P"),
        "P",
        Mu(ID("_", "P"), "P", Goal("1", "P"), ID("alt", "P")),
        Laog("1", "P"),
    )

    expander = ThetaExpander("P", mode="term")
    out = expander.visit(exposed)

    assert expander.found_target is True
    assert expander.changed is False
    assert isinstance(out, Mu)
    assert out.id.name == "alt"
    assert isinstance(out.term, Mu)
    assert out.term.id.name == "_"


def test_default_eta_exposes_inner_goal_inside_exposed_term_target():
    exposed = Mu(
        ID("alt", "P"),
        "P",
        Mu(ID("_", "P"), "P", Goal("1", "P"), ID("alt", "P")),
        Laog("1", "P"),
    )

    expander = ThetaExpander("P", mode="term")
    out = expander.visit(exposed.term)

    assert expander.found_target is True
    assert expander.changed is True
    assert isinstance(out, Mu)
    assert out.id.name == "alt"
    assert isinstance(out.term, Mu)
    assert isinstance(out.term.term, Mu)


def test_default_eta_exposes_atomic_context_once():
    laog = Laog("1", "P")

    expander = ThetaExpander("P", mode="context")
    out = expander.visit(laog)

    assert expander.found_target is True
    assert expander.changed is True
    assert isinstance(out, Mutilde)
    assert out.di.name == "alt"
    assert isinstance(out.term, Goal)
    assert out.term.prop == "P"
    assert isinstance(out.context, Mutilde)
    assert out.context.di.name == "_"


def test_default_eta_does_not_reexpand_exposed_context_target():
    exposed = Mutilde(
        DI("alt", "P"),
        "P",
        Goal("1", "P"),
        Mutilde(DI("_", "P"), "P", DI("alt", "P"), Laog("1", "P")),
    )

    expander = ThetaExpander("P", mode="context")
    out = expander.visit(exposed)

    assert expander.found_target is True
    assert expander.changed is False
    assert isinstance(out, Mutilde)
    assert out.di.name == "alt"
    assert isinstance(out.context, Mutilde)
    assert out.context.di.name == "_"


from pathlib import Path

from wrap.cli import execute_script


def test_peirce_repeated_support_reaches_inner_default(prover):
    script = Path(__file__).parent / "peirces_law.fspy"
    assert script.exists()

    execute_script(prover, str(script), strict=False, isolate=False)

    d3 = prover.get_argument("d3")
    assert d3 is not None
    assert d3.proof_term is not None

    proof_term = d3.proof_term.replace(" ", "")
    assert "μalpha:P.<λg:P->Q.μalt" in proof_term
    assert "λg2" not in proof_term
    assert "thesis32" not in proof_term
    assert proof_term.count("μalt") >= 2
    assert proof_term.count(":P") >= 6
