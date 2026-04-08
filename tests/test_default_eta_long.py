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


def test_instruction_generator_skips_literal_thesis_root():
    from core.ac.instructions import InstructionsGenerationVisitor

    root = Mu(ID("thesis", "P"), "P", Goal("1", "P"), Laog("2", "P"))
    root.contr = "P"

    instructions = list(InstructionsGenerationVisitor(root_name="demo").return_instructions(root))

    assert instructions == ["next", "next"]


def test_instruction_generator_still_emits_cut_for_non_thesis_root_name():
    from core.ac.instructions import InstructionsGenerationVisitor

    root = Mu(ID("demo", "P"), "P", Goal("1", "P"), Laog("2", "P"))
    root.contr = "P"

    instructions = list(InstructionsGenerationVisitor(root_name="demo").return_instructions(root))

    assert instructions == ["cut (P) demo", "next", "next"]

def test_argument_rename_outer_binder_rewrites_bound_occurrences():
    from core.dc.argument import Argument

    class _DummyProver:
        declarations = {}

    arg = Argument(_DummyProver(), "demo", "P")
    body = Mu(ID("thesis", "P"), "P", Goal("1", "P"), ID("thesis", "P"))

    arg._rename_outer_binder(body, "demo")

    assert body.id.name == "demo"
    assert body.context.name == "demo"


def test_argument_synthetic_root_name_reads_outer_binder_name():
    from core.dc.argument import Argument

    class _DummyProver:
        declarations = {}

    arg = Argument(_DummyProver(), "demo", "P")
    arg.body = Mutilde(DI("rootish", "P"), "P", DI("rootish", "P"), Laog("1", "P"))

    assert arg._synthetic_root_name() == "rootish"


def test_argument_eta_reduce_body_refreshes_enriched_proof_term():
    from core.dc.argument import Argument

    class _DummyProver:
        declarations = {}

    arg = Argument(_DummyProver(), "demo", "P")
    arg.body = Mutilde(DI("demo", "P"), "P", DI("demo", "P"), Laog("1", "P"))
    arg.enrich_props()
    arg.generate_proof_term()

    assert arg.enriched_proof_term == "μ'demo:P.<demo:P||1:P>"

    arg._eta_reduce_body()

    assert isinstance(arg.body, Laog)
    assert arg.enriched_proof_term == "1:P"