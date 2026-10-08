
from core.ac.ast import Goal, Laog, ID, DI, Mu, Mutilde
from core.comp.reduce import ThetaExpander

A = "P"


def _term_expander(*, expand_defaults: str = "also") -> ThetaExpander:
    return ThetaExpander(A, mode="term", expand_defaults=expand_defaults)


def _context_expander(*, expand_defaults: str = "also") -> ThetaExpander:
    return ThetaExpander(A, mode="context", expand_defaults=expand_defaults)


def test_thetaexpander_only_expands_plain_term_default():
    expr = Goal("1", A)

    expander = _term_expander(expand_defaults="only")
    out = expander.visit(expr)

    assert expander.found_target is True
    assert expander.changed is True
    assert isinstance(out, Mu)
    assert getattr(out, "prop", None) == A


def test_thetaexpander_no_skips_plain_term_default():
    expr = Goal("1", A)

    expander = _term_expander(expand_defaults="no")
    out = expander.visit(expr)

    assert expander.found_target is False
    assert expander.changed is False
    assert isinstance(out, Goal)


def test_thetaexpander_only_expands_bureaucratic_term_default():
    expr = Mu(
        ID("alpha", A),
        A,
        Mu(ID("_", A), A, Goal("1", A), ID("alpha", A)),
        Laog("L", A),
    )

    expander = _term_expander(expand_defaults="only")
    out = expander.visit(expr)

    assert expander.found_target is True
    assert expander.changed is True
    assert isinstance(out, Mu)
    assert getattr(out, "prop", None) == A
    assert isinstance(out.term, Mu)
    assert isinstance(out.term.term, Mu)


def test_thetaexpander_no_skips_bureaucratic_term_default():
    expr = Mu(
        ID("alpha", A),
        A,
        Mu(ID("_", A), A, Goal("1", A), ID("alpha", A)),
        Laog("L", A),
    )

    expander = _term_expander(expand_defaults="no")
    out = expander.visit(expr)

    assert expander.found_target is False
    assert expander.changed is False
    assert isinstance(out, Mu)
    assert getattr(out, "pres", None) == getattr(expr, "pres", None)


def test_thetaexpander_only_skips_nondefault_term_target():
    expr = Mu(
        ID("alpha", A),
        A,
        Mu(ID("_", A), A, Goal("1", A), ID("alpha", A)),
        Mutilde(DI("gamma", A), A, DI("ctx", A), Laog("L", A)),
    )

    expander = _term_expander(expand_defaults="only")
    out = expander.visit(expr)

    assert expander.found_target is True
    assert expander.changed is True
    assert isinstance(out, Mu)


def test_thetaexpander_no_expands_nondefault_term_target():
    expr = Mu(
        ID("alpha", A),
        A,
        Mu(ID("_", A), A, Goal("1", A), ID("alpha", A)),
        Mutilde(DI("gamma", A), A, DI("ctx", A), Laog("L", A)),
    )

    expander = _term_expander(expand_defaults="no")
    out = expander.visit(expr)

    assert expander.found_target is True
    assert expander.changed is True
    assert isinstance(out, Mu)


def test_thetaexpander_only_expands_plain_context_default():
    expr = Laog("1", A)

    expander = _context_expander(expand_defaults="only")
    out = expander.visit(expr)

    assert expander.found_target is True
    assert expander.changed is True
    assert isinstance(out, Mutilde)
    assert getattr(out, "prop", None) == A


def test_thetaexpander_no_skips_plain_context_default():
    expr = Laog("1", A)

    expander = _context_expander(expand_defaults="no")
    out = expander.visit(expr)

    assert expander.found_target is False
    assert expander.changed is False
    assert isinstance(out, Laog)


def test_thetaexpander_only_expands_bureaucratic_context_default():
    expr = Mutilde(
        DI("alpha", A),
        A,
        Goal("1", A),
        Mutilde(DI("_", A), A, DI("alpha", A), Laog("L", A)),
    )

    expander = _context_expander(expand_defaults="only")
    out = expander.visit(expr)

    assert expander.found_target is True
    assert expander.changed is True
    assert isinstance(out, Mutilde)
    assert getattr(out, "prop", None) == A
    assert isinstance(out.context, Mutilde)
    assert isinstance(out.context.context, Mutilde)


def test_thetaexpander_no_skips_pure_default_context_target():
    expr = Mutilde(
        DI("alpha", A),
        A,
        Mu(ID("gamma", A), A, Goal("2", A), Laog("ctx", A)),
        Mutilde(DI("_", A), A, DI("alpha", A), Laog("L", A)),
    )

    expander = _context_expander(expand_defaults="no")
    out = expander.visit(expr)

    assert expander.found_target is False
    assert expander.changed is False
    assert isinstance(out, Mutilde)


def test_thetaexpander_strict_off_skips_declared_strict_proof():
    expr = Mu(ID("alpha", A), A, DI("strict_axiom", A), ID("alpha", A))

    expander = ThetaExpander(
        A,
        mode="term",
        expand_defaults="no",
        allow_strict=False,
        strict_names={"strict_axiom"},
    )
    out = expander.visit(expr)

    assert expander.found_target is False
    assert expander.changed is False
    assert isinstance(out, Mu)


def test_thetaexpander_strict_on_exposes_declared_strict_proof():
    expr = Mu(ID("alpha", A), A, DI("strict_axiom", A), ID("alpha", A))

    expander = ThetaExpander(
        A,
        mode="term",
        expand_defaults="no",
        allow_strict=True,
        strict_names={"strict_axiom"},
    )
    out = expander.visit(expr)

    assert expander.found_target is True
    assert expander.changed is True
    assert isinstance(out, Mu)


def test_thetaexpander_strict_on_ignores_undeclared_leaf():
    expr = Mu(ID("alpha", A), A, DI("ordinary_leaf", A), ID("alpha", A))

    expander = ThetaExpander(
        A,
        mode="term",
        expand_defaults="only",
        allow_strict=True,
        strict_names={"strict_axiom"},
    )
    out = expander.visit(expr)

    assert expander.found_target is False
    assert expander.changed is False
    assert isinstance(out, Mu)
