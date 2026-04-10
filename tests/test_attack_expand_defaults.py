from unittest.mock import patch

from core.ac.ast import Goal, Laog, ID, DI, Mu, Mutilde
from core.comp.reduce import ThetaExpander
from core.dc.argument import Argument

A = "P"


def _term_expander(*, expand_defaults: str = "also") -> ThetaExpander:
    return ThetaExpander(A, mode="term", expand_defaults=expand_defaults)


def _context_expander(*, expand_defaults: str = "also") -> ThetaExpander:
    return ThetaExpander(A, mode="context", expand_defaults=expand_defaults)


class _DummyProver:
    declarations = {}


def _mk_argument(body, *, name: str, conclusion: str = A):
    arg = object.__new__(Argument)
    arg.prover = _DummyProver()
    arg.name = name
    arg.conclusion = conclusion
    arg.executed = True
    arg.body = body
    arg.assumptions = {"1": {"prop": conclusion, "label": None}}
    return arg


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


def test_attack_rejects_default_only_when_expand_defaults_no():
    attacker = _mk_argument(Mu(ID("atk", A), A, Goal("1", A), Laog("atkctx", A)), name="attacker")
    target = _mk_argument(Goal("1", A), name="target")

    with patch.object(Argument, "_theta_expand", return_value=(target.body, False, False)):
        try:
            attacker.attack(target, expand_defaults="no")
            assert False, "expected ValueError"
        except ValueError as e:
            assert "expand_defaults=no" in str(e)


def test_attack_rejects_already_exposed_target_when_theta_expand_noops():
    attacker = _mk_argument(Mu(ID("atk", A), A, Goal("1", A), Laog("atkctx", A)), name="attacker")
    target = _mk_argument(Goal("1", A), name="target")

    with patch.object(Argument, "_theta_expand", return_value=(target.body, True, False)):
        try:
            attacker.attack(target, expand_defaults="only")
            assert False, "expected ValueError"
        except ValueError as e:
            assert "already in exposed form" in str(e)


def test_support_rejects_nondefault_only_when_expand_defaults_only():
    supporter = _mk_argument(Mu(ID("sup", A), A, Goal("1", A), Laog("supctx", A)), name="supporter")
    target = _mk_argument(Goal("1", A), name="target")

    with patch.object(Argument, "_theta_expand", return_value=(target.body, False, False)):
        try:
            supporter.support(target, expand_defaults="only")
            assert False, "expected ValueError"
        except ValueError as e:
            assert "expand_defaults=only" in str(e)


def test_support_rejects_already_exposed_target_when_theta_expand_noops():
    supporter = _mk_argument(Mu(ID("sup", A), A, Goal("1", A), Laog("supctx", A)), name="supporter")
    target = _mk_argument(Goal("1", A), name="target")

    with patch.object(Argument, "_theta_expand", return_value=(target.body, True, False)):
        try:
            supporter.support(target, expand_defaults="no")
            assert False, "expected ValueError"
        except ValueError as e:
            assert "already in exposed form" in str(e)


def test_undergird_delegates_to_support_only():
    supporter = _mk_argument(Mu(ID("sup", A), A, Goal("1", A), Laog("supctx", A)), name="supporter")
    target = _mk_argument(Goal("1", A), name="target")

    with patch.object(Argument, "support", return_value="ok") as mocked:
        out = supporter.undergird(target, name="u1", on=A)

    assert out == "ok"
    mocked.assert_called_once_with(target, name="u1", on=A, expand_defaults="only")


def test_reinforce_delegates_to_support_no():
    supporter = _mk_argument(Mu(ID("sup", A), A, Goal("1", A), Laog("supctx", A)), name="supporter")
    target = _mk_argument(Goal("1", A), name="target")

    with patch.object(Argument, "support", return_value="ok") as mocked:
        out = supporter.reinforce(target, name="r1", on=A)

    assert out == "ok"
    mocked.assert_called_once_with(target, name="r1", on=A, expand_defaults="no")
