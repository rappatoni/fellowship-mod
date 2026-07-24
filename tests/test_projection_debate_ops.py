try:
    import pytest
except ModuleNotFoundError:
    class _Raises:
        def __init__(self, expected, match=None):
            self.expected = expected
            self.match = match

        def __enter__(self):
            return self

        def __exit__(self, exc_type, exc, tb):
            if exc_type is None:
                raise AssertionError(f"expected {self.expected.__name__}")
            if not issubclass(exc_type, self.expected):
                return False
            if self.match is not None and self.match not in str(exc):
                raise AssertionError(f"expected message containing {self.match!r}, got {exc!r}")
            return True

    class _PytestFallback:
        @staticmethod
        def raises(expected, match=None):
            return _Raises(expected, match=match)

    pytest = _PytestFallback()

from core.ac.ast import Cons, DI, Goal, ID, Laog, Mu, Mutilde, Sonc
from core.dc.argument import Argument
from wrap.cli import _parse_projection_debate_command, projection_debate_cmd


class _FakeProver:
    def __init__(self):
        self.declarations = {}
        self.arguments = {}

    def get_argument(self, name):
        return self.arguments.get(name)

    def register_argument(self, argument):
        self.arguments[argument.name] = argument


def _arg(name, body, conclusion="A"):
    prover = _FakeProver()
    argument = Argument(prover, name, conclusion)
    argument.body = body
    argument.executed = True
    argument.assumptions = {"old": {"prop": conclusion, "label": None}}
    prover.register_argument(argument)
    return prover, argument


def _alt_pair(name="alt", prop="A", left=None, right=None):
    left = left or DI("case1", prop)
    right = right or DI("case2", prop)
    return Mu(
        ID(name, prop),
        prop,
        Mu(ID("_", prop), prop, left, ID(name, prop)),
        Mutilde(DI("_", prop), prop, right, ID(name, prop)),
    )


def _counterexample_pair(name="alt", prop="A", left=None, right=None):
    left = left or ID("cond1", prop)
    right = right or ID("cond2", prop)
    return Mutilde(
        DI(name, prop),
        prop,
        Mu(ID("_", prop), prop, DI(name, prop), left),
        Mutilde(DI("_", prop), prop, DI(name, prop), right),
    )


def test_out_extracts_indexed_alt_term_and_marks_unexecuted():
    case1 = DI("case1", "A")
    case2 = DI("case2", "A")
    _, argument = _arg("arg", _alt_pair(left=case1, right=case2))

    projected = argument.out(1, name="picked")

    assert projected.name == "picked"
    assert isinstance(projected.body, DI)
    assert projected.body.name == "case2"
    assert projected.conclusion == "A"
    assert projected.executed is False
    assert projected.assumptions == argument.assumptions
    assert projected.body is not case2


def test_out_non_alt_index_zero_copies_term_and_larger_index_errors():
    _, argument = _arg("arg", DI("leaf", "A"))

    projected = argument.out(0, name="copy")

    assert isinstance(projected.body, DI)
    assert projected.body.name == "leaf"
    with pytest.raises(IndexError, match="index out of range"):
        argument.out(1, name="bad")


def test_tou_extracts_indexed_counterexample_context():
    cond1 = ID("cond1", "A")
    cond2 = ID("cond2", "A")
    _, argument = _arg("arg", _counterexample_pair(left=cond1, right=cond2))

    projected = argument.tou(0, name="picked")

    assert projected.name == "picked"
    assert isinstance(projected.body, ID)
    assert projected.body.name == "cond1"
    assert projected.conclusion == "A"
    assert projected.executed is False


def test_sub_and_bus_project_application_conditions():
    function = DI("f", "B->A")
    argument_term = DI("v", "B")
    _, application_arg = _arg("app", Mu(ID("x", "A"), "A", function, Cons(argument_term, ID("x", "A"))))

    sub = application_arg.sub("subarg")

    assert isinstance(sub.body, DI)
    assert sub.body.name == "v"
    assert sub.conclusion == "B"

    condition = ID("e", "B")
    warrant = ID("f", "B->A")
    _, dual_arg = _arg("dual", Mutilde(DI("x", "A"), "A", Sonc(condition, DI("x", "A")), warrant))

    bus = dual_arg.bus("busarg")

    assert isinstance(bus.body, ID)
    assert bus.body.name == "e"
    assert bus.conclusion == "B"


def test_attacker_and_regatta_project_exception_and_support():
    support = DI("support", "A")
    exception = ID("exception", "A")
    _, warrant_arg = _arg(
        "w",
        Mu(
            ID("w", "A"),
            "A",
            Mu(ID("_", "A"), "A", support, ID("w", "A")),
            Mutilde(DI("_", "A"), "A", DI("support", "A"), exception),
        ),
    )

    attacker = warrant_arg.attacker("att")

    assert isinstance(attacker.body, ID)
    assert attacker.body.name == "exception"

    requirement = ID("requirement", "A")
    _, dual_warrant_arg = _arg(
        "dw",
        Mutilde(
            DI("w", "A"),
            "A",
            Mu(ID("_", "A"), "A", support, requirement),
            Mutilde(DI("_", "A"), "A", DI("w", "A"), ID("requirement", "A")),
        ),
    )

    regatta = dual_warrant_arg.regatta("reg")

    assert isinstance(regatta.body, DI)
    assert regatta.body.name == "support"


def test_removed_outer_binder_occurrences_become_open_leaves():
    _, argument = _arg("arg", _alt_pair(name="x", left=DI("x", "A"), right=DI("case2", "A")))

    projected = argument.out(0, name="open")

    assert isinstance(projected.body, Goal)
    assert projected.body.prop == "A"
    assert projected.body.number.startswith("g")

    _, counter = _arg("counter", _counterexample_pair(name="x", left=ID("x", "A"), right=ID("cond2", "A")))

    projected_context = counter.tou(0, name="openctx")

    assert isinstance(projected_context.body, Laog)
    assert projected_context.body.prop == "A"
    assert projected_context.body.number.startswith("l")


def test_projection_operators_ignore_outer_eta_wrappers():
    wrapped_alt = Mu(
        ID("eta", "A"),
        "A",
        _alt_pair(left=DI("case1", "A"), right=DI("case2", "A")),
        ID("eta", "A"),
    )
    _, wrapped_arg = _arg("wrapped", wrapped_alt)

    projected = wrapped_arg.out(1, name="picked")

    assert isinstance(projected.body, DI)
    assert projected.body.name == "case2"

    wrapped_counterexample = Mutilde(
        DI("eta", "A"),
        "A",
        DI("eta", "A"),
        _counterexample_pair(left=ID("cond1", "A"), right=ID("cond2", "A")),
    )
    _, wrapped_counter = _arg("wrapped_counter", wrapped_counterexample)

    projected_context = wrapped_counter.tou(1, name="picked_context")

    assert isinstance(projected_context.body, ID)
    assert projected_context.body.name == "cond2"

    application = Mu(
        ID("x", "A"),
        "A",
        DI("f", "B->A"),
        Cons(DI("v", "B"), ID("x", "A")),
    )
    wrapped_application = Mu(ID("eta", "A"), "A", application, ID("eta", "A"))
    _, wrapped_application_arg = _arg("wrapped_application", wrapped_application)

    sub = wrapped_application_arg.sub("sub")

    assert isinstance(sub.body, DI)
    assert sub.body.name == "v"


def test_projection_rejects_missing_props_and_wrong_structures():
    _, no_prop = _arg("arg", DI("leaf", None))
    with pytest.raises(ValueError, match="projected body has no proposition"):
        no_prop.out(0, name="bad")

    _, context_arg = _arg("ctx", ID("ctx", "A"))
    with pytest.raises(ValueError, match="top level is not an `AltStructure`"):
        context_arg.out(0, name="bad")

    _, term_arg = _arg("term", DI("term", "A"))
    with pytest.raises(ValueError, match="top level is not an `AlternativeCounterexampleStructure`"):
        term_arg.tou(0, name="bad")


def test_cli_projection_command_parses_both_orders_and_registers():
    prover, argument = _arg("arg", DI("leaf", "A"))

    assert _parse_projection_debate_command(prover, "out 0 arg picked") == ("out", "arg", "picked", 0)
    assert _parse_projection_debate_command(prover, "out picked arg 0") == ("out", "arg", "picked", 0)
    assert _parse_projection_debate_command(prover, "sub arg subarg") == ("sub", "arg", "subarg", None)
    assert _parse_projection_debate_command(prover, "sub subarg arg") == ("sub", "arg", "subarg", None)

    result = projection_debate_cmd(prover, "out 0 arg picked")

    assert result.name == "picked"
    assert prover.get_argument("picked") is result
    assert argument.executed is True


if __name__ == "__main__":
    for _name, _fn in sorted(globals().items()):
        if _name.startswith("test_") and callable(_fn):
            _fn()
    print("projection debate op tests passed")