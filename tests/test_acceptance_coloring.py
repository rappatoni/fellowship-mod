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

import ast
import shlex
from pathlib import Path

from core.ac.ast import Deleg, DI, Geled, Goal, ID, Laog, Mu, Mutilde
from core.comp.color import AcceptanceColoringVisitor, pretty_colored_proof_term


RED = "\x1b[31m"
GREEN = "\x1b[32m"
YELLOW = "\x1b[33m"
RESET = "\x1b[0m"


def test_parametric_coloring_colors_goal_laog_deleg_geled_by_prop():
    visitor = AcceptanceColoringVisitor(
        prop_colors=[("A", "red"), ("B", "green"), ("C", "yellow"), ("D", "red")]
    )

    assert visitor.visit(Goal("g", "A")) == f"{RED}g:A{RESET}"
    assert visitor.visit(Laog("l", "B")) == f"{GREEN}l:B{RESET}"
    assert visitor.visit(Deleg("d", "C")) == f"{YELLOW}?d:C{RESET}"
    assert visitor.visit(Geled("e", "D")) == f"{RED}?e:D{RESET}"


def test_parametric_coloring_affects_composite_classification():
    pt = Mu(ID("x", "A"), "A", Goal("g", "A"), ID("x", "A"))

    rendered = pretty_colored_proof_term(pt, prop_colors=[("A", "red")])

    assert rendered.startswith(GREEN)
    assert f"{RED}g:A{RESET}" in rendered


def test_parametric_coloring_supports_deleg_geled_as_open_leaves():
    term_open = Mu(ID("x", "A"), "A", Deleg("d", "A"), ID("x", "A"))
    context_open = Mutilde(DI("x", "B"), "B", DI("y", "B"), Geled("e", "B"))

    assert AcceptanceColoringVisitor(prop_colors=[("A", "red")]).classify(term_open) == "green"
    assert AcceptanceColoringVisitor(prop_colors=[("B", "green")]).classify(context_open) == "red"


def test_parametric_coloring_rejects_unknown_colors():
    with pytest.raises(ValueError, match="Unknown acceptance colors"):
        AcceptanceColoringVisitor(prop_colors=[("A", "blue")])


def test_cli_color_mapping_parser_accepts_quoted_props_and_rejects_bad_mappings():
    source = Path("wrap/cli.py").read_text(encoding="utf-8")
    module = ast.parse(source)
    parser_defs = [
        node
        for node in module.body
        if isinstance(node, ast.FunctionDef) and node.name == "_parse_color_argument_spec"
    ]
    assert parser_defs

    namespace = {"shlex": shlex}
    exec(compile(ast.Module(body=parser_defs, type_ignores=[]), "wrap/cli.py", "exec"), namespace)
    parse = namespace["_parse_color_argument_spec"]

    assert parse("arg A=red B=green") == ("arg", [("A", "red"), ("B", "green")])
    assert parse('arg "Bird Tweety=yellow"') == ("arg", [("Bird Tweety", "yellow")])
    assert parse("arg") == ("arg", None)
    with pytest.raises(ValueError, match="Invalid color mapping"):
        parse("arg malformed")


if __name__ == "__main__":
    for _name, _fn in sorted(globals().items()):
        if _name.startswith("test_") and callable(_fn):
            _fn()
    print("acceptance coloring tests passed")