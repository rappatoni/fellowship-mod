"""Wrapper-level decorations: the `decorate` command, rendering, and replay."""

from core.ac.ast import DI, ID, Mu
from pres.decorations import parse_decorate_command, render_declaration, render_prop
from pres.nl import natural_language_argumentative_rendering, pretty_natural
from pres.tree import render_acceptance_tree_dot
from wrap.cli import execute_script


def test_decoration_command_parser_and_compositional_rendering():
    assert parse_decorate_command("decorate Bird : '@arg1 is a bird'.") == ("Bird", "@arg1 is a bird")

    declarations = {
        "A": "bool",
        "B": "bool",
        "ax": "A -> B",
        "bird_tweety": "Bird Tweety",
    }
    decorations = {
        "A": "Foo",
        "B": "Bar",
        "ax": "If @arg1 then @arg2",
        "Bird": "@arg1 is a bird",
    }

    assert render_prop("Bird Tweety", declarations, decorations) == "Tweety is a bird"
    assert render_declaration("bird_tweety", declarations, decorations) == "Tweety is a bird"
    assert render_declaration("ax", declarations, decorations) == "If Foo then Bar"
    assert render_prop("A -> B", declarations, decorations, {"->": "@left impliziert @right"}) == "Foo impliziert Bar"


def test_declaration_names_are_not_implicit_decorations_for_their_types():
    declarations = {"A": "bool", "a": "A"}

    assert render_prop("A", declarations, {}) == "A"
    assert render_declaration("a", declarations, {}) == "A"
    assert render_prop("A -> A", declarations, {}, {"->": "@left impliziert @right"}) == "A impliziert A"

    assert render_prop("A", declarations, {"a": "Decorated a"}) == "A"
    assert render_prop("A", declarations, {"A": "Decorated A"}) == "Decorated A"
    assert render_declaration("a", declarations, {"a": "Decorated a"}) == "Decorated a"
    assert render_declaration("a", declarations, {"A": "Decorated A", "a": "Decorated a"}) == "Decorated a"


def test_natural_language_and_tree_renderers_consume_decorations():
    body = Mu(ID("alpha", "Bird Tweety"), "Bird Tweety", DI("bird_tweety", "Bird Tweety"), ID("alpha", "Bird Tweety"))
    declarations = {"bird_tweety": "Bird Tweety"}
    decorations = {"Bird": "@arg1 is a bird", "Tweety": "Tweety"}

    rendered = pretty_natural(
        body,
        natural_language_argumentative_rendering,
        declarations=declarations,
        decorations=decorations,
    )
    dot = render_acceptance_tree_dot(
        body,
        label_mode="nl",
        declarations=declarations,
        decorations=decorations,
    )

    assert "Tweety is a bird" in rendered
    assert "Bird Tweety" not in rendered
    assert "Tweety is a bird" in dot


def test_execute_script_handles_decorate_wrapper_only(tmp_path):
    class FakeProver:
        def __init__(self):
            import threading
            self.echo_notes = False
            self.decorations = {}
            self.commands = []
            self.lock = threading.RLock()

        def register_decoration(self, name, template):
            self.decorations[name] = template

        def send_command(self, command, **kwargs):
            self.commands.append(command)
            return {}

    script = tmp_path / "decorations.fspy"
    script.write_text(
        "decorate Bird : '@arg1 is a bird'.\n"
        'decorate Bird_list : "@arg1 ist eine \\\\sn{Liste}".\n'
        "declare A:bool.\n"
    )
    prover = FakeProver()

    execute_script(prover, str(script), isolate=False)

    assert prover.decorations == {
        "Bird": "@arg1 is a bird",
        "Bird_list": "@arg1 ist eine \\sn{Liste}",
    }
    assert prover.commands == ["declare A:bool."]
