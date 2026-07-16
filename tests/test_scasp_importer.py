import json
import re
import subprocess

from core.ac.ast import Cons, DI, Goal, ID, Mu, Mutilde, Sonc
from core.ac.instructions import InstructionsGenerationVisitor
from core.ac.prop_render import prop_to_command
from pres.gen import ProofTermGenerationVisitor
from pres.nl import natural_language_argumentative_rendering, pretty_natural
from pres.tree import render_acceptance_tree_dot
from pres.decorations import parse_decorate_command, render_declaration, render_prop
from scasp_import import importer
from scasp_import.importer import atom_to_prop, run_scasp_json, translate_json
from wrap.cli import _handle_import_command, execute_script


def _answer_tree(*trees):
    return {"answers": [{"tree": list(trees)}]}


def _answers(*answer_trees, query="a"):
    return {
        "query": [{"type": "atom", "value": query}],
        "answers": [{"tree": list(trees)} for trees in answer_trees],
    }


def _atom(name, children=None):
    return {
        "node": {"type": "atom", "value": name},
        "children": list(children or []),
    }


def _compound(functor, args, children=None):
    return {
        "node": {"type": "compound", "functor": functor, "args": list(args)},
        "children": list(children or []),
    }


def _const(name):
    return {"type": "atom", "value": name}


def _var(name):
    return {"type": "var", "name": name}


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


def test_scasp_display_metadata_emits_wrapper_decorations():
    payload = _answer_tree(
        _compound(
            "bird",
            [_const("tweety")],
            children=[],
        )
    )
    payload["answers"][0]["tree"][0]["display"] = {"text": "@X is a bird", "type": "pred"}

    result = translate_json(payload)

    assert result.decorations == {"Bird": "@arg1 is a bird"}
    script = result.to_fspy(name="birds")
    assert 'decorate Bird : "@arg1 is a bird"' in script
    assert "register birds : Bird[Tweety] := μ" in script


def test_scasp_non_pred_display_metadata_does_not_emit_decorations():
    payload = _answer_tree(_atom("a"))
    payload["answers"][0]["tree"][0]["display"] = {"text": "a", "type": "mid"}

    result = translate_json(payload)

    assert result.decorations == {}
    script = result.to_fspy(name="plain_bool")
    assert "decorate A" not in script
    assert "declare A:bool." in script
    assert "declare a:(A)." in script


def test_scasp_display_markup_is_simplified_to_positional_template():
    payload = _answer_tree(
        _compound(
            "bird_list",
            [[_const("tweety"), _const("clumsy")]],
            children=[],
        )
    )
    payload["answers"][0]["tree"][0]["display"] = {
        # This is the runtime string after JSON has unescaped "\\sn" and "\\sr".
        "text": "@Y ist eine \\sn{Liste} von \\sr{Vogel}{Vögeln.}",
        "type": "pred",
    }

    result = translate_json(payload)

    assert result.decorations == {"Bird_list": "@arg1 ist eine \\sn{Liste} von \\sr{Vogel}{Vögeln.}"}

    decorate_line = next(line for line in result.to_fspy(name="birds").splitlines() if line.startswith("decorate "))
    assert decorate_line == 'decorate Bird_list : "@arg1 ist eine \\\\sn{Liste} von \\\\sr{Vogel}{Vögeln.}"'
    assert parse_decorate_command(decorate_line) == (
        "Bird_list",
        "@arg1 ist eine \\sn{Liste} von \\sr{Vogel}{Vögeln.}",
    )


def test_execute_script_handles_decorate_wrapper_only(tmp_path):
    class FakeProver:
        def __init__(self):
            self.echo_notes = False
            self.decorations = {}
            self.commands = []

        def register_decoration(self, name, template):
            self.decorations[name] = template

        def send_command(self, command):
            self.commands.append(command)
            return {}

    script = tmp_path / "decorations.fspy"
    script.write_text(
        "decorate Bird : '@arg1 is a bird'.\n"
        'decorate Bird_list : "@arg1 ist eine \\\\sn{Liste}"\n'
        "declare A:bool.\n"
    )
    prover = FakeProver()

    execute_script(prover, str(script), isolate=False)

    assert prover.decorations == {
        "Bird": "@arg1 is a bird",
        "Bird_list": "@arg1 ist eine \\sn{Liste}",
    }
    assert prover.commands == ["declare A:bool."]


def test_atom_to_prop_name_mapping():
    assert atom_to_prop("a") == "A"
    assert atom_to_prop("efficientmetro") == "Efficientmetro"
    assert atom_to_prop("efficientMetro") == "EfficientMetro"


def test_translate_fact_generates_replayable_leaf_and_declarations():
    result = translate_json(_answer_tree(_atom("a")))

    assert result.conclusion == "A"
    assert result.bools == {"A"}
    assert result.declarations == {"a": "A"}
    assert result.denials == {}
    assert result.setup_commands() == ["lk.", "declare A:bool.", "declare a:(A)."]

    body = result.body
    assert isinstance(body, Mu)
    assert body.prop == "A"
    assert body.id.name == "alpha1"
    assert isinstance(body.term, DI)
    assert body.term.name == "a"
    assert isinstance(body.context, ID)
    assert body.context.name == "alpha1"



def test_translate_single_child_generates_function_declaration_and_cons_context():
    result = translate_json(_answer_tree(_atom("a", [_atom("b")])))

    assert result.conclusion == "A"
    assert result.bools == {"A", "B"}
    assert result.declarations == {"b": "B", "f_B_A": "B -> A"}
    assert result.denials == {}

    body = result.body
    assert isinstance(body, Mu)
    assert body.id.name == "alpha2"
    assert isinstance(body.term, DI)
    assert body.term.name == "f_B_A"
    assert isinstance(body.context, Cons)
    assert isinstance(body.context.term, Mu)
    assert body.context.term.prop == "B"
    assert body.context.term.id.name == "alpha1"
    assert isinstance(body.context.context, ID)
    assert body.context.context.name == "alpha2"



def test_translate_multi_child_generates_helper_bool_denials_and_barred_adapters():
    result = translate_json(_answer_tree(_atom("a", [_atom("b"), _atom("c")])))

    assert result.conclusion == "A"
    assert result.bools == {"A", "B", "C", "H_A"}
    assert result.declarations == {
        "b": "B",
        "c": "C",
        "f_H_A_A": "H_A -> A",
    }
    assert result.denials == {
        "h_H_A_B": "H_A-B",
        "h_H_A_C": "H_A-C",
    }

    body = result.body
    assert isinstance(body, Mu)
    root_binder = body.id.name
    assert root_binder.startswith("alpha")
    assert isinstance(body.term, DI)
    assert body.term.name == "f_H_A_A"
    assert isinstance(body.context, Cons)
    assert isinstance(body.context.term, Mu)
    assert body.context.term.prop == "H_A"
    assert isinstance(body.context.term.context, Mutilde)

    list_minus = body.context.term.context.context
    assert isinstance(list_minus, Mutilde)
    left_bar = list_minus.term.context
    assert isinstance(left_bar, Mutilde)
    assert isinstance(left_bar.term, Sonc)
    assert isinstance(left_bar.context, ID)
    assert left_bar.context.name == "h_H_A_B"

    rendered = ProofTermGenerationVisitor().visit(body).pres
    assert rendered.startswith(f"μ{root_binder}:A.<f_H_A_A:H_A -> A||")
    assert "h_H_A_B:H_A-B" in rendered
    assert "h_H_A_C:H_A-C" in rendered

    replay_rendered = ProofTermGenerationVisitor(verbose=-1).visit(body).pres
    assert replay_rendered.startswith(f"μ{root_binder}:A.<f_H_A_A||")
    assert "h_H_A_B:H_A-B" not in replay_rendered
    assert "h_H_A_C:H_A-C" not in replay_rendered
    assert re.search(r"μ'?_\d", replay_rendered) is None
    assert "μ_:" in replay_rendered
    assert "μ'_:" in replay_rendered
    assert "!5:H_A" in replay_rendered
    assert "3:B!" in replay_rendered
    assert "1:C!" in replay_rendered

    assert result.setup_commands() == [
        "lk.",
        "declare A,B,C,H_A:bool.",
        "declare b:(B).",
        "declare c:(C).",
        "declare f_H_A_A:(H_A -> A).",
        "deny h_H_A_B:(H_A-B).",
        "deny h_H_A_C:(H_A-C).",
    ]


def test_translate_json_combines_multiple_answers_and_warns_for_global_constraint_and_extra_roots():
    result = translate_json(
        {
            "answers": [
                {
                    "tree": [
                        _atom("o_nmr_check"),
                        _atom("a"),
                        _atom("b"),
                    ]
                },
                {"tree": [_atom("a", [_atom("c")])]},
            ]
        }
    )

    assert result.conclusion == "A"
    assert result.bools == {"A", "C"}
    assert result.declarations == {"a": "A", "c": "C", "f_C_A": "C -> A"}
    assert any("o_nmr_check" in warning for warning in result.warnings)
    assert any("additional top-level" in warning for warning in result.warnings)

    body = result.body
    assert isinstance(body, Mu)
    assert body.prop == "A"
    assert isinstance(body.term, Mu)
    assert body.term.prop == "A"
    assert isinstance(body.context, Mutilde)
    assert body.context.prop == "A"


def test_translate_json_empty_answers_imports_empty_positive_enum_from_query():
    result = translate_json({"query": [{"type": "atom", "value": "a"}], "answers": []})

    assert result.conclusion == "A"
    assert result.bools == {"A"}
    assert result.declarations == {}
    assert result.denials == {}
    assert any("no answers" in warning for warning in result.warnings)

    body = result.body
    assert isinstance(body, Mu)
    assert body.prop == "A"
    assert isinstance(body.term, Goal)
    assert body.term.prop == "A"
    assert isinstance(body.context, ID)
    assert result.proof_term_string() == "μalpha1:A.<?1:A||alpha1>"


def test_translate_json_empty_answers_requires_single_positive_atom_query():
    invalid_payloads = [
        {"answers": []},
        {"query": [], "answers": []},
        {"query": [{"type": "atom", "value": "a"}, {"type": "atom", "value": "b"}], "answers": []},
        {"query": [{"type": "compound", "value": "a"}], "answers": []},
    ]

    for payload in invalid_payloads:
        try:
            translate_json(payload)
        except importer.ScaspImportError as exc:
            assert "positive atom query" in str(exc)
        else:
            raise AssertionError("Expected ScaspImportError")


def test_translate_json_errors_for_only_global_constraint_answer():
    try:
        translate_json(_answers([_atom("o_nmr_check")]))
    except importer.ScaspImportError as exc:
        assert "only o_nmr_check" in str(exc)
    else:
        raise AssertionError("Expected ScaspImportError")


def test_translate_json_errors_for_mismatched_answer_conclusions():
    try:
        translate_json(_answers([_atom("a")], [_atom("b")]))
    except importer.ScaspImportError as exc:
        assert "concludes 'B', expected 'A'" in str(exc)
    else:
        raise AssertionError("Expected ScaspImportError")


def test_translate_json_omits_unsupported_children_with_warning():
    unsupported = {
        "node": {"type": "compound", "functor": "not", "args": [{"type": "atom", "value": "b"}]},
        "children": [],
    }

    result = translate_json(_answer_tree(_atom("a", [unsupported])))

    assert result.conclusion == "A"
    assert result.declarations == {"a": "A"}
    assert any("unsupported child" in warning for warning in result.warnings)


def test_prop_to_command_renders_predicate_applications_and_connectives():
    assert prop_to_command("Child Bob") == "Child[Bob]"
    assert prop_to_command("Father Rich Bob") == "Father[Rich][Bob]"
    assert prop_to_command("Child Bob -> Orphan Bob") == "Child[Bob] -> Orphan[Bob]"
    assert prop_to_command("H_Orphan Bob-Child Bob") == "H_Orphan[Bob]-Child[Bob]"


def test_translate_orphans_json_imports_structured_predicates():
    payload = _answer_tree(
        _compound("orphan", [_const("bob")], [
            _compound("child", [_const("bob")]),
            _compound("parents_dead", [_const("bob")], [
                _compound("father", [_const("rich"), _const("bob")]),
                _compound("mother", [_const("patty"), _const("bob")]),
                _compound("dead", [_const("rich")]),
                _compound("dead", [_const("patty")]),
            ]),
        ]),
        _atom("o_nmr_check"),
    )

    result = translate_json(payload)

    assert result.conclusion == "Orphan Bob"
    assert result.constants == {"Bob", "Patty", "Rich"}
    assert result.predicates == {
        "Child": 1,
        "Dead": 1,
        "Father": 2,
        "Mother": 2,
        "Orphan": 1,
        "Parents_dead": 1,
    }
    assert result.declarations["child_bob"] == "Child Bob"
    assert result.declarations["father_rich_bob"] == "Father Rich Bob"
    assert result.declarations["mother_patty_bob"] == "Mother Patty Bob"
    assert result.declarations["dead_rich"] == "Dead Rich"
    assert result.declarations["dead_patty"] == "Dead Patty"
    assert any("o_nmr_check" in warning for warning in result.warnings)

    setup = result.setup_commands()
    assert "declare iota:type." in setup
    assert "declare Bob,Patty,Rich:iota." in setup
    assert "declare Father:iota -> iota -> bool." in setup
    assert "declare child_bob:(Child[Bob])." in setup
    assert "declare father_rich_bob:(Father[Rich][Bob])." in setup

    rendered = result.proof_term_string()
    assert "μalpha1:Child Bob.<child_bob||alpha1>" in rendered
    assert "Child[Bob]" not in rendered

    script = result.to_fspy(name="orphans")
    assert "register orphans : Orphan[Bob] := μ" in script
    assert "μalpha1:Child Bob.<child_bob||alpha1>" in script


def test_translate_tree_rejects_variable_terms():
    try:
        translate_json(_answer_tree(_compound("child", [_var("X")])))
    except importer.ScaspImportError as exc:
        assert "non-ground" in str(exc)
    else:
        raise AssertionError("Expected ScaspImportError")


def test_translate_structured_equality_and_inequality_atoms():
    result = translate_json(
        _answer_tree(
            _compound("=", [_const("bob"), _const("bob")], [
                _compound("!=", [_const("bob"), _const("mary")])
            ])
        )
    )

    assert result.conclusion == "Eq Bob Bob"
    assert result.constants == {"Bob", "Mary"}
    assert result.predicates == {"Eq": 2, "Neq": 2}
    assert "declare Eq:iota -> iota -> bool." in result.setup_commands()
    assert "declare Neq:iota -> iota -> bool." in result.setup_commands()
    assert "register imported : Eq[Bob][Bob] := μ" in result.to_fspy()


def test_instruction_generation_renders_predicate_cut_commands_with_brackets():
    body = Mu(
        ID("alpha", "Orphan Bob"),
        "Orphan Bob",
        DI("f_Child_Bob_Orphan_Bob", "Child Bob -> Orphan Bob"),
        ID("alpha", "Orphan Bob"),
    )
    body.contr = "Child Bob -> Orphan Bob"

    instructions = list(InstructionsGenerationVisitor().return_instructions(body))

    assert any(instr == "cut (Child[Bob] -> Orphan[Bob]) alpha" for instr in instructions)


def test_run_scasp_json_accepts_nonzero_exit_when_json_exists(tmp_path, monkeypatch):
    source = tmp_path / "program.pl"
    source.write_text("a.\n?-a.\n")

    def fake_run(cmd, stdout, stderr, text, check, timeout):
        json_arg = next(part for part in cmd if part.startswith("--json="))
        stem = json_arg.split("=", 1)[1]
        json_path = importer.Path(stem).with_suffix(".json")
        json_path.write_text(json.dumps(_answer_tree(_atom("a"))))
        return subprocess.CompletedProcess(cmd, 1, stdout="", stderr="past_end_of_stream")

    monkeypatch.setattr(importer.subprocess, "run", fake_run)

    data = run_scasp_json(source, scasp_bin="fake-scasp")

    assert data["answers"][0]["tree"][0]["node"]["value"] == "a"


def test_result_to_fspy_renders_replayable_register_script():
    result = translate_json(_answer_tree(_atom("a")))

    script = result.to_fspy(name="from_scasp")

    assert script.startswith("lk.\ndeclare A:bool.\ndeclare a:(A).\n")
    assert "register from_scasp : A := μalpha1:A.<a||alpha1>" in script
    assert script.endswith("\n")



def test_result_write_fspy_writes_script_to_disk(tmp_path):
    result = translate_json(_answer_tree(_atom("a")))
    target = tmp_path / "nested" / "imported.fspy"

    written = result.write_fspy(target)

    assert written == target
    assert target.is_file()
    assert "register imported : A := μalpha1:A.<a||alpha1>" in target.read_text()



def test_cli_import_file_mode_writes_fspy_script(tmp_path):
    class _Parser:
        def error(self, message):
            raise AssertionError(message)

    source = tmp_path / "source.json"
    source.write_text(json.dumps(_answer_tree(_atom("a"))))
    target = tmp_path / "target.fspy"

    _handle_import_command(_Parser(), ["scasp", str(source), "file", str(target)])

    assert target.is_file()
    assert "declare A:bool." in target.read_text()
    assert "declare a:(A)." in target.read_text()
    assert "register source : A := μalpha1:A.<a||alpha1>" in target.read_text()