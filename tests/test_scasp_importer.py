import json
import re
import subprocess

from core.ac.ast import Cons, DI, ID, Mu, Mutilde, Sonc
from pres.gen import ProofTermGenerationVisitor
from scasp_import import importer
from scasp_import.importer import atom_to_prop, run_scasp_json, translate_json
from wrap.cli import _handle_import_command


def _answer_tree(*trees):
    return {"answers": [{"tree": list(trees)}]}


def _atom(name, children=None):
    return {
        "node": {"type": "atom", "value": name},
        "children": list(children or []),
    }


def test_atom_to_prop_capitalizes_only_first_character():
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


def test_translate_json_warns_for_ignored_answers_global_constraint_and_extra_roots():
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
                {"tree": [_atom("c")]},
            ]
        }
    )

    assert result.conclusion == "A"
    assert any("additional sCASP answers" in warning for warning in result.warnings)
    assert any("o_nmr_check" in warning for warning in result.warnings)
    assert any("additional top-level" in warning for warning in result.warnings)


def test_translate_json_omits_unsupported_children_with_warning():
    unsupported = {
        "node": {"type": "compound", "functor": "not", "args": [{"type": "atom", "value": "b"}]},
        "children": [],
    }

    result = translate_json(_answer_tree(_atom("a", [unsupported])))

    assert result.conclusion == "A"
    assert result.declarations == {"a": "A"}
    assert any("unsupported child" in warning for warning in result.warnings)


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