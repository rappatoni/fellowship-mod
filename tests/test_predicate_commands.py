"""Predicate applications in prover commands: bracketed arguments."""

from core.ac.ast import DI, ID, Mu
from core.ac.instructions import InstructionsGenerationVisitor
from core.ac.prop_render import prop_to_command


def test_prop_to_command_renders_predicate_applications_and_connectives():
    assert prop_to_command("Child Bob") == "Child[Bob]"
    assert prop_to_command("Father Rich Bob") == "Father[Rich][Bob]"
    assert prop_to_command("Child Bob -> Orphan Bob") == "Child[Bob] -> Orphan[Bob]"
    assert prop_to_command("H_Orphan Bob-Child Bob") == "H_Orphan[Bob]-Child[Bob]"


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
