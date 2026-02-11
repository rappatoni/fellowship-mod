import types as _types

import pytest

from core.ac.ast import Goal, Laog, ID, DI, Mu, Mutilde
from core.dc.argument import Argument


def test_execute_with_empty_instructions_uses_start_payload_and_sets_assumptions():
    """Regression test for B-0013: execute() must not crash when instructions=[]
    and must parse proof state from the start command payload."""

    class FakeProver:
        def __init__(self):
            self.commands = []
            self.declarations = {}

        def send_command(self, cmd):
            self.commands.append(cmd)
            # Minimal payload matching Argument._parse_proof_state expectations.
            return {
                "proof-term": '";:thesis:A.<thesis||?1>"',
                "goals": [
                    [
                        "goal",
                        ["meta", '"1"'],
                        ["side", "rhs"],
                        ["active-prop", '"A"'],
                    ]
                ],
            }

        def close(self):
            pass

    fake = FakeProver()

    # Body starts with Mu ⇒ execute() starts a theorem; keep instructions empty.
    body = Mu(
        id_=ID("alpha", "A"),
        prop="A",
        term=Goal("1", "A"),
        context=ID("alpha", "A"),
    )

    arg = Argument(fake, name="thA_empty", conclusion="A", instructions=[])
    arg.body = body

    # Must not throw; must parse goals from start payload.
    arg.execute()

    assert fake.commands, "no commands sent"
    assert fake.commands[0] == "theorem thA_empty : (A)."

    # Assumptions extracted from machine payload.
    assert isinstance(arg.assumptions, dict)
    assert arg.assumptions.get("1", {}).get("prop") == "A"

    # Proof term must have been set.
    assert isinstance(arg.proof_term, str) and arg.proof_term
