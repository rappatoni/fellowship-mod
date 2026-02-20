import builtins
import pytest

from wrap.cli import interactive_mode
from wrap.prover import ProverError, MachinePayloadError


class _FakeProver:
    def __init__(self):
        self.closed = False
        self.commands = []

    def send_command(self, command: str, silent: int = 1, *, include_ui: bool = False, allow_incomplete: bool = False):
        self.commands.append(command)
        if command == "bad.":
            # Simulate Fellowship rejecting a command with a syntax error.
            # Here we emulate the wrapper surfacing the extracted parse error.
            raise ProverError("Parse error: unexpected token")
        if command == "declare A:bool":
            # Fellowship waits for '.' before returning a prompt.
            return {"_need_more_input": True, "_ui": "declare A:bool"} if include_ui else {"_need_more_input": True}
        if command == ".":
            return {"_ui": "> A defined."} if include_ui else {}
        if command == "desync.":
            # Simulate wrapper deeming this fatal (no machine block, not a parse error).
            raise MachinePayloadError("Machine block missing in prover output (possible desync).")
        return {"_ui": f"ok: {command}"} if include_ui else {}

    def get_argument(self, name: str):
        return None

    def register_argument(self, arg):
        raise AssertionError("not expected")

    def close(self):
        self.closed = True


def test_interactive_mode_ignores_prover_error_and_continues(capsys, monkeypatch):
    prover = _FakeProver()
    inputs = iter(["bad.", "good.", "exit"])
    monkeypatch.setattr(builtins, "input", lambda _prompt="": next(inputs))

    interactive_mode(prover)

    out = capsys.readouterr().out
    assert "acdc: ignored command" in out
    assert "ok: good." in out
    assert prover.closed is True


def test_interactive_mode_fatal_on_machine_payload_error(capsys, monkeypatch):
    prover = _FakeProver()
    inputs = iter(["desync.", "good.", "exit"])
    monkeypatch.setattr(builtins, "input", lambda _prompt="": next(inputs))

    interactive_mode(prover)

    # Should stop before executing further commands
    assert prover.commands == ["desync."]
    assert prover.closed is True
