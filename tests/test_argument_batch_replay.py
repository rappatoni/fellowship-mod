import pytest

from core.dc.argument import Argument
from wrap.prover import ProverError, ProverWrapper
from wrap.sexp_parser import SexpParser


class _SpawnFakeChild:
    def __init__(self):
        self.before = ""
        self.delaybeforesend = 0.05
        self.expected = []

    def expect(self, pattern):
        self.expected.append(pattern)


def test_prover_wrapper_disables_pexpect_send_delay_after_initial_prompt(monkeypatch):
    child = _SpawnFakeChild()
    monkeypatch.setattr("wrap.prover.pexpect.spawn", lambda *args, **kwargs: child)

    ProverWrapper("fsp")

    assert child.expected == ["fsp <"]
    assert child.delaybeforesend is None


class _BatchFakeProver:
    def __init__(self, *, fail_final_next: bool = False):
        self.commands = []
        self.batches = []
        self.quiet_batches = []
        self.tactics = []
        self.declarations = {}
        self.decorations = {}
        self.fail_final_next = fail_final_next

    def _state(self):
        return {
            "proof-term": '";:thesis:A.<thesis||1?>"',
            "goals": [
                ["goal", ["meta", '"1"'], ["side", "rhs"], ["active-prop", '"A"']]
            ],
        }

    def send_command(self, cmd, *args, **kwargs):
        self.commands.append(cmd)
        if self.fail_final_next and cmd == "next.":
            raise ProverError("no next goal")
        return self._state()

    def send_commands(self, commands, *args, **kwargs):
        self.batches.append(tuple(commands))
        return self._state()

    def send_commands_quiet_final(self, commands, *args, **kwargs):
        self.quiet_batches.append(tuple(commands))
        return self._state()

    def execute_tactic(self, tactic_name, *args):
        self.tactics.append((tactic_name, args))
        return self._state()


@pytest.mark.parametrize("env_value", ["0", "false", "no"])
def test_argument_execute_can_disable_batch_replay(monkeypatch, env_value):
    monkeypatch.setenv("FSP_BATCH_REPLAY", env_value)
    prover = _BatchFakeProver()
    arg = Argument(prover, "demo", "A", instructions=["intro", "axiom"])

    arg.execute()

    assert prover.batches == []
    assert prover.quiet_batches == []
    assert prover.commands[:3] == ["theorem demo : (A).", "intro.", "axiom."]
    assert prover.commands[-1] == "discard theorem."


def test_argument_execute_batches_contiguous_fellowship_instructions(monkeypatch):
    monkeypatch.delenv("FSP_BATCH_REPLAY", raising=False)
    monkeypatch.setenv("FSP_QUIET_REPLAY", "0")
    prover = _BatchFakeProver()
    arg = Argument(prover, "demo", "A", instructions=["intro", "axiom"])

    arg.execute()

    assert prover.commands[0] == "theorem demo : (A)."
    assert prover.batches == [("intro.", "axiom.")]
    assert prover.quiet_batches == []
    assert prover.commands[-1] == "discard theorem."


def test_argument_execute_prefers_quiet_final_replay(monkeypatch):
    monkeypatch.delenv("FSP_BATCH_REPLAY", raising=False)
    monkeypatch.delenv("FSP_QUIET_REPLAY", raising=False)
    prover = _BatchFakeProver()
    arg = Argument(prover, "demo", "A", instructions=["intro", "axiom"])

    arg.execute()

    assert prover.commands[0] == "theorem demo : (A)."
    assert prover.batches == []
    assert prover.quiet_batches == [("intro.", "axiom.")]
    assert prover.commands[-1] == "discard theorem."


def test_argument_execute_flushes_batch_around_custom_tactics(monkeypatch):
    monkeypatch.delenv("FSP_BATCH_REPLAY", raising=False)
    prover = _BatchFakeProver()
    arg = Argument(prover, "demo", "A", instructions=["intro", "tactic foo X Y", "axiom"])

    arg.execute()

    assert prover.batches == []
    assert prover.quiet_batches == [("intro.",), ("axiom.",)]
    assert prover.tactics == [("foo", ("X", "Y"))]


def test_argument_execute_preserves_final_next_error_special_case(monkeypatch):
    monkeypatch.delenv("FSP_BATCH_REPLAY", raising=False)
    prover = _BatchFakeProver(fail_final_next=True)
    arg = Argument(prover, "demo", "A", instructions=["intro", "next"])

    arg.execute()

    assert prover.batches == []
    assert prover.quiet_batches == [("intro.",)]
    assert "next." in prover.commands
    assert prover.commands[-1] == "discard theorem."


class _PromptFakeChild:
    def __init__(self, outputs):
        self.outputs = list(outputs)
        self.sent = None
        self.before = ""

    def send(self, text):
        self.sent = text

    def expect(self, _pattern):
        self.before = self.outputs.pop(0)


def _machine_output(messages: str = "(errors) (warnings) (notes)") -> str:
    return f";;BEGIN_ML_DATA;;(state (messages {messages}) (proof-term \"pt\"));;END_ML_DATA;;"


def test_send_commands_surfaces_intermediate_errors():
    wrapper = object.__new__(ProverWrapper)
    wrapper.prover = _PromptFakeChild([
        _machine_output(),
        _machine_output('(errors "boom") (warnings) (notes)'),
        _machine_output(),
    ])
    wrapper._sexp = SexpParser()
    wrapper.echo_notes = False
    wrapper.declarations = {}
    wrapper.last_state = None

    with pytest.raises(ProverError, match="boom"):
        wrapper.send_commands(["first.", "second.", "third."])

    assert wrapper.prover.sent == "first.\nsecond.\nthird.\n"


def test_send_commands_quiet_final_toggles_quiet_and_returns_full_state(monkeypatch):
    wrapper = object.__new__(ProverWrapper)
    wrapper.commands = []
    wrapper.batches = []

    def send_command(cmd, *args, **kwargs):
        wrapper.commands.append(cmd)
        return {
            "proof-term": '"pt"',
            "goals": [],
            "errors": [],
            "warnings": [],
            "notes": [],
        }

    def send_commands(commands, *args, **kwargs):
        wrapper.batches.append(tuple(commands))
        return {
            "mode": "subgoals",
            "errors": [],
            "warnings": [],
            "notes": [],
        }

    monkeypatch.setattr(wrapper, "send_command", send_command)
    monkeypatch.setattr(wrapper, "send_commands", send_commands)

    state = ProverWrapper.send_commands_quiet_final(wrapper, ["intro.", "axiom."])

    assert wrapper.commands == ["machine quiet on.", "intro.", "axiom.", "machine quiet off."]
    assert wrapper.batches == []
    assert state["proof-term"] == '"pt"'


def test_send_commands_quiet_final_falls_back_when_quiet_command_unavailable(monkeypatch):
    wrapper = object.__new__(ProverWrapper)
    wrapper.commands = []
    wrapper.batches = []

    def send_command(cmd, *args, **kwargs):
        wrapper.commands.append(cmd)
        if cmd == "machine quiet on.":
            raise ProverError("unknown command")
        return {}

    def send_commands(commands, *args, **kwargs):
        wrapper.batches.append(tuple(commands))
        return {"proof-term": '"fallback"'}

    monkeypatch.setattr(wrapper, "send_command", send_command)
    monkeypatch.setattr(wrapper, "send_commands", send_commands)

    state = ProverWrapper.send_commands_quiet_final(wrapper, ["intro."])

    assert wrapper.commands == ["machine quiet on."]
    assert wrapper.batches == [("intro.",)]
    assert state["proof-term"] == '"fallback"'


def test_send_commands_quiet_final_restores_quiet_mode_on_replay_error(monkeypatch):
    wrapper = object.__new__(ProverWrapper)
    wrapper.commands = []

    def send_command(cmd, *args, **kwargs):
        wrapper.commands.append(cmd)
        if cmd == "intro.":
            raise ProverError("boom")
        return {}

    def send_commands(commands, *args, **kwargs):
        raise AssertionError("quiet replay should send commands stepwise")

    monkeypatch.setattr(wrapper, "send_command", send_command)
    monkeypatch.setattr(wrapper, "send_commands", send_commands)

    with pytest.raises(ProverError, match="boom"):
        ProverWrapper.send_commands_quiet_final(wrapper, ["intro."])

    assert wrapper.commands == ["machine quiet on.", "intro.", "machine quiet off."]
