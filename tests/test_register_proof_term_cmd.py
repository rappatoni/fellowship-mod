from core.ac.grammar import Grammar, ProofTermTransformer
from wrap.cli import _parse_register_command, register_argument_cmd


class _FakeProver:
    def __init__(self):
        self.commands = []
        self.declarations = {}
        self.arguments = {}
        self.echo_notes = False

    def send_command(self, cmd, *args, **kwargs):
        self.commands.append(cmd)
        return {
            "proof-term": '";:thesis:A.<?1:A||1:A?>"',
            "goals": [
                [
                    "goal",
                    ["meta", '"1"'],
                    ["side", "rhs"],
                    ["active-prop", '"A"'],
                ]
            ],
        }

    def register_argument(self, argument):
        self.arguments[argument.name] = argument


def test_parse_register_command_delimited_type_and_strict_flag():
    assert _parse_register_command(
        "register demo strict : A -> B := μthesis:A->B.<?1:A||1:A?>"
    ) == (
        "demo",
        "A -> B",
        True,
        "μthesis:A->B.<?1:A||1:A?>",
    )


def test_grammar_parses_enriched_open_placeholders():
    body = ProofTermTransformer().transform(
        Grammar().parser.parse("μthesis:A.<?1:A||1:A?>")
    )

    assert body.prop == "A"
    assert body.term.number == "1"
    assert body.term.prop == "A"
    assert body.context.number == "1"
    assert body.context.prop == "A"



def test_grammar_parses_enriched_typed_leaves_and_replay_untyped_leaves():
    enriched = ProofTermTransformer().transform(
        Grammar().parser.parse("μalpha:A.<a:A||alpha:A>")
    )
    replay = ProofTermTransformer().transform(
        Grammar().parser.parse("μalpha:A.<a||alpha>")
    )

    assert enriched.term.name == "a"
    assert enriched.term.prop == "A"
    assert enriched.context.name == "alpha"
    assert enriched.context.prop == "A"
    assert replay.term.name == "a"
    assert replay.term.prop is None
    assert replay.context.name == "alpha"
    assert replay.context.prop is None


def test_register_argument_cmd_replays_and_discards_by_default():
    prover = _FakeProver()

    arg = register_argument_cmd(prover, "register demo : A := μthesis:A.<?1:A||1:A?>")

    assert arg.name == "demo"
    assert arg.conclusion == "A"
    assert prover.arguments["demo"] is arg
    assert prover.commands[0] == "theorem demo : (A)."
    assert prover.commands[-1] == "discard theorem."


def test_register_argument_cmd_strict_replays_and_qeds():
    prover = _FakeProver()

    arg = register_argument_cmd(prover, "register demo strict : A := μthesis:A.<?1:A||1:A?>")

    assert arg.name == "demo"
    assert arg.conclusion == "A"
    assert prover.arguments["demo"] is arg
    assert prover.commands[0] == "theorem demo : (A)."
    assert prover.commands[-1] == "qed."