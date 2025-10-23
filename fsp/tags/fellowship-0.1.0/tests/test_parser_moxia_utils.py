import pytest

import parser as pt
from wrapper import Argument
from wrapper import setup_prover, execute_script, ProverError, MachinePayloadError
from pathlib import Path
import logging
logger = logging.getLogger(__name__)


def test_enrichment_laog_plain_and_bar():
    # Laog enrichment with plain prop
    laog_plain = pt.Laog("1", None)
    v = pt.PropEnrichmentVisitor(assumptions={"1": {"prop": "A"}})
    out = v.visit(laog_plain)
    assert out.prop == "A"

    # Laog enrichment with “_bar” fallback for compatibility
    laog_bar = pt.Laog("2", None)
    v2 = pt.PropEnrichmentVisitor(assumptions={"2": {"prop": "A_bar"}})
    out2 = v2.visit(laog_bar)
    assert out2.prop == "A"  # parser.py strips _bar in visit_Laog


def test_instruction_generation_uses_moxia_for_ID_and_axiom_for_DI():
    gen = pt.InstructionsGenerationVisitor()

    # ID (context) should generate a left-side close: "moxia name."
    id_node = pt.ID("mA", "A")
    instrs_id = gen.return_instructions(id_node)
    assert list(instrs_id) == ["moxia mA."]

    # DI (term) should generate a right-side close: "axiom name."
    gen2 = pt.InstructionsGenerationVisitor()
    di_node = pt.DI("axA", "A")
    instrs_di = gen2.return_instructions(di_node)
    assert list(instrs_di) == ["axiom axA."]


def test_argument_execute_starts_antitheorem_for_mutilde_body(monkeypatch):
    # Minimal fake prover capturing commands; return a machine dict
    class FakeProver:
        def __init__(self):
            self.commands = []
            self.declarations = {}

        def send_command(self, cmd):
            self.commands.append(cmd)
            # minimal machine payload matching Argument._parse_proof_state expectations
            return {
                "proof-term": '";:thesis:A.<thesis||?1>"',
                "goals": [
                    ["goal", ["meta", '"1"'], ["side", "lhs"], ["active-prop", '"A"']]
                ],
            }

        def close(self):
            pass

    fake = FakeProver()

    # Body starts with Mutilde ⇒ Argument.execute must send antitheorem ...
    body = pt.Mutilde(
        di_=pt.DI("x", "A"),
        prop="A",
        term=pt.Goal("2", "A"),
        context=pt.Laog("1", "A"),
    )
    arg = Argument(fake, name="notA", conclusion="A", instructions=[])
    arg.body = body

    arg.execute()  # should issue antitheorem first
    assert fake.commands, "no commands sent"
    assert fake.commands[0] == "antitheorem notA : (A)."


def test_argument_execute_starts_theorem_for_mu_body(monkeypatch):
    class FakeProver:
        def __init__(self):
            self.commands = []
            self.declarations = {}

        def send_command(self, cmd):
            self.commands.append(cmd)
            return {
                "proof-term": '";:thesis:A.<thesis||?1>"',
                "goals": [
                    ["goal", ["meta", '"1"'], ["side", "rhs"], ["active-prop", '"A"']]
                ],
            }

        def close(self):
            pass

    fake = FakeProver()

    # Body starts with Mu ⇒ Argument.execute must send theorem ...
    body = pt.Mu(
        id_=pt.ID("alpha", "A"),
        prop="A",
        term=pt.Goal("1", "A"),
        context=pt.ID("alpha", "A"),
    )
    arg = Argument(fake, name="thA", conclusion="A", instructions=[])
    arg.body = body

    arg.execute()
    assert fake.commands, "no commands sent"
    assert fake.commands[0] == "theorem thA : (A)."


def test_fspy_moxia_antitheorem_batch(monkeypatch):
    # run from tag root so './fsp' is found
    tests_dir = Path(__file__).resolve().parent
    tag_root = tests_dir.parent
    monkeypatch.chdir(tag_root)

    script = tests_dir / "moxia_antitheorem.fspy"
    assert script.is_file(), "missing tests/moxia_antitheorem.fspy"

    prover = setup_prover()
    try:
        execute_script(
            prover,
            str(script),
            strict=True,
            stop_on_error=True,
            echo_notes=False,
            isolate=False
        )
        st = prover.last_state or {}
        assert st.get('mode') == 'idle'
        assert st.get('errors') == []

        decls = prover.declarations
        assert decls.get('A') == 'bool'
        assert decls.get('mA') == 'A'
        # anti-theorem becomes a named moxia after qed
        assert decls.get('notA') == 'A'
        logger.info("decls %r", decls)
    finally:
        try:
            prover.close()
        except Exception:
            pass


def test_fspy_moxia_smart_batch(monkeypatch):
    # run from tag root so './fsp' is found
    tests_dir = Path(__file__).resolve().parent
    tag_root = tests_dir.parent
    monkeypatch.chdir(tag_root)

    script = tests_dir / "moxia_smart.fspy"
    assert script.is_file(), "missing tests/moxia_smart.fspy"

    prover = setup_prover()
    try:
        execute_script(
            prover,
            str(script),
            strict=True,
            stop_on_error=True,
            echo_notes=False,
            isolate=False
        )
        st = prover.last_state or {}
        assert st.get('mode') == 'idle'
        assert st.get('errors') == []

        decls = prover.declarations
        assert decls.get('A') == 'bool'
        # smart moxia should auto-pick mA to close the LHS goal
        assert decls.get('mA') == 'A'
        # anti-theorem notA2 is recorded as a moxia after qed
        assert decls.get('notA2') == 'A'
        logger.info("decls %r", decls)
    finally:
        try:
            prover.close()
        except Exception:
            pass


def test_fspy_counterargument_recording_and_rendering(monkeypatch):
    # run from tag root so './fsp' is found
    tests_dir = Path(__file__).resolve().parent
    tag_root = tests_dir.parent
    monkeypatch.chdir(tag_root)

    script = tests_dir / "argument_counter_render_normalize.fspy"
    assert script.is_file(), "missing tests/argument_counter_render_normalize.fspy"

    prover = setup_prover()
    try:
        execute_script(
            prover,
            str(script),
            strict=True,
            stop_on_error=True,
            echo_notes=False,
            isolate=False
        )
        # the argument should be registered by the wrapper
        arg = prover.get_argument("notA")
        assert arg is not None, "Argument 'notA' was not registered"

        # render/normalize produced artifacts
        assert isinstance(arg.representation, str) and arg.representation
        assert isinstance(arg.normal_form, str) and arg.normal_form
        assert isinstance(arg.normal_representation, str) and arg.normal_representation

         # Check that the argument is correctly executed:
        assert isinstance(arg.body, pt.Mutilde)
        assert arg.body.term.DI.name == 'thesis'
        assert arg.body.context.ID.name == 'mA'

        # decls include sort and the denied moxia; recorded counterargument is NOT QED, so it's not in decls
        decls = prover.declarations
        assert decls.get('A') == 'bool'
        assert decls.get('mA') == 'A'
        assert 'notA' not in decls
        # also verify we recorded a counterargument intent
        assert getattr(arg, "is_anti", False) is True
        logger.info("decls %r", decls)
    finally:
        try:
            prover.close()
        except Exception:
            pass
