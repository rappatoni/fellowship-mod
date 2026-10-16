"""The negation-elimination shortcut of the instruction generator.

``InstructionsGenerationVisitor.visit_Mu`` replays
μH1:¬A.<λH2:A.μH3:⊥.<H2||X>||H1> as the single command ``elim H1``.  It
used to check only the left half, so a scaffold whose original is such a
lambda (a lower supporter of a negation in a stack of the unfolded term)
lost its wiring and the rest of the replay was one binder off (tasks.org,
aida-replay-neg-elim-shortcut).
"""
import io
import warnings
from contextlib import redirect_stdout

import pytest

from core.ac.grammar import Grammar, ProofTermTransformer
from core.ac.instructions import InstructionsGenerationVisitor
from core.comp.enrich import PropEnrichmentVisitor
from core.dc.typecheck import typecheck
from core.dc.unfold import argument_edge, unfold, unfold_argument
from wrap.cli import setup_prover, execute_script

#: What `elim n` builds for the goal ~Q.
NEGATION = "μn:~Q.<λh:Q.μa:false.<h:Q||1:Q?>||n:~Q>"
#: The same with Q->false: `elim n` builds λn:Q.?:false there instead.
ARROW = "μn:Q->false.<λh:Q.μa:false.<h:Q||1:Q?>||n:Q->false>"
#: A support scaffold whose original is the lambda (lesson 15's shape).
SCAFFOLD = "μalt:Q->false.<λh:Q.μa:false.<h:Q||1:Q?>||μ'b:Q->false.<b:Q->false||alt:Q->false>>"
CONCLUSION = {NEGATION: "~Q", ARROW: "Q->false", SCAFFOLD: "Q->false"}


def _quiet_instructions(term):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        body = ProofTermTransformer().transform(Grammar().parser.parse(term))
        body = PropEnrichmentVisitor(axiom_props={"Q": "bool"}).visit(body)
        return list(InstructionsGenerationVisitor().return_instructions(body))


def test_the_negation_shape_is_one_elim():
    assert _quiet_instructions(NEGATION) == ["elim n"]


def test_an_arrow_into_falsum_is_a_cut():
    assert _quiet_instructions(ARROW)[0] == "cut (Q -> false) n"


def test_a_scaffold_on_the_lambda_is_a_cut():
    instructions = _quiet_instructions(SCAFFOLD)
    assert instructions[0] == "cut (Q -> false) alt"
    assert instructions[-2:] == ["axiom b", "moxia alt"]


@pytest.fixture
def prover():
    prover = setup_prover()
    yield prover
    prover.close()


def _load(prover, script):
    with warnings.catch_warnings(), redirect_stdout(io.StringIO()):
        warnings.simplefilter("ignore")
        execute_script(prover, script, strict=False, stop_on_error=False, isolate=False,
                       render_files=False, stop_marker=False)


@pytest.mark.parametrize("term", [NEGATION, ARROW, SCAFFOLD])
def test_each_shape_replays_through_fellowship(prover, term):
    prover.send_command("declare Q : bool.")
    body = ProofTermTransformer().transform(Grammar().parser.parse(term))
    typecheck(prover, body, "shortcut", CONCLUSION[term], False)


def test_every_lesson15_term_replays_through_fellowship(prover):
    """Lesson 15 states "Q fails" twice, by the lambdas of p1 and p2, so the
    stacked debate about Q->false has a lambda as the original of a support
    scaffold.  Every argument and issue term replays."""
    _load(prover, "tests/minicourse/lesson15_capture.fspy")
    doc = prover.graph
    replayed = 0
    for name in ("p1", "p2", "q"):
        edge = argument_edge(doc, name)
        typecheck(prover, unfold_argument(doc, edge), name, doc.nodes[edge.target_key], False)
        replayed += 1
    for key, side in doc.statements():
        typecheck(prover, unfold(doc, (key, side)), "issue", doc.nodes[key], side == "context")
        replayed += 1
    assert replayed == 11
