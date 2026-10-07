"""The compilation-evaluation pipeline narrates itself.

Before 2026-09-26 not one module of the pipeline defined a logger, so
`evaluate` printed a verdict and nothing else.  These tests pin that every
stage now speaks, that the decisions worth reviewing are among what it
says, and that saying it costs nothing when nobody is listening.

Assertions are on record attributes and on semantic fragments, never on
whole prose lines: the wording is meant to be improved, the facts are not.
"""

import logging
from pathlib import Path

import pytest

from core.comp.adf_label import grounded_labels
from core.comp.evaluate import evaluate_debate
from core.dc.debate_graph import declaration_kinds
from core.dc.unfold import unfold
from core.logging_util import TRACE
from wrap.cli import _PIPELINE_LOGGERS, execute_script, explain_argument_cmd, setup_prover


@pytest.fixture
def fresh():
    """A prover of this test's own.

    The session-wide prover cannot be reused here: these tests replay
    several different fixtures, and their `declare` lines collide, leaving
    a half-built document.  One launch per test is the price of replaying
    a fixture faithfully.
    """
    from mod import store
    store.arguments.clear()
    store.document.clear()
    pw = setup_prover()
    yield pw
    pw.close()


def evaluate_fixture(prover, script, name, **kwargs):
    """Replay a fixture and evaluate one of its arguments."""
    execute_script(prover, script, strict=False, stop_on_error=False, isolate=False)
    sn, sk = prover.declarations.keys(), declaration_kinds(prover.declarations)
    term = unfold(prover.document, prover.issue_of(prover.get_argument(name)))
    return evaluate_debate(term, name, strict_names=sn, strict_kinds=sk, **kwargs)


def messages(caplog, logger_name=None):
    return [r.getMessage() for r in caplog.records
            if logger_name is None or r.name == logger_name]


class TestEveryStageSpeaks:
    def test_all_pipeline_loggers_emit(self, fresh, caplog):
        caplog.set_level(logging.DEBUG)
        evaluate_fixture(fresh, "tests/rationality/cyclic_undercut.fspy", "argA",
                         mode="credulous")
        spoke = {r.name for r in caplog.records}
        # typecheck only runs through the CLI path, and the solver only when a
        # labelling is needed; every other stage must have said something.
        expected = set(_PIPELINE_LOGGERS) - {"core.dc.typecheck"}
        assert expected <= spoke, f"silent: {sorted(expected - spoke)}"

    def test_the_unfolder_reports_its_choice(self, fresh, caplog):
        caplog.set_level(logging.DEBUG)
        evaluate_fixture(fresh, "tests/rationality/cyclic_undercut.fspy", "argA",
                         mode="credulous")
        said = " ".join(messages(caplog, "core.dc.unfold"))
        assert "expanded" in said
        # every binder captures, the scaffolds' too: a statement already
        # being unfolded is captured, never left as a site
        # (aida-unfold-scaffold-binders-capture)
        assert "captured as" in said and "no binder in scope" not in said
        # the stacked shape (aida-unfold-entrypoints): a supporter joins the
        # stack, the contrary's debate attacks
        assert "+support" in said and "attacked by the debate about" in said

    def test_the_solver_subprocess_is_visible(self, fresh, caplog):
        caplog.set_level(logging.DEBUG)
        evaluate_fixture(fresh, "tests/rationality/contested.fspy", "pArg", mode="credulous")
        said = " ".join(messages(caplog, "core.comp.oracle"))
        assert "adf-bdd" in said and "exit 0" in said


class TestDecisionsAreVisible:
    def test_self_attack_reports_its_strict_edge(self, fresh, caplog):
        caplog.set_level(logging.DEBUG)
        evaluate_fixture(fresh, "tests/rationality/self_attack_lk.fspy", "SelfAttack",
                         mode="credulous")
        said = " ".join(messages(caplog, "core.dc.strict"))
        assert "SelfAttack*" in said
        assert "a closed derivation the framework missed" in said

    def test_the_witness_number_is_reported(self, fresh, caplog):
        caplog.set_level(logging.DEBUG)
        evaluate_fixture(fresh, "tests/rationality/contested.fspy", "pArg", mode="credulous")
        said = " ".join(messages(caplog, "core.comp.evaluate"))
        assert "[1]" in said and "chosen" in said

    def test_the_silent_mode_switch_is_now_loud(self, fresh, caplog):
        """Credulously rejected: the evaluator falls back to grounded and
        resolves SKEPTICALLY, contradicting the mode asked for.  That must be
        visible at INFO, not only at DEBUG."""
        caplog.set_level(logging.INFO)
        evaluate_fixture(fresh, "tests/rationality/modus_tollens.fspy", "tp",
                         mode="credulous")
        said = " ".join(messages(caplog, "core.comp.evaluate"))
        assert "resolved skeptically" in said

    def test_the_classifier_says_why(self, fresh, caplog):
        caplog.set_level(logging.DEBUG)
        evaluate_fixture(fresh, "tests/rationality/cyclic_undercut.fspy", "argA",
                         mode="credulous")
        said = " ".join(messages(caplog, "core.comp.oracle_terms"))
        assert "normal form after" in said
        assert "VALUE" in said or "OPEN" in said or "EXCEPTION" in said


class TestArtifactsAreReported:
    """Each phase prints what it PRODUCED, not only what it decided.  That is
    what an implementation is verified against (author, 2026-09-26)."""

    def artifacts(self, caplog):
        """The label lines: a phase's artifact header ends in a colon."""
        return [m for m in messages(caplog) if m.rstrip().endswith(":")]

    def test_every_phase_hands_over_its_artifact(self, fresh, caplog):
        caplog.set_level(logging.DEBUG)
        execute_script(fresh, "tests/rationality/cyclic_undercut.fspy",
                       strict=False, stop_on_error=False, isolate=False)
        caplog.clear()
        from wrap.cli import evaluate_argument_cmd
        evaluate_argument_cmd(fresh, "d", mode="credulous")
        labels = " | ".join(self.artifacts(caplog))
        for expected in (
            "registered with",                  # 1. the term before unfolding
            "unfolded from the document",       # 2. the debate term (the default, unfolded route)
            "the term sent", "Fellowship rebuilt",   # 3. both sides of the check
            "argumentation framework",          # 4. the compiled graph
            "the term going in", "the term coming out",   # 5. strict, in and out
            "strict edges it contributes",
            "acceptance conditions",            # 6. labelling input
            "labelling(s), in the canonical numbering",   # 6. labelling output
            "witness labelling sigma",          # 7. the chosen sigma
            "going into normalisation",         # 8. what is reduced
            "the normal form",
        ):
            assert expected in labels, f"no artifact labelled {expected!r}"

    def test_the_unfolded_pipeline_hands_over_the_unfolded_term(self, fresh, caplog):
        # `pipeline unfolded` selects the reference route (tasks.org,
        # aida-shared-subarguments): phase 2 is then the term unfolded from
        # the document, and the later phases work on it.
        caplog.set_level(logging.DEBUG)
        execute_script(fresh, "tests/rationality/cyclic_undercut.fspy",
                       strict=False, stop_on_error=False, isolate=False)
        fresh.pipeline_unfolded = True
        caplog.clear()
        from wrap.cli import evaluate_argument_cmd
        evaluate_argument_cmd(fresh, "d", mode="credulous")
        labels = " | ".join(self.artifacts(caplog))
        for expected in ("unfolded from the document", "the term going in",
                         "the term coming out", "strict edges it contributes",
                         "going into normalisation", "the normal form"):
            assert expected in labels, f"no artifact labelled {expected!r}"

    def test_the_unfolded_term_is_printed_in_full(self, fresh, caplog):
        caplog.set_level(logging.DEBUG)
        evaluate_fixture(fresh, "tests/rationality/cyclic_undercut.fspy", "argA",
                         mode="credulous")
        said = messages(caplog, "core.dc.unfold")
        i = next(i for i, m in enumerate(said) if m.endswith("document:"))
        # The term is laid out as a dialectical tree (pres.gen.pres_tree):
        # the issue's support scaffold, its scope closed by the last line.
        block = []
        for m in said[i + 1:]:
            if not m.startswith("    "):
                break
            block.append(m[4:])
        assert block[0].startswith("SUP(") and block[-1] == ")"
        assert "argA" in "\n".join(block) and len(block) > 10

    def test_a_dropped_wing_says_what_went_with_it(self, fresh, caplog):
        """Resolution recurses only into the wing it keeps, so one decision at
        the root can collapse a whole debate to a single site.  The account has
        to say so, or the jump from the strict phase's term to the term entering
        normalisation looks like magic (author, 2026-09-26)."""
        caplog.set_level(logging.DEBUG)
        evaluate_fixture(fresh, "tests/rationality/cyclic_undercut.fspy", "argB")
        said = " ".join(messages(caplog, "core.comp.evaluate"))
        assert "dropping the" in said
        assert "scaffold(s) inside it" in said

    def test_a_shape_mismatch_names_its_locus(self):
        """On a type-check failure the reviewer needs the position, not just
        the fact that the two differ."""
        from core.dc.typecheck import shape_mismatch
        sent = ("mu", "A", ("di", 0), ("id", 1))
        rebuilt = ("mu", "A", ("di", 0), ("id", 2))
        path, mine, theirs = shape_mismatch(sent, rebuilt)
        assert path == ("mu", 3, "id", 1) and (mine, theirs) == (1, 2)
        assert shape_mismatch(sent, sent) is None


class TestNarrationIsFree:
    def test_no_pretty_printing_at_info(self, fresh, caplog, monkeypatch):
        """pres_str deep-copies, so every message needing it is guarded.  The
        legacy reducer pays that cost at every level; the pipeline must not."""
        import pres.gen as gen
        calls = []
        real = gen.pres_str
        monkeypatch.setattr(gen, "pres_str", lambda n: calls.append(1) or real(n))
        caplog.set_level(logging.INFO)
        evaluate_fixture(fresh, "tests/rationality/cyclic_undercut.fspy", "argA",
                         mode="credulous")
        assert calls == []

    def test_trace_needs_no_cli_import(self):
        """TRACE used to be installed by importing wrap.cli, so core modules
        could only log at TRACE by accident.  It lives in core now."""
        import subprocess
        import sys
        out = subprocess.run(
            [sys.executable, "-c",
             "import core.logging_util as L, sys;"
             "assert 'wrap.cli' not in sys.modules;"
             "print(L.TRACE)"],
            capture_output=True, text=True, cwd=Path(__file__).resolve().parents[1])
        assert out.returncode == 0, out.stderr
        assert out.stdout.strip() == "5"

    def test_reduction_steps_appear_at_trace(self, fresh, caplog):
        caplog.set_level(TRACE)
        evaluate_fixture(fresh, "tests/rationality/cyclic_undercut.fspy", "argA",
                         mode="credulous")
        said = " ".join(messages(caplog, "core.comp.oracle_terms"))
        assert "normalise: [1]" in said       # step number and the rule that fired


class TestExplainCommand:
    def test_explain_reports_every_stage_and_restores_the_level(self, fresh, caplog):
        caplog.set_level(logging.INFO)
        execute_script(fresh, "tests/rationality/cyclic_undercut.fspy",
                       strict=False, stop_on_error=False, isolate=False)
        before = {name: logging.getLogger(name).level for name in _PIPELINE_LOGGERS}
        root_before = logging.getLogger().level
        caplog.clear()
        explain_argument_cmd(fresh, "d", mode="credulous")
        said = "\n".join(messages(caplog, "fsp.wrapper"))
        assert "stage by stage" in said
        for stage in ("unfold", "compile", "strict", "label", "witness", "sigma", "classify"):
            assert stage in said, f"no {stage} line in the account"
        # the verdict comes last, after the account that led to it
        assert said.index("stage by stage") < said.index("Evaluated 'd'")
        assert {name: logging.getLogger(name).level for name in _PIPELINE_LOGGERS} == before
        assert logging.getLogger().level == root_before


class TestGraphFileOutput:
    def test_render_files_off_writes_nothing_and_says_so(self, fresh, caplog, tmp_path,
                                                         monkeypatch):
        """Ten fixtures carry `graph ... show`; a suite run must not litter the
        working directory nor open a viewer."""
        from wrap.cli import graph_argument_cmd
        monkeypatch.chdir(tmp_path)
        execute_script(fresh, str(Path(__file__).resolve().parents[1] /
                                   "tests/rationality/contested.fspy"),
                       strict=False, stop_on_error=False, isolate=False, render_files=False)
        caplog.set_level(logging.INFO)
        graph_argument_cmd(fresh, "datt", show=True)
        assert list(tmp_path.iterdir()) == []
        said = " ".join(messages(caplog, "fsp.wrapper"))
        assert "file output is off" in said.lower()

    def test_render_files_none_keeps_the_session_setting(self, fresh):
        """execute_script(render_files=None) must not override ACDC_NO_RENDER."""
        before = fresh.render_files
        execute_script(fresh, "tests/rationality/contested.fspy",
                       strict=False, stop_on_error=False, isolate=False)
        assert fresh.render_files is before
