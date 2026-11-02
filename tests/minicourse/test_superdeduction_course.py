"""Pins every value quoted in docs/minicourses/minicourse-superdeduction.org.

The course computes super rules the way the wrapper-side architecture would:
by letting Fellowship decompose a definition and reading the open goals and
the partial proof term off the machine payload.  It also shows the supercut's
critical pair with the normaliser.  If Fellowship or the normaliser change,
these fail and the lesson text must be updated.
"""

import re
import subprocess
from pathlib import Path

import pytest

from core.ac.syntax import parse_proof_term
from core.comp.oracle_terms import normalize_command
from pres.gen import pres_str

HERE = Path(__file__).parent / "superdeduction"
FSP = Path(__file__).resolve().parents[2] / "wrap" / "fellowship" / "fsp"

pytestmark = pytest.mark.skipif(not FSP.exists(), reason="fsp is not built")


def final_state(script: str) -> str:
    """The last machine payload Fellowship prints for a script."""
    out = subprocess.run([str(FSP), "-ascii", "-c", str(HERE / script)],
                         capture_output=True, text=True, timeout=30).stdout
    payloads = [line for line in out.splitlines() if ";;BEGIN_ML_DATA;;" in line]
    return payloads[-1]


def proof_term(payload: str) -> str:
    return re.search(r'\(proof-term "([^"]*)"\)', payload).group(1)


def open_goals(payload: str) -> list[tuple[str, str, str]]:
    """(meta, side, active proposition) of every open goal."""
    return re.findall(r'\(meta "([^"]*)"\)\(kind goal\)\(side (\w+)\)\(active-prop "([^"]*)"\)',
                      payload)


class TestLessonS2Inclusion:
    def test_right_rule(self):
        payload = final_state("s2_inclusion_right.fsp")
        assert proof_term(payload) == (
            r";:thesis:forall x:El,mem x A->mem x B.<\x:El.\h:mem x A.?1.1.1||thesis>")
        assert open_goals(payload) == [("1.1.1", "rhs", "mem x B")]
        assert '(hyps ((name "h")(prop "mem x A")(visible t)))' in payload
        assert '(env ((name "x")(sort "El")))' in payload

    def test_left_rule(self):
        payload = final_state("s2_inclusion_left.fsp")
        assert proof_term(payload) == (
            r";:thesis:(forall x:El,mem x A->mem x B)->C.<\H:forall x:El,mem x A->mem x B."
            r";:th:C.<H||t*?1.1.2.1.1*1.1.2.1.2?>||thesis>")
        assert open_goals(payload) == [("1.1.2.1.1", "rhs", "mem t A"),
                                       ("1.1.2.1.2", "lhs", "mem t B")]


class TestLessonS3Fly:
    def test_right_rule(self):
        payload = final_state("s3_fly_right.fsp")
        assert proof_term(payload) == (
            r";:thesis:Bird/\~Abnormal.<(?1.1,;:H1:~Abnormal.<\H2:Abnormal."
            r";:H3:false.<H2||1.2.1?>||H1>)||thesis>")
        assert open_goals(payload) == [("1.1", "rhs", "Bird"),
                                       ("1.2.1", "lhs", "Abnormal")]

    def test_left_rule(self):
        payload = final_state("s3_fly_left.fsp")
        assert proof_term(payload) == (
            r";:thesis:Bird/\~Abnormal->C.<\H:Bird/\~Abnormal.;:th:C.<H||(hb:Bird,hn:~Abnormal)."
            r"<;:k:C.<hn||;:'H1:~Abnormal.<H1||?1.1.2.1.2.1*_F_>>||1.1.2.2?>>||thesis>")
        assert open_goals(payload) == [("1.1.2.1.2.1", "rhs", "Abnormal"),
                                       ("1.1.2.2", "lhs", "C")]


class TestLessonS5Supercut:
    PI = "λh:A.μa:B.<h:A||4:A?>"
    E = "μb:A.<!2:A||b:A>*μ'y:B.<!5:B||3:B!>"

    @pytest.mark.parametrize("strategy, expected", [
        ("cbn", ("!5:B", "3:B!")),
        ("cbv", ("!2:A", "4:A?")),
    ])
    def test_critical_pair(self, strategy, expected):
        pi = parse_proof_term(self.PI, category="term")
        e = parse_proof_term(self.E, category="context")
        result = normalize_command(pi, e, strategy)
        assert tuple(pres_str(x) for x in result) == expected

    @pytest.mark.parametrize("strategy, expected", [
        ("cbn", ("r1:A->B", "μb2:A.<!2:A||b2:A>*3:B!")),
        ("cbv", ("r1:A->B", "!2:A*μ'b1:B.<b1:B||3:B!>")),
    ])
    def test_linear_premises_differ_only_by_eta(self, strategy, expected):
        """Exercise S5.2: with every binder used once there is no real choice."""
        pi = parse_proof_term("λh:A.μa:B.<r1:A->B||h:A*a:B>", category="term")
        e = parse_proof_term("μb:A.<!2:A||b:A>*μ'y:B.<y:B||3:B!>", category="context")
        result = normalize_command(pi, e, strategy)
        assert tuple(pres_str(x) for x in result) == expected


class TestLessonS6Crab:
    def test_both_theorems_are_accepted_under_lj(self):
        payload = final_state("s6_crab.fsp")
        assert '((name "ac")(kind prop)(prop "A->C"))' in payload
        assert '((name "bc")(kind prop)(prop "B->C"))' in payload

    def test_proof_terms(self):
        out = subprocess.run([str(FSP), "-ascii", "-c", str(HERE / "s6_crab.fsp")],
                             capture_output=True, text=True, timeout=30).stdout
        closed = {t for t in re.findall(r'\(proof-term "([^"]*)"\)', out) if "?" not in t}
        assert closed == {
            r";:thesis:A->C.<\hA:A.;:y:C.<crab_l||hA*(hB2:B,hAC:A->C).<;:z:C.<hAC||hA*z>||y>>||thesis>",
            r";:thesis:B->C.<\hB:B.;:x:C.<ac||;:r:A.<crab_r||(hB,ac)*r>*x>||thesis>",
        }
