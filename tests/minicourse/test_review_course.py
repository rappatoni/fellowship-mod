"""Pins every value quoted in docs/minicourses/minicourse-evaluation.org, lesson 13b: the
review of the semantics decisions of 2026-09-25 (tasks.org,
aida-review-guard-and-strict-decisions).

The graph-level examples are built with coursekit as in docs/minicourses/minicourse.org
lesson 4; the strict-phase examples with the helpers of tests/test_strict.py;
the fixture rows run the CLI on the committed fixtures.  If the
implementation changes, these fail and the lesson text must be updated.
"""

import os
import subprocess
import sys
import warnings
from copy import deepcopy
from pathlib import Path

import pytest

HERE = Path(__file__).parent
ROOT = HERE.parents[1]
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(ROOT / "tests"))

from coursekit import conditions_of, graph_from_edges, label_of  # noqa: E402
from core.ac.ast import ID, DI, Mu  # noqa: E402
from core.comp.adf_label import OpposingPresumptions, grounded_labels  # noqa: E402
from core.comp.oracle_terms import classify_nf, normalize_strong  # noqa: E402
from core.dc.strict import strict_resolve  # noqa: E402
from pres.gen import pres_str  # noqa: E402
from test_strict import A, STRICT, axiom, denied, presumed, tatt, tsup  # noqa: E402

P, O = "presumption", "obligation"


def labels(graph):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", OpposingPresumptions)
        return {label_of(s, graph): v for s, v in grounded_labels(graph).items()}


def conditions(graph):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore", OpposingPresumptions)
        return conditions_of(graph)


def edge(name, target, side, sources=(), strict=False):
    return (name, target, side, list(sources), strict, "argument")


# --- point 1 -----------------------------------------------------------

G1A = graph_from_edges(["Q", "S"], [edge("qAtt", "Q", "context", [("S", "term", P)])],
                       [("Q", "term", P), ("S", "term", P)])
G1B = graph_from_edges(["Q"], [edge("qAtt", "Q", "context", strict=True)], [("Q", "term", P)])
G1C = graph_from_edges(["Q", "R", "S"],
                       [edge("qArg", "Q", "term", [("R", "term", P)]),
                        edge("qAtt", "Q", "context", [("S", "term", P)])],
                       [("R", "term", P), ("S", "term", P)])
G1D = graph_from_edges(["Q", "R"],
                       [edge("qArg", "Q", "term", [("R", "term", P)]),
                        edge("qAtt", "Q", "context", strict=True)],
                       [("R", "term", P)])


@pytest.mark.parametrize("graph, qt, qc, cond_t, cond_c", [
    (G1A, "OUT", "IN", "(true & not Q[c])", "S[t]"),
    (G1B, "OUT", "IN", "(true & not Q[c])", "(true | false)"),
    (G1C, "UNDEC", "UNDEC", "(R[t] & not Q[c])", "(S[t] & not Q[t])"),
    (G1D, "OUT", "IN", "(R[t] & not Q[c])", "(true | false)"),
], ids=["1a", "1b", "1c", "1d"])
def test_point1_strictness_against_a_bare_default(graph, qt, qc, cond_t, cond_c):
    got = labels(graph)
    assert (got["Q[t]"], got["Q[c]"]) == (qt, qc)
    c = conditions(graph)
    assert (c["Q[t]"], c["Q[c]"]) == (cond_t, cond_c)


# --- point 2 -----------------------------------------------------------

def test_point2b_a_dead_argument_does_not_decide_two_defaults():
    g = graph_from_edges(["Q", "R"], [edge("qArg", "Q", "term", [("R", "term", O)])],
                         [("Q", "term", P), ("Q", "context", P), ("R", "term", O)])
    assert conditions(g) == {"Q[t]": "(R[t] | (true & not Q[c]))",
                             "R[t]": "false", "Q[c]": "(true & not Q[t])"}
    assert labels(g) == {"Q[t]": "UNDEC", "R[t]": "OUT", "Q[c]": "UNDEC"}


def test_point2a_a_dead_argument_turns_a_defeat_into_a_stalemate():
    g = graph_from_edges(["Q", "R", "S"],
                         [edge("qArg", "Q", "term", [("R", "term", O)]),
                          edge("qAtt", "Q", "context", [("S", "term", P)])],
                         [("Q", "term", P), ("R", "term", O), ("S", "term", P)])
    c = conditions(g)
    assert (c["Q[t]"], c["Q[c]"]) == ("((R[t] | true) & not Q[c])", "(S[t] & not Q[t])")
    assert labels(g) == {"Q[t]": "UNDEC", "R[t]": "OUT", "Q[c]": "UNDEC", "S[t]": "IN"}
    assert (labels(G1A)["Q[t]"], labels(G1A)["Q[c]"]) == ("OUT", "IN")


# --- point 3 -----------------------------------------------------------

def test_point3_two_bare_defaults_warn_and_stay_undecided():
    g = graph_from_edges(["Q"], [], [("Q", "term", P), ("Q", "context", P)])
    with pytest.warns(OpposingPresumptions):
        got = grounded_labels(g)
    assert {label_of(s, g): v for s, v in got.items()} == {"Q[t]": "UNDEC", "Q[c]": "UNDEC"}


# --- point 4 -----------------------------------------------------------

def test_point4_the_multiple_undercut_graph_grounds_two_valued():
    g = graph_from_edges(["A", "B", "C", "D"],
                         [edge("arg1", "B", "term", [("A", "term", P)]),
                          edge("counter1", "A", "context", [("B", "term", O)]),
                          edge("counter2", "A", "context", [("C", "term", P)]),
                          edge("counter3", "A", "context", [("D", "term", O)])])
    assert labels(g) == {"B[t]": "OUT", "A[t]": "OUT", "A[c]": "IN", "C[t]": "IN", "D[t]": "OUT"}


def test_point4_the_reductio_graph_is_undecided():
    g = graph_from_edges(["Q"], [edge("red", "Q", "term", [("Q", "context", P)])],
                         [("Q", "context", P)])
    assert conditions(g) == {"Q[t]": "Q[c]", "Q[c]": "(true & not Q[t])"}
    assert labels(g) == {"Q[t]": "UNDEC", "Q[c]": "UNDEC"}


def run_cli(tmp_path, fixture, commands):
    """Run a committed fixture without its own graph/label/evaluate lines,
    followed by ``commands``; return the output."""
    lines = (ROOT / fixture).read_text(encoding="utf-8").splitlines()
    kept = [l for l in lines if not l.startswith(("graph", "label", "evaluate", "explain"))]
    script = tmp_path / Path(fixture).name
    script.write_text("\n".join(kept + commands) + "\n", encoding="utf-8")
    env = dict(os.environ, ACDC_NO_RENDER="1", ACDC_NO_OPEN="1")
    done = subprocess.run([str(ROOT / ".venv/bin/acdc"), "--script", str(script)],
                          cwd=ROOT, env=env, capture_output=True, text=True, timeout=300)
    return done.stdout + done.stderr


def test_point4_the_even_loop_document_has_two_stable_labellings(tmp_path):
    out = run_cli(tmp_path, "tests/rationality/even_loop_lk.fspy", ["label document stable."])
    assert "2 stable labellings for 'document'" in out


def test_point4_cyclic_undercut(tmp_path):
    out = run_cli(tmp_path, "tests/rationality/cyclic_undercut.fspy",
                  ["label document.", "evaluate d credulous.", "evaluate d skeptical."])
    grounded = out.split("Grounded labelling for 'document':")[1].split("Evaluated")[0].split()
    rows = {(grounded[i], grounded[i + 1]): grounded[i + 2] for i in range(0, 18, 3)}
    assert rows == {("A", "term"): "IN", ("Q", "term"): "IN", ("B", "term"): "OUT",
                    ("R", "term"): "OUT", ("Q", "context"): "OUT", ("R", "context"): "IN"}
    assert out.count("normal form: μargA:A.<qa:Q->A||!IN:Q*argA:A>") == 2


# --- point 5 -----------------------------------------------------------

def test_point5_the_self_attack(tmp_path):
    out = run_cli(tmp_path, "tests/rationality/self_attack_lk.fspy",
                  ["label SelfAttack.", "explain SelfAttack credulous."])
    assert "P[t] supporter strict" in out
    assert "edge 'SelfAttack*' on P[t] (a closed derivation the framework missed)" in out
    assert "[1] P->false[t]=OUT  P[c]=OUT  P[t]=IN" in out
    assert "normal form: μalt2:P.<pRule:(P->false)->P||λb1:P.μb2:false.<b1:P||alt2:P>*alt2:P>" in out


@pytest.mark.parametrize("name, mine, other", [("Pdefault", "P", "Q"), ("Qdefault", "Q", "P")])
def test_point5_the_even_loop_is_symmetric(tmp_path, name, mine, other):
    out = run_cli(tmp_path, "tests/rationality/even_loop_lk.fspy",
                  [f"explain {name} credulous."])
    other_name = "Qdefault" if name == "Pdefault" else "Pdefault"
    for line in (f"{other}[t] supporter strict",
                 f"{other}[t] attacker strict: defeat",
                 f"{mine}[t] delayed for the labelling (neither the original nor the supporter is strict)",
                 f"no edge for '{name}' on {mine}[t]: not closed (an open site remains)",
                 f"no edge for '{other_name}' on {other}[t]: not closed (free: rule)",
                 f"2 decision(s), 0 strict edge(s) for '{name}'"):
        assert line in out
    rule, other_rule = f"{mine.lower()}Rule", f"{other.lower()}Rule"
    assert (f"normal form: μalt4:{mine}.<{rule}:({other}->false)->{mine}||λb1:{other}.μb2:false."
            f"<{other_rule}:({mine}->false)->{other}||λb2:{mine}.μb5:false.<b2:{mine}||alt4:{mine}>"
            f"*IN:{other}!>*alt4:{mine}>") in out


# --- points 6 and 7 ----------------------------------------------------

CLASH = Mu(ID("c", A), A, DI("ax", A), ID("nx", A))


@pytest.mark.parametrize("supporter", [axiom, presumed], ids=["strict", "presumed"])
def test_point6_a_site_free_clash_wins(supporter):
    term = Mu(ID("d", A), A, tsup(deepcopy(CLASH), deepcopy(supporter)), ID("d", A))
    trace = []
    out, edges = strict_resolve(deepcopy(term), STRICT, trace=trace)
    assert [w for _, w in trace] == ["original strict"]
    assert pres_str(deepcopy(out)) == "μd:A.<μc:A.<ax:A||nx:A>||d:A>"
    assert classify_nf(normalize_strong(deepcopy(out))) == "exception"
    assert edges == []


def test_point7_one_edge_for_the_outermost_closed_wrapper():
    term = Mu(ID("d", A), A, tsup(tatt(deepcopy(presumed), deepcopy(denied)), deepcopy(axiom)),
              ID("d", A))
    trace = []
    _, edges = strict_resolve(deepcopy(term), STRICT, trace=trace)
    assert [w for _, w in trace] == ["attacker strict: defeat", "supporter strict"]
    assert [e.name for e in edges] == ["d*"]

