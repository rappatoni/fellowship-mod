"""Evaluation under stable semantics against s(CASP) (tasks.org,
aida-scasp-stable-correspondence).

A propositional normal program is translated into a document:

    h.                       declare f : H;  an argument  mu a:H.< f || a >   (strict)
    h :- b, not c.           declare r : B -> (C->false) -> H;
                             an argument  mu a:H.< r || ?1:B * lambda x:C. mu z:false.< x || 2:C! > * a >
    every atom a             the presumed refutation  mu' n:A.< n || 1:A! >  (closed world)

An atom premise is an obligation, ``not c`` the presumed refutation of c
(as in tests/naf_olon.fspy), and every atom is presumed false unless
derived (the closed-world assumption of stable models; without it no
``not a`` that no rule body uses is ever accepted).

For every atom a, the queries ``a`` (issue A[t]) and ``not a`` (A[c]) are
compared three ways: the program's stable models (brute force,
Gelfond-Lifschitz), s(CASP)'s answer to the query, and ``evaluate_debate``
on the issue's unfolded term, VALUE counting as accepted.  Credulous:
some stable model makes the query true; skeptical: there is a stable
model and every one makes it true.

The disagreements are listed, each with its cause (DISAGREE_*); every
other row agrees.
"""

import copy
import itertools
import os
import shutil
import subprocess
import warnings

import pytest

from core.comp.evaluate import evaluate_debate, EvaluationRefused
from core.dc.debate_graph import declaration_kinds, canonical_prop as K
from core.dc.unfold import unfold
from wrap.cli import execute_script, setup_prover


PROGRAMS = {
    "pos":           "a :- b. b :- a.",
    "pos_founded":   "a :- b. b :- a. a :- not c.",
    "even":          "p :- not q. q :- not p.",
    "odd":           "p :- not p.",
    "odd_forced":    "r :- not s. s :- not r. p :- not p, s.",
    "fact_two":      "c. d :- c, not e.",
    "odd_elsewhere": "q. p :- not p.",
    "pos_even":      "a :- b. b :- a. b :- not c. c :- not b.",
}

#: Relevance: an issue graph holds only what the issue depends on, while
#: stable models - and s(CASP), through its global NMR check - are
#: constrained by every odd loop of the program, also one that depends on
#: the issue (odd_forced: p :- not p, s forbids s) or is unrelated to it
#: (odd_elsewhere: no stable model at all).
RELEVANCE = "relevance: an odd loop outside the issue graph constrains the stable models"

#: (program, query, mode) -> cause, for stable semantics.
DISAGREE_STABLE = {
    ("odd_forced", "not r", "credulous"): RELEVANCE,
    ("odd_forced", "s", "credulous"): RELEVANCE,
    ("odd_forced", "r", "skeptical"): RELEVANCE,
    ("odd_forced", "not s", "skeptical"): RELEVANCE,
    ("odd_elsewhere", "q", "credulous"): RELEVANCE,
    ("odd_elsewhere", "q", "skeptical"): RELEVANCE,
}

#: Where preferred differs from stable (supported vs stable semantics).
#: SUPPORTED: the positive loop a :- b, b :- a is admissible IN/IN, so
#: credulous preferred accepts it coinductively and skeptical preferred
#: leaves its refutation UNDEC.  CAPTURE: in p :- not p the argument's own
#: continuation for P captures its presumption of "not p", so the argument
#: proves (P->false)->P => P classically (consequentia mirabilis) and is
#: strict; preferred labels P UNDEC, and the strict phase keeps the proof.
#: Stable has no labelling there (refused, like s(CASP)'s "no models").
SUPPORTED = "supported semantics: a positive loop is admissible IN"
CAPTURE = "the argument's own continuation captures its 'not p': classically a proof of p"
DIFFER_PREFERRED = {
    ("pos", "a", "credulous"): SUPPORTED,
    ("pos", "b", "credulous"): SUPPORTED,
    ("pos", "not a", "skeptical"): SUPPORTED,
    ("pos", "not b", "skeptical"): SUPPORTED,
    ("odd", "p", "credulous"): CAPTURE,
    ("odd", "p", "skeptical"): CAPTURE,
    ("odd_elsewhere", "p", "credulous"): CAPTURE,
    ("odd_elsewhere", "p", "skeptical"): CAPTURE,
    ("odd_forced", "not p", "skeptical"): "the forced odd loop leaves P UNDEC in a preferred labelling",
}


# ---------------------------------------------------------------------------
# Programs
# ---------------------------------------------------------------------------

def parse(src):
    rules = []
    for clause in (c.strip() for c in src.split(".")):
        if not clause:
            continue
        head, _, body = clause.partition(":-")
        pos, neg = [], []
        for lit in (part.strip() for part in body.split(",")):
            if lit.startswith("not "):
                neg.append(lit[4:].strip())
            elif lit:
                pos.append(lit)
        rules.append((head.strip(), pos, neg))
    return rules


def atoms(rules):
    return sorted({h for h, _, _ in rules} | {a for _, p, n in rules for a in p + n})


def stable_models(rules):
    """Every M with M = the least model of the reduct P^M."""
    found = []
    universe = atoms(rules)
    for bits in itertools.product((False, True), repeat=len(universe)):
        model = {a for a, bit in zip(universe, bits) if bit}
        reduct = [(h, pos) for h, pos, neg in rules if not set(neg) & model]
        least, changed = set(), True
        while changed:
            changed = False
            for h, pos in reduct:
                if h not in least and set(pos) <= least:
                    least.add(h)
                    changed = True
        if least == model:
            found.append(model)
    return found


def prop(atom):
    return atom.upper()


def to_fspy(rules):
    universe = atoms(rules)
    lines = ["lk.", f"declare {','.join(prop(a) for a in universe)}:bool."]
    registers = [f"register cwa_{a} : {prop(a)} := \"μ'cwa_{a}:{prop(a)}.<cwa_{a}||1:{prop(a)}!>\"."
                 for a in universe]
    for i, (head, pos, neg) in enumerate(rules, 1):
        name, h = f"a{i}_{head}", prop(head)
        if not pos and not neg:
            lines.append(f"declare f{i}:({h}).")
            registers.append(f"register {name} : {h} := \"μ{name}:{h}.<f{i}||{name}>\".")
            continue
        premises = [prop(a) for a in pos] + [f"({prop(b)}->false)" for b in neg]
        lines.append(f"declare r{i}:({' -> '.join(premises + [h])}).")
        sites = itertools.count(1)
        args = ([f"?{next(sites)}:{prop(a)}" for a in pos]
                + [f"λx:{prop(b)}.μz:false.<x||{next(sites)}:{prop(b)}!>" for b in neg])
        registers.append(f"register {name} : {h} := \"μ{name}:{h}.<r{i}||{'*'.join(args + [name])}>\".")
    return "\n".join(lines + registers) + "\n"


def queries(rules):
    for a in atoms(rules):
        yield a, (K(prop(a)), "term")
        yield f"not {a}", (K(prop(a)), "context")


def truth(models, query, mode):
    holds = [(query.removeprefix("not ") in m) != query.startswith("not ") for m in models]
    return any(holds) if mode == "credulous" else bool(models) and all(holds)


# ---------------------------------------------------------------------------
# Evaluation
# ---------------------------------------------------------------------------

@pytest.fixture(scope="module")
def verdicts(tmp_path_factory):
    """{(program, query, semantics, mode): accepted}."""
    out = {}
    tmp = tmp_path_factory.mktemp("scasp_programs")
    for name, src in PROGRAMS.items():
        rules = parse(src)
        path = tmp / f"{name}.fspy"
        path.write_text(to_fspy(rules))
        prover = setup_prover()
        try:
            with warnings.catch_warnings():
                warnings.simplefilter("ignore")
                execute_script(prover, str(path), strict=False, stop_on_error=True, isolate=False,
                               render_files=False, stop_marker=False)
            doc = prover.graph
            options = dict(strict_names=list(prover.declarations),
                           strict_kinds=declaration_kinds(prover.declarations))
            for query, statement in queries(rules):
                term = unfold(doc, statement)
                for semantics in ("stable", "preferred"):
                    for mode in ("credulous", "skeptical"):
                        try:
                            with warnings.catch_warnings():
                                warnings.simplefilter("ignore")
                                _, cls, *_ = evaluate_debate(copy.deepcopy(term), name, mode=mode,
                                                             semantics=semantics, **options)
                            accepted = cls == "value"
                        except EvaluationRefused:
                            accepted = False          # no labelling of that semantics
                        out[(name, query, semantics, mode)] = accepted
        finally:
            prover.close()
    return out


def rows():
    for name, src in PROGRAMS.items():
        for query, _ in queries(parse(src)):
            for mode in ("credulous", "skeptical"):
                yield name, query, mode


ROWS = list(rows())


@pytest.mark.parametrize("name, query, mode", ROWS)
def test_stable_evaluation_against_stable_models(verdicts, name, query, mode):
    expected = truth(stable_models(parse(PROGRAMS[name])), query, mode)
    found = verdicts[(name, query, "stable", mode)]
    if (name, query, mode) in DISAGREE_STABLE:
        assert found != expected, f"listed as a disagreement but agrees: {name} {query} {mode}"
    else:
        assert found == expected


@pytest.mark.parametrize("name, query, mode", ROWS)
def test_preferred_differs_from_stable_only_where_listed(verdicts, name, query, mode):
    differs = verdicts[(name, query, "preferred", mode)] != verdicts[(name, query, "stable", mode)]
    assert differs == ((name, query, mode) in DIFFER_PREFERRED)


def test_the_listed_disagreements_are_all_relevance():
    # The only way stable evaluation and the stable models part: the
    # issue graph does not see an odd loop outside it.
    assert set(DISAGREE_STABLE.values()) == {RELEVANCE}
    assert {name for name, _, _ in DISAGREE_STABLE} == {"odd_forced", "odd_elsewhere"}


# ---------------------------------------------------------------------------
# s(CASP)
# ---------------------------------------------------------------------------

SCASP = os.environ.get("SCASP_BIN") or shutil.which("scasp")


@pytest.mark.skipif(not SCASP, reason="s(CASP) not installed (set SCASP_BIN)")
@pytest.mark.parametrize("name, query", [(n, q) for n, q, m in ROWS if m == "credulous"])
def test_scasp_answers_a_query_iff_some_stable_model_makes_it_true(tmp_path, name, query):
    # s(CASP) is a goal-directed procedure for stable models, with the
    # global NMR check: its answers are the brute-force credulous truth,
    # so stable evaluation relates to s(CASP) as above (relevance apart).
    path = tmp_path / f"{name}.pl"
    path.write_text(f"{PROGRAMS[name]}\n?- {query}.\n")
    out = subprocess.run([SCASP, "-s1", str(path)], capture_output=True, text=True,
                         timeout=30).stdout
    assert ("ANSWER" in out) == truth(stable_models(parse(PROGRAMS[name])), query, "credulous")


# ---------------------------------------------------------------------------
# The document graph
# ---------------------------------------------------------------------------

@pytest.mark.parametrize("name", list(PROGRAMS))
def test_the_documents_stable_labellings_are_the_stable_models(tmp_path, name):
    # Labelled as a whole - every argument, not only those an issue depends
    # on - the document has one stable labelling per stable model: A[t] is
    # IN iff a is in the model, and A[c] is IN iff A[t] is not (an atom no
    # rule derives has no A[t] node).  So the six disagreements above are
    # relevance alone: they come from labelling the issue's debate instead
    # of the document.
    from core.comp.adf_label import labellings
    rules = parse(PROGRAMS[name])
    path = tmp_path / f"{name}.fspy"
    path.write_text(to_fspy(rules))
    prover = setup_prover()
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            execute_script(prover, str(path), strict=False, stop_on_error=True, isolate=False,
                           render_files=False, stop_marker=False)
        found = []
        for labelling in labellings(prover.graph, "stable"):
            model = {a for a in atoms(rules) if labelling.get((K(prop(a)), "term")) == "IN"}
            for a in atoms(rules):
                assert (labelling[(K(prop(a)), "context")] == "IN") == (a not in model)
            found.append(sorted(model))
    finally:
        prover.close()
    assert sorted(found) == sorted(sorted(m) for m in stable_models(rules))
