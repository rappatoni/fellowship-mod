"""The IRIS follow-up (lib/iris_experiment/, tasks.org aida-iris-usufruct):
the Org tangler, and every literate document of the experiment checked as
a whole by the service."""

import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))

from tools.tangle_fspy import TangleError, read_org, tangle, tangle_text, unescape  # noqa: E402
from wrap.service import Service  # noqa: E402

FIXTURES = Path(__file__).resolve().parent / "fixtures"
IRIS = ROOT / "lib" / "iris_experiment"


def test_tangle_prelude_property_name_and_escape(tmp_path):
    out = {p.name: p.read_text() for p in tangle([FIXTURES / "smoke.org"], tmp_path)}
    assert set(out) == {"smoke.fspy", "smoke_noweb.fspy"}
    smoke = out["smoke.fspy"]
    # the prelude comes first, then the file's blocks; the `:tangle no`
    # block is left out
    assert smoke.startswith("lk.\ndeclare Q, P : bool.\n")
    assert "\n# P follows from Q by the rule; Q is delegated.\n" in smoke
    assert "this is not AIDA" not in smoke
    # noweb: <<body>> is replaced by the named block, and the prelude is
    # put in front of this target too
    noweb = out["smoke_noweb.fspy"]
    assert noweb.count("argument pArg : (P).") == 1
    assert noweb.rstrip().endswith("evaluate pArg.")


def test_include_takes_blocks_without_prelude_or_combined_no(tmp_path):
    (FIXTURES / "smoke.org").read_text()
    out = {p.name: p.read_text() for p in tangle([FIXTURES / "smoke_combined.org"], tmp_path)}
    text = out["smoke_combined.fspy"]
    # the vocabulary once (this file's prelude), smoke.org's argument once,
    # its noweb target's blocks too, then this file's block last
    assert text.count("declare Q, P : bool.") == 1
    assert text.count("argument pArg : (P).") == 2      # body + the noweb copy
    assert text.rstrip().endswith("label pArg.")


def test_org_comma_escapes_are_undone():
    assert unescape(",* heading") == "* heading"
    assert unescape("  ,#+begin_src") == "  #+begin_src"
    assert unescape(",# narration") == ",# narration"   # not an Org escape


def test_tangle_refuses_unknown_and_cyclic_references(tmp_path):
    bad = tmp_path / "bad.org"
    bad.write_text("#+begin_src fspy :noweb yes :tangle x.fspy\n<<nowhere>>\n#+end_src\n")
    with pytest.raises(TangleError, match="no block named 'nowhere'"):
        tangle_text(read_org(bad))
    loop = tmp_path / "loop.org"
    loop.write_text("#+name: a\n#+begin_src fspy :noweb yes :tangle x.fspy\n<<a>>\n#+end_src\n")
    with pytest.raises(TangleError, match="cycle"):
        tangle_text(read_org(loop))


def test_tangled_smoke_document_checks(prover, tmp_path):
    svc = Service.of(prover)
    for path in tangle([FIXTURES / "smoke.org"], tmp_path):
        report = svc.check(path.read_text())
        assert report.ok, [(e.status, e.message) for e in report.entries if e.status != "ok"]


def _iris_scripts(tmp):
    return tangle(sorted(IRIS.glob("*.org")), tmp)


def test_every_iris_document_checks(prover, tmp_path):
    """Every literate document of the experiment tangles and replays without
    a refusal or an error (the values each one shows are pinned per
    document as the phases add them)."""
    scripts = _iris_scripts(tmp_path)
    svc = Service.of(prover)
    for path in scripts:
        report = svc.check(path.read_text())
        assert report.ok, (path.name, [(e.status, e.message) for e in report.entries
                                       if e.status in ("refused", "error", "fatal")])


# --- Phase 1 spike (lib/iris_experiment/analysis.org, section 5) ----------

def _spike(name, tmp):
    for path in _iris_scripts(tmp):
        if path.name == name:
            return path.read_text()
    raise AssertionError(f"{name} not tangled")


def _verdict(svc, text, target, upto=None, **options):
    if upto is not None:
        text = text[: text.index(upto)]
    report = svc.check(text)
    assert report.ok, [(e.status, e.message) for e in report.entries
                       if e.status in ("refused", "error", "fatal")]
    ev = svc.evaluate(svc.target(target), **options)
    from pres.gen import ProofTermGenerationVisitor
    import copy
    return ev.nf_class, ProofTermGenerationVisitor().visit(copy.deepcopy(ev.normal_form)).pres


def test_spike_helper_conjunction_burden_per_conjunct(prover, tmp_path):
    """Fly's refutation presumed (the proponent must prove Fly), Feathers'
    refutation owed (an opponent must refute it)."""
    svc, text = Service.of(prover), _spike("spike-helper.fspy", tmp_path)
    cls, nf = _verdict(svc, text, "bird", upto="argument fly : (Fly).")
    assert cls == "open" and "?OUT:Bird" in nf
    cls, nf = _verdict(svc, text, "bird")
    assert cls == "value" and "!IN:H" in nf


def test_spike_subtraction_conjunction(prover, tmp_path):
    svc, text = Service.of(prover), _spike("spike-subtraction.fspy", tmp_path)
    assert _verdict(svc, text, "bird", upto="argument featherless")[0] == "value"
    assert _verdict(svc, text, "bird")[0] == "open"


def test_spike_first_order_scenarios(prover, tmp_path):
    """No registrability argument: the claim-duty cycle is undecided
    (credulously the bare delegation); argued from a substantial need: a
    value; with a justified doubt: undecided again."""
    svc, text = Service.of(prover), _spike("spike-first-order.fspy", tmp_path)
    s1 = "# Scenario 2"
    cls, nf = _verdict(svc, text, "claim", upto=s1)
    assert cls == "open" and "?UNDEC:Claim kg court e1" in nf
    cls, nf = _verdict(svc, text, "claim", upto=s1, mode="credulous")
    assert cls == "value" and nf.strip().startswith("!IN:Claim kg court e1")
    cls, nf = _verdict(svc, text, "claim", upto=s1, semantics="stable")
    assert cls == "open" and "?OUT:Claim kg court e1" in nf
    assert _verdict(svc, text, "claim", upto="# Scenario 3")[0] == "value"
    cls, nf = _verdict(svc, text, "claim")
    assert cls == "open" and "?UNDEC:Claim kg court e1" in nf


@pytest.mark.xfail(reason="tasks.org aida-unfold-negation-binder-capture", strict=True)
def test_bug_unfold_negation_lemma(prover):
    svc = Service.of(prover)
    text = (FIXTURES / "bug_unfold_negation_lemma.fspy").read_text()
    report = svc.check(text.replace("graph d.", ""))
    assert report.ok
    svc.graph(svc.target("d"))


# --- Phase 2: each decision's own debate (lib/iris_experiment/rq3-*.org) ----

@pytest.mark.parametrize("script, debate, skeptical, credulous", [
    ("rq3-stuttgart.fspy", "stg", "value", "value"),
    ("rq3-oldenburg.fspy", "old", "value", "value"),
    # the refutation-only courts: their own debate is undecided skeptically
    # (the § 1071 readings and the influence presumption meet), their
    # position one consistent extension (credulous)
    ("rq3-muenchen.fspy", "muc", "open", "value"),
    ("rq3-koeln.fspy", "koe", "open", "value"),
])
def test_rq3_court_debates(prover, tmp_path, script, debate, skeptical, credulous):
    svc, text = Service.of(prover), _spike(script, tmp_path)
    assert _verdict(svc, text, debate)[0] == skeptical
    cls, nf = _verdict(svc, text, debate, mode="credulous")
    assert cls == credulous
    if debate in ("muc", "koe"):
        assert nf.startswith(f"μ'{debate}_restraint:Registrable usufruct.")


# --- Phase 2: the showcase fragments (lib/iris_experiment/rq3-fragments.org) --

FRAGMENTS = [
    # name, skeptical (class, normal-form mark), credulous class
    ("frag_a", ("value", "!IN:Excluded usufruct"), "value"),
    ("frag_b", ("value", "μold_registrable:Registrable usufruct"), "value"),
    ("frag_c", ("value", "μold_need_codet:Need usufruct"), "value"),
    ("frag_d", ("open", "?OUT:Need usufruct"), "open"),
    ("frag_e", ("open", "?OUT:Registrable usufruct"), "open"),
    ("frag_f", ("open", "?UNDEC:CoDetermines usufruct"), "value"),
    ("frag_g", ("open", "UNDEC:Registrable usufruct"), "value"),
]


def test_rq3_fragments_under_stable(prover, tmp_path):
    """Each fragment's verdict as quoted in rq3-fragments.org (stable)."""
    svc, text = Service.of(prover), _spike("rq3-fragments.fspy", tmp_path)
    report = svc.check(text)
    assert report.ok, [(e.status, e.message) for e in report.entries
                       if e.status in ("refused", "error", "fatal")]
    from pres.gen import ProofTermGenerationVisitor
    import copy

    def verdict(name, **options):
        ev = svc.evaluate(svc.target(name), semantics="stable", **options)
        return ev.nf_class, ProofTermGenerationVisitor().visit(copy.deepcopy(ev.normal_form)).pres

    for name, (cls, mark), credulous in FRAGMENTS:
        got, nf = verdict(name)
        assert got == cls and mark in nf, (name, got, nf)
        assert verdict(name, mode="credulous")[0] == credulous, name


# --- Phase 2: the four decisions in one document (rq3-combined.org) --------

def test_rq3_combined_stable_labellings(prover, tmp_path):
    """Five stable labellings of the issue: one where Registrable is OUT (the
    position of München and Köln), four where it is IN (Stuttgart and
    Oldenburg), which vary over ConsentConstitutive x LikeExecutorship."""
    svc, text = Service.of(prover), _spike("rq3-combined.fspy", tmp_path)
    report = svc.check(text)
    assert report.ok, [(e.status, e.message) for e in report.entries
                       if e.status in ("refused", "error", "fatal")]
    lab = svc.label(svc.target("issue :Registrable[usufruct]"), "stable")
    key = {str(v): k for k, v in lab.graph.nodes.items()}

    def row(statement, side="term"):
        return [L.get((key[statement], side)) for L in lab.labellings]

    assert len(lab.labellings) == 5
    reg = row("Registrable usufruct")
    assert reg.count("OUT") == 1
    con = reg.index("OUT")
    assert row("CoDetermines usufruct")[con] == "OUT"
    assert row("Powerless usufruct")[con] == "IN"
    pro = [i for i in range(5) if i != con]
    combos = {(row("ConsentConstitutive usufruct")[i], row("LikeExecutorship usufruct")[i])
              for i in pro}
    assert combos == {("IN", "IN"), ("IN", "OUT"), ("OUT", "IN"), ("OUT", "OUT")}
    for statement in ("VotesDayToDay usufruct", "Liability usufruct", "InfoSuffices"):
        assert set(row(statement)) == {"OUT"}, statement
    assert set(row("Unlike usufruct")) == {"IN"}
    ev = svc.evaluate(svc.target("issue :Registrable[usufruct]"), semantics="stable")
    assert ev.nf_class == "open"


# --- Phases 3-5: the meta-norm, the burden of proof, the cycle -------------
# (lib/iris_experiment/rq4-6-7-metanorm.org; the expected verdicts are
# written in the document before its scenarios)

METANORM = [
    # script, target, skeptical class, mark in the normal form
    ("rq4-a.fspy", "duty", "value", "!IN:MetaNorm"),
    ("rq4-b.fspy", "duty", "open", "?OUT:Duty court app e1"),
    ("rq4-c.fspy", "duty", "open", "?OUT:Duty court app e1"),
    ("rq4-d.fspy", "duty", "value", "!IN:MetaNorm"),
    ("rq6-a.fspy", "duty", "value", "!IN:Passed e1"),
    ("rq6-b.fspy", "duty", "open", "?OUT:Duty court app e1"),
    ("rq6-c.fspy", "duty", "open", "?OUT:Duty court app e1"),
    ("rq6-d.fspy", "duty", "value", "!IN:Passed e1"),
    ("rq7-a.fspy", "claim", "open", "?UNDEC:Claim app court e1"),
    ("rq7-b.fspy", "claim", "value", "μclaim:Claim app court e1"),
    ("rq7-c.fspy", "claim", "open", "?OUT:Claim app court e1"),
    ("rq7-d.fspy", "claim", "value", "μclaim:Claim app court e1"),
]


@pytest.mark.parametrize("script, target, cls, mark", METANORM)
def test_metanorm_scenarios(prover, tmp_path, script, target, cls, mark):
    from core.comp.adf_label import opposing_presumptions
    svc, text = Service.of(prover), _spike(script, tmp_path)
    got, nf = _verdict(svc, text, target, semantics="stable")
    assert got == cls and mark in nf, (got, nf)
    # the burden of proof stays consistent: nothing presumed on both sides
    lab = svc.label(svc.target(target), "stable")
    assert not opposing_presumptions(lab.graph)


def test_rq7_cycle_two_readings(prover, tmp_path):
    """The claim-scope cycle alone: grounded undecided; two stable
    labellings, claim and duty IN together or OUT together."""
    svc, text = Service.of(prover), _spike("rq7-a.fspy", tmp_path)
    assert svc.check(text).ok
    tgt = svc.target("claim")
    lab = svc.label(tgt, "stable")
    key = {str(v): k for k, v in lab.graph.nodes.items()}
    rows = {s: [L.get((key[s], "term")) for L in lab.labellings]
            for s in ("Claim app court e1", "Duty court app e1", "Limited e1", "Passed e1")}
    assert sorted(rows["Claim app court e1"]) == ["IN", "OUT"]
    for s in rows:
        assert rows[s] == rows["Claim app court e1"], s
    grounded = svc.label(tgt, "grounded").labellings[0]
    assert grounded.get((key["Claim app court e1"], "term")) == "UNDEC"


# --- RQ 6 processing: the evaluation returns the court's order --------------

@pytest.mark.parametrize("script, witness", [
    ("rq6p-a.fspy", "o_entry"),
    ("rq6p-b.fspy", "o_submit"),
    ("rq6p-c.fspy", "o_prove"),
    ("rq6p-d.fspy", "o_entry"),
])
def test_rq6_decision_witness(prover, tmp_path, script, witness):
    svc, text = Service.of(prover), _spike(script, tmp_path)
    cls, nf = _verdict(svc, text, "decision", semantics="stable")
    assert cls == "value" and f"<({witness}," in nf, nf


# --- RQ 4 dynamics: the case law as one debate (rq4-gap-dynamics.org) -------

def test_rq4_gap_dynamics_stages(prover, tmp_path):
    """The status of the issue after each stage of the case law: closed by
    the restraint, opened and filled by Stuttgart and Oldenburg, contested
    from München on (5 stable labellings, 1 con)."""
    import re
    svc, text = Service.of(prover), _spike("rq4-gap-dynamics.fspy", tmp_path)
    cut = [m.start() for m in re.finditer(r"^label lawdev stable\.", text, re.M)]
    assert len(cut) == 5
    expected = [("value", 1), ("exception", 1), ("exception", 1), ("open", 5), ("open", 5)]
    for upto, (cls, n) in zip(cut, expected):
        prefix = text[:upto] + "cedat tempus.\n"
        report = svc.check(prefix)
        assert report.ok, [(e.status, e.message) for e in report.entries
                           if e.status in ("refused", "error", "fatal")]
        assert svc.evaluate(svc.target("lawdev"), semantics="stable").nf_class == cls
        assert len(svc.label(svc.target("lawdev"), "stable").labellings) == n


# --- RQ 3: the course of a debate (rq3-debate-dynamics.org) -----------------

def test_rq3_oldenburg_course(prover, tmp_path):
    """Oldenburg's decision as a trajectory: each objection opens the issue,
    each answer closes it again."""
    import re
    svc, text = Service.of(prover), _spike("rq3-debate-dynamics.fspy", tmp_path)
    head = text[: text.index("debate con closed muc_t")]
    stops = [m.start() for m in re.finditer(r"^evaluate old_t stable\.", head, re.M)]
    got = []
    for upto in stops:
        assert svc.check(head[:upto] + "cedat tempus.\n").ok
        got.append(svc.evaluate(svc.target("old_t"), semantics="stable").nf_class)
    assert got == ["open", "open", "value", "value", "value", "value",
                   "open", "value", "value", "value", "open", "value"]


# --- RQ 6: the witness rises to the top (rq4-6-7-metanorm.org, layer 2c) ----

@pytest.mark.parametrize("script, target, head", [
    ("rq6w-a.fspy", "decision_c", None),             # finding defeated: open
    ("rq6w-b.fspy", "decision_c", "interim d_appl"),  # computed by cut elimination
    ("rq6k-a.fspy", "decision_k", "o_entry"),         # classical: duty established
    ("rq6k-b.fspy", "decision_k", "o_submit"),        # classical: duty not established
    ("rq6k-c.fspy", "decision_k", "o_submit"),
    ("rq6k-d.fspy", "decision_k", "o_entry"),
])
def test_rq6_witness_rises(prover, tmp_path, script, target, head):
    from core.comp.adf_label import opposing_presumptions
    svc, text = Service.of(prover), _spike(script, tmp_path)
    cls, nf = _verdict(svc, text, target, semantics="stable")
    if head is None:
        assert cls == "open", nf
    else:
        assert cls == "value" and f".<({head}," in nf, nf
    assert not opposing_presumptions(svc.label(svc.target(target), "stable").graph)


# --- Paper renderings: the witness in the check-list styles ------------------

def test_rq6_witness_rendered_as_a_checklist(prover, tmp_path):
    """The computed order reads as prose: the function symbol `interim` is
    decorated, and the witness and instantiation lines carry it."""
    svc, text = Service.of(prover), _spike("rq6w-b.fspy", tmp_path)
    assert _verdict(svc, text, "decision_c", semantics="stable")[0] == "value"
    en = svc.render("decision_c", "evaluated", "checklist").text
    assert "witnessed by an interim order to submit the application in certified form" in en
    assert "instantiating d with the application in certified form" in en
    de = svc.render("decision_c", "evaluated", "pruefschema").text
    assert "belegt durch an interim order to submit the application in certified form" in de
    assert "Prüfung decision_c abgeschlossen (bejaht)." in de
    assert "Starting check decision_c:" in en and "Check decision_c completed (holds)." in en
