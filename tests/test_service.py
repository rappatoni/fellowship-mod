"""The service layer (wrap/service.py; tasks.org, aida-service-layer): every
operation returns a result object or raises an AidaError with a stable code,
holds the session lock and collects what it logged."""

import threading
import warnings

import pytest

from core.dc.debate_graph import canonical_prop as K
from wrap.cli import setup_prover, execute_script
from wrap.service import (
    Service, Target, AidaError, NotFound, InvalidRequest, LogicRefused, ContentRefused,
    Evaluation, WitnessEvaluations, GraphResult, LabelResult, TermResult, Rendering, Shared,
    Tree, Inventory, Done,
)


@pytest.fixture
def svc():
    p = setup_prover()
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        execute_script(p, "tests/debates.fspy", strict=True, isolate=False)
    yield Service.of(p)
    p.close()


# -- targets -----------------------------------------------------------------

def test_targets(svc):
    assert svc.target("tweety") == Target("argument", "tweety")
    assert svc.target("document").kind == "document"
    assert svc.target("issue :Flies").issue == (K("Flies"), "term")
    assert svc.target("issue Flies:").issue == (K("Flies"), "context")
    with pytest.raises(InvalidRequest):
        svc.target("issue Flies")
    svc.start_debate("d", "Flies")
    assert svc.target("d").kind == "debate"


def test_one_service_per_session(svc):
    assert Service.of(svc.session) is svc


# -- queries -----------------------------------------------------------------

def test_graph_label_evaluate(svc):
    g = svc.graph("tweety", labels=True)
    assert isinstance(g, GraphResult) and {e.name for e in g.graph.edges} >= {"tweety", "penguin"}
    assert g.labels[(K("Flies"), "term")] == "IN"
    lab = svc.label("tweety", "preferred")
    assert isinstance(lab, LabelResult) and len(lab.labellings) >= 1
    ev = svc.evaluate("tweety")
    assert isinstance(ev, Evaluation) and ev.nf_class == "value" and ev.sigma[(K("Flies"), "term")] == "IN"
    assert svc.session.arguments["tweety"].labelled_nf is ev.normal_form
    allw = svc.evaluate("tweety", mode="credulous", witness="all")
    assert isinstance(allw, WitnessEvaluations) and allw.results


def test_the_document_graph(svc):
    g = svc.graph("document")
    assert g.graph is svc.session.graph


def test_terms_and_renderings(svc):
    t = svc.term("tweety")
    assert isinstance(t, TermResult) and t.which == "unfolded"
    assert svc.term("tweety", "registered").term          # the text Fellowship returned
    with pytest.raises(NotFound):
        svc.term("tweety", "evaluated")                    # not evaluated yet
    svc.evaluate("tweety")
    assert svc.term("tweety", "evaluated").term is not None
    r = svc.render("tweety", "unfolded", "vanilla")
    assert isinstance(r, Rendering) and r.text
    assert isinstance(svc.unfold("issue :Flies"), TermResult)


def test_share_and_tree(svc):
    sh = svc.share("tweety")
    assert isinstance(sh, Shared) and sh.display == "Flies[t]" and sh.text
    tr = svc.tree("tweety", which="unfolded")
    assert isinstance(tr, Tree) and tr.dot.startswith("digraph") and tr.labelled


def test_inventory(svc):
    inv = svc.inventory()
    assert isinstance(inv, Inventory) and inv.logic == "lk"
    names = {a.name: a for a in inv.arguments}
    assert names["penguin"].anti and names["tweety"].in_document
    assert inv.declarations["bird"][1] == "Bird" and inv.names["tweety"] == "argument"


# -- errors ------------------------------------------------------------------

def test_not_found_and_invalid(svc):
    with pytest.raises(NotFound) as e:
        svc.evaluate("nobody")
    assert e.value.code == "not_found" and e.value.stage is None
    with pytest.raises(InvalidRequest):
        svc.evaluate("tweety", mode="sceptical")
    with pytest.raises(InvalidRequest):
        svc.evaluate("document")


def test_a_refusal_names_its_stage(svc):
    svc.new_document("lj")
    svc.command("declare P : bool.")
    svc.record_argument("p", "P", ["by default"])
    with pytest.raises(LogicRefused) as e:
        svc.graph("p")
    assert e.value.code == "logic" and e.value.stage == "graph"
    assert isinstance(e.value, AidaError)


# -- diagnostics -------------------------------------------------------------

def test_warnings_are_collected_on_the_result(svc):
    # penguin presumes Penguin, photo refutes it on a presumption of Photo:
    # nothing opposes; build an onus conflict to get a warning.
    svc.record_argument("pengNot", "Penguin", ["by default"], anti=True)
    svc.record_argument("pengYes", "Penguin", ["by default"])
    g = svc.graph("penguin")
    assert any("Opposing presumptions" in d.message for d in g.diagnostics)


def test_warnings_are_collected_on_the_error(svc):
    svc.new_document("lj")
    svc.command("declare P : bool.")
    svc.record_argument("p", "P", ["by default"])
    with pytest.raises(LogicRefused) as e:
        svc.graph("p")
    assert any("refused in lj" in d.message for d in e.value.diagnostics)


def test_a_thread_collects_only_its_own_warnings(svc):
    import logging
    barrier = threading.Barrier(2)

    def noisy():
        barrier.wait()
        logging.getLogger("core.test").warning("from another thread")

    t = threading.Thread(target=noisy)
    t.start()
    barrier.wait()
    seen = svc.graph("tweety").diagnostics
    t.join()
    assert not any("another thread" in d.message for d in seen)


# -- content -----------------------------------------------------------------

def test_record_state_prove(svc):
    done = svc.record_argument("wing", "Flies", ["cut (Bird -> Flies) r", "by default", "next",
                                                  "elim", "axiom bird", "axiom r"])
    assert isinstance(done, Done) and "wing" in svc.session.arguments
    with pytest.raises(ContentRefused) as e:
        svc.record_argument("wing", "Flies", ["by default"])
    assert e.value.cause.__class__.__name__ == "NameClash"
    svc.state("lemma", "lb", "Bird")
    assert svc.session.names["lb"] == "statement"
    proved = svc.prove("lb", ["axiom bird"])
    assert proved.value.citable


def test_register_and_adopt(svc):
    svc.register("demo", "Bird", "μthesis:Bird.<bird:Bird||thesis:Bird>", strict=True)
    assert svc.session.arguments["demo"].citable
    with pytest.raises(NotFound):
        svc.adopt("nothing*", "x")


def test_debates(svc):
    svc.start_debate("d", "Flies", "pro", "closed")
    svc.move("tweety")
    svc.move("penguin", "rebut", "tweety")
    with pytest.raises(ContentRefused):
        svc.move("photo", "rebut", "penguin")               # a presumption, not an obligation
    svc.move("photo", "undermine", "penguin")
    done = svc.close_debate()
    assert done.value.finished and svc.evaluate("d").nf_class == "value"


def test_settings_and_documents(svc):
    assert svc.set_typecheck("off").message == "off" and svc.session.typecheck_enabled is False
    svc.set_typecheck("on")
    assert svc.set_pipeline("shared").message == "shared debate"
    svc.set_pipeline("unfolded")
    with pytest.raises(InvalidRequest):
        svc.set_pipeline("sideways")
    svc.new_document("lk", True)
    assert svc.inventory().logic == "minimal lk" and not svc.session.arguments
    svc.load("tests/demo/05_even_loop.fspy")
    assert "Pdefault" in svc.session.arguments


def test_operations_hold_the_session_lock(svc):
    # While another thread holds the lock, an operation waits.
    held, released, done = threading.Event(), threading.Event(), []

    def holder():
        with svc.session.lock:
            held.set()
            released.wait(5)

    t = threading.Thread(target=holder)
    t.start()
    held.wait(5)
    worker = threading.Thread(target=lambda: done.append(svc.inventory()))
    worker.start()
    worker.join(0.3)
    assert not done                                   # blocked on the lock
    released.set()
    worker.join(5)
    t.join(5)
    assert done


# -- checking a whole document (aida-document-check) --------------------------

CHECKED = open("tests/debates.fspy").read() + """debate pro closed d : Flies.
tweety.
rebut penguin tweety.
rebut photo penguin.
ct.
evaluate d.
evaluate nobody.
explain d.
argument broken : (Flies).
axiom nothing.
dixi.
"""


def test_a_document_check_reports_every_command(svc):
    import json
    import jsonschema
    from wrap.serialize import to_json
    report = svc.check(CHECKED)
    out = to_json(report, svc.session)
    jsonschema.validate(out, json.load(open("wrap/schemas/aida.schema.json")))
    assert out["kind"] == "report" and out["ok"] is False
    problems = {(e["span"]["line"], e["command"], e["status"]) for e in out["entries"]
                if e["status"] not in ("ok", "comment")}
    assert problems == {(44, "move", "refused"), (47, "evaluate", "reported"),
                        (48, "explain", "skipped"), (51, "dixi", "refused")}
    evaluation = next(e for e in out["entries"] if e["command"] == "evaluate" and e["status"] == "ok")
    assert evaluation["span"]["line"] == 46 and evaluation["result"]["verdict"] == "open"


def test_a_check_starts_a_new_document_each_time(svc):
    first = svc.check("lk.\ndeclare A : bool.\n")
    second = svc.check("lk.\ndeclare A : bool.\n")
    assert first.ok and second.ok                        # no "already declared"


def test_a_check_stops_at_stop(svc):
    report = svc.check("lk.\n%stop\ndeclare A : bool.\n")
    assert [o.status for o in report.entries] == ["ok", "stop"]


# -- the transport (aida-transport-limits) -------------------------------------

def test_long_commands_reach_fellowship(svc):
    s = svc.session
    names = ",".join(f"X{i}" for i in range(400))
    svc.command(f"declare {names} : bool.")
    assert "X399" in s.declarations
    prop = " -> ".join(f"X{i}" for i in range(300)) + " -> X0"
    assert len(prop) > 2000
    state = s.send_command(f"theorem long : ({prop}).", include_ui=True)   # Fellowship itself
    assert "goal" in str(state.get("goals")) or state.get("goals")
    s.send_command("discard theorem.")


# -- importing (aida-scasp-importer-provenance) -----------------------------------

class _StubImport:
    def to_fspy_with_sources(self, *, name="imported"):
        text = 'lk.\ndeclare A:bool.\ndeclare a:(A).\nregister ' + name + ' : A := "μalpha1:A.<a||alpha1>".\n'
        return text, {"blocks": [{"span": {"line": 4, "col": 1, "end_line": 4, "end_col": 52},
                                  "kind": "argument", "name": name,
                                  "source": {"pointer": "/answers/0/tree/0", "atom": "a"}}],
                      "binders": {"alpha1": {"pointer": "/answers/0/tree/0", "atom": "a"}}}

    def to_fspy(self, *, name="imported"):
        return self.to_fspy_with_sources(name=name)[0]


def test_an_import_is_checked_with_its_provenance(svc, monkeypatch):
    import json
    import jsonschema
    from wrap import importers
    from wrap.serialize import to_json
    monkeypatch.setattr(importers, "load_importer",
                        lambda language: (lambda data: _StubImport()) if language == "stub"
                        else (_ for _ in ()).throw(importers.ImporterNotFound(language)))
    report = svc.import_document("stub", {"answers": []}, name="from_stub")
    out = to_json(report, svc.session)
    jsonschema.validate(out, json.load(open("wrap/schemas/aida.schema.json")))
    assert out["kind"] == "import" and out["ok"] is True
    assert "from_stub" in svc.session.arguments
    assert out["sources"]["binders"]["alpha1"]["atom"] == "a"
    with pytest.raises(NotFound):
        svc.import_document("klingon", {})


def test_a_failed_block_leaves_fellowship_clean(svc):
    # the refused replay closes the theorem it opened, so the next command -
    # here the type check of an evaluation - is not refused for an open proof
    report = svc.check(open("tests/debates.fspy").read()
                       + "argument broken : (Flies).\naxiom nothing.\ndixi.\n")
    assert [(o.kind, o.status) for o in report.entries if o.status not in ("ok", "comment")] == \
        [("dixi", "refused")]
    assert svc.evaluate("tweety").nf_class == "value"
