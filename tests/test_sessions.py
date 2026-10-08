"""Sessions and documents (wrap/document.py; tasks.org,
aida-sessions-and-documents): a session owns a Fellowship process, its
settings and a lock, and holds one document at a time; `new document`
replaces it.  Nothing is module-global."""

import threading
import warnings

import pytest

from wrap.cli import setup_prover, execute_script, parse_new_document
from wrap.prover import ProverError


@pytest.fixture
def two():
    a, b = setup_prover(), setup_prover()
    yield a, b
    a.close()
    b.close()


def script(prover, path, **kw):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        execute_script(prover, path, isolate=False, **kw)


def test_two_sessions_do_not_share_a_document(two):
    a, b = two
    script(a, "tests/debates.fspy")
    assert "tweety" in a.arguments and a.graph.edges
    assert not b.arguments and not b.graph.edges and not b.declarations
    b.new_document()
    assert "tweety" in a.arguments and "Flies" in a.declarations


def test_a_new_document_resets_the_document_and_fellowship():
    p = setup_prover()
    try:
        script(p, "tests/debates.fspy")
        assert p.debates == {} and p.arguments and p.names and p.declarations
        revision = p.revision
        p.new_document()
        assert not (p.arguments or p.names or p.declarations or p.debates
                    or p.graph.edges or p.doc.issue_terms or p.doc.strict_edges)
        assert p.revision > revision                  # no older cache looks fresh
        # Fellowship forgot the signature: an axiom over Bird, declared in
        # the old document, is refused now.
        state = p.send_command("declare bird : (Bird).", include_ui=True)
        assert "Symbol Bird undefined" in state["_ui"] and "bird" not in p.declarations
    finally:
        p.close()


@pytest.mark.parametrize("line, logic, minimal, name", [
    ("new document.", "lk", False, "lk"),
    ("new document lj.", "lj", False, "lj"),
    ("new document minimal lk.", "lk", True, "minimal lk"),
    ("new document minimal lj.", "lj", True, "minimal lj"),
])
def test_the_four_logics(prover, line, logic, minimal, name):
    prover.new_document(*parse_new_document(line))
    assert (prover.logic, prover.minimal, prover.doc.logic_name) == (logic, minimal, name)


def test_new_document_syntax():
    assert parse_new_document("new document.") == ("lk", False)
    assert parse_new_document("lk.") is None
    with pytest.raises(ValueError):
        parse_new_document("new document intuitionistic.")


def test_a_toggle_at_the_head_is_accepted_and_a_change_later_refused(prover):
    prover.send_command("lj.")
    assert prover.logic == "lj"
    prover.send_command("declare A : bool.")
    with pytest.raises(ProverError, match="logic is chosen when a document starts"):
        prover.send_command("lk.")
    with pytest.raises(ProverError):
        prover.send_command("minimal.")
    assert (prover.logic, prover.minimal) == ("lj", False) and "A" in prover.declarations
    prover.send_command("lj.")                        # the logic in force: a no-op
    prover.send_command("full.")


def test_load_replaces_the_document(prover, tmp_path):
    # aida-load-replaces-document: the second file's document only
    script(prover, "tests/demo/01_arguments.fspy", new_document=True, stop_marker=False)
    assert "tweety" in prover.arguments
    script(prover, "tests/demo/05_even_loop.fspy", new_document=True, stop_marker=False)
    assert "tweety" not in prover.arguments and "Pdefault" in prover.arguments
    # loading the same file twice gives no "already defined"
    script(prover, "tests/demo/05_even_loop.fspy", new_document=True, stop_marker=False,
           strict=True)


def test_the_new_document_command_in_a_script(prover, tmp_path):
    path = tmp_path / "two.fspy"
    path.write_text("declare A : bool.\nnew document lj.\ndeclare A : bool.\n")
    script(prover, str(path), strict=True)
    assert prover.logic == "lj" and list(prover.declarations) == ["A"]


def test_settings_survive_a_new_document(prover):
    prover.typecheck_enabled = False
    prover.pipeline_unfolded = False
    prover.new_document()
    assert prover.typecheck_enabled is False and prover.pipeline_unfolded is False
    prover.typecheck_enabled = True
    prover.pipeline_unfolded = True


def test_sends_from_two_threads_do_not_cross(prover):
    prover.send_command("declare A, B : bool.")
    barrier = threading.Barrier(2)
    replies, errors = {}, []

    def worker(name, prop):
        try:
            barrier.wait()
            for i in range(10):
                state = prover.send_command(f"declare {name}{i} : ({prop}).")
                assert "errors" not in state or not state["errors"]
            replies[name] = True
        except Exception as e:                        # pragma: no cover - reported below
            errors.append(e)

    threads = [threading.Thread(target=worker, args=("a", "A")),
               threading.Thread(target=worker, args=("b", "B"))]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    assert not errors and set(replies) == {"a", "b"}
    for i in range(10):
        assert prover.declarations[f"a{i}"] == "A" and prover.declarations[f"b{i}"] == "B"
