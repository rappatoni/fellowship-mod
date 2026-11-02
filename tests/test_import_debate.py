"""Importing a debate from a source language (tasks.org,
aida-registered-debate-import):

    register debate NAME import LANGUAGE "SOURCE".

The importer (wrap/importers.py) writes the registered-debate form - the
declarations the debate shares, then one `register debate NAME` block -
and that text is run into the current document.  A stub importer stands
in for scasp-aida here; it writes the form from the stored imports in
tests/import/."""

import json
import sys
import warnings

import pytest

from wrap import importers
from wrap.cli import execute_script, setup_prover
from wrap.importers import SourceImportError
from wrap.interpreter import Interpreter
from wrap.service import AidaError, ContentRefused, InvalidRequest, NotFound, Service
from wrap.syntax import SyntaxRefused, parse

sys.path.insert(0, "tests")
from test_registered_debate import block  # noqa: E402

#: Binders of tests/import/simple.fspy, as scasp-aida maps them to the
#: justification tree (JSON pointers into its output).
BINDERS = {"alpha9": {"pointer": "/answers/0/tree/1", "atom": "a"},
           "alpha1": {"pointer": "/answers/0/tree/1/children/0", "atom": "b"},
           "alpha2": {"pointer": "/answers/0/tree/1/children/1", "atom": "c"}}


class Stub:
    """What scasp-aida's translate returns, in the registered-debate form."""

    def __init__(self, fixture="tests/import/simple.fspy", extra=""):
        self.fixture, self.extra = fixture, extra

    def to_fspy_with_sources(self, *, name):
        text = self.extra + block(self.fixture, name)
        return text, {"blocks": [{"span": [1, 0, 1, 10], "kind": "declaration"}], "binders": BINDERS}


@pytest.fixture
def prover():
    p = setup_prover()
    yield p
    p.close()


@pytest.fixture
def stub(monkeypatch):
    """The stub registered as language `stub`; ``seen`` records what it
    was called with."""
    seen = {}

    def translate(data):
        seen["data"] = data
        if data.get("fail"):
            raise SourceImportError("the answer has no tree")
        return Stub(extra=data.get("extra", ""))

    def reader(path):
        seen["path"] = path
        return Stub()

    monkeypatch.setattr(importers, "load_importer",
                        lambda language: translate if language == "stub" else _missing(language))
    monkeypatch.setattr(importers, "load_source_reader",
                        lambda language: reader if seen.get("reads_programs") else None)
    return seen


def _missing(language):
    raise importers.ImporterNotFound(language)


def run(prover, text):
    return list(Interpreter(prover).run(text))


def test_syntax():
    c = parse('register debate s import scasp "tests/fixtures/simple.pl"')
    assert (c.kind, c["name"], c["language"], c["source"]) == (
        "import_debate", "s", "scasp", "tests/fixtures/simple.pl")
    with pytest.raises(SyntaxRefused):
        parse("register debate s import scasp tests/fixtures/simple.pl")


class TestImport:
    def test_from_json_relative_to_the_script(self, prover, stub, tmp_path):
        (tmp_path / "answer.json").write_text(json.dumps({"program": "simple"}))
        script = tmp_path / "doc.fspy"
        script.write_text('register debate s import stub "answer.json".\nevaluate s.\n')
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            execute_script(prover, str(script), strict=True, isolate=False)
        assert stub["data"] == {"program": "simple"}
        debate = prover.debates["s"]
        assert len(debate.extracted) == 5 and Service.of(prover).evaluate("s").nf_class == "value"
        assert {"A", "B", "C"} <= set(prover.declarations)        # what it shares
        assert "s__H_A" in debate.debate_session.session.declarations   # what is its own

    def test_provenance_maps_each_arguments_binders_to_the_source(self, prover, stub):
        Service.of(prover).import_debate("s", "stub", data={"program": "simple"})
        provenance = prover.debates["s"].provenance
        assert provenance["language"] == "stub" and "register debate s legacy : A." in provenance["text"]
        assert provenance["blocks"] and provenance["binders"] == BINDERS
        # s__a1 is A's argument (mu alpha9), s__a5 b's proof (mu alpha1)
        assert provenance["arguments"]["s__a1"] == {"alpha9": BINDERS["alpha9"]}
        assert provenance["arguments"]["s__a5"] == {"alpha1": BINDERS["alpha1"]}

    def test_a_program_through_the_importers_reader(self, prover, stub, tmp_path):
        stub["reads_programs"] = True
        Service.of(prover).import_debate("s", "stub", "prog.pl", base_dir=str(tmp_path))
        assert stub["path"] == str(tmp_path / "prog.pl") and "s" in prover.debates

    def test_a_program_without_a_reader_is_refused(self, prover, stub):
        with pytest.raises(InvalidRequest, match="reads its JSON only"):
            Service.of(prover).import_debate("s", "stub", "prog.pl")

    def test_the_documents_declarations_are_reused(self, prover, stub):
        run(prover, "declare A, B, C : bool.")
        Service.of(prover).import_debate("s", "stub", data={"program": "simple"})
        assert "s" in prover.debates

    def test_two_imports_share_the_programs_atoms(self, prover, stub):
        Service.of(prover).import_debate("s", "stub", data={})
        Service.of(prover).import_debate("t", "stub", data={})
        assert {"s", "t"} <= set(prover.debates)
        assert "s__H_A" in prover.debates["s"].debate_session.session.declarations
        assert "t__H_A" in prover.debates["t"].debate_session.session.declarations


class TestRefusals:
    def test_a_conflicting_declaration(self, prover, stub):
        run(prover, "declare B : type.")
        with pytest.raises(ContentRefused, match="the document declares B : type"):
            Service.of(prover).import_debate("s", "stub", data={})

    def test_only_declarations_and_the_debate(self, prover, stub):
        with pytest.raises(ContentRefused, match="not `graph document`"):
            Service.of(prover).import_debate("s", "stub", data={"extra": "graph document.\n"})

    def test_exactly_the_debate_named(self, prover, stub, monkeypatch):
        monkeypatch.setattr(Stub, "to_fspy_with_sources",
                            lambda self, *, name: (block(self.fixture, "other"), {}))
        with pytest.raises(ContentRefused, match="exactly one debate, 's'"):
            Service.of(prover).import_debate("s", "stub", data={})

    def test_the_importers_refusal(self, prover, stub):
        with pytest.raises(ContentRefused, match="no tree"):
            Service.of(prover).import_debate("s", "stub", data={"fail": True})

    def test_an_unknown_language(self, prover, stub):
        with pytest.raises(NotFound, match="no importer"):
            Service.of(prover).import_debate("s", "nolang", data={})

    def test_a_missing_file(self, prover, stub):
        with pytest.raises(NotFound, match="cannot read"):
            Service.of(prover).import_debate("s", "stub", "nowhere.json")

    def test_from_a_script_the_refusal_is_the_commands(self, prover, stub):
        outcome = run(prover, 'register debate s import stub "nowhere.json".')[-1]
        assert outcome.status == "refused" and "cannot read" in outcome.message
