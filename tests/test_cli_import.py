"""`acdc --import`: importer lookup and the file mode, with a stub importer."""

from pathlib import Path

import pytest

from wrap import cli, importers
from wrap.importers import ImporterNotFound, SourceImportError


class _ParserError(Exception):
    pass


class _Parser:
    def error(self, message):
        raise _ParserError(message)


class _StubResult:
    def write_fspy(self, path, *, name=None):
        path = Path(path)
        path.write_text(f"register {name} : A := stub\n")
        return path


def _stub_translate(data):
    if data.get("fail"):
        raise SourceImportError("cannot translate")
    return _StubResult()


@pytest.fixture
def stub_importer(monkeypatch):
    def load(language):
        if language == "stub":
            return _stub_translate
        raise ImporterNotFound(language)

    monkeypatch.setattr(cli, "load_importer", load)


def test_file_mode_writes_the_importer_script(stub_importer, tmp_path):
    source = tmp_path / "source.json"
    source.write_text("{}")
    target = tmp_path / "target.fspy"

    cli._handle_import_command(_Parser(), ["stub", str(source), "file", str(target)])

    assert target.read_text() == "register source : A := stub\n"


def test_importer_errors_are_reported_through_the_parser(stub_importer, tmp_path):
    source = tmp_path / "source.json"
    source.write_text('{"fail": true}')

    with pytest.raises(_ParserError, match="cannot translate"):
        cli._handle_import_command(_Parser(), ["stub", str(source), "file"])


def test_missing_importer_names_the_package_to_install(stub_importer, tmp_path):
    source = tmp_path / "source.json"
    source.write_text("{}")

    with pytest.raises(_ParserError, match="install the scasp-aida package"):
        cli._handle_import_command(_Parser(), ["scasp", str(source), "file"])


def test_load_importer_raises_for_unregistered_language():
    with pytest.raises(ImporterNotFound):
        importers.load_importer("no-such-language")
