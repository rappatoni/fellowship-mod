"""Source-language importers for `acdc --import`.

Importers live in separate packages and register themselves under the
``acdc.importers`` entry-point group, keyed by source language, e.g. in the
importer's ``pyproject.toml``::

    [project.entry-points."acdc.importers"]
    scasp = "scasp_aida.importer:translate_json"

The registered object takes the parsed source JSON and returns an
`ImportResult`. Translation failures are reported by raising a subclass of
`SourceImportError`.

An `ImportResult` renders the translation as an AIDA document in the
command syntax of `wrap/syntax.py`: `to_fspy()` gives the text, `write_fspy()`
writes it.  It may also offer `to_fspy_with_sources()`, the text with its
provenance - `{"blocks": [{"span", "kind", "name", "source"}], "binders":
{binder: source}}`, where a source points into the importer's input - which
`Service.import_document` passes on, so errors and labels can be shown
against the source.

An importer may also read its source language directly - an s(CASP)
program rather than s(CASP)'s JSON - under the ``acdc.importer_sources``
group: a callable taking the path and returning an `ImportResult`
(scasp-aida's ``import_scasp_file`` runs s(CASP) itself).  `register
debate NAME import LANGUAGE "SOURCE".` uses it when SOURCE is not JSON.

For `register debate ... import`, ``to_fspy(name=NAME)`` writes the
registered-debate form: the declarations the debate shares with the
document, then one ``register debate NAME [legacy] : ISSUE.`` block with
its own declarations, its ``term "..."`` and ``cedat tempus.``.
"""
from __future__ import annotations

from importlib.metadata import entry_points
from pathlib import Path
from typing import Any, Callable, Optional, Protocol

GROUP = "acdc.importers"
#: Importers that read the source language itself, keyed alike.
SOURCE_GROUP = "acdc.importer_sources"

#: Packages that provide importers, for the "not installed" hint.
KNOWN_PACKAGES = {"scasp": "scasp-aida"}


class SourceImportError(Exception):
    """Raised by an importer when its input cannot be translated."""


class ImporterNotFound(LookupError):
    """No importer is installed for the requested source language."""


class ImportResult(Protocol):
    def to_fspy(self, *, name: str = "imported") -> str: ...

    def write_fspy(self, path: str | Path, *, name: str | None = None) -> Path: ...


Translator = Callable[[Any], ImportResult]


def available_importers() -> list[str]:
    """Return the source languages of all installed importers."""
    return sorted({ep.name for ep in entry_points(group=GROUP)})


def load_importer(language: str) -> Translator:
    """Return the translator registered for ``language``."""
    for ep in entry_points(group=GROUP, name=language):
        return ep.load()
    raise ImporterNotFound(language)


def load_source_reader(language: str) -> Optional[Callable[[str], ImportResult]]:
    """The reader of ``language``'s own files (a program, not JSON), or
    None if the importer offers none."""
    for ep in entry_points(group=SOURCE_GROUP, name=language):
        return ep.load()
    return None


def missing_importer_message(language: str) -> str:
    available = ", ".join(available_importers()) or "none"
    message = f"no importer for source language '{language}' is installed (available: {available})"
    package = KNOWN_PACKAGES.get(language)
    if package:
        message += f"; install the {package} package"
    return message
