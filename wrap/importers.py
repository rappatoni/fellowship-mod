"""Source-language importers for `acdc --import`.

Importers live in separate packages and register themselves under the
``acdc.importers`` entry-point group, keyed by source language, e.g. in the
importer's ``pyproject.toml``::

    [project.entry-points."acdc.importers"]
    scasp = "scasp_aida.importer:translate_json"

The registered object takes the parsed source JSON and returns an
`ImportResult`. Translation failures are reported by raising a subclass of
`SourceImportError`.
"""
from __future__ import annotations

from importlib.metadata import entry_points
from pathlib import Path
from typing import Any, Callable, Protocol

GROUP = "acdc.importers"

#: Packages that provide importers, for the "not installed" hint.
KNOWN_PACKAGES = {"scasp": "scasp-aida"}


class SourceImportError(Exception):
    """Raised by an importer when its input cannot be translated."""


class ImporterNotFound(LookupError):
    """No importer is installed for the requested source language."""


class ImportResult(Protocol):
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


def missing_importer_message(language: str) -> str:
    available = ", ".join(available_importers()) or "none"
    message = f"no importer for source language '{language}' is installed (available: {available})"
    package = KNOWN_PACKAGES.get(language)
    if package:
        message += f"; install the {package} package"
    return message
