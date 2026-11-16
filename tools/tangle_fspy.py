"""Tangle the ``fspy`` source blocks of Org files into ``.fspy`` scripts.

A small subset of Org-babel tangling, so that tests and ``make`` can run
literate AIDA documents without Emacs (lib/iris_experiment/):

- ``#+begin_src fspy :tangle FILE`` ... ``#+end_src`` blocks are written
  to FILE in document order; FILE is relative to the Org file. A default
  target may be set file-wide with
  ``#+property: header-args:fspy :tangle FILE``; ``:tangle no`` (Org's
  default) skips a block.
- ``#+name: NAME`` directly above a block names it; in a block with
  ``:noweb yes`` a line consisting of ``<<NAME>>`` (indentation kept) is
  replaced by that block's body, recursively, as Org does.
- Org's comma escapes inside a block (``,*`` and ``,#+``) are undone.
- ``#+fspy_prelude: OTHER.org`` (not an Org keyword; Org ignores it) puts
  everything OTHER.org tangles in front of each target this file
  tangles: the shared vocabulary of several documents. Emacs's own
  tangling leaves it out.
- ``#+fspy_include: OTHER.org`` (likewise ours) inserts, at that point of
  this file, the blocks OTHER.org tangles - without OTHER.org's prelude
  and without its blocks marked ``:combined no`` - into every target of
  this file: one document assembled from several (the combined RQ 3
  graph). The included blocks come before the blocks below the keyword.
  ``#+fspy_include: OTHER.org NAME ...`` inserts only the blocks of
  OTHER.org named NAME, in the order listed, whether or not OTHER.org
  tangles them: definitions reused by another document (the meta-norm
  process of rq4-gap-dynamics.org).

Usage: ``python tools/tangle_fspy.py [--outdir DIR] FILE.org ...``
With ``--outdir`` each target is written to DIR under its base name.
"""

from __future__ import annotations

import argparse
import re
import shlex
from dataclasses import dataclass, field
from pathlib import Path

BEGIN = re.compile(r"^\s*#\+begin_src\s+(\S+)(.*)$", re.IGNORECASE)
END = re.compile(r"^\s*#\+end_src\s*$", re.IGNORECASE)
NAME = re.compile(r"^\s*#\+name:\s*(\S+)\s*$", re.IGNORECASE)
PROPERTY = re.compile(r"^\s*#\+property:\s*header-args:fspy\s+(.*)$", re.IGNORECASE)
PRELUDE = re.compile(r"^\s*#\+fspy_prelude:\s*(\S+)\s*$", re.IGNORECASE)
INCLUDE = re.compile(r"^\s*#\+fspy_include:\s*(\S+)((?:\s+\S+)*)\s*$", re.IGNORECASE)
NOWEB_REF = re.compile(r"^(\s*)<<([^<>\s]+)>>\s*$")


class TangleError(Exception):
    pass


@dataclass
class Block:
    name: str | None
    args: dict[str, str]
    body: list[str]
    line: int


@dataclass
class OrgFile:
    path: Path
    blocks: list[Block] = field(default_factory=list)
    defaults: dict[str, str] = field(default_factory=dict)
    prelude: Path | None = None
    # (position in org.blocks, included file, block names or ()): the
    # included text goes before block N
    includes: list[tuple[int, Path, tuple[str, ...]]] = field(default_factory=list)


def parse_args(text: str) -> dict[str, str]:
    """``:tangle x.fspy :noweb yes`` -> {"tangle": "x.fspy", "noweb": "yes"}."""
    out: dict[str, str] = {}
    key = None
    for tok in shlex.split(text):
        if tok.startswith(":"):
            key = tok[1:]
            out[key] = ""
        elif key is not None:
            out[key] = (out[key] + " " + tok).strip()
    return out


def unescape(line: str) -> str:
    return re.sub(r"^(\s*),(\*|#\+)", r"\1\2", line)


def read_org(path: Path) -> OrgFile:
    org = OrgFile(path)
    lines = path.read_text(encoding="utf-8").split("\n")
    pending_name = None
    i = 0
    while i < len(lines):
        line = lines[i]
        if m := PROPERTY.match(line):
            org.defaults.update(parse_args(m.group(1)))
        elif m := PRELUDE.match(line):
            org.prelude = (path.parent / m.group(1)).resolve()
        elif m := INCLUDE.match(line):
            org.includes.append((len(org.blocks), (path.parent / m.group(1)).resolve(),
                                 tuple(m.group(2).split())))
        elif m := NAME.match(line):
            pending_name = m.group(1)
            i += 1
            continue
        elif m := BEGIN.match(line):
            lang, args = m.group(1), m.group(2)
            start = i + 1
            body = []
            i += 1
            while i < len(lines) and not END.match(lines[i]):
                body.append(unescape(lines[i]))
                i += 1
            if i == len(lines):
                raise TangleError(f"{path}:{start}: #+begin_src without #+end_src")
            if lang == "fspy":
                merged = dict(org.defaults)
                merged.update(parse_args(args))
                org.blocks.append(Block(pending_name, merged, body, start))
        # A name applies only to the element directly below it.
        pending_name = None
        i += 1
    return org


def expand(org: OrgFile, block: Block, seen: tuple[str, ...] = ()) -> list[str]:
    if block.args.get("noweb", "no") != "yes":
        return block.body
    named = {b.name: b for b in org.blocks if b.name}
    out = []
    for line in block.body:
        m = NOWEB_REF.match(line)
        if not m:
            out.append(line)
            continue
        indent, ref = m.groups()
        if ref in seen:
            raise TangleError(f"{org.path}:{block.line}: noweb reference cycle via <<{ref}>>")
        if ref not in named:
            raise TangleError(f"{org.path}:{block.line}: no block named {ref!r}")
        target = named[ref]
        sub = expand(org, Block(target.name, {**target.args, "noweb": block.args.get("noweb")},
                                target.body, target.line), seen + (ref,))
        out.extend(indent + s if s else s for s in sub)
    return out


def included_text(path: Path, names: tuple[str, ...] = ()) -> str:
    """What an included file contributes: its tangled blocks, in order, with
    neither its prelude nor its blocks marked ``:combined no``; or, if NAMES
    are given, just the blocks so named, in that order."""
    org = read_org(path)
    out = []
    if names:
        named = {b.name: b for b in org.blocks if b.name}
        for name in names:
            if name not in named:
                raise TangleError(f"{path}: no block named {name!r} to include")
            out.append("\n".join(expand(org, named[name])).rstrip("\n"))
        return "\n".join(out) + "\n"
    for i, block in enumerate(org.blocks):
        for pos, inc, sub in org.includes:
            if pos == i:
                out.append(included_text(inc, sub).rstrip("\n"))
        if block.args.get("tangle", "no") in ("no", "") or block.args.get("combined") == "no":
            continue
        out.append("\n".join(expand(org, block)).rstrip("\n"))
    return "\n".join(out) + "\n"


def tangle_text(org: OrgFile) -> dict[Path, str]:
    """Target path -> tangled text, prelude and includes included."""
    chunks: dict[Path, list[str]] = {}
    pending: list[str] = []
    for i, block in enumerate(org.blocks):
        pending.extend(included_text(inc, sub).rstrip("\n") for pos, inc, sub in org.includes if pos == i)
        target = block.args.get("tangle", "no")
        if target in ("no", ""):
            continue
        path = (org.path.parent / target).resolve()
        chunk = chunks.setdefault(path, [])
        chunk.extend(pending)
        pending = []
        chunk.append("\n".join(expand(org, block)).rstrip("\n"))
    prelude = ""
    if org.prelude is not None:
        pre = tangle_text(read_org(org.prelude))
        prelude = "".join(pre[p] for p in pre)
    return {p: prelude + "\n".join(c) + "\n" for p, c in chunks.items()}


def tangle(paths: list[Path], outdir: Path | None = None) -> list[Path]:
    written = []
    for p in paths:
        for target, text in tangle_text(read_org(Path(p).resolve())).items():
            dest = (outdir / target.name) if outdir else target
            dest.parent.mkdir(parents=True, exist_ok=True)
            dest.write_text(text, encoding="utf-8")
            written.append(dest)
    return written


def main(argv: list[str] | None = None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--outdir", type=Path)
    ap.add_argument("files", nargs="+", type=Path)
    ns = ap.parse_args(argv)
    for w in tangle(ns.files, ns.outdir):
        print(w)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
