"""Migrate the .fspy scripts written inline in Python test files to the
2026-10-09 command syntax (tools/migrate_syntax.py, line by line).

A string literal counts as a script when it holds a line break (an escaped
``\\n`` or a real one, in a triple-quoted string) and one of its lines starts
with a command word.  Usage: python tools/migrate_test_scripts.py FILE...
"""

import io
import re
import sys
import tokenize

sys.path.insert(0, ".")
from tools.migrate_syntax import migrate_line  # noqa: E402

WORDS = {"lk", "lj", "declare", "deny", "argument", "counterargument", "dixi", "prove", "refine",
         "refute", "argue", "dispute", "qed", "theorem", "lemma", "claim", "proposition",
         "antitheorem", "antilemma", "graph", "label", "evaluate", "explain", "render", "tree",
         "unfold", "share", "adopt", "cite", "axiom", "cut", "elim", "next", "by", "moxia",
         "register", "decorate", "typecheck", "pipeline", "debate", "cedat", "new", "start", "end",
         "hora", "minimal", "full", "load", "expand"}
_PREFIX = re.compile(r"^([rRbBuUfF]*)('''|\"\"\"|'|\")")


def _migrate_body(body: str, sep: str) -> str:
    parts = body.split(sep)
    out = []
    for part in parts:
        if not part.strip():
            out.append(part)
            continue
        out.append(migrate_line(part))
    return sep.join(out)


def _first(line: str) -> str:
    return line.strip().split(" ")[0].rstrip(".")


def _is_script(body: str, sep: str) -> bool:
    if sep not in body:
        return False
    lines = [p for p in body.split(sep) if p.strip()]
    if not any(_first(p) in WORDS for p in lines):
        return False
    if sep == "\\n":
        return True                            # "...\n..." in one literal
    # a triple-quoted string: a script only if every line reads as one -
    # a command word, a comment, or a line that already ends a command
    return all(_first(p) in WORDS or p.strip()[0] in "#%" or p.rstrip().endswith(".")
               or "{" in p for p in lines)


def migrate_source(src: str) -> str:
    tokens = list(tokenize.generate_tokens(io.StringIO(src).readline))
    lines = src.splitlines(keepends=True)
    offsets = [0]
    for line in lines:
        offsets.append(offsets[-1] + len(line))
    edits = []
    for tok in tokens:
        if tok.type != tokenize.STRING:
            continue
        m = _PREFIX.match(tok.string)
        if not m or "b" in m.group(1).lower():
            continue
        quote = m.group(2)
        body = tok.string[len(m.group(0)):-len(quote)]
        sep = "\n" if len(quote) == 3 else "\\n"
        if not _is_script(body, sep):
            continue
        new_body = _migrate_body(body, sep)
        if new_body != body:
            start = offsets[tok.start[0] - 1] + tok.start[1]
            end = offsets[tok.end[0] - 1] + tok.end[1]
            edits.append((start, end, m.group(0) + new_body + quote))
    for start, end, text in reversed(edits):
        src = src[:start] + text + src[end:]
    return src


if __name__ == "__main__":
    for path in sys.argv[1:]:
        old = open(path).read()
        new = migrate_source(old)
        if new != old:
            open(path, "w").write(new)
            print("migrated", path)
