"""Migrate .fspy text to the 2026-10-09 command syntax (tasks.org,
aida-command-syntax).

    start argument N C            ->  argument N : (C).
    start counterargument N C     ->  counterargument N : (C).
    start antitheorem N C         ->  counterargument N : (C).
    end argument | ...            ->  dixi.
    hora est.                     ->  cedat tempus.
    register N : P := TERM        ->  register N : P := "TERM".
    graph X out.dot show          ->  graph X "out.dot" show.
    load FILE                     ->  load "FILE".
    decorate N : 'template'       ->  decorate N : "template".
    any other command line        ->  the same, ending in "."

The old syntax put one command on a line; comment lines (# and %) stay as
they are.  Usage: python tools/migrate_syntax.py FILE... (in place).
"""

import json
import re
import sys

_START = re.compile(r"^start\s+(argument|counterargument|antitheorem)\s+(\S+)\s+(.+?)\s*$")
_END = re.compile(r"^end\s+(argument|counterargument|antitheorem)\s*\.?$")
_REGISTER = re.compile(r"^(register\s+\S+(?:\s+strict)?\s*:\s*.+?:=)\s*(.+?)\s*$")
_DECORATE = re.compile(r"^(decorate\s+\S+\s*:\s*)'(.*)'\s*\.?\s*$")


def _paren(prop: str) -> str:
    prop = prop.strip().rstrip(".").strip()
    return prop if prop.startswith("(") and prop.endswith(")") else f"({prop})"


def migrate_line(line: str) -> str:
    indent = line[: len(line) - len(line.lstrip())]
    text = line.strip()
    if not text or text[0] in "#%":
        return line
    m = _START.match(text)
    if m:
        kind = "argument" if m.group(1) == "argument" else "counterargument"
        return f"{indent}{kind} {m.group(2)} : {_paren(m.group(3))}."
    if _END.match(text):
        return f"{indent}dixi."
    if text.rstrip(".").strip() == "hora est":
        return f"{indent}cedat tempus."
    m = _REGISTER.match(text)
    if m:
        term = m.group(2).rstrip()
        if not term.startswith('"'):
            term = json.dumps(term, ensure_ascii=False)
        return f"{indent}{m.group(1)} {term}."
    m = _DECORATE.match(text)
    if m:
        return f"{indent}{m.group(1)}{json.dumps(m.group(2), ensure_ascii=False)}."
    words = text.rstrip(".").split()
    if words and words[0] in ("graph", "load"):
        quoted = [w if ("." not in w or w.startswith('"')) or w in (words[0],) else json.dumps(w)
                  for w in words]
        if words[0] == "graph" and len(words) > 1 and words[1] == "issue":
            quoted[2] = words[2]                 # an issue target is no path
        return f"{indent}{' '.join(quoted)}."
    if text.endswith("."):
        return line
    return f"{indent}{text}."


def migrate(text: str) -> str:
    return "\n".join(migrate_line(line) for line in text.split("\n"))


if __name__ == "__main__":
    for path in sys.argv[1:]:
        with open(path) as f:
            old = f.read()
        new = migrate(old)
        if new != old:
            with open(path, "w") as f:
                f.write(new)
            print("migrated", path)
