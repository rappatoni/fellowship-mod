"""The command syntax of AIDA documents (.fspy) and of the REPL
(tasks.org, aida-command-syntax; Phase 5 of aida-dui-api).

A document is a sequence of *units*:

- comment lines: ``# narration`` (shown), ``% silent`` (ignored), ``%stop``
  (scripts stop here; what follows is for the presenter to paste);
- commands, each ending in ``.``.  A command may span several lines, and
  several may share one (``minimal. lk.``).  Every ``.`` outside double
  quotes ends a command, so what may contain dots is double-quoted: proof
  terms (``register N : A := "mu x:A.<x||?1:A?>".``), decoration templates
  and file names (``load "tests/demo/01_arguments.fspy".``).  A single
  quote is an ordinary character - ``A'``, the binder ``μ'``.

``split`` cuts a text into units with their spans; ``parse`` reads one
command.  The wrapper's own commands are described by ``COMMANDS`` below;
any other command - a Fellowship command (``declare``, ``deny``, ``lk``)
or tactic (``cut``, ``axiom``, ``elim``, ``by default``) - is *opaque*: it
is passed to Fellowship whole, and ``parser.mly`` stays its only grammar.
The forms the syntax replaced (``start argument``, ``end argument``,
``hora est``) are refused with the form that replaces them.
"""

from __future__ import annotations

import re
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional


@dataclass(frozen=True)
class Span:
    """Where a unit sits in its text: 1-based lines and columns, the end
    inclusive (the command's final ``.``)."""
    line: int
    col: int
    end_line: int
    end_col: int

    def __str__(self) -> str:
        return f"{self.line}:{self.col}"


@dataclass
class Unit:
    kind: str            # "command" | "narration" | "silent" | "stop" | "incomplete"
    text: str            # a command without its final "."; a comment without its marker
    span: Span


@dataclass
class Command:
    kind: str
    text: str
    span: Optional[Span] = None
    args: Dict[str, Any] = field(default_factory=dict)

    def __getitem__(self, key):
        return self.args[key]

    def get(self, key, default=None):
        return self.args.get(key, default)


class SyntaxRefused(ValueError):
    """A command that does not parse; ``span`` says where."""

    def __init__(self, message: str, span: Optional[Span] = None):
        super().__init__(message)
        self.span = span


# ---------------------------------------------------------------------------
# Splitting
# ---------------------------------------------------------------------------

def unquote(text: str) -> str:
    """``"x"`` -> ``x`` (JSON escapes read); anything else unchanged."""
    import json
    text = text.strip()
    if len(text) >= 2 and text[0] == text[-1] == '"':
        try:
            return json.loads(text)
        except ValueError:
            return text[1:-1]
    return text


def split(text: str) -> List[Unit]:
    """The units of ``text``, in order, with their spans."""
    units: List[Unit] = []
    n = len(text)
    i, line, col = 0, 1, 1

    def advance(k: int):
        nonlocal i, line, col
        for _ in range(k):
            if text[i] == "\n":
                line, col = line + 1, 1
            else:
                col += 1
            i += 1

    while i < n:
        if text[i].isspace():
            advance(1)
            continue
        start_line, start_col = line, col
        if text[i] in "#%":
            end = text.find("\n", i)
            end = n if end == -1 else end
            body = text[i + 1:end].strip()
            if text[i] == "%":
                kind = "stop" if body.rstrip(".").strip().lower() == "stop" else "silent"
            else:
                kind = "narration"
            span = Span(start_line, start_col, start_line, start_col + (end - i) - 1)
            units.append(Unit(kind, body, span))
            advance(end - i)
            continue
        begin, quoted = i, False
        while i < n:
            ch = text[i]
            if quoted:
                if ch == "\\":
                    advance(1)                       # an escaped character
                elif ch == '"':
                    quoted = False
            elif ch == '"':
                quoted = True
            elif ch == ".":
                break
            advance(1)
        if i >= n:
            body = text[begin:].strip()
            units.append(Unit("incomplete", body, Span(start_line, start_col, line, max(col - 1, 1))))
            break
        units.append(Unit("command", text[begin:i].strip(), Span(start_line, start_col, line, col)))
        advance(1)                                   # the final "."
    return units


# ---------------------------------------------------------------------------
# Parsing
# ---------------------------------------------------------------------------

STATEMENT_KEYWORDS = ("theorem", "lemma", "proposition", "claim")
ANTI_KEYWORDS = tuple("anti" + k for k in STATEMENT_KEYWORDS)
REFINE_VERBS = ("refine", "prove", "argue", "refute", "dispute")
VERBS = ("attack", "rebut", "undermine", "undercut", "support", "buttress", "reinforce", "undergird")
TERM_SELECTORS = ("registered", "enriched", "unfolded", "normal", "evaluated")
RENDER_STYLES = ("argumentation", "dialectical", "intuitionistic", "vanilla", "pruefschema")
PROJECTIONS = ("out", "tou", "sub", "bus", "attacker", "regatta")

_NAME = r"[^\s:()]+"
_HEADER = re.compile(rf"^({_NAME})\s*:\s*(.+)$", re.S)


def strip_outer_parens(text: str) -> str:
    """``(A -> B)`` -> ``A -> B``, only when the parentheses enclose it all."""
    text = text.strip()
    while text.startswith("(") and text.endswith(")"):
        depth = 0
        for i, ch in enumerate(text):
            depth += ch == "("
            depth -= ch == ")"
            if depth == 0 and i != len(text) - 1:
                return text
        text = text[1:-1].strip()
    return text


def _target(words: List[str], usage: str):
    """(target text, rest) from the words after a verb: a name, or
    ``issue :X`` / ``issue X:`` joined."""
    if not words:
        raise SyntaxRefused(usage)
    if words[0] == "issue":
        if len(words) < 2:
            raise SyntaxRefused(usage)
        return f"issue {words[1]}", words[2:]
    return words[0], words[1:]


def _header(rest: str, usage: str):
    match = _HEADER.match(rest.strip())
    if match is None:
        raise SyntaxRefused(usage)
    return match.group(1), strip_outer_parens(match.group(2))


def _p_new(words, rest, text):
    if words[:1] != ["document"]:
        return None
    tail = words[1:]
    minimal = bool(tail) and tail[0] == "minimal"
    tail = tail[1:] if minimal else tail
    if len(tail) > 1 or (tail and tail[0] not in ("lk", "lj")):
        raise SyntaxRefused("Use: new document [minimal] [lk|lj].  (lk, classical, is the default.)")
    return Command("new_document", text, args={"logic": tail[0] if tail else "lk", "minimal": minimal})


def _p_argument(kind):
    def parse(words, rest, text):
        name, conclusion = _header(rest, f"Use: {kind} NAME : (PROPOSITION).")
        return Command("argument", text, args={"name": name, "conclusion": conclusion,
                                               "anti": kind == "counterargument"})
    return parse


def _p_statement(keyword):
    def parse(words, rest, text):
        name, conclusion = _header(rest, f"Use: {keyword} NAME : (PROPOSITION).")
        return Command("statement", text, args={"keyword": keyword, "name": name,
                                                "conclusion": conclusion,
                                                "anti": keyword.startswith("anti")})
    return parse


def _p_refine(verb):
    def parse(words, rest, text):
        if len(words) != 1:
            raise SyntaxRefused(f"Use: {verb} NAME.")
        return Command("refine", text, args={"verb": verb, "name": words[0]})
    return parse


def _p_bare(kind, usage):
    def parse(words, rest, text):
        if words:
            raise SyntaxRefused(usage)
        return Command(kind, text)
    return parse


_DEBATE = re.compile(r"^(\S+)\s+(\S+)\s+([^\s:]+)\s*:\s*(.+)$", re.S)


def _p_debate(words, rest, text):
    match = _DEBATE.match(rest.strip())
    if match is None:
        raise SyntaxRefused("Use: debate pro|con open|closed NAME : ISSUE.  "
                            "(The shared form of an argument's debate is `share ARG.`)")
    onus, scope, name, issue = match.groups()
    return Command("debate", text, args={"onus": onus, "scope": scope, "name": name,
                                         "issue": issue.strip()})


def _p_case(words, rest, text):
    if words != ["rested"]:
        raise SyntaxRefused("Use: case rested.  (closes an argument, like dixi.)")
    return Command("dixi", text)


def _p_cedat(words, rest, text):
    if words != ["tempus"]:
        raise SyntaxRefused("Use: cedat tempus.")
    return Command("close_debate", text)


def _p_verb(verb):
    def parse(words, rest, text):
        if len(words) == 2:
            return Command("move", text, args={"verb": verb, "argument": words[0], "target": words[1]})
        if len(words) == 3:
            raise SyntaxRefused(f"`{verb} NEW A B` is gone: a debate is recorded with "
                                f"`debate pro|con open|closed NAME : ISSUE.`, its moves and "
                                f"`cedat tempus.` (see the README, *Debates*).")
        raise SyntaxRefused(f"Use: {verb} ARGUMENT TARGET.  (a move of the debate being recorded)")
    return parse


def _p_register(words, rest, text):
    usage = 'Use: register NAME [strict] : PROPOSITION := "PROOF_TERM".'
    try:
        name, body = rest.strip().split(maxsplit=1)
    except ValueError:
        raise SyntaxRefused(usage)
    strict = False
    body = body.strip()
    if body.startswith("strict") and body[6:7].isspace():
        strict, body = True, body[6:].strip()
    if not body.startswith(":"):
        raise SyntaxRefused(usage)
    conclusion, sep, term = body[1:].partition(":=")
    term = term.strip()
    if sep != ":=" or not conclusion.strip() or not term:
        raise SyntaxRefused(usage)
    if not (len(term) >= 2 and term[0] == term[-1] == '"'):
        raise SyntaxRefused('register: double-quote the proof term, which contains dots: '
                            'register NAME : PROPOSITION := "TERM".')
    return Command("register", text, args={"name": name, "strict": strict,
                                           "conclusion": conclusion.strip(), "proof_term": unquote(term)})


def _p_decorate(words, rest, text):
    from pres.decorations import parse_decorate_command, DecorationError
    try:
        name, template = parse_decorate_command(text + ".")
    except DecorationError as e:
        raise SyntaxRefused(str(e))
    return Command("decorate", text, args={"name": name, "template": template})


def _p_adopt(words, rest, text):
    if len(words) != 3 or words[1] != "as":
        raise SyntaxRefused("Use: adopt EDGE* as NAME., with an edge name shown by `graph ARG.`")
    return Command("adopt", text, args={"edge": words[0], "name": words[2]})


_EVAL_OPTIONS = ("skeptical", "credulous", "grounded", "complete", "preferred", "stable", "cbn", "cbv")


def _eval_options(verb, rest_words):
    favour = "favour" in rest_words
    mode = semantics = base = witness = None
    for tok in (t for t in rest_words if t != "favour"):
        if tok in ("skeptical", "credulous"):
            mode = tok
        elif tok in ("grounded", "complete", "preferred", "stable"):
            semantics = tok
        elif tok in ("cbn", "cbv"):
            base = tok
        elif tok == "all":
            witness = "all"
        elif tok.isdigit() and int(tok) >= 1:
            witness = int(tok)
        else:
            raise SyntaxRefused(f"{verb}: unknown option '{tok}' (expected one of {_EVAL_OPTIONS}, "
                                f"a witness number or 'all')")
    if witness is not None and (mode or "skeptical") != "credulous":
        raise SyntaxRefused(f"{verb}: a witness number or 'all' requires credulous mode")
    return {"mode": mode or "skeptical", "semantics": semantics or "preferred",
            "base": base or "cbn", "witness": witness, "favour": favour}


def _p_query(verb):
    usage = {
        "graph": 'Use: graph ARG|DEBATE|issue :X|X:|document [all] ["FILE.dot"] [show].',
        "label": "Use: label ARG|DEBATE|issue :X|X: [grounded|complete|preferred|stable].",
        "evaluate": "Use: evaluate ARG|DEBATE|issue :X|X: [MODE] [SEMANTICS] [BASE] [N|all] [favour].",
        "explain": "Use: explain ARG|DEBATE|issue :X|X: [MODE] [SEMANTICS] [BASE] [N|all] [favour].",
        "render": "Use: render ARG|DEBATE|issue :X|X: [STYLE] [registered|enriched|unfolded|normal|evaluated].",
        "tree": "Use: tree ARG [nl [STYLE]|pt] [registered|enriched|unfolded|normal|evaluated].",
    }[verb]

    def parse(words, rest, text):
        target, more = _target(words, usage)
        args: Dict[str, Any] = {"target": target}
        if verb == "graph":
            path = next((o for o in more if o not in ("show", "all")), None)
            args.update(whole="all" in more, show="show" in more,
                        dot_path=unquote(path) if path else None)
        elif verb == "label":
            if len(more) > 1 or (more and more[0] not in ("grounded", "complete", "preferred", "stable")):
                raise SyntaxRefused(f"label: unknown option(s) {' '.join(more)}")
            args["semantics"] = more[0] if more else "grounded"
        elif verb in ("evaluate", "explain"):
            args.update(_eval_options(verb, more))
        elif verb == "render":
            args["which"] = next((t for t in more if t in TERM_SELECTORS), None)
            args["style"] = next((t for t in more if t not in TERM_SELECTORS), None)
        elif verb == "tree":
            args["which"] = next((t for t in more if t in TERM_SELECTORS), None)
            plain = [t for t in more if t not in TERM_SELECTORS]
            args["mode"] = plain[0] if plain else "pt"
            args["nl_style"] = plain[1] if (args["mode"] == "nl" and len(plain) > 1) else "argumentation"
        return Command(verb, text, args=args)
    return parse


def _p_unfold(words, rest, text):
    usage = "Use: unfold argument NAME. | unfold issue :X. | unfold issue X:. | unfold debate NAME."
    if len(words) != 2 or words[0] not in ("argument", "issue", "debate"):
        raise SyntaxRefused(usage)
    target = f"issue {words[1]}" if words[0] == "issue" else words[1]
    return Command("unfold", text, args={"what": words[0], "target": target})


def _p_one_name(kind, usage):
    def parse(words, rest, text):
        if len(words) != 1:
            raise SyntaxRefused(usage)
        return Command(kind, text, args={"name": words[0]})
    return parse


def _p_choice(kind, choices):
    def parse(words, rest, text):
        if len(words) != 1 or words[0] not in choices:
            raise SyntaxRefused(f"Use: {kind} {'|'.join(choices)}.")
        return Command(kind, text, args={"mode": words[0]})
    return parse


def _p_load(words, rest, text):
    if not rest.strip():
        raise SyntaxRefused('Use: load "FILE".')
    return Command("load", text, args={"path": unquote(rest)})


def _p_render_nf(words, rest, text):
    if not words:
        raise SyntaxRefused("Use: render-nf ARG [STYLE].")
    return Command("render_nf", text, args={"name": words[0], "style": words[1] if len(words) > 1 else None})


def _p_projection(verb):
    def parse(words, rest, text):
        return Command("projection", text, args={"verb": verb, "words": words})
    return parse


def _p_tactic(words, rest, text):
    raise SyntaxRefused("`tactic` is gone: the wrapper has no tactics of its own "
                        "(tasks.org, aida-tactic-registration).")


def _p_old(replacement):
    def parse(words, rest, text):
        raise SyntaxRefused(replacement)
    return parse


def _p_start(words, rest, text):
    kind = words[0] if words else "argument"
    if kind == "antitheorem":
        kind = "counterargument"
    raise SyntaxRefused(f"`start {kind}` is gone: write `{kind} NAME : (PROPOSITION).` ... `dixi.`")


def _p_end(words, rest, text):
    raise SyntaxRefused("`end argument` is gone: an argument ends with `dixi.`")


def _p_hora(words, rest, text):
    raise SyntaxRefused("`hora est.` is gone: a debate ends with `cedat tempus.`")


#: The wrapper's commands, by their first word.  Every other first word
#: makes the command opaque: it goes to Fellowship whole.
COMMANDS = {
    "new": _p_new,
    "argument": _p_argument("argument"),
    "counterargument": _p_argument("counterargument"),
    "dixi": _p_bare("dixi", "Use: dixi."),
    "cr": _p_bare("dixi", "Use: cr.  (short for case rested.)"),
    "case": _p_case,
    "qed": _p_bare("qed", "Use: qed."),
    "debate": _p_debate,
    "cedat": _p_cedat,
    "ct": _p_bare("close_debate", "Use: ct.  (short for cedat tempus.)"),
    "register": _p_register,
    "decorate": _p_decorate,
    "adopt": _p_adopt,
    "graph": _p_query("graph"),
    "label": _p_query("label"),
    "evaluate": _p_query("evaluate"),
    "explain": _p_query("explain"),
    "render": _p_query("render"),
    "tree": _p_query("tree"),
    "render-nf": _p_render_nf,
    "unfold": _p_unfold,
    "share": _p_one_name("share", "Use: share ARG."),
    "expand": _p_one_name("expand", "Use: expand ARG."),
    "reduce": _p_one_name("reduce", "Use: reduce ARG."),
    "normalize": _p_one_name("normalize", "Use: normalize ARG."),
    "typecheck": _p_choice("typecheck", ("on", "off", "expanded")),
    "pipeline": _p_choice("pipeline", ("shared", "unfolded")),
    "load": _p_load,
    "tactic": _p_tactic,
    "start": _p_start,
    "end": _p_end,
    "hora": _p_hora,
    **{k: _p_statement(k) for k in STATEMENT_KEYWORDS + ANTI_KEYWORDS},
    **{v: _p_refine(v) for v in REFINE_VERBS},
    **{v: _p_verb(v) for v in VERBS},
    **{p: _p_projection(p) for p in PROJECTIONS},
}


#: Keywords read regardless of case, as they always were.
_CASELESS = set(STATEMENT_KEYWORDS + ANTI_KEYWORDS + REFINE_VERBS)


def parse(text: str, span: Optional[Span] = None) -> Command:
    """Read one command (without its final ``.``)."""
    text = text.strip()
    words = text.split()
    if not words:
        raise SyntaxRefused("an empty command", span)
    head = words[0]
    rest = text[len(head):]
    parser = COMMANDS.get(head)
    if parser is None and head.lower() in _CASELESS:
        parser = COMMANDS[head.lower()]           # `Lemma foo : (A).` as before
    if parser is None:
        return Command("opaque", text, span, {"word": head})
    try:
        command = parser(words[1:], rest, text)
    except SyntaxRefused as e:
        e.span = span
        raise
    if command is None:                           # not ours after all
        return Command("opaque", text, span, {"word": head})
    command.span = span
    return command


def parse_units(text: str) -> List[Any]:
    """Every unit of ``text``: a Command, a comment Unit, or the
    SyntaxRefused of a command that does not parse."""
    out: List[Any] = []
    for unit in split(text):
        if unit.kind != "command":
            out.append(unit)
            continue
        try:
            out.append(parse(unit.text, unit.span))
        except SyntaxRefused as e:
            out.append(e)
    return out
