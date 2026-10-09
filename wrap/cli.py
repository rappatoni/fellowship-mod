from __future__ import annotations
import os, re, sys
import copy
import atexit
import collections
import json
import tempfile
from pathlib import Path
import shutil
import subprocess
import argparse
import logging
from typing import Any, Optional
from pres.tree import render_acceptance_tree_dot
from wrap.prover import ProverWrapper, ProverError, MachinePayloadError
from core.dc.argument import Argument
from wrap.registry import (
    parse_statement, parse_refine, reopen, abandon, start_recording, is_qed,
)
from core.dc.cite import citation_target, CitationError, is_strict_citation, parse_cite
from core.dc.debate import DebateError
from wrap.service import (
    Service, AidaError, NotFound, InvalidRequest, Refused, UnfoldRefused, render_term,
)
from core.ac.grammar import Grammar, ProofTermTransformer
from core.ac.ast import Mutilde
from core.comp.oracle import AdfBddNotFound
from wrap.importers import ImporterNotFound, SourceImportError, load_importer, missing_importer_message
from pres.decorations import parse_decorate_command

logger = logging.getLogger('fsp.wrapper')
logger.propagate = True

# The TRACE level (below DEBUG) and Logger.trace() live in core.logging_util,
# so that core modules can log at TRACE without importing the CLI.
from core.logging_util import TRACE, artifact  # noqa: E402  (re-exported: callers import them from here)

#: The CLI's own slot in the pipeline's account: what it hands to the
#: unfolder, under a name `explain` groups with the rest.
_pipeline_logger = logging.getLogger("core.dc.issue")

def configure_logging_cli(level_name: Optional[str] = None, log_file: Optional[str] = None) -> None:
    """
    Configure root logger for CLI usage:
      - Level from explicit argument or env FSP_LOGLEVEL (default INFO)
      - Message-only format
      - Stream to stdout
      - Avoid duplicate handlers if already configured
    """
    # The CLI reports onus conflicts itself (wrap/service.py, _report_onus_conflicts), with
    # a message aimed at the person at the prompt; the library warning would
    # only duplicate it on stderr.
    import warnings as _warnings
    from core.comp.adf_label import OpposingPresumptions as _OpposingPresumptions
    _warnings.filterwarnings("ignore", category=_OpposingPresumptions)

    level_name = (level_name or os.getenv("FSP_LOGLEVEL", "INFO")).upper()
    level = getattr(logging, level_name, None)
    if level is None:
        # Allow custom TRACE
        level = TRACE if level_name == "TRACE" else logging.INFO
    root = logging.getLogger()
    root.setLevel(level)
    if not root.handlers:
        h = logging.StreamHandler(sys.stdout)
        h.setLevel(level)
        h.setFormatter(logging.Formatter("%(message)s"))
        root.addHandler(h)
    else:
        for h in root.handlers:
            try:
                h.setLevel(level)
            except Exception:
                pass
    if log_file:
        fh = logging.FileHandler(log_file, encoding="utf-8")
        fh.setLevel(level)
        fh.setFormatter(logging.Formatter("%(asctime)s %(levelname)s %(name)s: %(message)s"))
        root.addHandler(fh)


def _refuse_in_script(error: Exception, strict: bool, script_path: str, lineno: int) -> None:
    """Log-and-refuse for the registry's refusals (a clashing name, a `qed`
    on a witness that is not strict): print and log, and in strict mode stop
    the script like any other prover error.  Scripts narrate through the
    logger only, so there is no separate print here."""
    logger.warning("Refused (%s:%d): %s", script_path, lineno, error)
    if strict:
        raise ProverError(f"{script_path}:{lineno}: {error}") from error


def _render(prover: ProverWrapper, o, *, interactive: bool = False) -> None:
    """Print an outcome of the interpreter (wrap/interpreter.py) as the CLI
    always has: query results in full, refusals as `STAGE: refused: ...`,
    and the messages of what the document took."""
    say = print if interactive else logger.info
    c = o.command
    if o.status == "comment":
        if o.kind == "narration":
            say(o.message)
        return
    if o.status == "reported":
        if o.kind == "share":
            _report_share(o.error)
        elif o.kind == "tree" and isinstance(o.error, InvalidRequest):
            logger.error("%s", o.error)
        else:
            _report(o.error)
        return
    if o.status != "ok":
        return
    k = o.kind
    if k == "graph":
        _show_graph(prover, c["target"], o.value, c["dot_path"], c["show"])
    elif k == "label":
        _show_label(c["target"], o.value)
    elif k == "evaluate":
        _show_evaluation(c["target"], o.value, c["mode"], c["base"], c["semantics"],
                         c["witness"], c["favour"])
    elif k == "render":
        value = o.value
        if hasattr(value, "description") and hasattr(value, "term"):
            _show_term(prover, c["target"], value.description, value.term, c["style"])
        else:
            logger.info("")
            logger.info("Rendering argument %s:", c["target"])
            logger.info(value.text)
            logger.info("")
    elif k == "tree":
        _show_tree(prover, c["target"], o.value)
    elif k == "unfold":
        _show_unfold(prover, o.value)
    elif k == "share":
        _show_share(c["name"], o.value)
    elif k == "typecheck":
        logger.info("Type checking of unfolded terms: %s", o.message)
    elif k == "pipeline":
        logger.info("Pipeline: %s", o.message)
    elif k in ("fellowship", "instruction"):
        return
    elif k in ("statement", "register", "adopt", "debate", "move", "close_debate") and not interactive:
        return                              # the service has logged it
    elif o.message:
        say(o.message)


def _run_cli_only(prover: ProverWrapper, c, *, interactive: bool = False) -> None:
    """The commands only the CLI runs: explain, the deprecated term-level
    commands and load."""
    k = c.kind
    if k == "explain":
        explain_argument_cmd(prover, c["target"], c["mode"], c["base"], c["semantics"],
                             c["witness"], c["favour"])
    elif k == "render_nf":
        render_argument_cmd(prover, c["name"], True, style=c["style"])
    elif k == "reduce":
        reduce_argument_cmd(prover, c["name"])
    elif k == "expand":
        expand_argument_cmd(prover, c["name"])
    elif k == "normalize":
        arg = prover.get_argument(c["name"])
        if arg:
            logger.info("Normalized argument '%s'; normal form cached in .normal_form", c["name"])
            arg.normalize()
            logger.info("normal form stored in .normal_form")
        else:
            logger.warning("Argument '%s' not found for normalization", c["name"])
    elif k == "projection":
        command = " ".join([c["verb"]] + c["words"])
        try:
            result = projection_debate_cmd(prover, command)
            (print if interactive else logger.info)(f"Constructed {c['verb']} '{result.name}'.")
        except Exception as e:
            logger.error("%s failed: %s", c["verb"].capitalize(), e)
    elif k == "load":
        path = Path(c["path"]).expanduser()
        if not path.is_file():
            print(f"load: no such file {path}")
            return
        try:
            execute_script(prover, str(path), strict=False, stop_on_error=False, isolate=False,
                           new_document=True)
        except (ProverError, MachinePayloadError) as e:
            print(f"load: stopped: {e}")


def execute_script(prover: ProverWrapper, script_path: str, *, strict: bool = False, stop_on_error: bool = True, echo_notes: bool = False, isolate: bool = True, stop_marker: bool = True, render_files: Optional[bool] = None, new_document: bool = False) -> None:
    """Run a .fspy script through the interpreter (wrap/interpreter.py; the
    syntax is wrap/syntax.py's, see the README).

    ``strict``: a refusal or a Fellowship error raises ProverError
    (``FILE:LINE: message``); otherwise it is logged, and a Fellowship
    error stops the script if ``stop_on_error``.  ``isolate`` runs it in a
    session of its own; ``new_document`` (``load``) replaces the session's
    document first; ``stop_marker`` stops at ``%stop``.
    """
    from wrap.interpreter import Interpreter
    from wrap.syntax import parse_units
    logger.info(
        "Running script %s (strict=%s, stop_on_error=%s, echo_notes=%s)",
        script_path, strict, stop_on_error, echo_notes
    )
    if isolate:
        prover = setup_prover()
    elif new_document:
        prover.new_document()
    prev_echo = getattr(prover, "echo_notes", False)
    prover.echo_notes = echo_notes
    # render_files=None keeps whatever the session has (ACDC_NO_RENDER); False
    # stops `graph ... show` and `tree` writing images and opening a viewer.
    prev_render = getattr(prover, "render_files", True)
    if render_files is not None:
        prover.render_files = render_files
    interpreter = Interpreter(prover)
    try:
        with open(script_path, "r") as handle:
            text = handle.read()
        for item in parse_units(text):
            o = interpreter.execute(item)
            where = f"{script_path}:{o.span.line if o.span else '?'}"
            if o.status == "stop":
                if stop_marker:
                    logger.info("Stopped at %%stop (%s); the rest of the file is yours to paste.", where)
                    break
                continue
            if o.status == "cli":
                _run_cli_only(prover, o.command)
                continue
            if o.status in ("ok", "comment", "reported"):
                _render(prover, o)
                continue
            if o.status == "refused":
                logger.warning("Refused (%s): %s", where, o.message)
                if strict:
                    raise ProverError(f"{where}: {o.message}")
                continue
            if o.status == "fatal":
                raise MachinePayloadError(f"{where}: {o.message}")
            # "error": Fellowship refused a command
            if strict:
                raise ProverError(f"{where}: {o.message}")
            logger.error("Prover error: %s", o.message)
            if stop_on_error:
                break
        if interpreter.recording is not None:
            logger.warning("'%s' was not closed with `dixi.`; it is not registered.",
                           interpreter.recording["name"])
        if getattr(prover, "recording_debate", None) is not None:
            logger.warning("Debate '%s' was not closed with `cedat tempus.`; it keeps the %d "
                           "move(s) recorded so far.", prover.recording_debate,
                           len(prover.debates[prover.recording_debate].moves))
    finally:
        if isolate:
            try:
                prover.close()
            except Exception:
                pass
        else:
            prover.echo_notes = prev_echo
            prover.render_files = prev_render
        logger.info("Finished script %s", script_path)


def _print_ui(state: Any) -> None:
    if not isinstance(state, dict):
        return

    ui = state.get('_ui')
    if isinstance(ui, str) and ui.strip():
        # Print Fellowship's user-facing output.
        print(ui.strip())

    perrs = state.get('_plain_errors')
    if isinstance(perrs, list) and perrs:
        # Make sure parse/syntax errors are clearly visible to the user.
        for e in perrs:
            if isinstance(e, str) and e.strip():
                print(e.strip())


def _setup_readline() -> None:
    """Line editing, history and paste-friendliness for the REPL.

    Emacs bindings (readline's default) so C-a/C-e/C-k/M-b work in a
    terminal such as vterm; a history file so the arrow keys recall
    earlier commands across sessions; bracketed paste off (GNU readline
    8.1+ would otherwise hand a pasted block to input() as one line with
    embedded newlines - _read_lines splits those anyway).  libedit on
    macOS ignores the GNU-only settings.
    """
    try:
        import readline
    except ImportError:
        return
    for binding in ("set editing-mode emacs", "set enable-bracketed-paste off"):
        try:
            readline.parse_and_bind(binding)
        except Exception:
            pass
    history = os.path.expanduser(os.getenv("ACDC_HISTORY", "~/.acdc_history"))
    try:
        readline.read_history_file(history)
    except (FileNotFoundError, OSError):
        pass
    try:
        readline.set_history_length(2000)
        atexit.register(readline.write_history_file, history)
    except Exception:
        pass


_pending_lines: "collections.deque[str]" = collections.deque()


def _read_line(prompt: str) -> str:
    """input() that hands a pasted multi-line block back one line at a
    time: the first call reads the block, later calls drain it."""
    while not _pending_lines:
        raw = input(prompt)
        _pending_lines.extend(raw.split("\n"))
    return _pending_lines.popleft().strip()


def interactive_mode(prover: ProverWrapper) -> None:
    """The REPL: commands in the syntax of scripts (wrap/syntax.py), each
    ending in "." - a command without one asks for more with `...` - run by
    the interpreter (wrap/interpreter.py) live: an argument block sends
    every line to Fellowship as it comes and shows the goals.  Lines
    starting with '#' are echoed, '%' lines ignored; `load 'FILE'.` runs a
    script in a new document; `exit` or `quit` leaves.  Line editing and
    history come from readline (emacs bindings)."""
    from wrap.interpreter import Interpreter
    from wrap.syntax import parse_units, split
    _setup_readline()
    interpreter = Interpreter(prover, live=True, show_ui=_print_ui,
                              read_more=lambda: _read_line('... '))
    buffer = ""
    try:
        while True:
            if buffer:
                prompt = '... '
            elif interpreter.recording is not None:
                prompt = 'acdc (recording)> '
            elif getattr(prover, "recording_debate", None):
                prompt = f'acdc (debate {prover.recording_debate})> '
            else:
                prompt = 'acdc> '
            try:
                line = _read_line(prompt)
            except EOFError:
                print("\nEOFError: No input detected. Exiting interactive mode.")
                break
            if not buffer and line.rstrip(".").strip().lower() in ("exit", "quit"):
                break
            buffer += line + "\n"
            units = split(buffer)
            if units and units[-1].kind == "incomplete":
                continue
            text, buffer = buffer, ""
            fatal = False
            for item in parse_units(text):
                o = interpreter.execute(item)
                if o.status == "cli":
                    _run_cli_only(prover, o.command, interactive=True)
                elif o.status in ("ok", "comment", "reported"):
                    _render(prover, o, interactive=True)
                elif o.status == "refused":
                    print(f"refused: {o.message}")
                    logger.warning("Refused: %s", o.message)
                elif o.status == "error":
                    print(f"acdc: ignored command due to prover error: {o.message}")
                    logger.error("Prover error: %s", o.message)
                elif o.status == "fatal":
                    print(f"acdc: fatal prover communication error: {o.message}")
                    logger.error("Fatal prover communication error: %s", o.message)
                    fatal = True
                    break
            if fatal:
                break
    finally:
        prover.close()


# ---------------------------------------------------------------------------
#  CLI helper commands                                                       
# ---------------------------------------------------------------------------


def _parse_projection_debate_command(prover: ProverWrapper, command: str) -> tuple[str, str, str, int | None]:
    parts = command.split()
    if not parts:
        raise ValueError("empty command")
    verb = parts[0]
    if verb in {"out", "tou"}:
        if len(parts) != 4:
            raise SyntaxError(f"Invalid {verb} command. Use: {verb} INDEX ARG NAME")
        if parts[1].lstrip("+-").isdigit():
            index_text, arg_name, new_name = parts[1], parts[2], parts[3]
        else:
            new_name, arg_name, index_text = parts[1], parts[2], parts[3]
        try:
            index = int(index_text)
        except ValueError as e:
            raise SyntaxError(f"Invalid {verb} command. Index must be an integer") from e
        return verb, arg_name, new_name, index

    if verb in {"sub", "bus", "attacker", "regatta"}:
        if len(parts) != 3:
            raise SyntaxError(f"Invalid {verb} command. Use: {verb} ARG NAME")
        if prover.get_argument(parts[1]) is not None and prover.get_argument(parts[2]) is None:
            arg_name, new_name = parts[1], parts[2]
        else:
            new_name, arg_name = parts[1], parts[2]
        return verb, arg_name, new_name, None

    raise ValueError(f"unknown projection debate command '{verb}'")


def projection_debate_cmd(prover: ProverWrapper, command: str) -> Argument:
    verb, arg_name, new_name, index = _parse_projection_debate_command(prover, command)
    arg = prover.get_argument(arg_name)
    if arg is None:
        raise ValueError(f"Argument '{arg_name}' not found")

    if verb == "out":
        assert index is not None
        result = arg.out(index, name=new_name)
    elif verb == "tou":
        assert index is not None
        result = arg.tou(index, name=new_name)
    elif verb == "sub":
        result = arg.sub(name=new_name)
    elif verb == "bus":
        result = arg.bus(name=new_name)
    elif verb == "attacker":
        result = arg.attacker(name=new_name)
    elif verb == "regatta":
        result = arg.regatta(name=new_name)
    else:
        raise ValueError(f"unknown projection debate command '{verb}'")

    prover.register_argument(result)
    return result


_DEBATE_HEADER = re.compile(r"^debate\s+(\S+)\s+(\S+)\s+([^\s:]+)\s*:\s*(.+?)\s*\.$")
_OLD_VERBS = ("chain", "attack", "support", "undercut", "undermine", "rebut",
              "undergird", "reinforce", "buttress")


_NEW_DOCUMENT = re.compile(r"^new\s+document(?:\s+(minimal))?(?:\s+(lk|lj))?\s*\.$")


def parse_new_document(command: str):
    """(logic, minimal) for `new document [minimal] [lk|lj].`, else None.
    A `new document` line that does not parse raises ValueError."""
    text = command.strip()
    if not re.match(r"^new\s+document\b", text):
        return None
    match = _NEW_DOCUMENT.match(text)
    if match is None:
        raise ValueError("Use: new document [minimal] [lk|lj].  (lk, classical, is the default.)")
    return (match.group(2) or "lk", bool(match.group(1)))


def debate_line(prover: ProverWrapper, command: str) -> bool:
    """A line of the debate syntax (core/dc/debate.py), or False:

        debate pro|con open|closed NAME : ISSUE.   starts recording NAME
        ARG.  /  ARG TARGET.  /  VERB ARG TARGET.  a move (while recording)
        cedat tempus.                               closes the debate

    A move is a line whose first word is a verb or a registered argument;
    any other line is left to the other commands, which keep working
    while a debate is recorded - `evaluate NAME` compiles the debate as
    it stands.  The work is the service's (wrap/service.py); a refusal
    raises DebateError or NameClash."""
    from core.dc.debate import VERBS

    text = command.strip()
    words = text.rstrip(".").split()
    if not words or not hasattr(prover, "debates"):
        return False
    service = Service.of(prover)

    def run(method, *args, **kwargs):
        try:
            return method(*args, **kwargs)
        except AidaError as e:
            if isinstance(e.cause, (DebateError, ProverError)):
                raise e.cause from None
            raise DebateError(str(e)) from None

    if words[0] == "debate":
        match = _DEBATE_HEADER.match(text)
        if match is None:
            raise DebateError("Use: debate pro|con open|closed NAME : ISSUE.  "
                              "(The shared form of an argument's debate is now `share ARG`.)")
        onus, scope, name, issue = match.groups()
        run(service.start_debate, name, issue, onus, scope)
        return True
    if words == ["cedat", "tempus"]:
        if not text.endswith("."):
            raise DebateError("`cedat tempus.` ends with a full stop.")
        run(service.close_debate)
        return True
    name = prover.recording_debate
    if words[0] in VERBS and len(words) == 3:
        verb, argument, target = words
    elif name is not None and words[0] in prover.arguments and len(words) <= 2:
        verb, argument, target = None, words[0], (words[1] if len(words) == 2 else None)
    elif words[0] in _OLD_VERBS and len(words) >= 3:
        raise DebateError(f"`{words[0]} NEW A B` is gone: a debate is recorded with "
                          f"`debate pro|con open|closed NAME : ISSUE.` and its moves "
                          f"(see the README, *Debates*); `chain` gave way to citation.")
    else:
        return False
    if name is None:
        raise DebateError(f"`{text}` is a debate move, but no debate is being recorded.")
    if not text.endswith("."):
        raise DebateError(f"a move ends with a full stop: `{text}.`")
    run(service.move, argument, verb, target)
    return True


def _parse_register_command(command: str) -> tuple[str, str, bool, str]:
    """Parse `register NAME [strict] : TYPE := PROOF_TERM`.

    The delimiter form is intentional: both proposition strings and proof-term
    strings may contain spaces, so whitespace-only parsing is ambiguous.
    """
    payload = command[len("register"):].strip()
    if not payload:
        raise ValueError("Invalid register command. Use: register NAME [strict] : TYPE := PROOF_TERM")

    try:
        name, rest = payload.split(maxsplit=1)
    except ValueError as e:
        raise ValueError("Invalid register command. Use: register NAME [strict] : TYPE := PROOF_TERM") from e

    declare_theorem = False
    rest = rest.strip()
    if rest.startswith("strict"):
        parts = rest.split(maxsplit=1)
        if parts[0] == "strict":
            declare_theorem = True
            rest = parts[1].strip() if len(parts) > 1 else ""

    if not rest.startswith(":"):
        raise ValueError("Invalid register command. Expected ':' before the proposition type")

    type_and_term = rest[1:].strip()
    conclusion, sep, proof_term = type_and_term.partition(":=")
    if sep != ":=":
        raise ValueError("Invalid register command. Expected ':=' before the proof term")

    conclusion = conclusion.strip()
    proof_term = proof_term.strip()
    if not name or not conclusion or not proof_term:
        raise ValueError("Invalid register command. Use: register NAME [strict] : TYPE := PROOF_TERM")

    return name, conclusion, declare_theorem, proof_term


def register_argument_cmd(prover: ProverWrapper, command: str) -> Argument:
    """Register an argument/theorem from a proof-term string
    (wrap/service.py, ``register``).

    Command syntax:
        register NAME [strict] : TYPE := PROOF_TERM

    With `strict`, replay is finalized with `qed.` so Fellowship declares the
    theorem/antitheorem; otherwise the replayed theorem is discarded after the
    wrapper extracts and registers the argument state.  A refusal raises the
    prover's own error (NameClash, StrictnessRefused, ...).
    """
    name, conclusion, declare_theorem, proof_term = _parse_register_command(command)
    try:
        return Service.of(prover).register(name, conclusion, proof_term, strict=declare_theorem).value
    except AidaError as e:
        if e.cause is not None:
            raise e.cause
        raise


def resolve_fsp_path() -> Path:
    """
    Find the Fellowship binary ('fsp') via, in order:
      1) env ACDC_FSP or FSP_PATH
      2) package path: wrap/fellowship/fsp (next to this file when installed/editable)
      3) repo path:   CWD/wrap/fellowship/fsp (running from source tree)
      4) PATH lookup: an 'fsp' executable in PATH
    """
    env_path = os.getenv("ACDC_FSP") or os.getenv("FSP_PATH")
    if env_path:
        p = Path(env_path).expanduser()
        if p.is_file() and os.access(p, os.X_OK):
            return p
    pkg = Path(__file__).resolve().parent / "fellowship" / "fsp"
    if pkg.is_file() and os.access(pkg, os.X_OK):
        return pkg
    repo = Path.cwd() / "wrap" / "fellowship" / "fsp"
    if repo.is_file() and os.access(repo, os.X_OK):
        return repo
    exe = shutil.which("fsp")
    if exe:
        return Path(exe)
    raise FileNotFoundError(
        "Fellowship binary 'fsp' not found.\n"
        "Build it (make -C wrap/fellowship) or set ACDC_FSP=/absolute/path/to/fsp.\n"
        "Searched: ACDC_FSP/FSP_PATH, package path, repo path, and PATH."
    )

def setup_prover() -> ProverWrapper:
    env = os.environ.copy()
    env.setdefault("FSP_MACHINE", "1")
    fsp_path = resolve_fsp_path()
    prover = ProverWrapper(str(fsp_path), env=env)
    # Fellowship starts a session in LJ; a session starts with a classical
    # document, the default of `new document`.  A script may still choose
    # its logic with `lk.`/`lj.`/`minimal.` at its head.
    prover.new_document()
    # Declare some booleans to work with.
    #prover.send_command('declare A,B,C,D:bool.')
    #logger.info("Prover decls %r", prover.declarations)
    return prover


def reduce_argument_cmd(prover: ProverWrapper, name: str) -> None:
    """ CLI for Argument Reduction """
    arg = prover.get_argument(name)
    if not arg:
        logger.error(f"Argument '{name}' not found.")
        return
    logger.info(f"Reducing argument {arg.name} with proof term {arg.proof_term}")
    arg.reduce()

def expand_argument_cmd(prover: ProverWrapper, name: str) -> None:
    """CLI: `expand ARG` - the full term a citing argument stands for, with
    every cited argument's own term grafted in, recursively.

    Terms show citations by name, as an axiom would appear, so they stay
    readable as the library grows and follow any refinement of what they
    cite (core/dc/cite.py).  This computes the expansion on demand.
    """
    from core.dc.cite import expand_citations, cited_names
    from pres.gen import pres_str
    arg = prover.get_argument(name)
    if arg is None or getattr(arg, "body", None) is None:
        logger.error("expand: no argument '%s'.", name)
        return
    cites = sorted(cited_names(arg.body))
    if not cites:
        logger.info("'%s' cites nothing; its term is already complete.", name)
    else:
        logger.info("'%s' cites %s; expanded:", name, ", ".join(f"'{c}'" for c in cites))
    logger.info("  %s", pres_str(expand_citations(arg.body, prover.get_argument)))


def set_typecheck_cmd(prover: ProverWrapper, command: str) -> None:
    """CLI: `typecheck on | off | expanded` (wrap/service.py, ``set_typecheck``)."""
    done = Service.of(prover).set_typecheck(command.split()[1])
    logger.info("Type checking of unfolded terms: %s", done.message)


def set_pipeline_cmd(prover: ProverWrapper, command: str) -> None:
    """CLI: `pipeline shared | unfolded` (wrap/service.py, ``set_pipeline``)."""
    done = Service.of(prover).set_pipeline(command.split()[1])
    logger.info("Pipeline: %s", done.message)


def share_argument_cmd(prover: ProverWrapper, name: str) -> None:
    """CLI: `share ARG.` - the debate about ARG's issue as named sub-debates:
    the issue's term, then one `NAME[open sites] := term` line per
    sub-debate it cites (wrap/service.py, ``share``; core/dc/share.py).

    A sub-debate needed in two or more places is written once and cited by
    name; one needed once stays in place.  A citation shows what its site
    does to the cited debate, `d[alpha -> !:A, B:?]`: alpha captures its
    delegation of A, its obligation B is left open.  Names are transparent:
    `graph`, `label` and `evaluate` work on the expansion.
    """
    try:
        shared = Service.of(prover).share(name)
    except AidaError as e:
        _report_share(e)
        return
    _show_share(name, shared)


def _report_share(e) -> None:
    if isinstance(e, UnfoldRefused):
        print(f"debate: refused: {e}")
    elif isinstance(e, Refused):
        print(f"debate: {e}")
    else:
        _report(e)


def _show_share(name: str, shared) -> None:
    print(f"Debate about '{name}' ({shared.display}), "
          f"{len(shared.named)} sub-debate(s) cited by name:")
    for line in shared.text.splitlines():
        print(f"  {line}")


#: Which of an argument's terms `render` and `tree` show
#: (aida-unfold-entrypoints).
TERM_SELECTORS = ("registered", "enriched", "unfolded", "normal", "evaluated")


def _select_term(prover: ProverWrapper, name: str, which: str):
    """(description, term) for one of NAME's terms (wrap/service.py,
    ``term``), or None after a message."""
    service = Service.of(prover)
    try:
        found = service.term(service.target(name), which)
    except AidaError as e:
        _report(e)
        return None
    return found.description, found.term


def _show_selected(prover: ProverWrapper, name: str, which: str, style: Optional[str]) -> None:
    found = _select_term(prover, name, which)
    if found is None:
        return
    what, term = found
    _show_term(prover, name, what, term, style)


def _show_term(prover: ProverWrapper, name: str, what: str, term, style: Optional[str]) -> None:
    from pres.gen import pres_str, pres_tree
    logger.info("")
    logger.info("Rendering %s, %s:", name, what)
    if isinstance(term, str):
        logger.info(term)
    elif style is None:
        logger.info(pres_str(term))
        logger.info(pres_tree(term))
    else:
        try:
            logger.info(render_term(term, style, getattr(prover, "declarations", {}),
                                    getattr(prover, "decorations", {})))
        except InvalidRequest:
            logger.error("Invalid render style '%s'", style)
            return
    logger.info("")


def _render_tokens(command: str):
    """(name, style, which) for `render NAME [style] [selector]`."""
    name, rest = _target(command.split())
    which = next((tok for tok in rest if tok in TERM_SELECTORS), None)
    style = next((tok for tok in rest if tok not in TERM_SELECTORS), None)
    return name, style, which


def unfold_cmd(prover: ProverWrapper, command: str) -> None:
    """CLI: unfold a debate term and keep it (aida-unfold-entrypoints).

    Syntax:
        unfold argument NAME.     the term biased towards NAME, cached on it
        unfold issue :X. | X:.    the canonical term of an issue
        unfold debate NAME.       the debate's term (core/dc/debate.py)

    The term is kept until the document changes (a new argument or
    declaration) and is what `evaluate`, `explain`, `render ... unfolded`
    and `tree ... unfolded` use; nothing is added to the document."""
    parts = command.rstrip(".").split()
    if len(parts) < 3 or parts[1] not in ("argument", "issue", "debate"):
        logger.error("Use: unfold argument NAME. | unfold issue :X. | unfold issue X:. | unfold debate NAME.")
        return
    if parts[1] == "debate" and parts[2] not in prover.debates:
        logger.error("unfold debate: no debate '%s'.", parts[2])
        return
    service = Service.of(prover)
    name = f"issue {parts[2]}" if parts[1] == "issue" else parts[2]
    try:
        result = service.unfold(service.target(name))
    except AidaError as e:
        _report(e)
        return
    _show_unfold(prover, result)


def _show_unfold(prover: ProverWrapper, result) -> None:
    from pres.gen import pres_str, pres_tree
    logger.info("Unfolded %s (document revision %d):", result.description, result.revision)
    logger.info("  %s", pres_str(result.term))
    logger.info(pres_tree(result.term))


def render_argument_cmd(prover: ProverWrapper, name: str, normalized: bool = False, *, style: Optional[str] = None,
                        which: Optional[str] = None) -> None:
    """CLI for Argument Rendering.

    style can be one of: argumentation|dialectical|intuitionistic|vanilla.
    If omitted, uses the argument's configured rendering.  ``which`` (one
    of TERM_SELECTORS) renders another of the argument's terms; an issue
    target (``issue :X``) renders its unfolded term.
    """
    if which is not None or name.startswith("issue ") or name in prover.debates:
        _show_selected(prover, name, which or "unfolded", style)
        return
    arg = prover.get_argument(name)
    if not arg:
        logger.error(f"Argument '{name}' not found.")
        return

    logger.info("")  # spacer before NL rendering
    if normalized:
        logger.info(f"Rendering argument {arg.name} in normal form:")
    else:
        logger.info(f"Rendering argument {arg.name}:")

    if style is None:
        logger.info(arg.render(normalized=normalized))
        logger.info("")
        return

    style = style.strip().lower()
    from pres.nl import (
        pretty_natural,
        natural_language_argumentative_rendering,
        dialectical_rendering,
        natural_language_rendering,
        pruefschema_rendering,
        vanilla_rendering,
    )
    sem_map = {
        "argumentation": natural_language_argumentative_rendering,
        "dialectical": dialectical_rendering,
        "intuitionistic": natural_language_rendering,
        "vanilla": vanilla_rendering,
        "pruefschema": pruefschema_rendering,
    }
    sem = sem_map.get(style)
    if sem is None:
        logger.error("Invalid render style '%s' (expected: %s)", style, ", ".join(sem_map.keys()))
        logger.info("")
        return

    # Ensure we have the right AST available
    if normalized:
        if arg.normal_body is None:
            arg.normalize()
        pt = arg.normal_body
    else:
        if not arg.executed:
            arg.execute()
        pt = arg.body

    logger.info(
        pretty_natural(
            pt,
            sem,
            declarations=getattr(prover, "declarations", {}),
            decorations=getattr(prover, "decorations", {}),
        )
    )
    logger.info("")  # spacer after NL rendering


def _call(method, *args, **kwargs):
    """Call a service operation from the dispatchers: a refusal raises the
    error that caused it (ProverError, NameClash, DebateError, ...), which
    the dispatchers already report."""
    try:
        return method(*args, **kwargs)
    except AidaError as e:
        raise (e.cause if isinstance(e.cause, Exception) else ProverError(str(e))) from None


def _finish(prover: ProverWrapper, current: dict, *, demand_strict: bool):
    """Replay and register a recording (wrap/service.py, ``finish_recording``)."""
    return _call(Service.of(prover).finish_recording, current, demand_strict=demand_strict).value


def _report(error) -> None:
    """Print a service refusal as the CLI always has: ``STAGE: refused:
    MESSAGE`` for a refusal of a pipeline stage, an error log line for a
    request that is malformed or names nothing."""
    if error.stage:
        print(f"{error.stage}: refused: {error}")
    else:
        logger.error("%s", error)


def _target(parts):
    """(name, rest) from a command's tokens after the verb: an argument
    name, or the two tokens of an issue target joined (``issue :X``)."""
    if len(parts) >= 3 and parts[1] == "issue":
        return f"issue {parts[2]}", parts[3:]
    return (parts[1] if len(parts) >= 2 else ""), parts[2:]


def _issue(prover: ProverWrapper, name: str, *, want_term: bool):
    """(arg, issue, term, shared) for NAME - an argument, a debate, or an
    issue (``issue :X`` / ``issue X:``) - from the service's ``issue_term``
    (wrap/service.py), or (None, None, None, None) after printing the
    refusal."""
    service = Service.of(prover)
    try:
        found = service.issue_term(service.target(name), want_term=want_term)
    except AidaError as e:
        _report(e)
        return None, None, None, None
    return found.argument, found.issue, found.term, found.shared


def _debate_logic_refusal(prover: ProverWrapper) -> Optional[str]:
    """Why the document's logic admits no debates, or None (wrap/service.py)."""
    return Service.of(prover)._logic_refusal()


def _debate_issue(prover: ProverWrapper, debate):
    """``_issue`` for a debate: (None, issue, term, None), or all None after
    printing the refusal."""
    return _issue(prover, debate.name, want_term=True)


def _issue_term(prover: ProverWrapper, name: str):
    """(arg, issue, term): the unfolded debate term for NAME's issue, for
    the commands that still work on the term (evaluate, explain)."""
    arg, issue, term, _shared = _issue(prover, name, want_term=True)
    return arg, issue, term


def adopt_strict_edge_cmd(prover: ProverWrapper, command: str):
    """CLI: `adopt EDGE* as NAME` - promote a strict edge that unfolding
    discovered (Peirce's thesis, say) to a theorem Fellowship holds
    (wrap/service.py, ``adopt``)."""
    parts = command.split()
    if len(parts) != 4 or parts[0] != "adopt" or parts[2] != "as":
        print("adopt: use `adopt EDGE* as NAME`, with an edge name shown by `graph ARG`.")
        return None
    try:
        return Service.of(prover).adopt(parts[1], parts[3]).value
    except NotFound as e:
        print(f"adopt: {e}")
    except AidaError as e:
        print(f"adopt: refused: {e}")
    return None



def _render_graph_image(dot_source: str, out_base: str, fmt: str = "png") -> Optional[str]:
    """Render DOT to an image file, or None when Graphviz is unavailable.

    Tries the graphviz Python package first, then the `dot` binary.
    """
    try:
        import graphviz  # type: ignore
        return graphviz.Source(dot_source).render(filename=out_base, format=fmt, cleanup=True)
    except Exception as e:
        logger.debug("graphviz package unavailable or failed: %s", e)
    dot_binary = shutil.which("dot")
    if dot_binary:
        out_path = f"{out_base}.{fmt}"
        try:
            subprocess.run([dot_binary, f"-T{fmt}", "-o", out_path],
                           input=dot_source, text=True, check=True,
                           capture_output=True, timeout=60)
            return out_path
        except Exception as e:
            logger.debug("dot binary failed: %s", e)
    return None


def _open_file(path: str) -> bool:
    """Open a file in the platform viewer; True if the opener was launched.
    ACDC_NO_OPEN=1 (tests, headless runs) skips the viewer."""
    if os.getenv("ACDC_NO_OPEN"):
        return False
    try:
        if sys.platform == "darwin":
            subprocess.run(["open", path], check=True, timeout=30)
        elif sys.platform.startswith("win"):
            os.startfile(path)  # type: ignore[attr-defined]
        else:
            subprocess.run(["xdg-open", path], check=True, timeout=30)
        return True
    except Exception as e:
        logger.debug("Could not open %s: %s", path, e)
        return False


def _dispatch_label(prover: ProverWrapper, command: str) -> None:
    name, rest = _target(command.split())
    try:
        _, semantics, _, _ = _split_eval_tokens(rest)
    except ValueError as e:
        logger.error("label: %s", e)
        return
    label_argument_cmd(prover, name, semantics or "grounded")


def _eval_options(verb: str, command: str):
    """(name, mode, semantics, base, witness, favour) for evaluate and
    explain, or None after an error message."""
    name, rest = _target(command.split())
    if not name:
        logger.error("%s: needs an argument or an issue. Use: %s ARG|issue :X|X: "
                     "[MODE] [SEMANTICS] [BASE] [N|all] [favour]", verb, verb)
        return None
    favour = "favour" in rest
    rest = [tok for tok in rest if tok != "favour"]
    try:
        mode, semantics, base, witness = _split_eval_tokens(rest)
    except ValueError as e:
        logger.error("%s: %s", verb, e)
        return None
    if witness is not None and (mode or "skeptical") != "credulous":
        logger.error("%s: a witness number or 'all' requires credulous mode", verb)
        return None
    return name, mode or "skeptical", semantics or "preferred", base or "cbn", witness, favour


def _dispatch_evaluate(prover: ProverWrapper, command: str) -> None:
    found = _eval_options("evaluate", command)
    if found is not None:
        name, mode, semantics, base, witness, favour = found
        evaluate_argument_cmd(prover, name, mode, base, semantics, witness, favour)


def _dispatch_explain(prover: ProverWrapper, command: str) -> None:
    found = _eval_options("explain", command)
    if found is not None:
        name, mode, semantics, base, witness, favour = found
        explain_argument_cmd(prover, name, mode, base, semantics, witness, favour)


#: The pipeline's loggers, in the order the stages run.  ``explain`` groups
#: its account by these, and the stage label is the last dotted component.
_PIPELINE_LOGGERS = (
    "core.dc.issue",
    "core.dc.unfold",
    "core.dc.typecheck",
    "core.dc.debate_graph",
    "core.comp.adf_label",
    "core.comp.oracle",
    "core.comp.evaluate",
    "core.comp.oracle_terms",
    "core.dc.strict",
)


class _StageRecorder(logging.Handler):
    """Collect the pipeline's records for one evaluation, in order."""

    def __init__(self):
        super().__init__(level=logging.DEBUG)
        self.records = []

    def emit(self, record):
        self.records.append(record)


def explain_argument_cmd(prover: ProverWrapper, name: str, mode: str = "skeptical",
                         base: str = "cbn", semantics: str = "preferred",
                         witness=None, favour: bool = False) -> None:
    """CLI: evaluate ARG and print the pipeline's own account of the run.

    Syntax:
        explain ARG [skeptical|credulous] [grounded|complete|preferred|stable] [cbn|cbv] [N|all]

    Same options as `evaluate`.  The account is the DEBUG narrative each
    stage already emits, captured for this one run and grouped by stage,
    so it needs no change to the global log level and stays correct as the
    instrumentation grows.  The verdict is printed last, after the account
    that led to it.
    """
    recorder = _StageRecorder()
    captured = [logging.getLogger(n) for n in _PIPELINE_LOGGERS]
    verdict_logger = logging.getLogger("fsp.wrapper")
    saved = [(lg, lg.level, lg.propagate) for lg in captured + [verdict_logger]]
    for lg in captured:
        lg.addHandler(recorder)
        lg.propagate = False              # no second copy on stdout at DEBUG
        if lg.level == logging.NOTSET or lg.level > logging.DEBUG:
            lg.setLevel(logging.DEBUG)
    verdict_logger.addHandler(recorder)
    verdict_logger.propagate = False      # hold the verdict back until the end
    try:
        evaluate_argument_cmd(prover, name, mode, base, semantics, witness, favour)
    finally:
        for lg in captured + [verdict_logger]:
            lg.removeHandler(recorder)
        for lg, level, propagate in saved:
            lg.setLevel(level)
            lg.propagate = propagate
    stages = [r for r in recorder.records if r.name in _PIPELINE_LOGGERS]
    verdict = [r for r in recorder.records if r.name == "fsp.wrapper"]
    if not stages:
        logger.info("explain: nothing to report for '%s' (it was refused before the pipeline ran).", name)
    else:
        logger.info("")
        logger.info("How '%s' was evaluated, stage by stage:", name)
        logger.info("")
        last_stage = ""
        for record in stages:
            message = record.getMessage()
            # Strip only the indentation `artifact` adds: a rendered term's
            # own leading spaces are its tree layout.
            if message.startswith("    "):
                indent, stripped = "  ", message[4:]
            else:
                indent, stripped = "", message.lstrip()
            # Each message opens with its own stage word - "unfold", "solver",
            # "sigma", "classify" - which names the step better than the module
            # does (one module runs several steps).  Lift it into the column.
            head, sep, rest = stripped.partition(": ")
            if sep and " " not in head:
                stage, stripped = head, rest
            elif indent and last_stage:
                stage = last_stage         # a continuation line of an artifact
            else:
                stage = record.name.rsplit(".", 1)[-1]
            last_stage = stage
            logger.info("  %-14s %s%s", stage, indent, stripped)
        logger.info("")
    for record in verdict:
        logger.info("%s", record.getMessage())


def graph_argument_cmd(prover: ProverWrapper, name: str, dot_path: Optional[str] = None,
                       show: bool = False, whole: bool = False) -> None:
    """CLI: compile an argument's debate graph; print a summary, optionally DOT
    (wrap/service.py, ``graph``).

    Syntax:
        graph ARG|DEBATE [all] ['FILE.dot'] [show].

    With `show`, render the graph and open it in the platform viewer;
    when Graphviz is not installed, print an indented text view instead
    (which always works and needs no dependencies).  For a debate the
    graph is what its conclusion reaches; with `all`, the whole of its
    scope - a move that connects to nothing included.
    """
    service = Service.of(prover)
    try:
        result = service.graph(service.target(name), whole=whole, labels=True)
    except AidaError as e:
        _report(e)
        return
    _show_graph(prover, name, result, dot_path, show)


def _show_graph(prover: ProverWrapper, name: str, result, dot_path: Optional[str] = None,
                show: bool = False) -> None:
    whole = result.whole
    graph, arg, debate = result.graph, result.argument, result.debate
    if whole:
        logger.info("Scope of debate '%s' (%s): %d nodes, %d edges", name, debate.scope,
                    len(graph.nodes), len(graph.edges))
    elif debate is not None:
        logger.info("Debate graph for debate '%s' (issue %s[%s], %s scope, %d move(s)): "
                    "%d nodes, %d edges", name, debate.issue_prop, debate.issue[1][0],
                    debate.scope, len(debate.moves), len(graph.nodes), len(graph.edges))
    elif name.startswith("issue "):
        logger.info("Debate graph for %s (unfolded from the document): %d nodes, %d edges",
                    name, len(graph.nodes), len(graph.edges))
    elif arg is None:
        logger.info("Document graph: %d nodes, %d edges (every registered atomic argument)",
                    len(graph.nodes), len(graph.edges))
    else:
        key, side = prover.issue_of(arg)
        logger.info("Debate graph for '%s' (issue %s[%s], unfolded from the document): %d nodes, %d edges",
                    name, graph.nodes.get(key, arg.conclusion), side[0], len(graph.nodes), len(graph.edges))
    for edge in graph.edges:
        sources = ", ".join(
            f"{graph.nodes[s.key]}[{s.side[0]}:{s.kind[:4]}]" for s in edge.sources
        ) or "-"
        strictness = "strict" if edge.strict else "defeasible"
        logger.info("  %s (%s, %s): %s <- %s",
                    edge.name, edge.role, strictness,
                    f"{graph.nodes[edge.target_key]}[{edge.target_side[0]}]", sources)
    for (key, side), kinds in graph.defaults.items():
        logger.info("  default %s[%s]: %s", graph.nodes[key], side[0], ", ".join(sorted(kinds)))
    logger.info("  fragment: %s",
                "acyclic" if graph.is_acyclic() else "cyclic (derivation cycle; labelled like any other)")
    labels = result.labels
    if result.labels_unavailable:
        print(f"graph: labels unavailable: {result.labels_unavailable}")
    may_render = getattr(prover, "render_files", True)
    if dot_path:
        if may_render:
            with open(dot_path, "w") as fh:
                fh.write(graph.to_dot(labels=labels))
            logger.info("DOT written to %s (render: dot -Tpng %s -o graph.png)", dot_path, dot_path)
        else:
            logger.info("File output is off (ACDC_NO_RENDER): not writing %s.", dot_path)
    if show:
        image = _render_graph_image(graph.to_dot(labels=labels), f"{name}_graph") if may_render else None
        if image and _open_file(image):
            logger.info("Graph rendered to %s and opened.", image)
            return
        if image:
            logger.info("Graph rendered to %s (open it manually).", image)
            return
        # No picture: either Graphviz is missing or file output is off.  Either
        # way the text view is the better thing to have in a log.
        if not may_render:
            logger.info("")
            logger.info("Debate graph '%s' (file output is off, ACDC_NO_RENDER):", name)
            logger.info("")
            for line in graph.to_text(labels=labels).splitlines():
                logger.info("  %s", line)
            logger.info("")
            return
        logger.info("")
        logger.info("Debate graph '%s' (install graphviz for a rendered picture):", name)
        logger.info("")
        for line in graph.to_text(labels=labels).splitlines():
            logger.info("  %s", line)
        logger.info("")


_SEMANTICS_TOKENS = ("grounded", "complete", "preferred", "stable")
_MODE_TOKENS = ("skeptical", "credulous")
_BASE_TOKENS = ("cbn", "cbv")


def _split_eval_tokens(tokens):
    """Order-free option parsing for label/evaluate: each token is a mode,
    a semantics, a base strategy, a witness number or "all".  Returns
    (mode, semantics, base, witness) with witness None, an int or "all";
    raises ValueError on an unknown token."""
    mode = semantics = base = witness = None
    for tok in tokens:
        if tok in _MODE_TOKENS:
            mode = tok
        elif tok in _SEMANTICS_TOKENS:
            semantics = tok
        elif tok in _BASE_TOKENS:
            base = tok
        elif tok == "all":
            witness = "all"
        elif tok.isdigit() and int(tok) >= 1:
            witness = int(tok)
        else:
            raise ValueError(
                f"unknown option '{tok}' (expected one of {_MODE_TOKENS + _SEMANTICS_TOKENS + _BASE_TOKENS}, "
                f"a witness number or 'all')"
            )
    return mode, semantics, base, witness


def label_argument_cmd(prover: ProverWrapper, name: str, semantics: str = "grounded") -> None:
    """CLI: ADF labelling(s) of an argument's debate graph (wrap/service.py,
    ``label``).

    Syntax:
        label ARG [grounded|complete|preferred|stable].

    grounded prints the one grounded labelling; the others print every
    labelling of that semantics, numbered.
    """
    service = Service.of(prover)
    try:
        result = service.label(service.target(name), semantics)
    except AidaError as e:
        _report(e)
        return
    _show_label(name, result)


def _show_label(name: str, result) -> None:
    semantics = result.semantics
    graph, found = result.graph, result.labellings
    if not found:
        logger.info("No %s labelling exists for '%s'.", semantics, name)
        return
    if len(found) == 1:
        logger.info("%s labelling for '%s':", semantics.capitalize(), name)
        for (key, side), label in found[0].items():
            logger.info("  %-40s %-8s %s", graph.nodes[key], side, label)
        return
    logger.info("%d %s labellings for '%s':", len(found), semantics, name)
    for i, labels in enumerate(found, 1):
        logger.info("  [%d]", i)
        for (key, side), label in labels.items():
            logger.info("    %-40s %-8s %s", graph.nodes[key], side, label)


def evaluate_argument_cmd(prover: ProverWrapper, name: str, mode: str = "skeptical",
                          base: str = "cbn", semantics: str = "preferred",
                          witness=None, favour: bool = False) -> None:
    """CLI: label-guided evaluation of an argument's debate term
    (wrap/service.py, ``evaluate``).

    Syntax:
        evaluate ARG [skeptical|credulous] [grounded|complete|preferred|stable] [cbn|cbv] [N|all] [favour].
        evaluate issue :X|X: [same options, no favour].

    On the unfolded route (the default) ARG's term is the one unfolded for
    it, biased towards it; an issue gets the canonical term.  ``favour``
    (credulous, an argument, the unfolded route) prefers a witness
    labelling in which the argument's own derivation is IN.  In credulous
    mode N picks labelling N of `label ARG SEMANTICS` as the witness and
    `all` evaluates under every labelling that accepts the issue.  The
    normal form (the last one, under `all`) is cached on the argument.
    """
    service = Service.of(prover)
    try:
        result = service.evaluate(service.target(name), mode=mode, semantics=semantics,
                                  base=base, witness=witness, favour=favour)
    except AidaError as e:
        _report(e)
        return
    _show_evaluation(name, result, mode, base, semantics, witness, favour)


def _show_evaluation(name: str, result, mode: str, base: str, semantics: str, witness, favour: bool) -> None:
    from pres.gen import ProofTermGenerationVisitor
    import copy as _copy
    if witness == "all":
        if not result.results:
            logger.info("Evaluated '%s' (credulous, %s, base %s): no labelling accepts the issue.",
                        name, semantics, base)
            return
        logger.info("Evaluated '%s' (credulous, %s, base %s) under %d accepting witness(es):",
                    name, semantics, base, len(result.results))
        for number, nf, nf_class, _sigma in result.results:
            pretty = ProofTermGenerationVisitor().visit(_copy.deepcopy(nf)).pres
            logger.info("  [%d] %s", number, nf_class.upper())
            logger.info("      normal form: %s", pretty)
        return
    pretty = ProofTermGenerationVisitor().visit(_copy.deepcopy(result.normal_form)).pres
    chosen = (f", witness {witness}" if witness is not None else "") + (", favoured" if favour else "")
    logger.info("Evaluated '%s' (%s, %s, base %s%s): %s", name, mode, semantics, base, chosen,
                result.nf_class.upper())
    logger.info("  normal form: %s", pretty)


def tree_argument_cmd(prover: ProverWrapper, name: str, fmt: str = "png", *, mode: str = "pt",
                      nl_style: str = "argumentation", which: Optional[str] = None) -> None:
    """CLI: render the acceptance tree (proof terms or NL), coloured by the
    grounded ADF labels of the argument's debate graph, and save it as a
    file (wrap/service.py, ``tree``, gives the DOT).  If the graph is
    refused or adf-bdd is missing the tree is written uncoloured with a
    one-line notice.  ``which`` (TERM_SELECTORS) draws another of the
    argument's terms than its normal form."""
    service = Service.of(prover)
    try:
        tree = service.tree(service.target(name), mode=mode, nl_style=nl_style, which=which)
    except InvalidRequest as e:
        logger.error("%s", e)
        return
    except AidaError as e:
        _report(e)
        return
    _show_tree(prover, name, tree, fmt)


def _show_tree(prover: ProverWrapper, name: str, tree, fmt: str = "png") -> None:
    if tree.graph_refused is not None:
        _report(tree.graph_refused)
    if tree.labels_unavailable:
        print(f"tree: labels unavailable: {tree.labels_unavailable}")
    dot = tree.dot
    out_base = f"{name}_tree"
    if not getattr(prover, "render_files", True):
        logger.info("File output is off (ACDC_NO_RENDER): not writing %s.%s for '%s'.",
                    out_base, fmt, name)
        return
    image = _render_graph_image(dot, out_base, fmt)        # graphviz package, else the dot binary
    if image:
        if _open_file(image):
            logger.info("Acceptance tree written to %s and opened.", image)
        else:
            logger.info("Acceptance tree written to %s.", image)
        return
    dot_path = f"{out_base}.dot"
    try:
        with open(dot_path, "w", encoding="utf-8") as f:
            f.write(dot)
        logger.warning("Graphviz not available. Wrote DOT to %s (render: dot -T%s %s -o %s.%s)",
                       dot_path, fmt, dot_path, out_base, fmt)
    except Exception as e2:
        logger.error("Failed to write DOT file: %s", e2)


def _handle_import_command(ap: argparse.ArgumentParser, import_args: list[str]) -> None:
    """Handle `--import SOURCE_LANGUAGE SOURCE_JSON MODE [TARGET_FILE_NAME]`."""
    if len(import_args) not in (3, 4):
        ap.error("--import expects: SOURCE_LANGUAGE SOURCE_JSON MODE [TARGET_FILE_NAME]")

    source_language, source_json, mode = import_args[:3]
    target_file_name = import_args[3] if len(import_args) == 4 else None

    try:
        translate = load_importer(source_language)
    except ImporterNotFound:
        ap.error(missing_importer_message(source_language))
    if mode not in ("file", "interactive"):
        ap.error("Unsupported import mode '%s' (expected 'file' or 'interactive')" % mode)

    json_path = Path(source_json).expanduser()
    if not json_path.is_file():
        ap.error(f"source_json file {json_path} does not exist")

    try:
        data = json.loads(json_path.read_text())
        result = translate(data)
    except (OSError, json.JSONDecodeError, SourceImportError) as e:
        ap.error(f"failed to import {json_path}: {e}")

    script_name = json_path.stem
    if mode == "file":
        target_path = Path(target_file_name).expanduser() if target_file_name else json_path.with_suffix(".fspy")
        result.write_fspy(target_path, name=script_name)
        logger.info("Imported %s JSON written to %s", source_language, target_path)
        return

    prover = setup_prover()
    with tempfile.TemporaryDirectory(prefix=f"{source_language}_import_cli_") as tmpdir:
        script_path = Path(tmpdir) / f"{script_name}.fspy"
        result.write_fspy(script_path, name=script_name)
        execute_script(prover, str(script_path), strict=True, stop_on_error=True, isolate=False)
        interactive_mode(prover)


def main() -> None:
    ap = argparse.ArgumentParser(
        prog='fellowship-wrapper',
        description='Run Fellowship prover in interactive or batch mode')
    g = ap.add_mutually_exclusive_group(required=True)
    ap.add_argument('--log-level', dest='log_level',
                    choices=['TRACE', 'DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'],
                    help='set logger level (overrides FSP_LOGLEVEL)')
    ap.add_argument('--log-file', dest='log_file',
                    help='write logs to FILE (in addition to stdout)')
    ap.add_argument('--load', metavar='FILE',
                    help='load a .fspy script before entering interactive mode (use with --interactive)')
    g.add_argument('--interactive', action='store_true',
                   help='start an interactive Fellowship REPL')
    g.add_argument('--script', metavar='FILE',
                   help='execute commands in FILE (same grammar as interactive mode)')
    g.add_argument('--import', dest='import_args', nargs='+', metavar='IMPORT_ARG',
                   help='import SOURCE_LANGUAGE SOURCE_JSON MODE [TARGET_FILE_NAME]; MODE is file or interactive')
    args = ap.parse_args()

    # Configure CLI logging (explicit --log-level wins; else env FSP_LOGLEVEL; default INFO)
    configure_logging_cli(args.log_level, args.log_file)

    if args.load and not args.interactive:
        ap.error('--load can only be used together with --interactive')

    if args.import_args is not None:
        _handle_import_command(ap, args.import_args)
        return

    prover = setup_prover()             # ⇐ creates the ProverWrapper,
                                        #    registers pop, declares A,B,C,D …

    if args.interactive:                # --- REPL -----------------
        if args.load:
            load_script = Path(args.load).expanduser()
            if not load_script.is_file():
                ap.error(f'load file {load_script} does not exist')
            execute_script(prover, str(load_script), strict=True, stop_on_error=True, isolate=False)
        interactive_mode(prover)

    else:                               # --- batch ----------------
        script = Path(args.script).expanduser()
        if not script.is_file():
            ap.error(f'script file {script} does not exist')
        execute_script(prover, script)

if __name__ == '__main__':
    main()
