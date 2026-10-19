"""One interpreter for AIDA documents (tasks.org, aida-command-syntax):
scripts, the REPL and the document check all run commands through it.

``Interpreter(session).execute(item)`` runs one unit of ``wrap.syntax`` - a
Command, a comment, or a command that did not parse - and returns an
``Outcome``: its span, its kind, a status, a message and the value the
service returned.  It prints nothing; the CLI renders outcomes, the
document check serialises them.

Statuses:
- ``ok``: done; ``value`` holds the result;
- ``refused``: the document refused it (a name clash, a failed replay, a
  move that does not fit, a syntax error) - a strict script stops;
- ``reported``: a query that the pipeline refused (``error``): shown, never
  stops a script;
- ``error``: Fellowship refused a command - a script stops unless told not
  to;
- ``fatal``: the prover connection broke;
- ``cli``: a command only the CLI runs (explain, the deprecated term-level
  commands, load, save); the CLI runs it, a document check skips it;
- ``comment`` and ``stop`` for comment lines and ``%stop``.

Arguments are recorded as blocks: ``argument N : (P).`` ... ``dixi.``, and
``prove N.`` ... ``qed.`` (or ``dixi.``).  In a script the block is
replayed when it closes.  With ``live`` (the REPL) every line is sent to
Fellowship as it comes, and ``show_ui`` gets Fellowship's reply - the open
goals - after each.

Every unit run is kept in ``transcript``; ``save "FILE".`` writes it back
as a script (``write_transcript``): the accepted commands, or with ``save
all`` every command, each failed one after a ``%`` line saying why.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Callable, List, Optional

from core.dc.cite import CitationError, citation_target, is_strict_citation
from core.dc.debate import DebateError
from wrap.prover import MachinePayloadError, ProverError
from wrap.service import AidaError, Service
from wrap.syntax import Command, Span, SyntaxRefused, Unit, parse_units, split, parse

QUERIES = ("graph", "label", "evaluate", "render", "tree", "unfold", "share")
CLI_ONLY = ("explain", "render_nf", "reduce", "normalize", "expand", "projection", "load", "save")
FAILED = ("refused", "error", "fatal", "reported")


@dataclass
class Outcome:
    span: Optional[Span]
    text: str
    kind: str
    status: str
    message: str = ""
    value: Any = None
    error: Optional[BaseException] = None
    command: Optional[Command] = None
    diagnostics: List[Any] = field(default_factory=list)


class Interpreter:
    def __init__(self, session, *, live: bool = False,
                 show_ui: Optional[Callable[[Any], None]] = None,
                 read_more: Optional[Callable[[], str]] = None):
        self.session = session
        self.service = Service.of(session)
        self.live = live
        self.show_ui = show_ui or (lambda state: None)
        self.read_more = read_more
        #: The argument block being recorded: its name, conclusion,
        #: instructions, whether it is anti, and (live) Fellowship's state.
        self.recording: Optional[dict] = None
        #: Every unit run, in order: its text as written back, why it
        #: failed (None if it did not), and whether the accepted transcript
        #: keeps it - not a line of a block that was refused or abandoned.
        self.transcript: List[dict] = []
        self._abandoned: Optional[dict] = None

    # -- running -------------------------------------------------------------

    def run(self, text: str):
        """Every unit of ``text``, executed in order, as outcomes."""
        for item in parse_units(text):
            yield self.execute(item)

    def execute(self, item) -> Outcome:
        outcome = self._execute(item)
        if not (isinstance(item, Command) and item.kind == "save"):
            self._log(item, outcome)
        return outcome

    def _log(self, item, o: Outcome) -> None:
        if isinstance(item, SyntaxRefused):
            text = item.text + "."
        elif isinstance(item, Unit):
            text = {"narration": f"# {item.text}", "silent": f"% {item.text}",
                    "stop": "%stop"}.get(item.kind, item.text)
        else:
            text = item.text + "."
        failure = None
        if o.status in FAILED:
            failure = f"{o.status}: " + " ".join((o.message or "").split())
        self.transcript.append({"text": text, "failure": failure, "kept": failure is None})
        if self._abandoned is not None:
            # the block, from its opening line to this one, did not survive
            for entry in self.transcript[self._abandoned["_transcript_from"]:]:
                entry["kept"] = False
            self._abandoned = None

    def write_transcript(self, path: str, *, every: bool = False) -> int:
        """Write the transcript to ``path`` as a script: the accepted
        commands, or with ``every`` all of them, each failed one after a
        ``%`` line saying why.  Returns the number of lines written."""
        lines = []
        for entry in self.transcript:
            if every:
                if entry["failure"]:
                    lines.append(f"% {entry['failure']}")
                lines.append(entry["text"])
            elif entry["kept"]:
                lines.append(entry["text"])
        if self.recording is not None:
            lines.append(f"% '{self.recording['name']}' was still being recorded; "
                         f"end it with `dixi.` (or `qed.`).")
        with open(path, "w", encoding="utf-8") as handle:
            handle.write("\n".join(lines) + "\n")
        return len(lines)

    def _execute(self, item) -> Outcome:
        if isinstance(item, SyntaxRefused):
            return Outcome(item.span, "", "syntax", "refused", str(item), error=item)
        if isinstance(item, Unit):
            if item.kind == "incomplete":
                return Outcome(item.span, item.text, "syntax", "refused",
                               f"`{item.text}` does not end with a '.'")
            if item.kind == "stop":
                return Outcome(item.span, item.text, "stop", "stop")
            return Outcome(item.span, item.text, item.kind, "comment", item.text)
        command: Command = item
        try:
            outcome = self._dispatch(command)
        except MachinePayloadError as e:
            outcome = Outcome(None, command.text, command.kind, "fatal", str(e), error=e)
        outcome.span = command.span
        outcome.text = command.text
        outcome.command = command
        return outcome

    # -- dispatch ------------------------------------------------------------

    def _dispatch(self, c: Command) -> Outcome:
        s, svc = self.session, self.service
        if c.kind == "new_document":
            if self.recording is not None:
                self._abandoned = self.recording
                self._drop_recording()
            return self._service(c, svc.new_document, c["logic"], c["minimal"])
        if c.kind == "save":
            return Outcome(None, c.text, c.kind, "cli")
        if self.recording is not None:
            return self._in_block(c)
        if c.kind == "argument":
            return self._open(c)
        if c.kind == "refine":
            return self._reopen(c)
        if c.kind in ("dixi", "qed"):
            return Outcome(None, c.text, c.kind, "refused", "Not currently recording an argument.")
        if c.kind == "statement":
            return self._service(c, svc.state, c["keyword"], c["name"], c["conclusion"], anti=c["anti"])
        if c.kind == "debate":
            return self._service(c, svc.start_debate, c["name"], c["issue"], c["onus"], c["scope"])
        if c.kind == "close_debate":
            return self._service(c, svc.close_debate)
        if c.kind == "move":
            return self._service(c, svc.move, c["argument"], c["verb"], c["target"])
        if c.kind == "register":
            return self._service(c, svc.register, c["name"], c["conclusion"], c["proof_term"],
                                 strict=c["strict"])
        if c.kind == "decorate":
            return self._service(c, svc.decorate, c["name"], c["template"])
        if c.kind == "adopt":
            return self._service(c, svc.adopt, c["edge"], c["name"])
        if c.kind == "typecheck":
            return self._service(c, svc.set_typecheck, c["mode"])
        if c.kind == "pipeline":
            return self._service(c, svc.set_pipeline, c["mode"])
        if c.kind == "subarguments":
            return self._service(c, svc.set_subarguments, c["mode"])
        if c.kind == "undecided":
            return self._service(c, svc.set_undecided, c["mode"])
        if c.kind in QUERIES:
            return self._query(c)
        if c.kind in CLI_ONLY:
            return Outcome(None, c.text, c.kind, "cli")
        # opaque: a debate move by name while a debate is recorded, or a
        # Fellowship command
        words = c.text.split()
        if (getattr(s, "recording_debate", None) is not None
                and words[0] in getattr(s, "arguments", {}) and len(words) <= 2):
            o = self._service(c, svc.move, words[0], None, words[1] if len(words) == 2 else None)
            o.kind = "move"
            return o
        return self._fellowship(c)

    def _service(self, c: Command, method, *args, **kwargs) -> Outcome:
        try:
            result = method(*args, **kwargs)
        except AidaError as e:
            return Outcome(None, c.text, c.kind, "refused", str(e), error=e,
                           diagnostics=list(e.diagnostics))
        made = getattr(result, "value", None)
        if c.kind in ("register", "statement", "adopt") and made is not None and c.span is not None:
            made.span = c.span                      # where it sits in the text
        return Outcome(None, c.text, c.kind, "ok", getattr(result, "message", ""), value=result,
                       diagnostics=list(getattr(result, "diagnostics", [])))

    def _query(self, c: Command) -> Outcome:
        svc = self.service
        try:
            if c.kind == "graph":
                result = svc.graph(svc.target(c["target"]), whole=c["whole"], labels=True)
            elif c.kind == "label":
                result = svc.label(svc.target(c["target"]), c["semantics"])
            elif c.kind == "evaluate":
                result = svc.evaluate(svc.target(c["target"]), mode=c["mode"], semantics=c["semantics"],
                                      base=c["base"], witness=c["witness"], favour=c["favour"])
            elif c.kind == "render":
                target = svc.target(c["target"])
                if c["which"] is None and target.kind == "argument":
                    result = svc.render(target, None, c["style"])
                else:
                    result = svc.term(target, c["which"] or "unfolded")
            elif c.kind == "tree":
                result = svc.tree(svc.target(c["target"]), mode=c["mode"], nl_style=c["nl_style"],
                                  which=c["which"])
            elif c.kind == "unfold":
                if c["what"] == "debate" and c["target"] not in self.session.debates:
                    raise AidaError(f"unfold debate: no debate '{c['target']}'.")
                result = svc.unfold(svc.target(c["target"]))
            else:                                        # share
                result = svc.share(c["name"])
        except AidaError as e:
            return Outcome(None, c.text, c.kind, "reported", str(e), error=e,
                           diagnostics=list(e.diagnostics))
        return Outcome(None, c.text, c.kind, "ok", value=result,
                       diagnostics=list(getattr(result, "diagnostics", [])))

    def _fellowship(self, c: Command, *, include_ui: bool = False) -> Outcome:
        s = self.session
        try:
            send = getattr(s, "send_declaring", s.send_command)
            state = send(c.text + ".", include_ui=self.live or include_ui,
                         allow_incomplete=self.live)
            if self.live:
                self.show_ui(state)
        except MachinePayloadError:
            raise
        except ProverError as e:
            return Outcome(None, c.text, "fellowship", "error", str(e), error=e)
        return Outcome(None, c.text, "fellowship", "ok", value=state)

    # -- argument blocks -----------------------------------------------------

    def _open(self, c: Command) -> Outcome:
        from wrap.registry import start_recording
        s = self.session
        name, conclusion, anti = c["name"], c["conclusion"], c["anti"]
        try:
            start_recording(s, name, anti)
        except ProverError as e:
            return Outcome(None, c.text, c.kind, "refused", str(e), error=e)
        current = {"name": name, "conclusion": conclusion, "instructions": [], "is_anti": anti,
                   "_opened_at": c.span, "_transcript_from": len(self.transcript)}
        kind = "counterargument" if anti else "argument"
        if self.live:
            opener = "antitheorem" if anti else "theorem"
            try:
                current["_state"] = s.send_command(f"{opener} {name} : ({conclusion}).", include_ui=True)
                self.show_ui(current["_state"])
            except MachinePayloadError:
                raise
            except ProverError as e:
                if s.names.get(name) == "recording":
                    s.names.pop(name)
                return Outcome(None, c.text, c.kind, "error", str(e), error=e)
        self.recording = current
        return Outcome(None, c.text, c.kind, "ok",
                       f"Started recording {kind} '{name}' with conclusion '{conclusion}'.")

    def _reopen(self, c: Command) -> Outcome:
        from wrap.registry import reopen, abandon
        s = self.session
        try:
            current = reopen(s, c["name"])
        except ProverError as e:
            return Outcome(None, c.text, c.kind, "refused", str(e), error=e)
        if self.live:
            opener = "antitheorem" if current["is_anti"] else "theorem"
            try:
                current["_state"] = s.send_command(
                    f"{opener} {c['name']} : ({current['conclusion']}).", include_ui=True)
            except MachinePayloadError:
                raise
            except ProverError as e:
                abandon(s, current)
                return Outcome(None, c.text, c.kind, "refused", str(e), error=e)
            replayed = [self._live_step(current, instr, record=False) for instr in current["instructions"]]
            if "refused" in replayed:
                abandon(s, current)
                s.send_command("discard theorem.")
                return Outcome(None, c.text, c.kind, "refused",
                               f"'{c['name']}' could not be replayed to continue it.")
            self.show_ui(current.get("_state"))
        current["_opened_at"] = c.span
        current["_transcript_from"] = len(self.transcript)
        self.recording = current
        return Outcome(None, c.text, c.kind, "ok",
                       f"Refining '{c['name']}' : {current['conclusion']}; end with `qed.` or `dixi.`")

    def _in_block(self, c: Command) -> Outcome:
        if c.kind in ("dixi", "qed"):
            return self._close(c, demand_strict=c.kind == "qed")
        if c.kind in ("argument", "refine", "statement", "debate"):
            return Outcome(None, c.text, c.kind, "refused",
                           f"'{self.recording['name']}' is still being recorded; end it with "
                           f"`dixi.` (or `qed.`) first.")
        if " ".join(c.text.split()) == "discard theorem":
            return self._discard(c)
        if not self.live:
            self.recording["instructions"].append(c.text)
            return Outcome(None, c.text, "instruction", "ok")
        status = self._live_step(self.recording, c.text)
        return Outcome(None, c.text, "instruction", status if status != "ok" else "ok",
                       "" if status == "ok" else "refused by Fellowship")

    def _close(self, c: Command, *, demand_strict: bool) -> Outcome:
        s = self.session
        current, self.recording = self.recording, None
        if self.live:
            try:
                self.show_ui(s.send_command("discard theorem.", include_ui=True))
            except MachinePayloadError:
                raise
            except ProverError:
                pass
        try:
            done = self.service.finish_recording(current, demand_strict=demand_strict)
        except AidaError as e:
            self._abandoned = current
            return Outcome(None, c.text, c.kind, "refused", str(e), error=e,
                           diagnostics=list(e.diagnostics))
        arg = done.value
        opened = current.get("_opened_at")
        if opened is not None and c.span is not None:
            # where the argument sits in the text: its whole block
            arg.span = Span(opened.line, opened.col, c.span.end_line, c.span.end_col)
        message = (f"'{arg.name}' proved: {arg.conclusion}." if demand_strict else
                   f"Argument '{arg.name}' executed and registered with conclusion '{arg.conclusion}'.")
        return Outcome(None, c.text, c.kind, "ok", message, value=arg, diagnostics=list(done.diagnostics))

    def _discard(self, c: Command) -> Outcome:
        """`discard theorem.` in a block abandons the recording, as it
        abandons a proof in Fellowship: nothing is registered, and the name
        goes back to what it was (a refinement leaves the argument as it
        was before `refine`)."""
        from wrap.registry import abandon
        current = self.recording
        self._abandoned = current
        self._drop_recording()
        abandon(self.session, current)
        return Outcome(None, c.text, c.kind, "ok",
                       f"Discarded the recording of '{current['name']}'; nothing registered.")

    def _drop_recording(self) -> None:
        current, self.recording = self.recording, None
        if self.live:
            try:
                self.session.send_command("discard theorem.")
            except ProverError:
                pass

    def _live_step(self, current: dict, command: str, *, record: bool = True) -> str:
        """Run one recorded line live: "ok" or "refused" (MachinePayloadError
        propagates).  `cite NAME` uses a registered argument at the focused
        goal: a strict one Fellowship closes itself, a defeasible one leaves
        the site open and moves on.  A cited site is closed as far as the
        author is concerned, so no later step may land on it: the focus is
        moved off first, and a step with only cited sites left is refused."""
        from core.dc.argument import Argument
        s = self.session
        probe = Argument(s, name=current["name"], conclusion=current["conclusion"])
        state = current.get("_state")
        cited_sites = current.setdefault("_cited_sites", set())

        def metas(st):
            return Argument._goal_metas(probe, st)

        try:
            cited = citation_target(s, command)
            if cited is not None:
                site, side, _prop = probe._cite_site(state, cited, command)
                if is_strict_citation(s, cited.name):
                    output = s.send_command(f"{'axiom' if side == 'rhs' else 'moxia'} {cited.name}.",
                                            include_ui=True)
                else:
                    cited_sites.add(site)
                    output = state
                    if len(metas(state)) > 1:
                        output = s.send_command("next.", include_ui=True)
                self.show_ui(output)
            else:
                if cited_sites:
                    for _ in range(len(metas(state)) + 1):
                        focused = probe._focused_goal(state)
                        if focused is None or focused[0] not in cited_sites:
                            break
                        if set(metas(state)) <= cited_sites:
                            return "refused"
                        state = s.send_command("next.", include_ui=True)
                output = s.send_command(command + ".", include_ui=True, allow_incomplete=True)
                self.show_ui(output)
                while isinstance(output, dict) and output.get("_need_more_input") and self.read_more:
                    output = s.send_command(self.read_more(), include_ui=True, allow_incomplete=True)
                    self.show_ui(output)
                if isinstance(output, dict) and output.get("_need_more_input"):
                    return "refused"
        except MachinePayloadError:
            raise
        except (ProverError, CitationError):
            return "refused"
        if isinstance(output, dict):
            current["_state"] = output
        if record:
            current["instructions"].append(command)
        return "ok"
