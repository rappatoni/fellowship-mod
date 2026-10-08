"""The service layer: what a session can do, as calls that return results
(tasks.org, aida-service-layer; Phase 3 of aida-dui-api).

``Service(session)`` wraps one session (``wrap.prover.ProverWrapper``).
Every operation

- holds the session's lock, so a compound operation (a replay, a type
  check, an evaluation) is never interleaved with another call;
- collects the warnings and errors the code it runs logs, in its own
  thread, into ``diagnostics`` - on the result, or on the error it raises;
- returns a result object (a dataclass carrying the domain objects: the
  graph, the terms, the labelling), or raises an ``AidaError`` with a
  stable ``code``.

Nothing here prints.  The CLI (wrap/cli.py) is a printer over these calls
and keeps its own output; JSON is the next phase.  Out of the service, and
CLI-only: the deprecated term-level commands (reduce, normalize,
render-nf, the projections, expand), ``tactic`` and ``explain``.  Proofs
are recorded as whole blocks; stepping through one with goal feedback is
not offered (tasks.org, aida-live-proving).
"""

from __future__ import annotations

import copy
import functools
import logging
import re
import threading
from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Tuple

from core.ac.ast import FirstOrderNotSupported
from core.comp.oracle import AdfBddNotFound
from core.dc.debate_graph import DebateCompileError, DebateGraph, canonical_prop, declaration_kinds
from core.logging_util import artifact
from wrap.prover import ProverError

logger = logging.getLogger("fsp.wrapper")
_pipeline_logger = logging.getLogger("core.dc.issue")


# ---------------------------------------------------------------------------
# Errors
# ---------------------------------------------------------------------------

class AidaError(Exception):
    """A refusal or failure of a service call.  ``code`` is stable;
    ``stage`` names the step that refused (graph, evaluate, label, ...);
    ``diagnostics`` holds what was logged during the call."""

    code = "error"

    def __init__(self, message: str, *, stage: Optional[str] = None, cause: Optional[BaseException] = None):
        super().__init__(message)
        self.stage = stage
        self.cause = cause
        self.diagnostics: List["Diagnostic"] = []


class NotFound(AidaError):
    code = "not_found"


class InvalidRequest(AidaError):
    """The request itself is malformed: an unknown option, an issue that
    does not parse, a target of the wrong kind."""
    code = "invalid"


class Refused(AidaError):
    """The pipeline refused the request: the cause says why."""
    code = "refused"


class LogicRefused(Refused):
    code = "logic"


class UnfoldRefused(Refused):
    code = "unfold"


class TypeCheckRefused(Refused):
    code = "typecheck"


class CompileRefused(Refused):
    code = "compile"


class EvaluationFailed(Refused):
    code = "evaluation"


class TooDeep(Refused):
    """A term walk exceeded the recursion limit (aida-deep-term-recursion)."""
    code = "recursion"


class LabellerMissing(AidaError):
    """adf-bdd is not installed: there is no labelling."""
    code = "adf_bdd_missing"


class ContentRefused(Refused):
    """The document refused a declaration, argument, statement or debate
    move: a name clash, a failed replay, a strictness demand."""
    code = "content"


# ---------------------------------------------------------------------------
# Diagnostics
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class Diagnostic:
    level: str          # "warning" | "error" | "critical"
    logger: str
    message: str


_CAPTURED_LOGGERS = ("core", "pres", "wrap", "fsp")
_depth = threading.local()


class _Collector(logging.Handler):
    """Collects WARNING+ records logged in one thread."""

    def __init__(self, thread: int):
        super().__init__(level=logging.WARNING)
        self.thread = thread
        self.items: List[Diagnostic] = []

    def emit(self, record: logging.LogRecord) -> None:
        if record.thread == self.thread:
            self.items.append(Diagnostic(record.levelname.lower(), record.name, record.getMessage()))


def _operation(method):
    """Run a service method under the session lock, collecting its
    diagnostics onto the result (a dataclass with ``diagnostics``) or onto
    the AidaError it raises.  Nested operations share the outer collector."""

    @functools.wraps(method)
    def wrapper(self, *args, **kwargs):
        with self.session.lock:
            outer = getattr(_depth, "collector", None)
            if outer is not None:
                return method(self, *args, **kwargs)
            collector = _Collector(threading.get_ident())
            loggers = [logging.getLogger(name) for name in _CAPTURED_LOGGERS]
            for lg in loggers:
                lg.addHandler(collector)
            _depth.collector = collector
            try:
                result = method(self, *args, **kwargs)
            except AidaError as e:
                e.diagnostics = list(collector.items)
                raise
            finally:
                _depth.collector = None
                for lg in loggers:
                    lg.removeHandler(collector)
            if hasattr(result, "diagnostics"):
                result.diagnostics = list(collector.items)
            return result

    return wrapper


# ---------------------------------------------------------------------------
# Targets
# ---------------------------------------------------------------------------

@dataclass(frozen=True)
class Target:
    """What a query is about: a registered argument, a recorded debate, an
    issue (a statement: canonical key and side), or the document itself.
    ``text`` is how the user wrote it, used in messages."""

    kind: str                       # "argument" | "debate" | "issue" | "document"
    text: str
    issue: Optional[Tuple[str, str]] = None

    @property
    def name(self) -> str:
        return self.text


def _parse_issue_text(text: str) -> Tuple[str, str]:
    spec = text[len("issue "):].strip()
    if spec.startswith(":") and not spec.endswith(":"):
        prop, side = spec[1:].strip(), "term"
    elif spec.endswith(":") and not spec.startswith(":"):
        prop, side = spec[:-1].strip(), "context"
    else:
        raise InvalidRequest(f"An issue is written ':X' (X proved) or 'X:' (X refuted), not '{spec}'.")
    try:
        return (canonical_prop(prop), side)
    except Exception as e:
        raise InvalidRequest(f"Cannot read the proposition '{prop}': {e}", cause=e)


# ---------------------------------------------------------------------------
# Results
# ---------------------------------------------------------------------------

@dataclass
class IssueTerm:
    """The debate about a target: its issue, the unfolded term (if asked
    for, or needed for the type check) and the shared form (None on the
    unfolded route's term, for a debate, and for an argument the document
    refused - ``own_term`` then marks that the term is the argument's own)."""
    target: Target
    argument: Any
    debate: Any
    issue: Optional[Tuple[str, str]]
    term: Any
    shared: Any
    own_term: bool = False
    diagnostics: List[Diagnostic] = field(default_factory=list)


@dataclass
class GraphResult:
    target: Target
    argument: Any
    debate: Any
    graph: DebateGraph
    term: Any = None
    whole: bool = False                       # a debate's whole scope
    labels: Optional[Dict] = None             # the grounded overlay
    labels_unavailable: Optional[str] = None  # why there is none
    diagnostics: List[Diagnostic] = field(default_factory=list)


@dataclass
class LabelResult:
    target: Target
    graph: DebateGraph
    semantics: str
    labellings: List[Dict]
    diagnostics: List[Diagnostic] = field(default_factory=list)


@dataclass
class Evaluation:
    target: Target
    mode: str
    semantics: str
    base: str
    witness: Any
    favour: bool
    normal_form: Any
    nf_class: str
    sigma: Dict
    graph: DebateGraph
    diagnostics: List[Diagnostic] = field(default_factory=list)


@dataclass
class WitnessEvaluations:
    """``evaluate ... credulous all``: one evaluation per accepting witness
    labelling, numbered as `label` numbers them."""
    target: Target
    semantics: str
    base: str
    results: List[Tuple[int, Any, str, Dict]]     # (number, normal form, class, sigma)
    diagnostics: List[Diagnostic] = field(default_factory=list)


@dataclass
class TermResult:
    target: Target
    which: str
    description: str
    term: Any                                 # a ProofTerm, or text for "registered"
    revision: int
    diagnostics: List[Diagnostic] = field(default_factory=list)


@dataclass
class Rendering:
    target: Target
    which: Optional[str]
    style: Optional[str]
    description: str
    text: str
    diagnostics: List[Diagnostic] = field(default_factory=list)


@dataclass
class Shared:
    target: Target
    issue: Tuple[str, str]
    display: str                              # the issue as `Prop[t]`
    text: str
    named: Dict
    diagnostics: List[Diagnostic] = field(default_factory=list)


@dataclass
class Tree:
    target: Target
    dot: str
    labelled: bool
    graph_refused: Optional["AidaError"] = None   # the graph refusal, if any
    labels_unavailable: Optional[str] = None      # adf-bdd missing
    diagnostics: List[Diagnostic] = field(default_factory=list)


@dataclass
class ArgumentInfo:
    name: str
    conclusion: str
    anti: bool
    kind: str                                 # "argument" | "statement"
    strict: bool
    in_document: bool


@dataclass
class DebateInfo:
    name: str
    issue: str
    onus: str
    scope: str
    moves: List[str]
    finished: bool


@dataclass
class Inventory:
    logic: str
    revision: int
    declarations: Dict[str, Tuple[str, str]]  # name -> (kind, text)
    arguments: List[ArgumentInfo]
    debates: List[DebateInfo]
    names: Dict[str, str]
    diagnostics: List[Diagnostic] = field(default_factory=list)


@dataclass
class Done:
    """A content operation that succeeded; ``value`` is what it made (an
    Argument, a Debate, a Move, a Document), ``message`` says so."""
    message: str
    value: Any = None
    diagnostics: List[Diagnostic] = field(default_factory=list)


#: Which of an argument's terms `term`/`render`/`tree` can show
#: (aida-unfold-entrypoints).
TERM_SELECTORS = ("registered", "enriched", "unfolded", "normal", "evaluated")
RENDER_STYLES = ("argumentation", "dialectical", "intuitionistic", "vanilla", "pruefschema")
MODES = ("skeptical", "credulous")
SEMANTICS = ("grounded", "complete", "preferred", "stable")
BASES = ("cbn", "cbv")


def render_term(term, style: str, declarations=None, decorations=None) -> str:
    """A term in one of the natural-language styles (pres/nl.py)."""
    from pres.nl import (pretty_natural, natural_language_argumentative_rendering,
                         dialectical_rendering, natural_language_rendering,
                         pruefschema_rendering, vanilla_rendering)
    sem = {"argumentation": natural_language_argumentative_rendering,
           "dialectical": dialectical_rendering,
           "intuitionistic": natural_language_rendering,
           "vanilla": vanilla_rendering,
           "pruefschema": pruefschema_rendering}.get(style.strip().lower())
    if sem is None:
        raise InvalidRequest(f"Invalid render style '{style}' (expected: {', '.join(RENDER_STYLES)})")
    return pretty_natural(term, sem, declarations=declarations or {}, decorations=decorations or {})


# ---------------------------------------------------------------------------
# The service
# ---------------------------------------------------------------------------

class Service:
    def __init__(self, session):
        self.session = session

    @classmethod
    def of(cls, session) -> "Service":
        """The service of a session (one per session, kept on it)."""
        service = getattr(session, "_service", None)
        if service is None:
            service = cls(session)
            session._service = service
        return service

    # -- targets -----------------------------------------------------------

    def target(self, text: str) -> Target:
        """Read a target as the CLI writes it: ``document``, ``issue :X`` /
        ``issue X:``, a debate's name, or an argument's name."""
        text = text.strip()
        if text == "document":
            return Target("document", text)
        if text.startswith("issue "):
            return Target("issue", text, _parse_issue_text(text))
        if text in self.session.debates:
            return Target("debate", text)
        return Target("argument", text)

    def _as_target(self, target) -> Target:
        return target if isinstance(target, Target) else self.target(target)

    def _logic_refusal(self) -> Optional[str]:
        """Why the document's logic admits no debates, or None.  Debates are
        classical: their scaffolds throw to a second conclusion, which LJ
        forbids.  Minimal logic changes the negation proof terms (no ex
        falso, no `_F_`), on which the compiler and the unfolder are
        untested."""
        if self.session.logic == "lj":
            return ("debates are classical (their scaffolds throw to a second conclusion, "
                    "which LJ forbids); start the document with `new document.` (lk).")
        if self.session.minimal:
            return ("debates are not supported in minimal logic yet (its negation proof "
                    "terms are untested in the compiler); start the document with `new document.`")
        return None

    # -- documents and settings ----------------------------------------------

    @_operation
    def new_document(self, logic: str = "lk", minimal: bool = False) -> Done:
        try:
            doc = self.session.new_document(logic, minimal)
        except (ValueError, ProverError) as e:
            raise ContentRefused(str(e), stage="new document", cause=e)
        return Done(f"New document ({doc.logic_name}).", doc)

    @_operation
    def load(self, path: str) -> Done:
        """Replace the document with the file's (``load FILE``)."""
        from wrap.cli import execute_script
        execute_script(self.session, path, strict=False, stop_on_error=False, isolate=False,
                       new_document=True)
        return Done(f"Loaded {path}.", self.session.doc)

    @_operation
    def set_typecheck(self, mode: str) -> Done:
        """``on`` replays each sub-debate once; ``expanded`` replays the
        whole unfolded term, the older and far larger check; ``off`` skips
        it (core/dc/typecheck.py)."""
        if mode not in ("on", "off", "expanded"):
            raise InvalidRequest(f"typecheck: on, off or expanded, not '{mode}'")
        self.session.typecheck_enabled = mode != "off"
        self.session.typecheck_expanded = mode == "expanded"
        return Done({"on": "on", "off": "off", "expanded": "on (the expanded term)"}[mode])

    @_operation
    def set_pipeline(self, mode: str) -> Done:
        """``unfolded`` (the default) works on the term unfolded from the
        document graph; ``shared`` on the debate as named sub-debates."""
        if mode not in ("shared", "unfolded"):
            raise InvalidRequest(f"pipeline: shared or unfolded, not '{mode}'")
        self.session.pipeline_unfolded = mode == "unfolded"
        return Done("unfolded term" if self.session.pipeline_unfolded else "shared debate")

    @_operation
    def inventory(self) -> Inventory:
        """What the document holds (aida-cli-inventory-and-naming)."""
        s = self.session
        declarations = {name: (getattr(text, "kind", "") or "", str(text))
                        for name, text in s.declarations.items()}
        statements = set(s.graph.statements())
        arguments = [ArgumentInfo(name, arg.conclusion, bool(getattr(arg, "is_anti", False)),
                                  "statement" if s.names.get(name) == "statement" else "argument",
                                  bool(getattr(arg, "citable", False)),
                                  s.issue_of(arg) in statements)
                     for name, arg in s.arguments.items()]
        debates = [DebateInfo(d.name, d.issue_prop, d.onus, d.scope, [str(m) for m in d.moves],
                              d.finished)
                   for d in s.debates.values()]
        return Inventory(s.doc.logic_name, s.revision, declarations, arguments, debates, dict(s.names))

    # -- content -------------------------------------------------------------

    @_operation
    def command(self, text: str) -> Done:
        """A Fellowship command (``declare``, ``deny``, a logic toggle ...).
        A ``declare`` takes its names first, so a clash is refused before
        Fellowship sees it."""
        try:
            self.session.claim_declared_names(text)
            state = self.session.send_command(text)
        except ProverError as e:
            raise ContentRefused(str(e), stage="prover", cause=e)
        return Done(text.strip(), state)

    @_operation
    def register(self, name: str, conclusion: str, proof_term: str, *, strict: bool = False) -> Done:
        """Register an argument from its proof term; with ``strict``,
        Fellowship also holds it as a theorem (or refuses it)."""
        from core.ac.grammar import Grammar, ProofTermTransformer
        from core.ac.ast import Mutilde
        from core.dc.argument import Argument
        from core.dc.cite import mark_citations
        s = self.session
        proof_term = Argument._normalize_pt_to_unicode(proof_term)
        try:
            body = ProofTermTransformer().transform(Grammar().parser.parse(proof_term))
        except Exception as e:
            raise InvalidRequest(f"register: cannot read the proof term: {e}", cause=e)
        # A term typed as a string has lost its citation marks: a free leaf
        # naming a registered argument is a citation (core/dc/cite.py).
        mark_citations(body, lambda n: n in s.arguments and n != name)
        arg = Argument(s, name=name, conclusion=conclusion, is_anti=isinstance(body, Mutilde))
        arg.body = body
        try:
            # The name is checked before the replay: a strict one ends in
            # `qed`, which would otherwise let Fellowship silently replace a
            # clashing name.
            s.claim_name(name, "argument", dry_run=True)
            # Not `strict`: registered either way, and held by Fellowship if
            # it turns out closed - the same rule as `end argument`.
            arg.execute(declare=True if strict else "auto", preserve_input_body=True)
            s.register_argument(arg)
        except ProverError as e:
            raise ContentRefused(str(e), stage="register", cause=e)
        logger.info("Registered argument '%s' with conclusion '%s'%s.", arg.name, arg.conclusion,
                    " and declared it in Fellowship" if strict else "")
        return Done(f"Registered argument '{arg.name}' with conclusion '{arg.conclusion}'.", arg)

    @_operation
    def record_argument(self, name: str, conclusion: str, instructions: List[str], *,
                        anti: bool = False, demand_strict: bool = False) -> Done:
        """Record a whole argument at once: claim the name, replay the
        instructions, register it (``start argument ... end argument``; with
        ``demand_strict``, ``... qed``)."""
        from wrap.registry import start_recording, finish_recording, abandon
        current = {"name": name, "conclusion": conclusion, "instructions": list(instructions),
                   "is_anti": anti}
        try:
            start_recording(self.session, name, anti)
        except ProverError as e:
            raise ContentRefused(str(e), stage="record", cause=e)
        return self._finish(current, demand_strict)

    def _finish(self, current: dict, demand_strict: bool) -> Done:
        from wrap.registry import finish_recording, abandon
        try:
            arg = finish_recording(self.session, current, demand_strict=demand_strict)
        except ProverError as e:
            abandon(self.session, current)
            raise ContentRefused(str(e), stage="record", cause=e)
        verb = "proved" if demand_strict else "registered"
        return Done(f"'{arg.name}' {verb}: {arg.conclusion}.", arg)

    @_operation
    def finish_recording(self, current: dict, *, demand_strict: bool) -> Done:
        """Finish a recording the caller kept (the CLI's ``start argument``
        block, or a ``prove NAME`` refinement): replay and register it, or
        give its name back and raise."""
        return self._finish(current, demand_strict)

    @_operation
    def state(self, keyword: str, name: str, conclusion: str, *, anti: bool = False) -> Done:
        """State a claim (``theorem NAME : (P).`` and its kin): registered as
        an enthymeme that owes a proof."""
        from wrap.registry import state
        try:
            claim = state(self.session, anti, name, conclusion, keyword)
        except ProverError as e:
            raise ContentRefused(str(e), stage="state", cause=e)
        return Done(f"Stated {keyword} '{name}' : {conclusion}; `prove {name}` opens its proof.", claim)

    @_operation
    def prove(self, name: str, instructions: List[str], *, demand_strict: bool = True) -> Done:
        """Continue a statement or argument with more instructions and
        finish it (``prove NAME ... qed``), as one block."""
        from wrap.registry import reopen
        try:
            current = reopen(self.session, name)
        except ProverError as e:
            raise ContentRefused(str(e), stage="refine", cause=e)
        current["instructions"] = current["instructions"] + list(instructions)
        return self._finish(current, demand_strict)

    @_operation
    def decorate(self, name: str, template: str) -> Done:
        self.session.register_decoration(name, template)
        return Done(f"Decorated '{name}'.")

    @_operation
    def adopt(self, edge_name: str, name: str) -> Done:
        """Promote a strict edge a query showed (``graph`` or ``evaluate``)
        to a theorem Fellowship holds.  The closed term stored on the edge
        is replayed with ``qed``: Fellowship checks it again."""
        from core.dc.argument import Argument
        s = self.session
        seen = s.doc.strict_edges
        if edge_name not in seen:
            raise NotFound(f"no strict edge '{edge_name}' has been shown in this document; "
                           f"run `graph ARG` for the argument whose graph has it.", stage="adopt")
        issue_name, edge, conclusion = seen[edge_name]
        try:
            s.claim_name(name, "argument", dry_run=True)
            arg = Argument(s, name=name, conclusion=conclusion, is_anti=edge.target_side == "context")
            arg.body = copy.deepcopy(edge.term)
            arg.execute(declare=True, preserve_input_body=True)
            s.register_argument(arg)
        except ProverError as e:
            logger.warning("Adopting '%s' as '%s' refused: %s", edge_name, name, e)
            raise ContentRefused(str(e), stage="adopt", cause=e)
        logger.info("Adopted '%s' (from the issue graph of '%s') as the theorem '%s' : %s.",
                    edge_name, issue_name, name, conclusion)
        return Done(f"Adopted as '{name}' : {conclusion}; `axiom {name}` cites it.", arg)

    # -- debates -------------------------------------------------------------

    @_operation
    def start_debate(self, name: str, issue: str, onus: str = "pro", scope: str = "closed") -> Done:
        from core.dc.debate import Debate, DebateError
        s = self.session
        if s.recording_debate is not None:
            raise ContentRefused(f"debate '{s.recording_debate}' is still being recorded; "
                                 f"close it with `hora est.` first.", stage="debate")
        try:
            debate = Debate(name, issue, onus, scope)
            debate.issue                       # refuse an unreadable issue before claiming the name
            s.register_debate(debate)
        except (DebateError, ValueError) as e:
            raise InvalidRequest(str(e), stage="debate", cause=e)
        except ProverError as e:
            raise ContentRefused(str(e), stage="debate", cause=e)
        s.recording_debate = name
        logger.info("Recording debate '%s' (%s, %s scope) about %s.", name, onus, scope,
                    f":{issue}" if onus == "pro" else f"{issue}:")
        return Done(f"Recording debate '{name}'.", debate)

    @_operation
    def move(self, argument: str, verb: Optional[str] = None, target: Optional[str] = None) -> Done:
        """A move of the debate being recorded: ``argument`` joins its
        scope; with ``verb``, checked against ``target``."""
        from dataclasses import replace
        from core.dc.debate import Move, DebateError, VERBS, check_opening, check_move
        s = self.session
        name = s.recording_debate
        if name is None:
            raise ContentRefused(f"`{argument}` is a debate move, but no debate is being recorded.",
                                 stage="debate")
        if verb is not None and verb not in VERBS:
            raise InvalidRequest(f"unknown verb '{verb}' (one of {', '.join(VERBS)})", stage="debate")
        move = Move(argument, verb, target)
        debate = s.debates[name]
        graph = s.debate_graph(replace(debate, moves=debate.moves + [move], _term=None))

        def statement_of(argument_name):
            arg = s.arguments.get(argument_name)
            if arg is None or getattr(arg, "composed", False):
                return None
            return s.issue_of(arg)

        try:
            if not debate.moves:
                if verb is not None or target is not None:
                    raise DebateError(f"debate '{name}': the opening move is an argument alone: "
                                      f"`{argument}.`")
                check_opening(debate, graph, argument, statement_of)
            else:
                check_move(debate, graph, move, statement_of)
        except DebateError as e:
            raise ContentRefused(str(e), stage="debate", cause=e)
        debate.moves.append(move)
        logger.info("Debate '%s', move %d: %s", name, len(debate.moves), move)
        return Done(f"Debate '{name}', move {len(debate.moves)}: {move}", move)

    @_operation
    def close_debate(self) -> Done:
        s = self.session
        name = s.recording_debate
        if name is None:
            raise ContentRefused("hora est: no debate is being recorded.", stage="debate")
        debate = s.debates[name]
        if not debate.moves:
            raise ContentRefused(f"debate '{name}' has no move yet; open it with an argument.",
                                 stage="debate")
        debate.finished = True
        s.recording_debate = None
        logger.info("Debate '%s' recorded: %d move(s).", name, len(debate.moves))
        return Done(f"Debate '{name}' recorded: {len(debate.moves)} move(s).", debate)

    # -- the issue of a target -----------------------------------------------

    @_operation
    def issue_term(self, target, *, want_term: bool = True) -> IssueTerm:
        """The debate about ``target``: its issue, its shared form and, if
        ``want_term`` or the type check needs it, the term unfolded from the
        document graph - the canonical shape with the argument on top of its
        supporter stack (aida-unfold-entrypoints), cached until the document
        changes.  An argument the document refused at registration falls
        back to its own term (``own_term``), with a warning.  Every unfolded
        term is type-checked (the type oracle), unless `typecheck off`."""
        from core.dc.unfold import unfold_legacy, UnfoldError
        s = self.session
        target = self._as_target(target)
        name = target.text
        if target.kind == "document":
            raise InvalidRequest("'document' has no issue; name an argument, a debate or an issue.")
        if target.kind == "debate":
            return self._debate_issue(target, s.debates[name])
        if target.kind == "issue":
            arg, issue = None, target.issue
        else:
            arg = s.get_argument(name)
            if not arg:
                raise NotFound(f"Argument '{name}' not found.")
            if not arg.executed:
                arg.execute()
            issue = s.issue_of(arg)
        document = s.graph
        refusal = self._logic_refusal()
        if refusal:
            logger.warning("Debate commands refused in %s for '%s'.", s.doc.logic_name, name)
            raise LogicRefused(refusal, stage="graph")
        if _pipeline_logger.isEnabledFor(logging.DEBUG) and arg is not None:
            from pres.gen import pres_tree
            _pipeline_logger.debug("issue: '%s' is about %s; the document has %d edge(s)",
                                   name, f"{document.nodes.get(issue[0], arg.conclusion)}[{issue[1][0]}]",
                                   len(document.edges))
            artifact(_pipeline_logger, "issue: the term '%s' was registered with (before unfolding)" % name,
                     pres_tree(arg.body))
        if arg is not None and issue not in set(document.statements()):
            logger.warning("'%s' is not in the document graph; using its own term.", name)
            _pipeline_logger.debug("issue: NOT unfolded and NOT type-checked - the issue is not in "
                                   "the document, so '%s' is evaluated as it was registered", name)
            return IssueTerm(target, arg, None, issue, arg.body, None, own_term=True)
        try:
            if s.pipeline_unfolded:
                # Built for the per-definition type check only; it is the
                # legacy shape (aida-shared-route-stack-shape), not the term
                # evaluated, so it must not narrate itself as the unfolding.
                unfold_log = logging.getLogger("core.dc.unfold")
                was_disabled, unfold_log.disabled = unfold_log.disabled, True
                try:
                    shared = s.shared_debate(issue)
                finally:
                    unfold_log.disabled = was_disabled
            else:
                shared = s.shared_debate(issue)
        except UnfoldError as e:
            logger.warning("Unfolding refused for '%s': %s", name, e)
            raise UnfoldRefused(str(e), stage="graph", cause=e)
        if _pipeline_logger.isEnabledFor(logging.DEBUG) and not s.pipeline_unfolded:
            artifact(_pipeline_logger, "issue: the debate as named sub-debates", shared.to_text(tree=True))
        # A statement spelled two ways (~A and A -> false) joins two
        # spellings only in the expanded term, so that is the one to
        # type-check then (tasks.org, aida-negation-spelling-in-unfolding).
        clashes = shared.spelling_clashes() if s.typecheck_enabled else {}
        expanded_check = s.typecheck_enabled and (s.typecheck_expanded or bool(clashes))
        term = None
        if want_term or expanded_check:
            try:
                if want_term:
                    term = s.unfolded_term(arg) if arg is not None else None
                    if term is None:            # an issue, or an argument without an edge
                        term = s.issue_term(issue)
                else:
                    term = unfold_legacy(document, issue)    # the shared route's reference
            except UnfoldError as e:
                logger.warning("Unfolding refused for '%s': %s", name, e)
                raise UnfoldRefused(str(e), stage="graph", cause=e)
        if s.typecheck_enabled:
            # The type oracle: the debate must replay through Fellowship
            # (core/dc/typecheck.py).  `typecheck off` skips it.
            from core.dc.typecheck import typecheck, typecheck_shared, TypeCheckFailed
            # Fellowship names the replayed theorems after the target, and an
            # issue target ("issue B:") is no identifier.
            check_name = re.sub(r"\W+", "_", name).strip("_") if arg is None else name
            try:
                if clashes:
                    _pipeline_logger.debug(
                        "typecheck: the expanded term is replayed, since the debate spells %s "
                        "in more than one way", ", ".join(sorted(clashes)))
                if expanded_check:
                    typecheck(s, term, check_name, document.nodes.get(issue[0], issue[0]),
                              issue[1] == "context")
                else:
                    # One replay per sub-debate instead of one of the whole
                    # unfolded term (tasks.org, aida-shared-subarguments, stage 2).
                    typecheck_shared(s, shared, check_name, s.typechecked())
            except TypeCheckFailed as e:
                logger.warning("Type check failed for '%s': %s", name, e)
                raise TypeCheckRefused(str(e), stage="graph", cause=e)
        return IssueTerm(target, arg, None, issue, term, shared)

    def _debate_issue(self, target: Target, debate) -> IssueTerm:
        """``issue_term`` for a debate (core/dc/debate.py): the term compiled
        from the debate's scope and moves - as recorded so far, while it is
        being recorded - and type-checked by replaying it whole.  A debate
        has no shared form."""
        from core.dc.unfold import UnfoldError
        s = self.session
        refusal = self._logic_refusal()
        if refusal:
            raise LogicRefused(refusal, stage="graph")
        if not debate.moves:
            raise Refused(f"debate '{debate.name}' has no move yet.", stage="graph")
        if _pipeline_logger.isEnabledFor(logging.DEBUG):
            _pipeline_logger.debug("issue: debate '%s' is about %s[%s]; %s scope, %d move(s)%s",
                                   debate.name, debate.issue_prop, debate.issue[1][0], debate.scope,
                                   len(debate.moves), "" if debate.finished else ", still being recorded")
            artifact(_pipeline_logger, "issue: the debate '%s' was registered with (before unfolding)"
                     % debate.name, "\n".join([debate.header()] + [str(m) for m in debate.moves]))
        try:
            term = s.debate_term(debate)
        except UnfoldError as e:
            logger.warning("Unfolding refused for debate '%s': %s", debate.name, e)
            raise UnfoldRefused(str(e), stage="graph", cause=e)
        if s.typecheck_enabled:
            from core.dc.typecheck import typecheck, TypeCheckFailed
            try:
                typecheck(s, term, debate.name, debate.issue_prop, debate.onus == "con")
            except TypeCheckFailed as e:
                logger.warning("Type check failed for debate '%s': %s", debate.name, e)
                raise TypeCheckRefused(str(e), stage="graph", cause=e)
        return IssueTerm(target, None, debate, debate.issue, term, None)

    # -- graphs and labels ---------------------------------------------------

    def _remember_strict_edges(self, graph, issue_name: str) -> None:
        """Keep the strict edges an issue graph showed, by name, so `adopt`
        can promote one the user has seen.  Scoped to the document."""
        seen = self.session.doc.strict_edges
        for edge in graph.edges:
            if edge.strict and edge.name.endswith("*") and getattr(edge, "term", None) is not None:
                seen[edge.name] = (issue_name, edge, graph.nodes.get(edge.target_key, edge.target_key))

    @staticmethod
    def _report_onus_conflicts(graph) -> None:
        """Warn about propositions presumed on BOTH sides: both delegate the
        burden of refutation, so neither holds it (aida-onus-delegation-polarity)."""
        from core.comp.adf_label import opposing_presumptions
        clash = opposing_presumptions(graph)
        if clash:
            names = ", ".join(graph.nodes.get(key, key) for key in clash)
            logger.warning("Opposing presumptions on %s: both sides delegate the onus "
                           "of refutation, so neither side holds it.", names)

    @_operation
    def graph(self, target, *, whole: bool = False, labels: bool = False) -> GraphResult:
        """The debate graph of ``target``: the issue's, compiled from the
        shared debate (one instance of a sub-debate at a time) or from the
        unfolded term; ``document``: the document graph itself; with
        ``whole``, a debate's whole scope.  ``labels`` adds the grounded
        overlay (None, with the reason, if adf-bdd is missing)."""
        from core.dc.instances import compile_issue_shared
        from core.dc.strict import compile_issue
        from core.dc.unfold import unfold_legacy
        s = self.session
        target = self._as_target(target)
        name = target.text
        if whole:
            if target.kind != "debate":
                raise InvalidRequest(f"graph: 'all' is for debates; '{name}' is not one.")
            debate = s.debates[name]
            result = GraphResult(target, None, debate, s.debate_graph(debate), whole=True)
        elif target.kind == "document":
            self._report_onus_conflicts(s.graph)
            result = GraphResult(target, None, None, s.graph)
        else:
            found = self.issue_term(target, want_term=s.pipeline_unfolded)
            term, shared = found.term, found.shared
            if s.pipeline_unfolded:
                shared = None                      # `pipeline unfolded`: the reference path
            options = dict(strict_names=s.declarations.keys(),
                           strict_kinds=declaration_kinds(s.declarations))
            try:
                graph = None
                if shared is not None:
                    try:
                        graph = compile_issue_shared(shared, name, **options)
                    except (DebateCompileError, FirstOrderNotSupported, RecursionError):
                        raise                      # the unfolded term is deeper still
                    except Exception as e:
                        # The instance-wise compiler is checked against the
                        # unfolded term on every fixture; should it ever fail,
                        # say so and use the reference rather than refuse a
                        # sound debate.
                        logger.warning("Compiling '%s' from its shared debate failed (%s: %s); "
                                       "compiling the unfolded term instead.", name, type(e).__name__, e)
                        term = term if term is not None else unfold_legacy(s.graph, found.issue)
                if graph is None:
                    graph = compile_issue(term, name, **options)
            except (DebateCompileError, FirstOrderNotSupported) as e:
                logger.warning("Debate graph compilation refused for '%s': %s", name, e)
                raise CompileRefused(str(e), stage="graph", cause=e)
            except RecursionError as e:
                # tasks.org, aida-deep-term-recursion: the term walks recurse.
                logger.warning("Debate graph compilation refused for '%s': recursion depth exceeded.", name)
                raise TooDeep(f"the debate about '{name}' is nested too deeply for the compiler, "
                              f"which follows a chain of sub-debates by recursion.", stage="graph", cause=e)
            self._report_onus_conflicts(graph)
            self._remember_strict_edges(graph, name)
            result = GraphResult(target, found.argument, found.debate, graph, term)
        if labels:
            from core.comp.adf_label import grounded_labels
            try:
                result.labels = grounded_labels(result.graph)
            except AdfBddNotFound as e:
                # The graph needs no labeller; labels are an overlay.  But
                # say loudly why they are missing - there is no fallback.
                logger.warning("Labels unavailable for the graph view: %s", e)
                result.labels_unavailable = str(e)
        return result

    @_operation
    def label(self, target, semantics: str = "grounded") -> LabelResult:
        """The ADF labellings of ``target``'s debate graph: the one grounded
        labelling, or every labelling of the semantics."""
        from core.comp.adf_label import labellings
        if semantics not in SEMANTICS:
            raise InvalidRequest(f"unknown semantics '{semantics}' (one of {', '.join(SEMANTICS)})")
        target = self._as_target(target)
        graph = self.graph(target).graph
        try:
            found = labellings(graph, semantics)
        except AdfBddNotFound as e:
            logger.error("Labelling refused for '%s': %s", target.text, e)
            raise LabellerMissing(str(e), stage="label", cause=e)
        return LabelResult(target, graph, semantics, found)

    # -- evaluation ----------------------------------------------------------

    def _remember_evaluation(self, holder, nf) -> None:
        """Cache an evaluated normal form on its argument or debate, with the
        revision it belongs to (an issue has nothing to cache it on)."""
        if holder is not None:
            holder.labelled_nf = nf
            holder.labelled_nf_revision = self.session.revision

    @_operation
    def evaluate(self, target, *, mode: str = "skeptical", semantics: str = "preferred",
                 base: str = "cbn", witness=None, favour: bool = False):
        """Label-guided evaluation of ``target``'s debate term (an
        ``Evaluation``; a ``WitnessEvaluations`` for ``witness="all"``).

        The mode ranges over the semantics; the base strategy resolves only
        the critical pairs the witness labelling leaves open.  In credulous
        mode ``witness`` N picks labelling N of `label`, ``"all"`` evaluates
        under every labelling that accepts the issue; ``favour`` (credulous,
        an argument, the unfolded route) prefers a witness in which the
        argument's own derivation is IN.  The normal form is cached on the
        argument or debate (`render NAME evaluated`)."""
        from core.comp.evaluate import (evaluate_debate, evaluate_witnesses, evaluate_shared,
                                        evaluate_witnesses_shared, EvaluationRefused)
        from core.dc.unfold import unfold_legacy, argument_edge
        s = self.session
        if mode not in MODES or semantics not in SEMANTICS or base not in BASES:
            raise InvalidRequest(f"evaluate: mode {MODES}, semantics {SEMANTICS}, base {BASES}")
        if witness is not None and mode != "credulous":
            raise InvalidRequest("evaluate: a witness number or 'all' requires credulous mode")
        target = self._as_target(target)
        name = target.text
        if target.kind == "document":
            raise InvalidRequest("evaluate needs an argument or debate name; 'document' has no issue.")
        found = self.issue_term(target, want_term=s.pipeline_unfolded)
        arg, issue, term, shared = found.argument, found.issue, found.term, found.shared
        holder = arg if arg is not None else found.debate
        if s.pipeline_unfolded:
            shared = None                      # `pipeline unfolded`: the reference path
        favoured = None
        if favour:
            favoured = argument_edge(s.graph, arg.name) if arg is not None else None
            if mode != "credulous" or witness is not None or shared is not None or favoured is None:
                raise InvalidRequest("'favour' needs credulous mode without a witness number, an "
                                     "argument with its own edge in the document, and the unfolded "
                                     "pipeline.", stage="evaluate")
        common = dict(strict_names=s.declarations.keys(),
                      strict_kinds=declaration_kinds(s.declarations),
                      base=base, semantics=semantics)

        def run(on_shared, on_term, **options):
            """Evaluate from the shared debate, one instance of a sub-debate
            at a time (core/dc/instances.py); should that route ever fail
            other than by a refusal, say so and evaluate the unfolded term,
            the reference it is tested against."""
            nonlocal term
            if shared is not None:
                try:
                    return on_shared(shared, name, **options)
                except (EvaluationRefused, DebateCompileError, FirstOrderNotSupported,
                        AdfBddNotFound, RecursionError):
                    raise
                except Exception as e:
                    logger.warning("Evaluating '%s' from its shared debate failed (%s: %s); "
                                   "evaluating the unfolded term instead.", name, type(e).__name__, e)
            if term is None:
                term = unfold_legacy(s.graph, issue)   # the shared route's reference
            return on_term(term, name, **options)

        try:
            if witness == "all":
                results, _ = run(evaluate_witnesses_shared, evaluate_witnesses, **common)
                for _number, nf, _cls, _sigma in results:
                    self._remember_evaluation(holder, nf)
                return WitnessEvaluations(target, semantics, base, list(results))
            options = dict(mode=mode, witness=witness, **common)
            if favoured is not None:
                options["favour"] = favoured
            nf, nf_class, sigma, graph = run(evaluate_shared, evaluate_debate, **options)
            self._remember_strict_edges(graph, name)
        except AdfBddNotFound as e:
            logger.warning("Evaluation refused for '%s': %s", name, e)
            raise LabellerMissing(str(e), stage="evaluate", cause=e)
        except (EvaluationRefused, DebateCompileError, FirstOrderNotSupported) as e:
            logger.warning("Evaluation refused for '%s': %s", name, e)
            raise EvaluationFailed(str(e), stage="evaluate", cause=e)
        except RecursionError as e:
            # tasks.org, aida-deep-term-recursion: the term walks recurse.
            logger.warning("Evaluation refused for '%s': recursion depth exceeded.", name)
            raise TooDeep(f"the debate about '{name}' is nested too deeply for the evaluator, "
                          f"which follows a chain of sub-debates by recursion.", stage="evaluate", cause=e)
        self._remember_evaluation(holder, nf)
        return Evaluation(target, mode, semantics, base, witness, favour, nf, nf_class, sigma, graph)

    # -- terms ---------------------------------------------------------------

    @_operation
    def term(self, target, which: Optional[str] = None) -> TermResult:
        """One of ``target``'s terms: ``registered`` (the term Fellowship
        returned, text only), ``enriched`` (the parsed, annotated body),
        ``unfolded`` (the debate term unfolded for it - the default),
        ``normal`` (its plain normal form), ``evaluated`` (the normal form of
        its last evaluation, refused if the document changed since).  An
        issue has only its unfolded term; a debate its term and its
        evaluation."""
        s = self.session
        target = self._as_target(target)
        name = target.text
        if which is not None and which not in TERM_SELECTORS:
            raise InvalidRequest(f"unknown term '{which}' (one of {', '.join(TERM_SELECTORS)})")
        if target.kind == "debate":
            debate = s.debates[name]
            if which == "evaluated":
                if debate.labelled_nf is None or debate.labelled_nf_revision != s.revision:
                    raise NotFound(f"Debate '{name}' has no evaluation at the document's current "
                                   f"revision; run `evaluate {name}` first.")
                return TermResult(target, which, "the normal form of the last evaluation",
                                  debate.labelled_nf, s.revision)
            if which not in (None, "unfolded"):
                raise InvalidRequest(f"A debate has only an unfolded and an evaluated term, not '{which}'.")
            found = self._debate_issue(target, debate)
            return TermResult(target, "unfolded", f"the term of debate '{name}'", found.term, s.revision)
        if target.kind == "issue":
            if which not in (None, "unfolded"):
                raise InvalidRequest(f"An issue has only an unfolded term, not '{which}'.")
            return TermResult(target, "unfolded", f"the canonical debate term of {name}",
                              s.issue_term(target.issue), s.revision)
        if target.kind == "document":
            raise InvalidRequest("'document' has no term; name an argument, a debate or an issue.")
        arg = s.get_argument(name)
        if not arg:
            raise NotFound(f"Argument '{name}' not found.")
        if not arg.executed:
            arg.execute()
        if which == "registered":
            return TermResult(target, which, "the term Fellowship returned", arg.proof_term, s.revision)
        if which == "enriched":
            return TermResult(target, which, "the enriched term", arg.body, s.revision)
        if which == "normal":
            if arg.normal_body is None:
                arg.normalize()
            return TermResult(target, which, "the normal form", arg.normal_body, s.revision)
        if which == "evaluated":
            if arg.labelled_nf is None or arg.labelled_nf_revision != s.revision:
                raise NotFound(f"'{name}' has no evaluation at the document's current revision; "
                               f"run `evaluate {name}` first.")
            return TermResult(target, which, "the normal form of the last evaluation",
                              arg.labelled_nf, s.revision)
        term = s.unfolded_term(arg)
        if term is None:
            return TermResult(target, "unfolded",
                              f"the canonical term of '{name}''s issue ('{name}' has no edge of its own)",
                              s.issue_term(s.issue_of(arg)), s.revision)
        return TermResult(target, "unfolded", f"the debate term unfolded for '{name}'", term, s.revision)

    @_operation
    def unfold(self, target) -> TermResult:
        """The debate term of an argument, issue or debate, kept until the
        document changes; nothing is added to the document."""
        refusal = self._logic_refusal()
        if refusal:
            raise LogicRefused(refusal, stage="unfold")
        return self.term(target, "unfolded")

    @_operation
    def render(self, target, which: Optional[str] = None, style: Optional[str] = None) -> Rendering:
        """A term of ``target`` as text: the term notation (``style`` None) or
        a natural-language style (RENDER_STYLES).  Without ``which`` an
        argument renders its registered body; anything else its unfolded
        term."""
        from pres.gen import pres_str, pres_tree
        s = self.session
        target = self._as_target(target)
        if which is None and target.kind == "argument":
            arg = s.get_argument(target.text)
            if not arg:
                raise NotFound(f"Argument '{target.text}' not found.")
            if not arg.executed:
                arg.execute()
            text = (arg.render(normalized=False) if style is None
                    else render_term(arg.body, style, s.declarations, s.decorations))
            return Rendering(target, None, style, f"argument {arg.name}", text)
        found = self.term(target, which or "unfolded")
        term = found.term
        if isinstance(term, str):
            text = term
        elif style is None:
            text = pres_str(term) + "\n" + pres_tree(term)
        else:
            text = render_term(term, style, s.declarations, s.decorations)
        return Rendering(target, found.which, style, found.description, text)

    @_operation
    def share(self, name: str) -> Shared:
        """The debate about an argument's issue as named sub-debates: the
        issue's term, then one `NAME[open sites] := term` line per sub-debate
        it cites (core/dc/share.py)."""
        from core.dc.unfold import UnfoldError
        s = self.session
        arg = s.get_argument(name)
        if arg is None:
            raise NotFound(f"debate: no argument '{name}'.")
        if not arg.executed:
            arg.execute()
        issue = s.issue_of(arg)
        document = s.graph
        target = Target("argument", name)
        if issue not in set(document.statements()):
            raise Refused(f"'{name}' is not in the document graph; it has no debate to share.",
                          stage="debate")
        try:
            shared = s.shared_debate(issue)
            text = shared.to_text()
        except UnfoldError as e:
            logger.warning("Sharing refused for '%s': %s", name, e)
            raise UnfoldRefused(str(e), stage="debate", cause=e)
        display = f"{document.nodes.get(issue[0], arg.conclusion)}[{issue[1][0]}]"
        return Shared(target, issue, display, text, shared.named())

    @_operation
    def tree(self, target, *, mode: str = "pt", nl_style: str = "argumentation",
             which: Optional[str] = None) -> Tree:
        """The acceptance tree of ``target`` as DOT, coloured by the grounded
        labels of its debate graph (uncoloured, with a warning, if the graph
        is refused or adf-bdd is missing).  Draws the argument's normal form,
        or the term ``which`` selects (a debate: its term)."""
        from core.comp.adf_label import grounded_labels
        from pres.tree import render_acceptance_tree_dot
        s = self.session
        target = self._as_target(target)
        name = target.text
        arg = s.get_argument(name) if target.kind == "argument" else None
        if target.kind == "argument" and arg is None:
            raise NotFound(f"Argument '{name}' not found.")
        if target.kind in ("debate", "issue") and which is None:
            which = "unfolded"              # no term of its own to normalise
        labels = refused = unavailable = None
        try:
            graph = self.graph(target).graph
        except NotFound:
            raise
        except AidaError as e:
            if target.kind != "argument":
                raise
            logger.warning("Tree for '%s' drawn without labels: debate graph refused.", name)
            graph, refused = None, e
        if graph is not None:
            try:
                labels = grounded_labels(graph)
            except AdfBddNotFound as e:
                logger.warning("Tree for '%s' drawn without labels: %s", name, e)
                unavailable = str(e)
        if which is not None:
            found = self.term(target, which)
            if isinstance(found.term, str):
                raise InvalidRequest("tree: the registered term is text only; choose another term.")
            drawn = found.term
        else:
            if arg.normal_body is None:
                arg.normalize()
            drawn = arg.normal_body
        try:
            dot = render_acceptance_tree_dot(
                drawn, verbose=False, label_mode="proof" if mode != "nl" else "nl",
                nl_style=nl_style, declarations=s.declarations, decorations=s.decorations,
                labels=labels)
        except Exception as e:
            logger.error("Failed to build acceptance tree for '%s': %s", name, e)
            raise Refused(f"Failed to build acceptance tree for '{name}': {e}", stage="tree", cause=e)
        return Tree(target, dot, labels is not None, refused, unavailable)
