import os, re, logging, threading, warnings
from typing import Any, List, Tuple, Optional, Dict
import pexpect
from pexpect.exceptions import EOF as PexpectEOF, TIMEOUT as PexpectTIMEOUT
from .sexp_parser import SexpParser
from core.ac.signature import Declaration
from wrap.document import Document
from core.dc.debate_graph import (
    DebateGraph, DebateCompileError, compile_debate, declaration_kinds, canonical_prop,
    SYNTHETIC_PREFIX,
)
from core.dc.share import ANON_PREFIX, SharedDebate, share
from core.ac.ast import FirstOrderNotSupported

logger = logging.getLogger('fsp.wrapper')

class ProverError(Exception):
    pass

class MachinePayloadError(ProverError):
    pass


class NameClash(ProverError):
    """A name is already taken in this document (tasks.org,
    aida-statements-and-witnesses).  Refused before the prover sees it."""


class CitationRefused(ProverError):
    """`axiom NAME` cited a registered argument that does not fit the goal:
    the wrong side or the wrong proposition."""


class StrictnessRefused(ProverError):
    """`qed` demanded a strict witness and the witness still has open
    obligations or presumptions.  Nothing reaches Fellowship's theorems."""


#: Prefixes of the names the wrapper generates itself: the ones it sends to
#: Fellowship (the type oracle's replay, the theta-expansion names) and the
#: ones it gives to sub-debates nobody named (core/dc/share.py).  A user name
#: with one of them could collide with a generated one, so it is refused.
RESERVED_PREFIXES = ("typecheck_", SYNTHETIC_PREFIX, ANON_PREFIX)

#: What may refine what: a recording becomes the argument it records, a
#: statement's enthymeme becomes its witness.  Anything else is a clash.
_REFINABLE = {"recording", "statement"}

MACHINE_BLOCK_RE = re.compile(r";;BEGIN_ML_DATA;;(.*?);;END_ML_DATA;;", re.S)

#: The end of one reply in machine mode: the machine block's end, then the
#: prompt.  Waiting for both, not for the prompt alone, keeps a stray prompt
#: (seen once in a while on the first exchange over pipes) from shifting
#: every later reply by one command.
REPLY_END = re.compile(r";;END_ML_DATA;;\s*fsp <")

#: Fellowship's logic toggles: valid only before a document's first
#: instruction.
LOGIC_TOGGLES = ("lk", "lj", "minimal", "full")
_LOGIC_REPLY = re.compile(r"Current logic:\s*(minimal\s+)?\s*(intuitionistic|classic)")
_TOO_LATE = "Only the first instructions can be used to specify a logical setting"


def _spawn(prover_cmd: str, env: dict):
    """The Fellowship process.  Pipes by default: a pty limits a line to
    1024 bytes in canonical mode (macOS), so a long command used to hang
    until the timeout (aida-transport-limits).  ACDC_TRANSPORT=pty keeps the
    old pty; ACDC_TIMEOUT (seconds, default 5) bounds one command."""
    timeout = float(os.getenv("ACDC_TIMEOUT", "5"))
    if os.getenv("ACDC_TRANSPORT", "pipe").lower() == "pty":
        return pexpect.spawn(prover_cmd, encoding='utf-8', timeout=timeout, env=env)
    from pexpect.popen_spawn import PopenSpawn
    return PopenSpawn(prover_cmd, encoding='utf-8', timeout=timeout, env=env)


def _env_flag(name: str, default: str, true_values=("1", "true", "yes", "on")) -> bool:
    return os.getenv(name, default).strip().lower() in true_values

class ProverWrapper:
    """
    The main class for the argumentation layer on top of the Fellowship prover. Includes utilities to execute an instance of the fellowship prover (self.prover, self.prover.expect), send commands to it and receive and process its output (send_command, self._sexp). Maintains a state in the form of Dicts of registered constant declarations and arguments (self.declarations, self.arguments) and parsed prover ouput (self.last_state).

TODO: Unified exception handling and logging.
TODO: Consistent use of type annotations.
TODO: Mechanism to declare a scenario of default assumptions.
    """
    def __init__(self, prover_cmd, env: Optional[dict] = None, log_level: Optional[int] = None):
        if log_level is not None:
            logger.setLevel(log_level)

        env_used = (env or os.environ).copy()
        env_used.setdefault("FSP_MACHINE", "1")

        self.prover = _spawn(prover_cmd, env_used)
        self.prover.expect(REPLY_END)
        # Pexpect sleeps for 50 ms before every send by default.  Fellowship is
        # already at a stable prompt here and does not perform password-style
        # terminal echo negotiation, so that defensive delay only adds linear
        # latency to proof replay.
        self.prover.delaybeforesend = None
        self.last_state: Any = None
        self._sexp = SexpParser()
        #: One Fellowship process answers one command at a time.  Every send
        #: holds the lock; a compound operation (a replay, a new document, a
        #: service call) holds it around all its sends - it is re-entrant.
        self.lock = threading.RLock()
        # Session settings: read from the environment once, here, and kept
        # across documents.
        self.echo_notes = os.getenv("FSP_ECHO_NOTES", "1").lower() not in {"0", "false", "no"}
        # Whether `graph ... show` and `tree` may write image/DOT files and open
        # a viewer.  ACDC_NO_RENDER=1 turns both off for a whole session (test
        # runs, headless CI); ACDC_NO_OPEN is the narrower "write but do not
        # open".  execute_script can override it per script.
        self.render_files = os.getenv("ACDC_NO_RENDER", "").lower() in {"", "0", "false", "no"}
        #: Whether unfolded terms are replayed through Fellowship before use
        #: (default on; FSP_TYPECHECK=0 or `typecheck off` disables).
        self.typecheck_enabled = os.getenv("FSP_TYPECHECK", "1") != "0"
        #: Whether the type check replays the whole unfolded term instead of
        #: one definition at a time (FSP_TYPECHECK=expanded or `typecheck
        #: expanded`).  The expanded replay is the older, exponentially larger
        #: check, kept as the reference the per-definition check is tested
        #: against (core/dc/typecheck.py).
        self.typecheck_expanded = os.getenv("FSP_TYPECHECK", "1") == "expanded"
        #: Whether graph, label and evaluate work on the term unfolded from
        #: the document graph instead of on the shared debate.  Default on
        #: since 2026-10-06: only the unfolded route builds the stacked shape
        #: and the argument entrypoints (aida-unfold-entrypoints).
        #: FSP_PIPELINE=shared or `pipeline shared` selects the shared route.
        self.pipeline_unfolded = os.getenv("FSP_PIPELINE", "unfolded") == "unfolded"
        #: The current document (wrap/document.py).  Fellowship starts in a
        #: fresh theory; ``new_document`` sets its logic (setup_prover).
        self.doc = Document()

    # -- the document --------------------------------------------------------

    @property
    def arguments(self) -> Dict[str, Any]:
        return self.doc.arguments

    @property
    def declarations(self) -> Dict[str, Declaration]:
        return self.doc.declarations

    @property
    def decorations(self) -> Dict[str, str]:
        return self.doc.decorations

    def new_document(self, logic: str = "lk", minimal: bool = False) -> Document:
        """Start a new document: reset Fellowship (``discard all.``), set the
        logic - both flags, since Fellowship keeps them across the reset -
        and replace the document.  Session settings survive.  Raises
        ProverError if Fellowship does not confirm the logic."""
        with self.lock:
            old = self.doc
            if old.recording_debate is not None:
                logger.warning("Debate '%s' was still being recorded; the new document "
                               "discards it.", old.recording_debate)
            # The new document is installed first, at its head, so that the
            # toggles below pass the head check and set its logic.
            self.doc = Document(logic, minimal, revision=old.revision + 1)
            # Fellowship's own notes ("Prover reset.", "Current logic: ...")
            # would repeat what the caller reports once.
            echo, self.echo_notes = self.echo_notes, False
            try:
                self.send_command("discard all.")
                self.doc.head = True
                self.send_command("minimal." if minimal else "full.")
                self.send_command(f"{logic}.")
            finally:
                self.echo_notes = echo
            if (self.doc.logic, self.doc.minimal) != (logic, minimal):
                raise ProverError(
                    f"Fellowship did not switch to {Document(logic, minimal).logic_name}; "
                    f"it reports {self.doc.logic_name}.")
            logger.debug("New document (%s).", self.doc.logic_name)
            return self.doc

    @staticmethod
    def _unquote(atom: Any) -> Any:
        if isinstance(atom, str) and len(atom) >= 2 and atom[0] == '"' and atom[-1] == '"':
            return atom[1:-1]
        return atom

    def send_command(self, command: str, silent: int = 1, *, include_ui: bool = False, allow_incomplete: bool = False) -> Dict[str, Any]:
        """Send a single command to Fellowship.

        command -- The command string
        silent -- A flag determining verbosity (off if 1). TODO: Replace by dedicated logger.

        Returns a preparsed (sexp) proof state.

        A logic toggle (``lk.``, ``lj.``, ``minimal.``, ``full.``) is valid
        only at the head of a document: later it raises ProverError, as
        Fellowship would refuse it; at the head the document records the
        logic Fellowship reports.  Any other command ends the head.
        """
        word = command.strip().rstrip(".").strip()
        with self.lock:
            if word in LOGIC_TOGGLES:
                return self._toggle_logic(command, silent=silent, include_ui=include_ui)
            if not word.startswith("machine ") and word != "discard all":
                self.doc.head = False
            return self._send(command, silent=silent, include_ui=include_ui,
                              allow_incomplete=allow_incomplete)

    def _toggle_logic(self, command: str, *, silent: int = 1, include_ui: bool = False) -> Dict[str, Any]:
        word = command.strip().rstrip(".").strip()
        if not self.doc.head:
            # Past the head only a change is refused: asking for the logic
            # in force (a script's `lk.` loaded into a classical document)
            # is a no-op, and Fellowship is not asked.
            unchanged = {"lk": self.doc.logic == "lk", "lj": self.doc.logic == "lj",
                         "minimal": self.doc.minimal, "full": not self.doc.minimal}[word]
            if unchanged:
                logger.debug("'%s': the document is already %s; nothing to switch.",
                             command.strip(), self.doc.logic_name)
                return dict(self.last_state or {})
            raise ProverError(
                f"'{command.strip()}': the logic is chosen when a document starts "
                f"(`new document [minimal] lk|lj.`); this document is {self.doc.logic_name}.")
        state = self._send(command, silent=silent, include_ui=True)
        reply = state.get("_ui", "")
        if _TOO_LATE in reply:
            self.doc.head = False
            raise ProverError(f"'{command.strip()}': Fellowship refuses it - {_TOO_LATE.lower()}.")
        match = _LOGIC_REPLY.search(reply)
        if match is None:
            raise ProverError(f"'{command.strip()}': Fellowship did not report the logic: {reply!r}")
        self.doc.minimal = bool(match.group(1))
        self.doc.logic = "lj" if match.group(2) == "intuitionistic" else "lk"
        if not include_ui:
            state.pop("_ui", None)
        return state

    def _reply(self) -> str:
        """The text of the reply just read: what came before the match and
        the matched machine-block end, without the prompt."""
        after = getattr(self.prover, "after", None)
        after = after if isinstance(after, str) else ""
        return self.prover.before + after[: after.rfind("fsp <")] if after else self.prover.before

    def _send(self, command: str, silent: int = 1, *, include_ui: bool = False, allow_incomplete: bool = False) -> Dict[str, Any]:
        stripped = command.strip()
        logger.log(5, ">> %s", stripped)
        try:
            self.prover.sendline(command)
            self.prover.expect(REPLY_END)
        except PexpectTIMEOUT as e:
            if allow_incomplete:
                # The prover may be waiting for a continuation line / trailing '.'
                # and thus has not printed the prompt yet.
                out = getattr(self.prover, "before", "")
                state: Dict[str, Any] = {"_need_more_input": True}
                if include_ui:
                    state["_ui"] = MACHINE_BLOCK_RE.sub("", out).strip()
                return state
            logger.error("pexpect timeout on command %r: %s", command, e)
            raise ProverError(f"Prover I/O timeout (possible incomplete command): {e}") from e
        except PexpectEOF as e:
            logger.error("pexpect EOF on command %r: %s", command, e)
            raise ProverError(f"Prover I/O EOF: {e}") from e

        output = self._reply()
        logger.log(5, "<< %s", output)
        return self._finalize_state_from_output(
            output,
            command_for_error=command,
            silent=silent,
            include_ui=include_ui,
            allow_incomplete=allow_incomplete,
        )

    def send_commands(self, commands: List[str], silent: int = 1, *, include_ui: bool = False) -> Dict[str, Any]:
        """Send a batch of complete Fellowship commands and return the final state.

        The commands are written to the prover in one block, then we consume one
        prompt per command.  Each prompt segment is finalized in order so errors
        from intermediate commands are surfaced just as they are with repeated
        :meth:`send_command` calls.
        """
        cleaned = [cmd.strip() for cmd in commands if cmd and cmd.strip()]
        if not cleaned:
            raise ValueError("send_commands requires at least one command")

        if any(c.rstrip(".").strip() in LOGIC_TOGGLES for c in cleaned):
            raise ProverError("a logic toggle cannot be batched; send it on its own")
        with self.lock:
            self.doc.head = False
            return self._send_batch(cleaned, silent=silent, include_ui=include_ui)

    def _send_batch(self, cleaned: List[str], *, silent: int = 1, include_ui: bool = False) -> Dict[str, Any]:
        block = "\n".join(cleaned)
        logger.log(5, ">> batch(%d)\n%s", len(cleaned), block)
        outputs: List[str] = []
        try:
            self.prover.send(block + "\n")
            for _cmd in cleaned:
                self.prover.expect(REPLY_END)
                outputs.append(self._reply())
        except PexpectTIMEOUT as e:
            logger.error("pexpect timeout on command batch %r: %s", cleaned, e)
            raise ProverError(f"Prover I/O timeout during command batch: {e}") from e
        except PexpectEOF as e:
            logger.error("pexpect EOF on command batch %r: %s", cleaned, e)
            raise ProverError(f"Prover I/O EOF during command batch: {e}") from e

        logger.log(5, "<< batch %s", "".join(outputs))
        state: Optional[Dict[str, Any]] = None
        for cmd, output in zip(cleaned, outputs):
            state = self._finalize_state_from_output(
                output,
                command_for_error=cmd,
                silent=silent,
                include_ui=include_ui,
                allow_incomplete=False,
            )
        if state is None:
            raise MachinePayloadError("Machine block missing in prover output.")
        return state

    def send_commands_quiet_final(self, commands: List[str], silent: int = 1) -> Dict[str, Any]:
        """Replay commands with lightweight intermediate machine payloads.

        Fellowship's quiet machine mode avoids serializing the full proof term
        after every replay step.  Intermediate quiet payloads are still parsed
        so errors stop replay immediately; turning quiet mode off emits one full
        final snapshot, which is returned to the caller.
        """
        cleaned = [cmd.strip() for cmd in commands if cmd and cmd.strip()]
        if not cleaned:
            raise ValueError("send_commands_quiet_final requires at least one command")

        with self.lock:
            try:
                self.send_command("machine quiet on.", silent=silent)
            except ProverError as e:
                logger.debug("Quiet replay unavailable; falling back to normal batch replay: %s", e)
                return self.send_commands(cleaned, silent=silent)
            try:
                for cmd in cleaned:
                    self.send_command(cmd, silent=silent)
            except Exception:
                try:
                    self.send_command("machine quiet off.", silent=silent)
                except Exception:
                    pass
                raise
            return self.send_command("machine quiet off.", silent=silent)

    def _finalize_state_from_output(
        self,
        output: str,
        *,
        command_for_error: str,
        silent: int = 1,
        include_ui: bool = False,
        allow_incomplete: bool = False,
    ) -> Dict[str, Any]:
        """Parse prover output and apply the standard state side effects."""
        state = self._extract_machine_block(output)
        if state is None:
            if allow_incomplete:
                # Some commands are dot-terminated. Fellowship may return to the prompt
                # before emitting a machine block, waiting for further input (e.g. a lone '.')
                # to complete the command.
                state = {"_need_more_input": True}
                if include_ui:
                    state["_ui"] = MACHINE_BLOCK_RE.sub("", output).strip()
            else:
                logger.error("Machine block missing in prover output for command %r", command_for_error)
                raise MachinePayloadError("Machine block missing in prover output.")

        # If there's no machine block, only allow continuing when we can prove it's a no-op
        # (currently: plaintext parse error detected) OR when the caller explicitly allows
        # dot-terminated / multi-line commands.
        if (not MACHINE_BLOCK_RE.search(output)) and not (state.get('_no_machine_block_ok') or allow_incomplete):
            logger.error("No machine block and no safe no-op indicator for command %r", command_for_error)
            raise MachinePayloadError("Machine block missing in prover output (possible desync).")

        if include_ui:
            # Keep prover UI text for interactive mode.
            state['_ui'] = MACHINE_BLOCK_RE.sub('', output).strip()
            # Also surface extracted plaintext parse errors explicitly so the CLI can show
            # a crisp error even if Fellowship's surrounding UI text is noisy.
            if state.get('_no_machine_block_ok') and state.get('errors'):
                state['_plain_errors'] = list(state.get('errors') or [])
        # warnings/errors from machine payload
        for w in state.get('warnings', []):
            warnings.warn(w)
        # surface notes from machine payload
        for note in state.get('notes', []):
            if self.echo_notes:
                logger.info("Prover note: %s", note)
            else:
                logger.debug("Prover note: %s", note)
        errs = state.get('errors', [])
        if errs:
            # keep last_state for post-mortem, then raise
            self.last_state = state
            raise ProverError("; ".join(errs))

        self.last_state = state
        self._update_declarations_from_state(state)
        if silent == 0:
            logger.debug("Machine state: %r", state)
        return state

    
    # ---------------- legacy helpers kept for now ----------------------
    # Deprecated, delete upon tests passing.
    # def _expect_ui(self, command: str) -> str:
    #     try:
    #         self.prover.sendline(command)
    #         self.prover.expect('fsp <')
    #         return self.prover.before
    #     except pexpect.EOF:
    #         print("Error: Unexpected EOF received from the prover process.")
    #         self.close(); raise
    #     except pexpect.TIMEOUT:
    #         print("Error: Prover did not respond in time.")
    #         self.close(); raise

    # def _parse_declare_line(self, line: str) -> Tuple[List[str], Optional[str]]:
    #     decl_str = line[len("declare"):].strip()
    #     if decl_str.endswith('.'): decl_str = decl_str[:-1].strip()
    #     if ':' not in decl_str: return [], None
    #     names_part, type_part = decl_str.split(':', 1)
    #     names = [n.strip() for n in names_part.split(',')]
    #     return names, type_part.strip()

    # def _extract_successfully_defined_names(self, output: str) -> set[str]:
    #     success_pattern = re.compile(r'>\s+([^>]+?)\s+defined\.')
    #     results = set()
    #     for match in success_pattern.finditer(output):
    #         for nm in match.group(1).strip().split(','):
    #             results.add(nm.strip())
    #     return results

    # ---------------- new machine helpers -----------------------------
    def _extract_plain_errors(self, output: str) -> Optional[List[str]]:
        # Capture “fsp < Line …, character …” + following “Parse error …”
        pat_block = re.compile(
            r'(?m)(?s)^\s*(?:fsp\s*<\s*)?Line\s+\d+,\s*character\s+\d+:[^\n]*\n\s*\.?\s*Parse error:[^\n]*'
        )
        hits = [m.group(0).strip() for m in pat_block.finditer(output)]
        # Also capture any standalone “Parse error:” lines
        pat_line = re.compile(r'(?mi)^\s*Parse error:[^\n]*$')
        hits += [m.group(0).strip() for m in pat_line.finditer(output)]
        return hits or None

    def _extract_machine_block(self, output: str) -> Optional[Dict[str, Any]]:
        # There can be multiple machine blocks in one prover turn; pick the last.
        matches = list(MACHINE_BLOCK_RE.finditer(output))
        state: Optional[Dict[str, Any]] = None
        if matches:
            payload = matches[-1].group(1).strip().encode('utf-8')
            sexp = self._sexp.parse(payload)
            state = self._from_machine_payload(sexp)
        else:
            state = {}

        # Merge plaintext parse errors regardless of machine block presence.
        # Important: if we have ONLY plaintext parse errors and no machine block,
        # this is a safe no-op (Fellowship rejects the command before changing state).
        errs = self._extract_plain_errors(output)
        if errs:
            state['errors'] = (state.get('errors', []) if state is not None else []) + errs
            state.setdefault('_no_machine_block_ok', not matches)

        # If truly nothing present, signal None
        if not state:
            return None
        return state


    def _from_machine_payload(self, sexp: Any) -> Dict[str, Any]:
         """Maps the  S‑exp structure to a dict.
         sexp: S-exp of the proof state.
         
         The raw S-exp is kept for debugging purtposes. Declarations and Goals are handled both for the case where is a single declaration/goal and for lists of declarations/goals.
         
         TODO: Pass on error messages from the prover.
         TODO: Handle additional content of the S-exp as it becomes useful.

         """
         if not isinstance(sexp, list) or not sexp or sexp[0] != 'state':
             raise ValueError(f"unexpected payload root: {sexp!r}")
         out: Dict[str, Any] = {"raw": sexp}
        
        
         for item in sexp[1:]:
             if not isinstance(item, list) or not item:
                 continue
             key = item[0]
             if key == 'decls':
                entries = []
                for entry in item[1:]:
                    if isinstance(entry, list):
                        entries.append(self._kv_list_to_dict(entry))
                out['decls'] = entries
             elif len(item) == 2 and isinstance(item[1], (str, list)):
                 out[key] = item[1]
                 continue
             #variadic lists like: (decls <entry> <entry> ...)
             
             elif key == 'goals':
                 goals = []
                 for g in item[1:]:
                     if isinstance(g, list) and g and g[0] == 'goal':
                         goals.append(g)
                     else:
                         warnings.warn(f'Unable to convert goal list to dict')
                 out['goals'] = goals
             elif key == 'messages':
                 errs, warns, notes = [], [], []
                 for sub in item[1:]:
                     if isinstance(sub, list) and sub:
                         tag = sub[0]
                         vals = []
                         for v in sub[1:]:
                             if isinstance(v, str):
                                 vals.append(self._unquote(v))
                         if tag == 'errors':
                             errs = vals
                         elif tag == 'warnings':
                             warns = vals
                         elif tag == 'notes':
                             notes = vals
                 out['errors'] = errs
                 out['warnings'] = warns
                 out['notes'] = notes
             else:
                 # keep raw for anything we don't special‑case yet
                 out[key] = item[1:]
                 
         return out

    def _kv_list_to_dict(self, node) -> Dict[str, Any]:
        """Convert a list like [(k v) (k v) ...] into a dict {k: v}."""
        out = {}
        for el in node:
            if isinstance(el, list) and len(el) == 2 and isinstance(el[0], str):
                out[el[0]] = el[1]
        return out
    
    # ------------------------------------------------------------------
    #  Decls sync from machine payload
    # ------------------------------------------------------------------

    def _update_declarations_from_state(self, state: Dict[str, Any]) -> None:
        """Merge `(decls ...)` from machine payload into `self.declarations`.

        Expected shape (per `machine.ml`):
            decls = [ [ ['name', '"A"'], ['kind','sort'], ['sort','"bool"'] ], ... ]

        Values are stored as `Declaration`, a `str` subclass that also carries
        the payload's `kind`.  The kind is what tells a sort apart from a
        proposition, which first-order proof-term resolution depends on.
        """
        decls = state.get('decls')
        if not isinstance(decls, list):
            return

        def tagged(value: Any, kind: str) -> Any:
            # A payload missing its sort/prop field used to store None; keep
            # that rather than turning it into the string "None".
            return Declaration(value, kind) if isinstance(value, str) else value

        for entry in decls:
            nm   = entry.get('name')
            kind = entry.get('kind')
            if not nm or not kind:
                continue
            if isinstance(nm, str):
                nm = self._unquote(nm)
            if not nm in self.declarations:
                # The value is kept as a Declaration -- a str carrying the kind
                # alongside the text.  Readers that only want the text are
                # unaffected; first-order resolution needs the kind to tell a
                # sort annotation from a proposition, which Fellowship prints
                # identically.
                if kind == 'sort':
                    typ = entry.get('sort')
                    if isinstance(typ, str):
                        typ = self._unquote(typ)
                    self.declarations[nm] = tagged(typ, 'sort')
                    self.bump_revision()
                    logger.info("'%s' : '%s'  declared.", nm, typ)
                elif kind == 'prop':
                    # We store the proposition string for axioms/theorems.
                    pr = entry.get('prop')
                    if isinstance(pr, str):
                        pr = self._unquote(pr)
                    self.declarations[nm] = tagged(pr, 'prop')
                    self.bump_revision()
                    logger.info("'%s' : '%s'  declared.", nm, pr)
                elif kind == 'moxia':
                    # Store the proposition string for refutations (deny).
                    pr = entry.get('prop')
                    if isinstance(pr, str):
                        pr = self._unquote(pr)
                    self.declarations[nm] = tagged(pr, 'moxia')
                    self.bump_revision()
                    logger.info("'%s' : '%s'  denied.", nm, pr)
            

    # def parse_proof_state(self, output):
    #     # Deprecated
    #     # Extract proof term
    #     proof_term_match = re.search(r'Proof term:\s*\n\s*(.*?)\n', output, re.DOTALL)
    #     proof_term = proof_term_match.group(1) if proof_term_match else None

    #     # Extract natural language explanation
    #     nl_match = re.search(r'Natural language:\s*\n\s*(.*?)\n\s*done', output, re.DOTALL)
    #     natural_language = nl_match.group(1) if nl_match else None

    #     # Extract goals
    #     goals_matches = re.findall(r'(\d+) goal[s]? yet to prove!', output)
    #     goals_match = goals_matches[-1] if len(goals_matches)>0 else None
    #     #goals = int(goals_match.group(1)) if goals_match else 0
    #     goals = int(goals_match) if goals_match else 0

    #     # Extract current goal
    #     current_goal_match = re.search(r'\|-----\s*([\d\.])\s*\n\s*([^\r\n]*)', output, re.DOTALL)
    #     current_goal = current_goal_match.group(2).strip() if current_goal_match else None

    #     return {
    #         'proof_term': proof_term,
    #         'natural_language': natural_language,
    #         'goals': goals,
    #         'current_goal': current_goal,
    #     }
    
    def register_decoration(self, name: str, template: str) -> None:
        """Register wrapper-side natural-language decoration metadata."""
        self.decorations[name] = template
        logger.info("'%s' decorated as '%s'.", name, template)

    # -- names ---------------------------------------------------------------

    @property
    def names(self) -> Dict[str, str]:
        """Every name taken in this document, with what took it:
        "declaration", "statement", "recording", "argument" or "debate".
        Scoped like the document itself: `new document` starts a new,
        empty table."""
        return self.doc.names

    def claim_name(self, name: str, kind: str, *, refine: bool = False, dry_run: bool = False) -> None:
        """Take ``name`` for ``kind`` or raise NameClash.

        One namespace for sorts, declared axioms, statements and arguments,
        checked here because only the wrapper sees every name: defeasible
        arguments never reach Fellowship, which silently replaces an
        existing theorem of the same name.  ``refine`` lets a recording
        become its argument and a statement become its witness."""
        if any(name.startswith(p) for p in RESERVED_PREFIXES):
            raise NameClash(
                f"'{name}' starts with a prefix the wrapper reserves for the names it "
                f"generates itself ({', '.join(RESERVED_PREFIXES)}); choose another name."
            )
        held = self.names.get(name)
        if held is not None and not (refine and held in _REFINABLE):
            raise NameClash(
                f"'{name}' is already a {held} in this document; names are unique "
                f"per document (`new document` starts a new one)."
            )
        if not dry_run:
            self.names[name] = kind

    def claim_declared_names(self, command: str) -> None:
        """The names a `declare N1, N2 : TYPE.` command introduces."""
        match = re.match(r"\s*declare\s+(.+?)\s*:", command)
        if not match:
            return
        for name in (n.strip() for n in match.group(1).split(",")):
            if name:
                self.claim_name(name, "declaration")

    def register_argument(self, argument: Any, *, replace: bool = False) -> None:
        """ Register a new argument (i.e. a partial Fellowship proof.)

        An atomic argument (not one the debate operators composed) also
        contributes its hyperedges to the document graph; a composed one
        is a *debate*, named for its issue, and adds nothing - the
        conflicts it names are already in the document (option (i)+(ii)
        of the aida-document-graph decision).

        The name must be free, or held by the recording or statement this
        argument refines; otherwise NameClash, and nothing is registered."""
        if not (replace and argument.name in self.arguments):
            self.claim_name(argument.name, "argument", refine=True)
        else:
            self.names[argument.name] = "argument"
        # A replacement (a refinement, or a citer replayed after its citation
        # became strict) keeps its position: dict assignment to an existing
        # key does, and registration order is the order unfolding nests
        # scaffolds in.
        self.arguments[argument.name] = argument
        if not getattr(argument, "composed", False):
            self.document_add(argument)
        self.bump_revision()

    def rebuild_document(self) -> None:
        """Recompile the document graph from the atomic arguments, in
        registration order.  Needed after a replacement: merging cannot take
        an old edge back out, and default markers are not tracked per edge."""
        self.doc.graph = DebateGraph()
        for argument in self.arguments.values():
            if not getattr(argument, "composed", False):
                self.document_add(argument)
        self.bump_revision()
        logger.debug("Document graph rebuilt: %d edge(s)", len(self.graph.edges))

    # -- the document graph (Phase C) --------------------------------------

    @property
    def graph(self) -> DebateGraph:
        """The document graph."""
        return self.doc.graph

    def typechecked(self) -> dict:
        """The definitions that already replayed in this document, with the
        term Fellowship rebuilt for each (``typecheck_shared``)."""
        return self.doc.typechecked

    @property
    def logic(self) -> str:
        """"lk" (classical, default) or "lj": the document's logic."""
        return self.doc.logic

    @property
    def minimal(self) -> bool:
        """Whether the document's logic is minimal (no ex falso)."""
        return self.doc.minimal

    def document_add(self, argument: Any) -> None:
        """Compile an argument into hyperedges and merge them into the
        document graph.  Refusals are logged, not raised: the argument is
        still registered for the term-level commands."""
        graph = self._compile_argument(argument, "the document graph")
        if graph is None:
            return
        self.graph.merge(graph)
        logger.debug("Document graph: +%d edges from '%s'", len(graph.edges), argument.name)

    def _compile_argument(self, argument: Any, into: str):
        """The hyperedges of one argument, or None (logged) if it cannot
        be compiled."""
        if not getattr(argument, "executed", False) or getattr(argument, "body", None) is None:
            return None
        # A citation is a name leaf in the body; the compiler reads it as an
        # obligation on the cited conclusion, or a strict leaf if Fellowship
        # holds the cited argument (core/dc/cite.py).
        try:
            return compile_debate(argument.body, argument.name,
                                  strict_names=self.declarations.keys(),
                                  strict_kinds=declaration_kinds(self.declarations))
        except (DebateCompileError, FirstOrderNotSupported) as e:
            logger.warning("Argument '%s' not added to %s: %s", argument.name, into, e)
            return None

    # -- debates (core/dc/debate.py, aida-debate-objects) ------------------

    @property
    def debates(self) -> Dict[str, Any]:
        """The recorded debates by name.  They share the document's
        namespace but not its graph: a debate adds no edge."""
        return self.doc.debates

    @property
    def recording_debate(self) -> Optional[str]:
        """The name of the debate being recorded (between its header and
        ``cedat tempus.``), or None."""
        return self.doc.recording_debate

    @recording_debate.setter
    def recording_debate(self, name: Optional[str]) -> None:
        self.doc.recording_debate = name

    def register_debate(self, debate: Any) -> None:
        self.claim_name(debate.name, "debate")
        self.debates[debate.name] = debate

    def debate_graph(self, debate: Any) -> DebateGraph:
        """The graph a debate is compiled from: the document (open scope),
        or the arguments of its closed scope compiled on their own, so
        that what is presumed is what the scope presumes."""
        if debate.scope == "open":
            return self.graph
        from core.dc.cite import cited_names
        from core.dc.debate import scope_names

        def cited(name):
            argument = self.arguments.get(name)
            body = getattr(argument, "body", None)
            return [n for n in cited_names(body) if n in self.arguments] if body is not None else []

        graph = DebateGraph()
        for name in scope_names(debate, cited):
            argument = self.arguments.get(name)
            compiled = self._compile_argument(argument, f"debate '{debate.name}'") if argument else None
            if compiled is not None:
                graph.merge(compiled)
        return graph

    def debate_term(self, debate: Any):
        """The debate's term, cached until the document or the debate
        changes."""
        from core.dc.unfold import report_cached
        from core.dc.debate import unfold_debate
        key = (self.revision, tuple(str(m) for m in debate.moves))
        if debate._term is not None and debate._term[0] == key:
            return report_cached(debate._term[1], f"the term of debate '{debate.name}'", self.revision)
        term = unfold_debate(self.debate_graph(debate), debate)
        debate._term = (key, term)
        return term

    @staticmethod
    def issue_of(argument: Any):
        """The statement an argument (or debate) is about."""
        side = "context" if getattr(argument, "is_anti", False) else "term"
        return (canonical_prop(argument.conclusion), side)

    def anon_name(self, statement) -> str:
        """The ``anon_k`` name of a statement's debate, allocated on first
        use and kept with the document so the numbers do not shift between
        commands (tasks.org, aida-shared-subarguments, decision 6)."""
        table = self.doc.anon
        if statement not in table:
            table[statement] = f"{ANON_PREFIX}{len(table) + 1}"
        return table[statement]

    def debate_names(self) -> Dict[Any, str]:
        """{statement: name} for the statements exactly one registered
        argument or debate is about: the name the author gave its debate.
        Where several arguments conclude one statement no single name
        denotes the whole debate, and it is left to ``anon_name``."""
        about: Dict[Any, List[str]] = {}
        for name, argument in self.arguments.items():
            if not getattr(argument, "conclusion", None):
                continue
            try:
                about.setdefault(self.issue_of(argument), []).append(name)
            except ValueError:          # a conclusion the graph has no node for
                continue
        return {statement: names[0] for statement, names in about.items() if len(names) == 1}

    # -- revisions and unfolded terms (aida-unfold-entrypoints) -----------

    @property
    def revision(self) -> int:
        """The document's revision: it changes whenever an argument is
        registered (or the document rebuilt) or a declaration is added, so
        a term unfolded or evaluated at an older revision may be stale - a
        new argument can support or attack, a new declaration can make a
        presumption strict or refuted."""
        return self.doc.revision

    def bump_revision(self) -> None:
        self.doc.revision += 1

    def unfolded_term(self, argument: Any):
        """The debate term unfolded for ``argument`` (biased towards it), from
        the cache when it is fresh; None if the argument has no edge of its
        own in the document (a composed debate, or one refused at
        registration)."""
        from core.dc.unfold import argument_edge, unfold_argument, report_cached
        if (getattr(argument, "unfolded_body", None) is not None
                and argument.unfolded_revision == self.revision):
            return report_cached(argument.unfolded_body,
                                 f"the term unfolded for '{argument.name}'", self.revision)
        edge = argument_edge(self.graph, argument.name)
        if edge is None:
            return None
        argument.unfolded_body = unfold_argument(self.graph, edge)
        argument.unfolded_revision = self.revision
        return argument.unfolded_body

    def issue_term(self, statement):
        """The canonical debate term of ``statement`` (an issue entrypoint),
        cached per revision."""
        from core.dc.unfold import unfold, report_cached
        cache = self.doc.issue_terms
        found = cache.get(statement)
        if found is not None and found[0] == self.revision:
            return report_cached(found[1], "the issue's canonical term", self.revision)
        term = unfold(self.graph, statement)
        cache[statement] = (self.revision, term)
        return term

    def shared_debate(self, issue) -> SharedDebate:
        """The debate of ``issue`` as named sub-debates (core/dc/share.py)."""
        return share(self.graph, issue, self.debate_names(), self.anon_name)

    def get_argument(self, name: str) -> Optional[Any]:
        """ Retrieve a registered argument """
        return self.arguments.get(name)
    
    def close(self) -> None:
        """ Close the prover """
        try:
            self.prover.sendline('quit.')
        finally:
            close = getattr(self.prover, "close", None)
            if close is not None:
                close()
            else:                                  # a pipe process
                proc = self.prover.proc
                try:
                    proc.wait(timeout=2)
                except Exception:
                    proc.kill()
