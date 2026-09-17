import os, re, logging, warnings
from typing import Any, List, Tuple, Optional, Dict, Callable
import pexpect
from pexpect.exceptions import EOF as PexpectEOF, TIMEOUT as PexpectTIMEOUT
from .sexp_parser import SexpParser
from core.ac.signature import Declaration
from mod import store
from core.dc.debate_graph import (
    DebateGraph, DebateCompileError, compile_debate, declaration_kinds, canonical_prop,
)
from core.ac.ast import FirstOrderNotSupported

logger = logging.getLogger('fsp.wrapper')

class ProverError(Exception):
    pass

class ProverNeedsMoreInput(ProverError):
    """The prover has not returned to the prompt yet (e.g. waiting for a trailing '.').

    This is not a desync; the caller should keep feeding input lines until a prompt appears.
    """
    pass

class MachinePayloadError(ProverError):
    pass

MACHINE_BLOCK_RE = re.compile(r";;BEGIN_ML_DATA;;(.*?);;END_ML_DATA;;", re.S)

class ProverWrapper:
    """
    The main class for the argumentation layer on top of the Fellowship prover. Includes utilities to execute an instance of the fellowship prover (self.prover, self.prover.expect), send commands to it and receive and process its output (send_command, self._sexp). Maintains a state in the form of Dicts of registered constant declarations and arguments (self.declarations, self.arguments) and parsed prover ouput (self.last_state). Allows for the registration and execution of custom tactics (self.custom_tactics).

TODO: Unified exception handling and logging.
TODO: Consistent use of type annotations.
TODO: Mechanism to declare a scenario of default assumptions.
    """
    def __init__(self, prover_cmd, env: Optional[dict] = None, log_level: Optional[int] = None):
        if log_level is not None:
            logger.setLevel(log_level)

        env_used = (env or os.environ).copy()
        env_used.setdefault("FSP_MACHINE", "1")

        self.prover = pexpect.spawn(prover_cmd, encoding='utf-8', timeout=5, env=env_used)
        self.prover.expect('fsp <')
        # Pexpect sleeps for 50 ms before every send by default.  Fellowship is
        # already at a stable prompt here and does not perform password-style
        # terminal echo negotiation, so that defensive delay only adds linear
        # latency to proof replay.
        self.prover.delaybeforesend = None
        self.custom_tactics : Dict[str, Any] = {} # Keeps custom tactics. Most importantly those that realize the argumentative layer (pop, chain, undercut, focussed undercut, rebut, support.)
        self.last_state: Any = None
        self.last_output_text: str = ""
        self._sexp = SexpParser()
        self.echo_notes = os.getenv("FSP_ECHO_NOTES", "1").lower() not in {"0", "false", "no"}
        self.declarations: Dict[str, Declaration] = {}
        self.decorations: Dict[str, str] = {}
 

    @property
    def arguments(self) -> Dict[str, Any]:
        return store.arguments

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
        """
        stripped = command.strip().rstrip(".").strip()
        if stripped in ("lj", "lk"):
            # Fellowship starts a new theory here; so does the document
            # (the type-check switch is a session setting and survives).
            typecheck = store.document.get("typecheck")
            store.document.clear()
            store.document["logic"] = stripped
            if typecheck is not None:
                store.document["typecheck"] = typecheck
        stripped = command.strip()
        logger.log(5, ">> %s", stripped)
        try:
            self.prover.sendline(command)
            self.prover.expect('fsp <')
        except PexpectTIMEOUT as e:
            if allow_incomplete:
                # The prover may be waiting for a continuation line / trailing '.'
                # and thus has not printed the prompt yet.
                out = getattr(self.prover, "before", "")
                self.last_output_text = out
                state: Dict[str, Any] = {"_need_more_input": True}
                if include_ui:
                    state["_ui"] = MACHINE_BLOCK_RE.sub("", out).strip()
                return state
            logger.error("pexpect timeout on command %r: %s", command, e)
            raise ProverError(f"Prover I/O timeout (possible incomplete command): {e}") from e
        except PexpectEOF as e:
            logger.error("pexpect EOF on command %r: %s", command, e)
            raise ProverError(f"Prover I/O EOF: {e}") from e

        output = self.prover.before
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

        block = "\n".join(cleaned)
        logger.log(5, ">> batch(%d)\n%s", len(cleaned), block)
        outputs: List[str] = []
        try:
            self.prover.send(block + "\n")
            for _cmd in cleaned:
                self.prover.expect('fsp <')
                outputs.append(self.prover.before)
        except PexpectTIMEOUT as e:
            out = "".join(outputs) + getattr(self.prover, "before", "")
            self.last_output_text = out
            logger.error("pexpect timeout on command batch %r: %s", cleaned, e)
            raise ProverError(f"Prover I/O timeout during command batch: {e}") from e
        except PexpectEOF as e:
            out = "".join(outputs) + getattr(self.prover, "before", "")
            self.last_output_text = out
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
        self.last_output_text = output
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
                    logger.info("'%s' : '%s'  declared.", nm, typ)
                elif kind == 'prop':
                    # We store the proposition string for axioms/theorems.
                    pr = entry.get('prop')
                    if isinstance(pr, str):
                        pr = self._unquote(pr)
                    self.declarations[nm] = tagged(pr, 'prop')
                    logger.info("'%s' : '%s'  declared.", nm, pr)
                elif kind == 'moxia':
                    # Store the proposition string for refutations (deny).
                    pr = entry.get('prop')
                    if isinstance(pr, str):
                        pr = self._unquote(pr)
                    self.declarations[nm] = tagged(pr, 'moxia')
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

    def register_custom_tactic(self, name: str, function: Callable[..., Any]) -> None:
        """ Register a custom tactic with its associated function """
        self.custom_tactics[name] = function

    def execute_tactic(self, tactic_name: str, *args: Any) -> Any:
        """ Execute a tactic, either a custom or predefined tactic """
        if tactic_name in self.custom_tactics:
            return self.custom_tactics[tactic_name](self, *args)
        else:
            return f"Error: Tactic '{tactic_name}' is not defined."

    def register_argument(self, argument: Any) -> None:
        """ Register a new argument (i.e. a partial Fellowship proof.)

        An atomic argument (not one the debate operators composed) also
        contributes its hyperedges to the document graph; a composed one
        is a *debate*, named for its issue, and adds nothing - the
        conflicts it names are already in the document (option (i)+(ii)
        of the aida-document-graph decision)."""
        self.arguments[argument.name] = argument
        if not getattr(argument, "composed", False):
            self.document_add(argument)

    # -- the document graph (Phase C) --------------------------------------

    @property
    def document(self) -> DebateGraph:
        return store.document.setdefault("graph", DebateGraph())

    @property
    def typecheck_enabled(self) -> bool:
        """Whether unfolded terms are replayed through Fellowship before
        use (default on; FSP_TYPECHECK=0 or `typecheck off` disables)."""
        return store.document.get("typecheck", os.getenv("FSP_TYPECHECK", "1") != "0")

    @typecheck_enabled.setter
    def typecheck_enabled(self, value: bool) -> None:
        store.document["typecheck"] = bool(value)

    @property
    def logic(self) -> str:
        """"lk" (classical, default) or "lj", as last selected by the user."""
        return store.document.get("logic", "lk")

    def document_add(self, argument: Any) -> None:
        """Compile an argument into hyperedges and merge them into the
        document graph.  Refusals are logged, not raised: the argument is
        still registered for the term-level commands."""
        if not getattr(argument, "executed", False) or getattr(argument, "body", None) is None:
            return
        try:
            graph = compile_debate(argument.body, argument.name,
                                   strict_names=self.declarations.keys(),
                                   strict_kinds=declaration_kinds(self.declarations))
        except (DebateCompileError, FirstOrderNotSupported) as e:
            logger.warning("Argument '%s' not added to the document graph: %s", argument.name, e)
            return
        self.document.merge(graph)
        logger.debug("Document graph: +%d edges from '%s'", len(graph.edges), argument.name)

    @staticmethod
    def issue_of(argument: Any):
        """The statement an argument (or debate) is about."""
        side = "context" if getattr(argument, "is_anti", False) else "term"
        return (canonical_prop(argument.conclusion), side)

    def check_reachable(self, host: Any, scion: Any, kind: str) -> None:
        """The debate verbs' assertion (aida-document-graph decision): an
        attacker must conclude the contrary of a statement reachable from
        the host's issue, a supporter must conclude such a statement.
        Only checked when the host is in the document graph."""
        issue = self.issue_of(host)
        if issue not in set(self.document.statements()):
            return
        reach = self.document.reachable(issue)
        key, side = self.issue_of(scion)
        wanted = (key, ("context" if side == "term" else "term")) if kind == "attack" else (key, side)
        if wanted not in reach:
            display = self.document.nodes.get(key, scion.conclusion)
            raise ProverError(
                f"{kind}: '{scion.name}' concludes {display}[{side[0]}], but no statement "
                f"{display}[{wanted[1][0]}] is reachable from '{host.name}' in the document graph."
            )

    def get_argument(self, name: str) -> Optional[Any]:
        """ Retrieve a registered argument """
        return self.arguments.get(name)
    
    def close(self) -> None:
        """ Close the prover """
        try:
            self.prover.sendline('quit.')
        finally:
            self.prover.close()
