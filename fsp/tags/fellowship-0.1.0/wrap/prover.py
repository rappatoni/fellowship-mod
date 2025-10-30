import os, re, logging, warnings
from typing import Any, List, Tuple, Optional, Dict, Callable
import pexpect
from pexpect.exceptions import EOF as PexpectEOF, TIMEOUT as PexpectTIMEOUT
from sexp_parser import SexpParser

logger = logging.getLogger('fsp.wrapper')

class ProverError(Exception):
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
        self.custom_tactics : Dict[str, Any] = {} # Keeps custom tactics. Most importantly those that realize the argumentative layer (pop, chain, undercut, focussed undercut, rebut, support.)
        self.arguments : Dict[str, Any]= {}  # Dictionary to store arguments by name
        self.declarations : Dict[str, str] = {} #Stores prover declarations materialized via "declare X : Y.".
        self.last_state: Any = None
        self._sexp = SexpParser()
        self.echo_notes = os.getenv("FSP_ECHO_NOTES", "1").lower() not in {"0", "false", "no"}

    @staticmethod
    def _unquote(atom: Any) -> Any:
        if isinstance(atom, str) and len(atom) >= 2 and atom[0] == '"' and atom[-1] == '"':
            return atom[1:-1]
        return atom

    def send_command(self, command: str, silent: int = 1) -> Dict[str, Any]:
        """Send a command to Fellowship

        command -- The command string
        silent -- A flag determining verbosity (off if 1). TODO: Replace by dedicated logger.

        Returns a preparsed (sexp) proof state.
        """
        
        stripped = command.strip()
        logger.trace(">> %s", stripped)
        try:
            self.prover.sendline(command)
            self.prover.expect('fsp <')
        except (PexpectEOF, PexpectTIMEOUT) as e:
            logger.error("pexpect error on command %r: %s", command, e)
            raise ProverError(f"Prover I/O error: {e}") from e

        output = self.prover.before
        logger.trace("<< %s", output)
        state = self._extract_machine_block(output)
        if state is None:
            logger.error("Machine block missing in prover output for command %r", command)
            raise MachinePayloadError("Machine block missing in prover output.")
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

        # Merge plaintext parse errors regardless of machine block presence
        errs = self._extract_plain_errors(output)
        if errs:
            existing = state.get('errors', []) if state is not None else []
            if state is None:
                state = {}
            state['errors'] = existing + errs

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
                out[el][0] = el[1]
        return out
    
    # ------------------------------------------------------------------
    #  Decls sync from machine payload
    # ------------------------------------------------------------------

    def _update_declarations_from_state(self, state: Dict[str, Any]) -> None:
        """Merge `(decls ...)` from machine payload into `self.declarations`.

        Expected shape (per `machine.ml`):
            decls = [ [ ['name', '"A"'], ['kind','sort'], ['sort','"bool"'] ], ... ]
        """
        decls = state.get('decls')
        if not isinstance(decls, list):
            return

        for entry in decls:
            nm   = entry.get('name')
            kind = entry.get('kind')
            if not nm or not kind:
                continue
            if isinstance(nm, str):
                nm = self._unquote(nm)
            if not nm in self.declarations:
                if kind == 'sort':
                    typ = entry.get('sort')
                    if isinstance(typ, str):
                        typ = self._unquote(typ)
                    self.declarations[nm]= typ
                    logger.info("'%s' : '%s'  declared.", nm, typ)
                elif kind == 'prop':
                    # We store the proposition string for axioms/theorems.
                    pr = entry.get('prop')
                    if isinstance(pr, str):
                        pr = self._unquote(pr)
                    self.declarations[nm] = pr
                    logger.info("'%s' : '%s'  declared.", nm, pr)
                elif kind == 'moxia':
                    # Store the proposition string for refutations (deny).
                    pr = entry.get('prop')
                    if isinstance(pr, str):
                        pr = self._unquote(pr)
                    self.declarations[nm] = pr
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
        """ Register a new argument (i.e. a partial Fellowship proof.)"""
        self.arguments[argument.name] = argument

    def get_argument(self, name: str) -> Optional[Any]:
        """ Retrieve a registered argument """
        return self.arguments.get(name)
    
    def close(self) -> None:
        """ Close the prover """
        try:
            self.prover.sendline('quit.')
        finally:
            self.prover.close()
