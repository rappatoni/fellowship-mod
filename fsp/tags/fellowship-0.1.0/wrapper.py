from __future__ import annotations
import os
import pexpect
import re
from lark import Lark, Transformer, v_args
import parser
import warnings
from pathlib import Path
import argparse
import copy
from typing import Any, List, Tuple, Optional, Dict, Callable

from sexp_parser import SexpParser

import logging
from pexpect.exceptions import EOF as PexpectEOF, TIMEOUT as PexpectTIMEOUT

logger = logging.getLogger('fsp.wrapper')
logger.propagate = True

class ProverError(Exception):
    pass

class MachinePayloadError(ProverError):
    pass

MACHINE_BLOCK_RE = re.compile(r";;BEGIN_ML_DATA;;(.*?);;END_ML_DATA;;", re.S)
#HUMAN_UI = os.getenv("FSP_HUMAN_UI", "0") not in {"0", "false", "False", ""}

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
        else:
            # Inherit level from root/pytest so INFO is visible in test output
            logger.setLevel(logging.NOTSET)

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
        try:
            self.prover.sendline(command)
            self.prover.expect('fsp <')
        except (PexpectEOF, PexpectTIMEOUT) as e:
            logger.error("pexpect error on command %r: %s", command, e)
            raise ProverError(f"Prover I/O error: {e}") from e

        output = self.prover.before
        state = self._extract_machine_block(output)
        if state is None:
            logger.error("Machine block missing in prover output for command %r", command)
            raise MachinePayloadError("Machine block missing in prover output.")
        # warnings/errors from machine payload
        for w in state.get('warnings', []):
            warnings.warn(w)
        # surface notes from machine payload
        for note in state.get('notes', []):
            logger.info("note: %s", note)
            if self.echo_notes:
                print(f"Prover note: {note}")
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
                out[el[0]] = el[1]
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
            if kind == 'sort':
                typ = entry.get('sort')
                if isinstance(typ, str):
                    typ = self._unquote(typ)
                    self.declarations[nm]= typ
            elif kind == 'prop':
                # We store the proposition string for axioms/theorems.
                pr = entry.get('prop')
                if isinstance(pr, str):
                    pr = self._unquote(pr)
                self.declarations[nm] = pr
            

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

    def register_argument(self, argument: "Argument") -> None:
        """ Register a new argument (i.e. a partial Fellowship proof.)"""
        self.arguments[argument.name] = argument

    def get_argument(self, name: str) -> Optional["Argument"]:
        """ Retrieve a registered argument """
        return self.arguments.get(name)
    
    def close(self) -> None:
        """ Close the prover """
        try:
            self.prover.sendline('quit.')
        finally:
            self.prover.close()


def check_for_errors(output,errors):
    """ Rudimentary exception surfacing from prover output.
    TODO: Update and test this for machine-oriented output.
    """
    for error in errors:
        if error in output:
            # print('Error detected: {error}.')
            raise Exception(f'Error detected: {error}')
            # return f'Error detected: {error}.'
        else:
            pass

        
# ---------------------------------------TACTICS-------------------------------------------------
#TODO: Add Implication Elimination tactic.
#TODO: Add Negation expansion tactic.

def pop(prover, x, y, closed=True, errors=['This is not trivial. Work some more.']):
    """DEPRECATED: Currently not used and code needs updating.
    """

    instructions = [f'cut ({y}) stash.', f'next.', f'cut ({x}) affine.',f'next.', f'axiom.']
    output = ''
    counter = 0
    for instruction in instructions:
        try:
            output += prover.send_command(instruction)
            check_for_errors(output, errors)
        except Exception as X:
            print(X)
            print('Undoing and aborting.')
            #Trying to undo the tactic application. Note that Fellowship's undo appears to be buggy so this will not always work.
            i=0
            while i<counter:
                output += prover.send_command(f'undo.')
                i+=1
            return output
        if instruction not in [f'next.',f'idtac.',f'prev.']:
            # Note that undo does not seem to work with idtac, but there should be no reason to use idtac in a tactic anyway.
            counter += 1
            
    return output

# -----------------------------------------Scripts/Interactive Mode -----------------------------

def execute_script(prover: ProverWrapper, script_path: str, *, strict: bool = False, stop_on_error: bool = True, echo_notes: bool = False, isolate: bool = True) -> None:
    """ Executes a .fspy script.
        script_path: .fspy file to be run.
        
        Syntax for scripts: 
          - All fellowship commands;
          - (Custom) tactics: "tactic <TacticName> <Args>"
          - Arguments: "start argument / end argument";
          - Executing/Reducing an argument : "reduce <ArgName>"
          - Normalize an argument (silent version of reduce): "normalize <ArgName>" 
          - Chaining (Grafting) two arguments "chain <Arg1> <Arg2>" (Arg1 is rootstock, Arg2 is scion)
          - Rendering arguments (unreduced term, normal form, respectively): "render <Arg>", "render-nf <Arg>".
          - TODO: undercut, support, rebut.
          - Lines starting with '#' are user-facing comments and are printed to stdout.
          - Lines starting with '%' are invisible comments and are ignored.
    """
    logger.info(
        "Running script %s (strict=%s, stop_on_error=%s, echo_notes=%s)",
        script_path, strict, stop_on_error, echo_notes
    )
    if isolate:
        # start a fresh prover session for this script
        prover = setup_prover()
    prev_echo = getattr(prover, "echo_notes", False)
    prover.echo_notes = echo_notes

    recording = False
    current_argument = None
    with open(script_path, 'r') as script_file:
        for lineno, line in enumerate(script_file, start=1):
            command = line.strip()
            if not command:
                continue
            if command.startswith('%'):
                # invisible comment, skip silently
                continue
            if command.startswith('#'):
                # user-facing comment/log
                logger.info(command[1:].lstrip())
                continue
            # developer-level trace only
            logger.debug("Sending command [%s:%d] %s", script_path, lineno, command)
            if command.startswith('start argument '):
                    if recording:
                        logger.warning("Already recording an argument. Please end the current recording first.")
                        continue
                    parts = command.split(' ', 3)
                    if len(parts) < 4:
                        logger.error("Invalid command. Use: start argument name conclusion")
                        continue
                    name = parts[2]
                    conclusion = parts[3].strip()
                    current_argument = {
                        'name': name,
                        'conclusion': conclusion,
                        'instructions': []
                    }
                    recording = True
                    logger.info("Started recording argument '%s' with conclusion '%s'.", name, conclusion)
            elif command == 'end argument':
                    if not recording:
                        logger.warning("Not currently recording an argument.")
                        continue
                    # Create and execute the argument
                    arg = Argument(
                        prover,
                        name=current_argument['name'],
                        conclusion=current_argument['conclusion'],
                        instructions=current_argument['instructions']
                    )
                    arg.execute()
                    # Store the argument for later use
                    prover.register_argument(arg)
                    logger.info("Argument '%s' saved with conclusion '%s'.", arg.name, arg.conclusion)
                    # Reset recording state
                    recording = False
                    current_argument = None
            elif recording:
                    # Record-only during scripts: do not execute lines now.
                    if command.startswith('tactic '):
                        # Keep the tactic line as-is; Argument.execute will run it.
                        current_argument['instructions'].append(command)
                    else:
                        # Strip trailing dot; Argument.execute will add a single '.'
                        instr = command.rstrip('.').strip()
                        current_argument['instructions'].append(instr)
            else:
                    # Handle commands outside of recording
                    if command.startswith('tactic '):
                        # Handle tactic commands outside of recording
                        parts = command.split()
                        tactic_name = parts[1]
                        tactic_args = parts[2:]  # Remaining parts are arguments to the tactic
                        output = prover.execute_tactic(tactic_name, *tactic_args)
                        #print(output)

                    elif command.startswith("reduce "):
                        reduce_argument_cmd(prover, command.split(maxsplit=1)[1])
                    elif command.startswith("render-nf "):
                        render_argument_cmd(prover, command.split(maxsplit=1)[1], True)
                    elif command.startswith("render "):
                        render_argument_cmd(prover, command.split(maxsplit=1)[1], False)
                    elif command.startswith("normalize "):
                        name = command.split(maxsplit=1)[1]
                        arg = prover.get_argument(name)
                        if arg:
                            logger.info("Normalized argument '%s'; normal form cached in .normal_form", name)
                            arg.normalize(); print("normal form stored in .normal_form")
                        else:
                            logger.warning("Argument '%s' not found for normalization", name)
                            #print(f"Argument '{name}' not found.")
                    elif command.startswith('undercut '):
                        # Format: undercut attacker target
                        parts = command.split()
                        if len(parts) != 3:
                            logger.error("Invalid undercut command. Use: undercut attacker target")
                            continue
                        attacker_name = parts[1]
                        target_name = parts[2]
                        attacker = prover.get_argument(attacker_name)
                        target = prover.get_argument(target_name)
                        if attacker and target:
                            try:
                                result = attacker.focussed_undercut(target)
                                prover.register_argument(result)
                                logger.info("Constructed undercut '%s' of '%s' by '%s'.",
                                            result.name, target.name, attacker.name)
                            except ProverError as e:
                                if strict:
                                    if isolate:
                                        try:
                                            prover.close()
                                        except Exception:
                                            pass
                                    else:
                                        prover.echo_notes = prev_echo
                                    logger.info("Finished script %s", script_path)
                                    raise ProverError(f"{script_path}:{lineno}: {e}") from e
                                logger.error("Prover error during undercut: %s", e)
                                if stop_on_error:
                                    break
                            except MachinePayloadError as e:
                                if strict:
                                    if isolate:
                                        try:
                                            prover.close()
                                        except Exception:
                                            pass
                                    else:
                                        prover.echo_notes = prev_echo
                                    logger.info("Finished script %s", script_path)
                                    raise MachinePayloadError(f"{script_path}:{lineno}: {e}") from e
                                logger.error("Prover error (no machine payload) during undercut: %s", e)
                                if stop_on_error:
                                    break
                        else:
                            logger.error("Undercut failed: one or both arguments not found ('%s', '%s').",
                                         attacker_name, target_name)
                            if strict:
                                if isolate:
                                    try:
                                        prover.close()
                                    except Exception:
                                        pass
                                else:
                                    prover.echo_notes = prev_echo
                                logger.info("Finished script %s", script_path)
                                raise ProverError(f"{script_path}:{lineno}: Undercut failed: missing arguments")
                            if stop_on_error:
                                break
                    elif command.startswith('chain '):
                        # Handle chaining of arguments
                        # Format: chain arg1 arg2
                        parts = command.split()
                        if len(parts) != 3:
                            logger.error("Invalid chain command. Use: chain arg1 arg2")
                            continue
                        arg1_name = parts[1]
                        arg2_name = parts[2]
                        arg1 = prover.get_argument(arg1_name)
                        arg2 = prover.get_argument(arg2_name)
                        if arg1 and arg2:
                            combined_arg = arg1.chain(arg2)
                            if combined_arg:
                                prover.register_argument(combined_arg)
                                logger.info("Arguments '%s' and '%s' chained into '%s'.",
                                            arg1_name, arg2_name, combined_arg.name)
                            else:
                                logger.error("Failed to chain arguments.")
                        else:
                            logger.error("One or both arguments not found.")
                    else:
                        # Execute other commands
                        try:
                            output = prover.send_command(command)
                        except ProverError as e:
                            if strict:
                                if isolate:
                                    try:
                                        prover.close()
                                    except Exception:
                                        pass
                                else:
                                    prover.echo_notes = prev_echo
                                logger.info("Finished script %s", script_path)
                                raise ProverError(f"{script_path}:{lineno}: {e}") from e
                            logger.error("Prover error: %s", e)
                            if stop_on_error:
                                break
                        except MachinePayloadError as e:
                            if strict:
                                if isolate:
                                    try:
                                        prover.close()
                                    except Exception:
                                        pass
                                else:
                                    prover.echo_notes = prev_echo
                                logger.info("Finished script %s", script_path)
                                raise MachinePayloadError(f"{script_path}:{lineno}: {e}") from e
                            logger.error("Prover error (no machine payload): %s", e)
                            if stop_on_error:
                                break
                        # print(output)
    # restore/close and announce completion
    if isolate:
        try:
            prover.close()
        except Exception:
            pass
    else:
        prover.echo_notes = prev_echo
    logger.info("Finished script %s", script_path)


def interactive_mode(prover: ProverWrapper) -> None:
    """Enables command line interaction with the wrapper.
        
        Syntax for commands: 
          - All fellowship commands;
          - (Custom) tactics: "tactic <TacticName> <Args>"
          - Arguments: "start argument / end argument";
          - Executing/Reducing an argument : "reduce <ArgName>"
          - Normalize an argument (silent version of reduce): "normalize <ArgName>" 
          - Chaining (Grafting) two arguments "chain <Arg1> <Arg2>" (Arg1 is rootstock, Arg2 is scion)
          - Rendering arguments (unreduced term, normal form, respectively): "render <Arg>", "render-nf <Arg>".
          - TODO: undercut, support, rebut.

        #TODO: implement human-oriented REPL output.
    """
    
    recording = False
    current_argument = None
    try:
        while True:
            try: 
                command = input('Enter command (or "exit" to quit): ').strip()
            except EOFError:
                print("\nEOFError: No input detected. Exiting interactive mode.")
                break
            # Remove trailing dots and whitespace
            #command = command.rstrip('.').strip()
            if command.lower() in ['exit', 'quit']:
                break
            elif command.startswith("reduce "):
                reduce_argument_cmd(prover, command.split(maxsplit=1)[1])
            elif command.startswith("render-nf "):
                    render_argument_cmd(prover, command.split(maxsplit=1)[1], True)
            elif command.startswith("render "):
                    render_argument_cmd(prover, command.split(maxsplit=1)[1], False)
            elif command.startswith("normalize "):
                name = command.split(maxsplit=1)[1]
                arg = prover.get_argument(name)
                if arg:
                    logger.info("Normalized argument '%s'; normal form cached in .normal_form", name)
                    arg.normalize(); print("normal form stored in .normal_form")
                else:
                    print(f"Argument '{name}' not found.")
                    logger.warning("Argument '%s' not found for normalization (interactive)", name)

            elif command.startswith('start argument '):
                # Parse the start argument command
                if recording:
                    print("Already recording an argument. Please end the current recording first.")
                    continue
                parts = command.split(' ', 3)
                if len(parts) < 4:
                    print("Invalid command. Use: start argument name conclusion")
                    continue
                name = parts[2]
                conclusion = parts[3].strip()
                current_argument = {
                    'name': name,
                    'conclusion': conclusion,
                    'instructions': []
                }
                recording = True
                print(f"Started recording argument '{name}' with conclusion '{conclusion}'.")
                logger.info("Started recording argument '%s' with conclusion '%s'.", name, conclusion)
                output = prover.send_command(f'theorem {name} : ({conclusion}).')
                # print(output)
            elif command == 'end argument':
                if not recording:
                    print("Not currently recording an argument.")
                    continue
                prover.send_command('discard theorem.')
                # Create and execute the argument
                arg = Argument(
                    prover,
                    name=current_argument['name'],
                    conclusion=current_argument['conclusion'],
                    instructions=current_argument['instructions']
                )
                arg.execute()
                # Store the argument for later use
                prover.register_argument(arg)
                print(f"Argument '{arg.name}' saved with conclusion '{arg.conclusion}'.")
                logger.info("Argument '%s' saved with conclusion '%s'.", arg.name, arg.conclusion)
                # Reset recording state
                recording = False
                current_argument = None
            elif recording:
                # Record the command as part of the argument
                if command:
                    if command.startswith('tactic '):
                        # Handle custom tactic invocation during recording
                        parts = command.split()
                        tactic_name = parts[1]
                        tactic_args = parts[2:]  # Remaining parts are arguments to the tactic
                        output = prover.execute_tactic(tactic_name, *tactic_args)
                        # print(output)
                        # Record the tactic command as part of the instructions
                        current_argument['instructions'].append(command)
                    else:
                        # Execute the command and record it
                        output = prover.send_command(command)
                        # print(output)
                        current_argument['instructions'].append(command)
            else:
                # Normal command execution
                if command.startswith('argument '):
                    # Handle argument definitions in one go
                    # Parse the argument definition
                    # Format: argument name conclusion instructions
                    # Example: argument argA "A" "axiom axA;"
                    parts = command.split(' ', 2)
                    if len(parts) < 3:
                        print("Invalid argument definition. Use: argument name conclusion instructions")
                        continue
                    name = parts[1]
                    rest = parts[2]
                    try:
                        conclusion_part, instructions_part = rest.split('"', 2)[1], rest.split('"', 2)[2]
                        conclusion = conclusion_part.strip()
                        instructions = [instr.strip() for instr in instructions_part.strip().split(';') if instr.strip()]
                        arg = Argument(prover, name, conclusion, instructions)
                        arg.execute()
                        print(f"Argument '{name}' defined with conclusion '{conclusion}'.")
                        logger.info("Argument '%s' defined with conclusion '%s'.", name, conclusion)
                    except Exception as e:
                        print(f"Error parsing argument: {e}")
                        logger.error("Error parsing argument '%s': %s", name, e)
                elif command.startswith('tactic '):
                    # Handle custom tactic invocation outside of recording
                    parts = command.split()
                    tactic_name = parts[1]
                    tactic_args = parts[2:]  # Remaining parts are arguments to the tactic
                    output = prover.execute_tactic(tactic_name, *tactic_args)
                    # print(output)
                elif command.startswith('chain'):
                    # Handle chaining of arguments
                    # Format: chain arg1 arg2
                    parts = command.split()
                    if len(parts) != 3:
                        print("Invalid chain command. Use: chain arg1 arg2")
                        continue
                    arg1_name = parts[1]
                    arg2_name = parts[2]
                    arg1 = prover.get_argument(arg1_name)
                    arg2 = prover.get_argument(arg2_name)
                    if arg1 and arg2:
                        combined_arg = arg1.chain(arg2)
                        if combined_arg:
                            prover.register_argument(combined_arg)
                            print(f"Arguments '{arg1_name}' and '{arg2_name}' chained into '{combined_arg.name}'.")
                            logger.info("Arguments '%s' and '%s' chained into '%s'.", arg1_name, arg2_name, combined_arg.name)
                        else:
                            print("Failed to chain arguments.")
                            logger.error("Failed to chain arguments in interactive mode")
                    else:
                        print("One or both arguments not found.")
                        logger.error("One or both arguments not found in interactive mode")
                else:
                    # Execute the command normally
                    output = prover.send_command(command)
                    # print(output)
    finally:
        prover.close()
                
class Argument:
    r"""Implementation of deductive arguments in \bar{\lamda}\mu\tilde{\mu} calculus. Atomic (counter-)arguments are terms of the (co-)intuitionistic (or (co-)minimal) fragment of the calculus enriched with default assumptions. Argumentation terms are constructed from atomic arguments and the undercut, rebut, support and chain operators.

       Argumentation terms are in normal form if they are \bar{\lamda}\mu\tilde{\mu} normal forms. Eta reduction is optional (TODO). 

Currently, a normalization of an argumentation Arg about issue A returns a non-affine term for A iff the top-level argument for A is skeptically accetable under admissible semantics (?) in the abstract argumentation framework induced by Arg.
    
    
    """
    
    def __init__(self, prover, name: str, conclusion: str, instructions: list = None, rendering = "argumentation", enrich : str = "PROPS" ):
        self.prover = prover
        self.name = name
        self.conclusion = conclusion #Conclusion of the argument.
        self.instructions = instructions  # List of instruction strings
        self.assumptions = {}  # Dict of Assumptions: {goal_number : {prop : some_str, goal_index : some_int, label : some_str}
        self.labelling = False # Deprecated: Flag to check if argument has been labelled.
        self.proof_term = None  # To store the proof term if needed.
        self.enriched_proof_term = None # To store an enriched and/or rewritten proof term if needed.
        self.body = None # parsed proof term created from proof_term
        self.rendering = rendering # Which rendering should be generated?
        self.enrich = enrich # Proof term enriched with additional type information
        self.representation = None # Natural language representation of the argument based on choice of rendering
        self.executed = False  # Flag to check if the argument has been executed
        self.attacks = {}  # Deprecated: Dict of self-attacks with corresponding subargument
        self.normal_body           = None  # reduced AST (deep‑copy)
        self.normal_form           = None  # Normalized proof term.
        self.normal_representation = None  # NL rendering of normal form.
        self.neg_norm_body = None # Stores the negation-normalized body (Workaround for Counterarguments in Fellowship).
        self.neg_norm_form = None # Negation-normalized proof term.
        
    def execute(self) -> None:
        """Sends the sequence of instructions corresponding to an argument (possibly generated from its body) to the Fellowship prover and populates the argument's proof term, body, assumptions, and renderings from the prover output. Can be used for type-checking (TODO).
        """
        if self.executed:
            logger.warning("Argument '%s' has already been executed.", self.name)
            return
        if self.instructions == None:
            if self.body == None:
                raise Exception("Argument instructions and body missing.")       
            else:
                self.enrich_props()
                generator = parser.InstructionsGenerationVisitor()
                self.instructions = generator.return_instructions(self.body)
        # Start the theorem
        output = self.prover.send_command(f'theorem {self.name} : ({self.conclusion}).')
        # Execute each instruction
        for instr in self.instructions:
            if instr.startswith('tactic '):
                # Handle custom tactic invocation within argument execution
                parts = instr.split()
                tactic_name = parts[1]
                tactic_args = parts[2:]  # Remaining parts are arguments to the tactic
                output = self.prover.execute_tactic(tactic_name, *tactic_args)
                #output += output_instr
            else:
                output = self.prover.send_command(instr.strip() + '.')
                logger.debug("Prover output: %s", output)
                #output += output_instr
            #print(output)
        # Capture the assumptions (open goals)
        self._parse_proof_state(output)
        # Parse proof term
        grammar = parser.Grammar()
        # parser = Lark(proof_term_grammar, start='start')
        parsed = grammar.parser.parse(self.proof_term)
        transformer = parser.ProofTermTransformer()
        self.body = transformer.transform(parsed)
        #Generate natural language representation
        if self.rendering == "argumentation":
            self.representation = parser.pretty_natural(self.body, parser.natural_language_argumentative_rendering)
        elif self.rendering == "dialectical":
            self.representation = parser.pretty_natural(self.body, parser.natural_language_dialectical_rendering)

        elif self.rendering == "intuitionistic":
            self.representation = parser.pretty_natural(self.body, parser.natural_language_rendering)
        # Discard the theorem to prevent closing it (since it may have open goals)
        self.prover.send_command('discard theorem.')
        logger.info("Argument '%s' executed", self.name)
        if self.enrich == "PROPS":    
            self.enrich_props()
            self.generate_proof_term()
        self.executed = True

    
    @staticmethod
    def _normalize_pt_to_unicode(pt_ascii: str) -> str:
        """Map Fellowship's ASCII fallbacks to the Unicode tokens your
        parser expects.  The examples observed were:
          • 'μ'   encoded as ";:"   (two chars)
          • 'μ̃'  (mu-tilde) encoded as ";:'" or ";:\'"
          • 'λ'   encoded as '\\' (backslash)
        Adjust if your printer uses slightly different sentinels.
        """
        s = pt_ascii
        # unify the two spellings we saw for mu-tilde first
        s = s.replace(";:\'", "μ'" )
        s = s.replace(";:'",  "μ'" )   # plain apostrophe variant
        # plain mu next (do *after* mu-tilde)
        s = s.replace(";:",   "μ")
        # lambda
        s = s.replace("\\",   "λ")
        s = s.replace("~", "¬")
        s = s.replace("false", "⊥")
        return s
    
    @staticmethod
    def _unquote(atom: Any) -> Any:
        """Strip surrounding double quotes from atoms like '"A"'."""
        if isinstance(atom, str) and len(atom) >= 2 and atom[0] == '"' and atom[-1] == '"':
            return atom[1:-1]
        return atom

    def _parse_proof_state(self, proof_state: Dict[str, Any]) -> None:
        """
        Build {goal_number: {"prop": some_str, "label": None}} from the
        machine payload dict produced by the wrapper.

        - goal_number := value of (meta ...)
        - prop        := value of (active-prop ...) (quotes removed)
        """
        self.proof_term = self._normalize_pt_to_unicode(proof_state.get('proof-term').strip('"'))
        res = {}
        goals = proof_state.get('goals')
    

        # Normalize: 'goals' may be a single `(goal ...)` list or a list of them.
        def _as_goal_list(gval):
            if isinstance(gval, list) and gval and gval[0] == 'goal':
                return [gval]
            if isinstance(gval, list):            
                #return [g for g in gval if isinstance(g, dict)]
                return gval
            return []

        goal_entries = _as_goal_list(goals)


        for g in goal_entries:
        #for g in goals:
            attrs = {}
            for item in g[1:]:
                if isinstance(item, list) and len(item) == 2:
                    k, v = item
                    attrs[k] = self._unquote(v)
            meta = self._unquote(attrs.get('meta')) if 'meta' in attrs else None
            prop = self._unquote(attrs.get('active-prop')) if 'active-prop' in attrs else None
            if meta and prop is not None:
                res[meta] = {"prop": prop, "label": None}
        self.assumptions = res

    # def extract_assumptions(self, output):
    #     #print("Match conclusion to assumption.")
    #     # Parse the output to find open goals (assumptions)
    #     preassumptions = []
    #     # temporarily ignore past proof steps. THIS IS DANGEROUS. While idtac should not change anything, it is risky to change the prover state merely to get a copy of the last output again.
    #     temp_output = self.prover.send_command(f'idtac.', 1)
        
    #     # Find the number of goals
    #     num_goals = self.prover.parse_proof_state(temp_output)['goals']
    #     if num_goals > 0 :
    #         # num_goals = int(goals_match.group(1))
    #         # Extract each goal
    #         goal_pattern_proof = re.compile(r'\|-----\s*(\d\.*.*\d*)\s*(\s*[^:]*:[^\s]*\s*)*\*:([^\s]*)')
    #         goal_pattern_refu = re.compile(r'\*:([^\s]*)\s*(\s*[^:]*:[^\s]*\s*)*\|-----\s*(\d\.*.*\d*)')

    #         match = goal_pattern_proof.search(temp_output)
    #         #print("matching")
    #         #print(match)
    #         antimatch = goal_pattern_refu.search(temp_output)
    #         # print("HERE!")
    #         print(" Match, Antimatch", match, antimatch)
    #         preassumptions = [(match.group(3).strip(), 0, match.group(1).strip())] if match else [(antimatch.group(1).strip()+'_bar', 0, antimatch.group(3).strip())]
    #         i=1
    #         print(" Preassumptions", preassumptions)
    #         #Test the while loop, may be buggy.
    #         while i<num_goals:
    #             #print(i)
    #             #print('WARNING')
    #             temp_output = self.prover.send_command('next.')
    #             #print(temp_output)
    #             plus_match = goal_pattern_proof.search(temp_output)
    #             plus_antimatch = goal_pattern_refu.search(temp_output)
    #             print("Test")
    #             test = goal_pattern_refu.search('2 goals yet to prove! \n *:A \n issue:A \n |-----  1.1.2 ')
    #             print(test)
    #             print(plus_match)
    #             print('ANTI')
    #             print(plus_antimatch)
    #             #print('append attempt')
    #             #print(plus_antimatch.group(1).strip()+'_bar')
    #             if plus_match:
    #                 preassumptions.append((plus_match.group(3).strip(), i, plus_match.group(1).strip()))
    #             else:
    #                 preassumptions.append((plus_antimatch.group(1).strip()+'_bar', i, plus_antimatch.group(3).strip()))
    #             #print(assumptions)
    #             i+=1
    #     assumptions = {}
    #     assumptions = {number.strip() : { "prop" : assumption_text.strip(), "index" : goal_index, "label" : None } for (assumption_text, goal_index, number) in preassumptions}
    #     print("Assumptions extracted:")
    #     print(assumptions)
    #     return assumptions

    def enrich_props(self) -> None:
        """
        Use PropEnrichmentVisitor with assumption_mapping = self.assumptions
        and axiom_props = self.axiom_props to fill .prop for each node.
        """
        from parser import PropEnrichmentVisitor
        logger.info("Enriching argument '%s'.", self.name)
        logger.debug("Declarations: %r", self.prover.declarations)
        visitor = PropEnrichmentVisitor(assumptions=self.assumptions,
                                        axiom_props=self.prover.declarations)
        self.body = visitor.visit(self.body)
        logger.info("Argument '%s' enriched.", self.name)
        return

    def generate_proof_term(self) -> None:
        """
        Use ProofTermGenerationVisitor to generate the string representation of an enriched proof term.
        """
        logger.info("Starting proof term generation for argument '%s'", self.name)
        from parser import ProofTermGenerationVisitor
        visitor = ProofTermGenerationVisitor()
        self.body = visitor.visit(self.body)
        self.enriched_proof_term = self.body.pres
        logger.info("Finished proof term generation for argument '%s'", self.name)
        return
        

    def label_own_assumptions(self):
        """Deprecated: Label the assumptions of an argument as IN, OUT, or UNDEC. This is currently not needed/was replaced by the reduction system.
        """
        for key in self.assumptions:
            self.assumptions[key]["label"] = parser.label_assumption(self.body, self.assumptions[key]["prop"],self.assumptions)
            self.labelling = True
        # self.labelling = {assumption_text.strip() :  parser.label_assumption(self.body, assumption_text.strip(), self.assumptions)for (assumption_text, goal_index, number) in self.assumptions}
        return 

    def check_self_attacks(self):
        """Deprecated: Checks consistency up to (i.e. without) logical inference. Self-attacks are simply handled by the reduction system."""
        # Self-attacks are cycles of length 1. This implies that the argument is hopeless/can never be accepted.
        # Generally, a proposition A is skeptically acceptable in a set S, if there is an exhaustive (all possible counterarguments refuted) non-cyclical argument from S for A.
        # It is credulously acceptable if there is an exhaustive even-cyclical argument from S for A.
        # Compute fixpoint of supporting arguments (upper bound) counterarguments (lower bound).
        # Generate all normal-form supporting arguments (counterarguments);
        # Start with atomic arguments (alternative proofs, internally no affine mus/mutildes).
        # Label, check for cycles.
        # Then generate all counterarguments for each argument (alternative proofs, undercut).
        # Label, check for cycles.
        # Iteratively update sets of accepted supporting arguments (counterarguments).
        # If not labelled, label.

        # If not activity-labelled, activity label.
        # If any active assumption is out, the argument is self-attacking.
        # Start naively: all assumptiosn are active. No supports, no attacks.

        #An alternative approach to self-attacks: restrict interaction of mu/lambda with defaults. No affine mu/lambda abstractions over default contradictions (with no other open assumptions) in atomic arguments (See notes).
        if self.labelling == True:
            print("ALREADY LABELLED")
            pass
        else:
            print("labelling...")
            self.label_own_assumptions()
        # for assumption in labelling:
        #     if assumption[value]=="OUT":
        #         # Get subargument:
                
        #         subargument = self.body
        #         self.attacks[assumption]=
        #     else:
        print(self.assumptions)
        for key in self.assumptions:
            if self.assumptions[key]["label"][0]=="OUT":
                self.assumptions[key]["attackers"] = "Sth"
        return
         
    def pop_arg(self, subargument):
        """Deprecated: Stash the current argument and pop a subargument. Currently all argumentation functionality is bottom-up, proceeding from a known conclusion. Investigating a top-down exploratory mode is future work. """
        # Execute args if necessary.
        if not self.executed:
            self.execute()
        if not subargument.executed:
            subargument.execute()
        print(self.body)
        print(subargument.body)
        if parser.is_subargument(self.body, subargument.body) == False:
            raise Exception("Not a subargument.")
        else:
            # Get the correct assumption (wlog we use the first assumption of the subargument).
            for key in subargument.assumptions:
                if subargument.assumptions[key]["index"] == 0:
                    pop_assumption = subargument.assumptions[key]["prop"]
            adapter = Argument(self.prover, f'pop_assumption_{pop_assumption}_of_{self.name}', pop_assumption, [f'tactic pop {pop_assumption} {self.conclusion} False'])
            print("Executing Adapter")
            adapter.execute()
            print(adapter.assumptions)
            print(self.conclusion)
            popped_arg = self.chain(adapter)
            final_arg = popped_arg.chain(subargument, True)
            return final_arg

    def match_conclusion_assumptions(self, conclusion: str, assumptions: dict) -> Optional[str]:
        matching_assumption = None
        for key in assumptions:
            logger.debug("Checking assumption key=%s", key)
            if conclusion.replace("~", "¬") in assumptions[key]["prop"]:
                matching_assumption = key
                logger.debug("Match found: %s", matching_assumption)
                break
        return matching_assumption


    def chain(self, other_argument: "Argument", close: bool = False) -> "Argument":
        """ Chains the current argument to another argument by using it to prove an assumption of the other argument. Automatically finds the matching assumption (assuming there is only one)."""
        #TODO: Implement type checking for resulting argument.
        # Check if this argument's conclusion matches any of the other argument's assumptions
        if not self.executed:
            self.execute()
        if not other_argument.executed:
            other_argument.execute()
        # Combine the instructions and navigate to the correct goal
        combined_name = f"{self.name}_{other_argument.name}"
        combined_conclusion = other_argument.conclusion
        #combined_instructions = []
        if self.neg_norm_body:
            logger.debug("Using negation‑normalized body: %r", self.neg_norm_body)
            logger.debug("Other argument assumptions: %r", other_argument.assumptions)
            combined_body = parser.graft_uniform(other_argument.body,  self.neg_norm_body)
        else:
            combined_body = parser.graft_uniform(other_argument.body,  self.body)
        logger.debug("Assumptions (self, other): %r ; %r",
                     self.assumptions, other_argument.assumptions)
        enricher = parser.PropEnrichmentVisitor(assumptions=other_argument.assumptions,
                                        axiom_props=self.prover.declarations)
        ptgenerator = parser.ProofTermGenerationVisitor()
        generator = parser.InstructionsGenerationVisitor()
        combined_body = ptgenerator.visit(enricher.visit(combined_body))
        combined_instructions = generator.return_instructions(combined_body)
        # Optionally close available goals using assumptions propagated from other to self:
        if close==True:
            # This is an ugly and brittle hack. Will figute out something better.
            combined_instructions.extend([f'axiom.', f'next.']*(len(self.assumptions)+2))
        # Create a new Argument instance
        combined_argument = Argument(self.prover, combined_name, combined_conclusion, combined_instructions)
        # Execute the combined argument
        combined_argument.execute()
        
        return combined_argument

    def undercut(self, other_argument):
        print ("Deprecated!")
        # Forward, exploratory version of undercut were the attacked argument is stashed.
        if not self.executed:
            self.execute()
        if not other_argument.executed:
            other_argument.execute()
        #Find the assumption that is to be attacked. This does not currently account for multiple assumptions of the same type.
        attacked_assumption = None
        for key in other_argument.assumptions:
            if self.conclusion.strip('~') in other_argument.assumptions[key]["prop"]:
                attacked_assumption = key
                break
        #The conclusion of the new argument is going to be the conclusion of the current argument.
        #print(attacked_assumption)

        # check if attack is not blocked. Or not? Allow endless oscillation? Even cycles should be acceptable? Determining if an assumption is blocked is itself a provability-complete problem, if arbitrary assumptions are permitted. Two solutions: a) no blocking for attacks, only block popping defeated subproofs; b) arbitrarily cut off blocking at e.g. equality. I.e. we only look for instances of the blocked formula itself. We choose option a) for now, but option b) may be required to ensure termination (or some other tie breaking mechanism).
        
        #invert the conclusion:
        issue = self.conclusion.strip('~')
        temp_concl_arg = Argument(self.prover, f'undercuts_on_{other_argument.assumptions[attacked_assumption]["prop"]}', self.conclusion, [f'elim.', f'cut ({issue}) issue', f'tactic pop {other_argument.assumptions[attacked_assumption]["prop"]} {other_argument.conclusion}', f'cut (~{issue}) adapter', f'next', f'elim', f'axiom'])
        temp_concl_arg.execute()
        #Chain the other argument to the adapter.
        adapted_argument = other_argument.chain(temp_concl_arg, close=True)
        print("ADAPTED")
        #print(adapted_argument)
        final_argument = self.chain(adapted_argument, close=True)
        return final_argument

    def resolve_attacked_assumption(self) -> str:
        """
        Decide which assumption an argument with conclusion *self.conclusion*
        is meant to attack.

        There are two cases:

        1.  **Pure outer negation** – the whole conclusion is a negation
            “~φ” (or “¬φ”).  In that case the attacked assumption is *φ*.

        2.  **Anything else**       – the conclusion is *not* merely a
            top-level negation (examples:  “φ”,  “~φ -> ψ”,  “ψ ∧ ¬φ”, …).
            In that case we follow the wrapper convention and attack the
            *barred* form, i.e. we return “φ_bar”.

        Precedence is handled in a minimal but robust way: we only regard
        the negation as *outer* if after stripping the leading ‘~’/‘¬’
        **no binary connective appears at the top level**.
        """

        def _strip_parens(s: str) -> str:
            """Remove one pair of *outer* parentheses ( ( … ) )."""
            while s.startswith('(') and s.endswith(')'):
                depth = 0
                balanced = True
                for i, ch in enumerate(s):
                    if ch == '(':
                        depth += 1
                    elif ch == ')':
                        depth -= 1
                        if depth == 0 and i != len(s) - 1:
                            balanced = False
                            break
                if balanced:
                    s = s[1:-1].strip()
                else:
                    break
            return s

        def _has_top_level_binary(s: str) -> bool:
            """Is there a ‘->’ (or other binary op) at paren-depth 0?"""
            depth = 0
            i = 0
            while i < len(s) - 1:
                ch = s[i]
                if ch == '(':
                    depth += 1
                elif ch == ')':
                    depth -= 1
                elif depth == 0 and s[i:i+2] == '->':
                    return True
                i += 1
            return False

        concl = self.conclusion.strip()
        concl = _strip_parens(concl)

        # Case 1 – try to recognise an outer ‘~’ / ‘¬’
        if concl.startswith(('~', '¬')):
            inner = concl[1:].strip()
            inner = _strip_parens(inner)
            if not _has_top_level_binary(inner):
                # genuine outer negation
                return inner

        # Case 2 – default: attack barred form
        return f"{concl}_bar"

    def focussed_undercut(self, other_argument: "Argument", *, on: str = "GoFigure", negated_attacker: bool = True) -> "Argument":
        from parser import Mu, Mutilde, Goal, Laog, ID, DI
        if on == "GoFigure":
            on = self.resolve_attacked_assumption()
            #print("ON", on)
        # arguments.executed:
        if not self.executed:
            self.execute()
        if negated_attacker:
            logger.info("Turning negated attacker into counterargument")
            self.negation_norm_body = self.negation_normalize()
            #print(self.neg_norm_body)
        if not other_argument.executed:
            other_argument.execute()

        #Find the assumption that is to be attacked. The uniiform graft method is used, so it does not matter which assumption is chosen if there are multiple ones with the same prop.
        attacked_assumption = None
        for key in other_argument.assumptions:
            if on in other_argument.assumptions[key]["prop"]:
                logger.debug("Found attacked assumption key=%s", key)
                attacked_assumption = key
                break
        issue = other_argument.assumptions[attacked_assumption]["prop"]
        logger.debug("Attacked issue: %s", issue)
        logger.debug("Building adapter body")
        if issue.endswith('_bar'): #This still needs to be tested.
            issue[:-4]
            #TODO (I think this TODO is deprecated? Check.)
            print("Not implemented yet.")
            pass
            #adaptercontext = Mutilde(DI("aff", issue), issue, Goal("2",issue), Laog("3",issue))
            #adapterterm = Mu(ID("aff", issue), issue, Goal("1", issue), ID("alt",issue))
            #adapterbody = Mu(ID("alt", issue), issue, adapterterm, adaptercontext)

        else:
            #BUG: This will fail if variable names are not fresh. Need to implement helper that generates fresh  variable names.
            adaptercontext = Mutilde(DI("aff", issue), issue, Goal("2", issue), Laog("3", issue))
            adapterterm = Mu(ID("aff", issue), issue, Goal("1",issue), ID("alt",issue))
            adapterbody = Mu(ID("alt", issue), issue, adapterterm, adaptercontext)
        adapter_arg = Argument(self.prover, f'undercuts_on_{other_argument.assumptions[attacked_assumption]["prop"]}', issue)
        logger.debug("Building adapter body")
        adapter_arg.body = adapterbody
        adapter_arg.execute()
        #print("Adapter_arg proof term",adapter_arg.proof_term)
        #print("Other arg proof term", other_argument.proof_term)
        adapted_argument = adapter_arg.chain(other_argument)
        logger.debug("Adapter argument constructed: %s", adapted_argument.proof_term)
        final_argument = self.chain(adapted_argument)
        return final_argument
    
    def get_assumptions(self) -> list[str]:
        if not self.executed:
            self.execute()
        return [self.assumptions[key]["prop"] for key in self.assumptions]

    def get_conclusion(self) -> str:
        return self.conclusion

    def support(self, other_argument: "Argument") -> "Argument":
        #Akin to chain but instead of filling in an assumption, provide an alternative proof for the assumption. Could be generalized to provide an alternative proof for any intermediate derivation of an argument.
        from parser import Mu, Mutilde, Goal, Laog, ID, DI
        if not self.executed:
            self.execute()
        if not other_argument.executed:
            other_argument.execute()
                # Find if other_argument has an assumption that matches self.conclusion

        matching_assumption = self.match_conclusion_assumptions(self.conclusion, other_argument.assumptions)
        issue = other_argument.assumptions[matching_assumption]["prop"]
        logger.debug("Issue: %s", issue)
        # The adapter

        adaptercontext = Mutilde(DI("aff2", issue), issue, Goal("2", issue), ID("alt1", issue))
        adapterterm = Mu(ID("aff1", issue), issue, Goal("1"), ID("alt1",issue))
        adapterbody = Mu(ID("alt1", issue), issue, adapterterm, adaptercontext)
        alternative_proof_adapter = Argument(self.prover, f'supports_on_{other_argument.assumptions[matching_assumption]["prop"]}', other_argument.assumptions[matching_assumption]["prop"])
        logger.debug("Building adapter body")
        alternative_proof_adapter.body = adapterbody

                                             #[f'cut ({other_argument.assumptions[matching_assumption]["prop"]}) support.', f'cut ({other_argument.assumptions[matching_assumption]["prop"]}) alt1.', f'next.', f'axiom support.', f'cut ({other_argument.assumptions[matching_assumption]["prop"]}) alt2.', f'next.', f'axiom support.'])
        alternative_proof_adapter.execute()
        adapted_argument =  alternative_proof_adapter.chain(other_argument)
        logger.debug("Adapter argument constructed: %s", adapted_argument.proof_term)
        final_argument = self.chain(adapted_argument)
        return final_argument

    def normalize(self, enrich: bool = True) -> parser.ProofTerm:
        """Compute and cache the normal form *(body, term, rendering) without mutating *self.body*."""
        if not self.executed:
            self.execute()
        #if self.normal_body is not None:
         #   return self.normal_body

        # 1. deep‑copy then reduce
        red_ast = copy.deepcopy(self.body)
        red_ast = parser.ArgumentTermReducer().reduce(red_ast)

        # 2. optionally enrich props/types
        if enrich:
            red_ast = parser.PropEnrichmentVisitor(assumptions=self.assumptions,
                                             axiom_props=self.prover.declarations).visit(red_ast)

        # 3. generate textual proof term
        red_ast = parser.ProofTermGenerationVisitor().visit(red_ast)
        self.normal_body = red_ast
        self.normal_form = red_ast.pres

        # 4. natural‑language rendering
        style = {
            "argumentation": parser.natural_language_argumentative_rendering,
            "dialectical":   parser.natural_language_dialectical_rendering,
            "intuitionistic": parser.natural_language_rendering,
        }[self.rendering]
        self.normal_representation = parser.pretty_natural(red_ast, style)
        return self.normal_body

    def render(self, normalized: bool = False) -> Optional[str]:
        """Return NL rendering.  If *normalized* is True, ensure normal form
        is computed first."""
        if normalized:
            #if self.normal_representation is None:
             #   self.normalize()
            return self.normal_representation
        else:
            if self.representation is None:
                self.execute()          # fills original representation
            return self.representation

    def reduce(self) -> None:
        """Normalise and print proof term + NL; keep originals intact."""
        logger.info("Starting argument reduction for '%s'", self.name)
        self.normalize()
        logger.info("Reduction finished for argument '%s'", self.name)
        logger.debug("Normal‑form proof term: %s", self.normal_form)
        logger.debug("NL rendering: %s", self.normal_representation)
        print("— reduction finished —\n")
        print("Normal‑form proof term:\n", self.normal_form)
        print("\nNatural language:\n", self.normal_representation)

    def negation_normalize(self) -> parser.ProofTerm:
        """
        Replace the outer ¬-introduction wrapper by its inner command.

        On success the fields
        • self.neg_norm_body
        • self.neg_norm_form
        are filled.

        If the pattern is *not* present we raise a ValueError.
        """
        from parser import NegIntroRewriter
        logger.info("Negation normalizing argument '%s'", self.name)
        if not self.executed:
            self.execute()
        logger.debug("Original proof term: %s", self.proof_term)
        rewriter = NegIntroRewriter()
        new_body = rewriter.rewrite(self.body)

        if not rewriter.changed:
            raise ValueError("negation_normalize: argument is not in the expected "
                                 "outer negation-introduction form")

        # cache results -----------------------------------------------------
        self.neg_norm_body = new_body
        # regenerate textual & NL forms – re-use existing visitors
        gen = parser.ProofTermGenerationVisitor()
        new_body = gen.visit(copy.deepcopy(new_body))
        self.neg_norm_form = new_body.pres
        logger.debug("Negation‑normalized proof term: %s", self.neg_norm_form)
        self.neg_norm_representation = parser.pretty_natural(
            new_body,
            {
                "argumentation": parser.natural_language_argumentative_rendering,
                "dialectical":   parser.natural_language_dialectical_rendering,
                "intuitionistic":parser.natural_language_rendering
            }[self.rendering]
        )
        return self.neg_norm_body
# ---------------------------------------------------------------------------
#  CLI helper commands                                                       
# ---------------------------------------------------------------------------

def setup_prover() -> ProverWrapper:
    env = os.environ.copy()
    env.setdefault("FSP_MACHINE", "1")
    prover = ProverWrapper('./fsp', env=env)
    prover.register_custom_tactic('pop', pop)
    # Switch to classical logic
    #prover.send_command('lk.')
    # Declare some booleans to work with.
    #prover.send_command('declare A,B,C,D:bool.')
    logger.info("Prover decls %r", prover.declarations)
    return prover


def reduce_argument_cmd(prover: ProverWrapper, name: str) -> None:
    """ CLI for Argument Reduction """
    arg = prover.get_argument(name)
    if not arg:
        print(f"Argument '{name}' not found.")
        return
    print(f"Reducing argument {arg.name} with proof term {arg.proof_term}")
    arg.reduce()

def render_argument_cmd(prover: ProverWrapper, name: str, normalized: bool = False) -> None:
    """ CLI for Argument Rendering. The normalized proof term can be selected."""
    arg = prover.get_argument(name)
    if not arg:
        print(f"Argument '{name}' not found.")
        return
    if normalized==True:
        print(f"Rendering argument {arg.name} in normal form.")
    else:
        print(f"Rendering argument {arg.name}")
    print(arg.render(normalized=normalized))

def main() -> None:
    ap = argparse.ArgumentParser(
        prog='fellowship-wrapper',
        description='Run Fellowship prover in interactive or batch mode')
    g = ap.add_mutually_exclusive_group(required=True)
    g.add_argument('--interactive', action='store_true',
                   help='start an interactive Fellowship REPL')
    g.add_argument('--script', metavar='FILE',
                   help='execute commands in FILE (same grammar as interactive mode)')
    args = ap.parse_args()

    # Configure root logger if no handlers (so INFO/ERROR show up by default)
    if not logging.getLogger().handlers:
        level_name = os.getenv("FSP_LOGLEVEL", "WARNING").upper()
        logging.basicConfig(
            level=getattr(logging, level_name, logging.WARNING),
            format="%(levelname)s:%(name)s:%(message)s"
        )

    prover = setup_prover()             # ⇐ creates the ProverWrapper, 
                                        #    registers pop, declares A,B,C,D …

    if args.interactive:                # --- REPL -----------------
        interactive_mode(prover)

    else:                               # --- batch ----------------
        script = Path(args.script).expanduser()
        if not script.is_file():
            ap.error(f'script file {script} does not exist')
        execute_script(prover, script)

if __name__ == '__main__':
    main()

        
