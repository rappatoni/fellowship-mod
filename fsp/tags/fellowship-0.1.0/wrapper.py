from __future__ import annotations
import os
import sys
import pexpect
import re
from pathlib import Path
import argparse
import copy
from typing import Any, List, Tuple, Optional, Dict, Callable
import tempfile
import shutil

from sexp_parser import SexpParser
from pres.gen import ProofTermGenerationVisitor
from pres.nl import (
    Rendering_Semantics,
    pretty_natural,
    natural_language_rendering,
    natural_language_dialectical_rendering,
    natural_language_argumentative_rendering,
)
from pres.color import pretty_colored_proof_term
from pres.tree import render_acceptance_tree_dot

from wrap.prover import ProverWrapper, ProverError, MachinePayloadError
from core.dc.argument import Argument
import logging
from pexpect.exceptions import EOF as PexpectEOF, TIMEOUT as PexpectTIMEOUT

logger = logging.getLogger('fsp.wrapper')
logger.propagate = True

# Install TRACE level (below DEBUG) and a Logger.trace() method
TRACE = 5
logging.addLevelName(TRACE, "TRACE")
def _trace(self, msg, *args, **kwargs):
    if self.isEnabledFor(TRACE):
        self._log(TRACE, msg, args, **kwargs)
logging.Logger.trace = _trace

def configure_logging_cli(level_name: Optional[str] = None, log_file: Optional[str] = None) -> None:
    """
    Configure root logger for CLI usage:
      - Level from explicit argument or env FSP_LOGLEVEL (default INFO)
      - Message-only format
      - Stream to stdout
      - Avoid duplicate handlers if already configured
    """
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
            if command.startswith('start counterargument ') or command.startswith('start antitheorem '):
                    if recording:
                        logger.warning("Already recording an argument. Please end the current recording first.")
                        continue
                    parts = command.split(' ', 3)
                    if len(parts) < 4:
                        logger.error("Invalid command. Use: start counterargument name conclusion")
                        continue
                    name = parts[2]
                    conclusion = parts[3].strip()
                    current_argument = {
                        'name': name,
                        'conclusion': conclusion,
                        'instructions': [],
                        'is_anti': True
                    }
                    recording = True
                    logger.info("Started recording counterargument '%s' with conclusion '%s'.", name, conclusion)
                    continue
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
                    logger.info("Finished recording argument. Constructing and executing argument '%s'.", current_argument['name'])
                    arg = Argument(
                        prover,
                        name=current_argument['name'],
                        conclusion=current_argument['conclusion'],
                        instructions=current_argument['instructions'],
                        is_anti=current_argument.get('is_anti', False)
                    )
                    arg.execute()
                    # Store the argument for later use
                    prover.register_argument(arg)
                    logger.info("Argument '%s' executed and registered  with conclusion '%s'.", arg.name, arg.conclusion)
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
                    elif command.startswith("color "):
                        color_argument_cmd(prover, command.split(maxsplit=1)[1])
                    elif command.startswith("tree "):
                        parts = command.split()
                        # Usage:
                        #   tree ARG
                        #   tree ARG nl [argumentation|dialectical|intuitionistic]
                        #   tree ARG pt
                        if len(parts) == 2:
                            tree_argument_cmd(prover, parts[1])
                        elif len(parts) >= 3:
                            mode = parts[2]
                            nl_style = parts[3] if (mode == "nl" and len(parts) >= 4) else "argumentation"
                            tree_argument_cmd(prover, parts[1], mode=mode, nl_style=nl_style)
                        else:
                            logger.error("Invalid tree command. Use: tree ARG [nl [argumentation|dialectical|intuitionistic]|pt]")
                    elif command.startswith("normalize "):
                        name = command.split(maxsplit=1)[1]
                        arg = prover.get_argument(name)
                        if arg:
                            logger.info("Normalized argument '%s'; normal form cached in .normal_form", name)
                            arg.normalize(); logger.info("normal form stored in .normal_form")
                        else:
                            logger.warning("Argument '%s' not found for normalization", name)
                            #print(f"Argument '{name}' not found.")
                    elif command.startswith('undercut '):
                        # Format: undercut NEW_NAME attacker target
                        parts = command.split()
                        if len(parts) != 4:
                            logger.error("Invalid undercut command. Use: undercut NEW_NAME attacker target")
                            continue
                        new_name = parts[1]
                        attacker_name = parts[2]
                        target_name = parts[3]
                        attacker = prover.get_argument(attacker_name)
                        target = prover.get_argument(target_name)
                        if attacker and target:
                            try:
                                result = attacker.focussed_undercut(target, name=new_name)
                                prover.register_argument(result)
                                logger.info("Constructed undercut '%s' (target '%s' by '%s').",
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
            elif command.startswith("color "):
                color_argument_cmd(prover, command.split(maxsplit=1)[1])
            elif command.startswith("tree "):
                parts = command.split()
                if len(parts) == 2:
                    tree_argument_cmd(prover, parts[1])
                elif len(parts) >= 3:
                    mode = parts[2]
                    nl_style = parts[3] if (mode == "nl" and len(parts) >= 4) else "argumentation"
                    tree_argument_cmd(prover, parts[1], mode=mode, nl_style=nl_style)
                else:
                    logger.error("Invalid tree command. Use: tree ARG [nl [argumentation|dialectical|intuitionistic]|pt]")
            elif command.startswith("normalize "):
                name = command.split(maxsplit=1)[1]
                arg = prover.get_argument(name)
                if arg:
                    logger.info("Normalized argument '%s'; normal form cached in .normal_form", name)
                    arg.normalize(); logger.info("normal form stored in .normal_form")
                else:
                    print(f"Argument '{name}' not found.")
                    logger.warning("Argument '%s' not found for normalization (interactive)", name)

            elif command.startswith("start counterargument ") or command.startswith("start antitheorem "):
                if recording:
                    print("Already recording an argument. Please end the current recording first.")
                    continue
                parts = command.split(' ', 3)
                if len(parts) < 4:
                    print("Invalid command. Use: start counterargument name conclusion")
                    continue
                name = parts[2]
                conclusion = parts[3].strip()
                current_argument = {
                    'name': name,
                    'conclusion': conclusion,
                    'instructions': [],
                    'is_anti': True
                }
                recording = True
                print(f"Started recording counterargument '{name}' with conclusion '{conclusion}'.")
                logger.info("Started recording counterargument '%s' with conclusion '%s'.", name, conclusion)
                output = prover.send_command(f'antitheorem {name} : ({conclusion}).')
                # print(output)
                continue
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
        logger.error(f"Argument '{name}' not found.")
        return
    logger.info(f"Reducing argument {arg.name} with proof term {arg.proof_term}")
    arg.reduce()

def render_argument_cmd(prover: ProverWrapper, name: str, normalized: bool = False) -> None:
    """ CLI for Argument Rendering. The normalized proof term can be selected."""
    arg = prover.get_argument(name)
    if not arg:
        logger.error(f"Argument '{name}' not found.")
        return
    logger.info("")  # spacer before NL rendering
    if normalized:
        logger.info(f"Rendering argument {arg.name} in normal form:")
    else:
        logger.info(f"Rendering argument {arg.name}:")
    logger.info(arg.render(normalized=normalized))
    logger.info("")  # spacer after NL rendering

def color_argument_cmd(prover: ProverWrapper, name: str) -> None:
    """CLI for coloring the normalized proof term of an argument."""
    arg = prover.get_argument(name)
    if not arg:
        logger.error(f"Argument '{name}' not found.")
        return
    # Ensure normalized body is available
    if arg.normal_body is None:
        arg.normalize()
    try:
        colored = pretty_colored_proof_term(arg.normal_body, verbose=False)
    except Exception as e:
        logger.error("Coloring failed for '%s': %s", arg.name, e)
        return
    logger.info("")  # spacer before colored output
    logger.info("Colored normalized proof term for %s:", arg.name)
    logger.info(colored)
    logger.info("")  # spacer after colored output

def tree_argument_cmd(prover: ProverWrapper, name: str, fmt: str = "svg", *, mode: str = "pt", nl_style: str = "argumentation") -> None:
    """CLI: render the colored acceptance tree (proof terms or NL) and save it as a file."""
    arg = prover.get_argument(name)
    if not arg:
        logger.error("Argument '%s' not found.", name)
        return
    if arg.normal_body is None:
        arg.normalize()
    try:
        label_mode = "proof" if mode != "nl" else "nl"
        dot = render_acceptance_tree_dot(arg.normal_body, verbose=False, label_mode=label_mode, nl_style=nl_style)
    except Exception as e:
        logger.error("Failed to build acceptance tree for '%s': %s", name, e)
        return
    out_base = f"{name}_tree"
    try:
        import graphviz  # type: ignore
        src = graphviz.Source(dot)
        path = src.render(filename=out_base, format=fmt, cleanup=True)
        logger.info("Acceptance tree written to %s", path)
    except Exception as e:
        # Fallback: write .dot file
        dot_path = f"{out_base}.dot"
        try:
            with open(dot_path, "w", encoding="utf-8") as f:
                f.write(dot)
            logger.warning("Graphviz not available (%s). Wrote DOT to %s", e, dot_path)
        except Exception as e2:
            logger.error("Failed to write DOT file: %s", e2)

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
    g.add_argument('--interactive', action='store_true',
                   help='start an interactive Fellowship REPL')
    g.add_argument('--script', metavar='FILE',
                   help='execute commands in FILE (same grammar as interactive mode)')
    args = ap.parse_args()

    # Configure CLI logging (explicit --log-level wins; else env FSP_LOGLEVEL; default INFO)
    configure_logging_cli(args.log_level, args.log_file)

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

        
