from __future__ import annotations
import os, sys
import atexit
import collections
import shlex
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
from core.ac.grammar import Grammar, ProofTermTransformer
from core.ac.ast import Mutilde
from core.comp.oracle import AdfBddNotFound
from scasp_import.importer import ScaspImportError, translate_json
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
    # The CLI reports onus conflicts itself, in _report_onus_conflicts, with
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

def execute_script(prover: ProverWrapper, script_path: str, *, strict: bool = False, stop_on_error: bool = True, echo_notes: bool = False, isolate: bool = True, stop_marker: bool = True, render_files: Optional[bool] = None) -> None:
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
          - Debate graph: "graph ARG [FILE.dot] [show]", "label ARG [SEMANTICS]",
            "evaluate ARG [MODE] [SEMANTICS] [BASE]"; "explain ARG [same options]" prints
            the pipeline's stage-by-stage account of one evaluation;
            "tree ARG [nl [STYLE]|pt]" colours by the grounded labels.
          - Debate ops: undermine NEW attacker target
                        undercut  NEW attacker target   (backward compatible alias)
                        undergird NEW supporter target [on PROP]
                        reinforce NEW supporter target [on PROP]
                        support   NEW supporter target [on PROP]
                        attack    NEW attacker  target [strict] [on PROP]
                        rebut     NEW attacker  target [on PROP]
                        out      INDEX ARG NAME   (also accepts: out NAME ARG INDEX)
                        tou      INDEX ARG NAME   (also accepts: tou NAME ARG INDEX)
                        sub      ARG NAME         (also accepts: sub NAME ARG)
                        bus      ARG NAME         (also accepts: bus NAME ARG)
                        attacker ARG NAME         (also accepts: attacker NAME ARG)
                        regatta  ARG NAME         (also accepts: regatta NAME ARG)
          - Register proof terms: "register NAME [strict] : TYPE := PROOF_TERM".
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
    # render_files=None keeps whatever the session has (ACDC_NO_RENDER); False
    # stops `graph ... show` and `tree` writing images and opening a viewer.
    prev_render = getattr(prover, "render_files", True)
    if render_files is not None:
        prover.render_files = render_files

    recording = False
    current_argument = None
    with open(script_path, 'r') as script_file:
        for lineno, line in enumerate(script_file, start=1):
            command = line.strip()
            if not command:
                continue
            if command.startswith('%'):
                if stop_marker and command.rstrip('.').strip().lower() == '%stop':
                    # The demo convention: execution stops here; what follows
                    # is for the presenter to paste into the session.
                    logger.info("Stopped at %%stop (%s:%d); the rest of the file is yours to paste.", script_path, lineno)
                    break
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
            elif command in {'end argument', 'end counterargument', 'end antitheorem'}:
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

                    elif command.startswith("decorate "):
                        try:
                            name, template = parse_decorate_command(command)
                            prover.register_decoration(name, template)
                        except Exception as e:
                            if strict:
                                if isolate:
                                    try:
                                        prover.close()
                                    except Exception:
                                        pass
                                else:
                                    prover.echo_notes = prev_echo
                                    prover.render_files = prev_render
                                logger.info("Finished script %s", script_path)
                                raise ProverError(f"{script_path}:{lineno}: {e}") from e
                            logger.error("Decorate failed: %s", e)
                            if stop_on_error:
                                break

                    elif command.startswith("register "):
                        try:
                            register_argument_cmd(prover, command)
                        except Exception as e:
                            if strict:
                                if isolate:
                                    try:
                                        prover.close()
                                    except Exception:
                                        pass
                                else:
                                    prover.echo_notes = prev_echo
                                    prover.render_files = prev_render
                                logger.info("Finished script %s", script_path)
                                if isinstance(e, MachinePayloadError):
                                    raise MachinePayloadError(f"{script_path}:{lineno}: {e}") from e
                                raise ProverError(f"{script_path}:{lineno}: {e}") from e
                            logger.error("Register failed: %s", e)
                            if stop_on_error:
                                break
                    elif command.startswith("reduce "):
                        reduce_argument_cmd(prover, command.split(maxsplit=1)[1])
                    elif command.startswith("render-nf "):
                        # Usage: render-nf ARG [style]
                        parts = command.split()
                        name = parts[1] if len(parts) >= 2 else ""
                        style = parts[2] if len(parts) >= 3 else None
                        render_argument_cmd(prover, name, True, style=style)
                    elif command.startswith("render "):
                        # Usage: render ARG [style]
                        parts = command.split()
                        name = parts[1] if len(parts) >= 2 else ""
                        style = parts[2] if len(parts) >= 3 else None
                        render_argument_cmd(prover, name, False, style=style)
                    elif command.startswith("graph "):
                        # Usage: graph ARG [FILE.dot] [show]
                        parts = command.split()
                        opts = parts[2:]
                        show = "show" in opts
                        dot_path = next((o for o in opts if o != "show"), None)
                        graph_argument_cmd(prover, parts[1], dot_path, show=show)
                    elif command in ("typecheck on", "typecheck off"):
                        prover.typecheck_enabled = command.endswith("on")
                        logger.info("Type checking of unfolded terms: %s", "on" if prover.typecheck_enabled else "off")
                    elif command.startswith("label "):
                        _dispatch_label(prover, command)
                    elif command.startswith("evaluate "):
                        _dispatch_evaluate(prover, command)
                    elif command.startswith("explain "):
                        _dispatch_explain(prover, command)
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
                    elif command.startswith(('out ', 'tou ', 'sub ', 'bus ', 'attacker ', 'regatta ')):
                        try:
                            result = projection_debate_cmd(prover, command)
                            logger.info("Constructed %s '%s'.", command.split()[0], result.name)
                        except Exception as e:
                            if strict:
                                if isolate:
                                    try:
                                        prover.close()
                                    except Exception:
                                        pass
                                else:
                                    prover.echo_notes = prev_echo
                                    prover.render_files = prev_render
                                logger.info("Finished script %s", script_path)
                                raise ProverError(f"{script_path}:{lineno}: {e}") from e
                            logger.error("%s failed: %s", command.split()[0].capitalize(), e)
                            if stop_on_error:
                                break

                    elif command.startswith('undermine ') or command.startswith('undercut '):
                        # Format: undermine NEW_NAME attacker target
                        # Legacy alias: undercut NEW_NAME attacker target
                        parts = command.split()
                        if len(parts) != 4:
                            logger.error("Invalid undermine command. Use: undermine NEW_NAME attacker target")
                            continue
                        new_name = parts[1]
                        attacker_name = parts[2]
                        target_name = parts[3]
                        attacker = prover.get_argument(attacker_name)
                        target = prover.get_argument(target_name)
                        if attacker and target:
                            try:
                                result = attacker.undercut(target, name=new_name)
                                prover.register_argument(result)
                                logger.info("Constructed undermine '%s' (target '%s' by '%s').",
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
                                        prover.render_files = prev_render
                                    logger.info("Finished script %s", script_path)
                                    raise ProverError(f"{script_path}:{lineno}: {e}") from e
                                logger.error("Prover error during undermine: %s", e)
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
                                        prover.render_files = prev_render
                                    logger.info("Finished script %s", script_path)
                                    raise MachinePayloadError(f"{script_path}:{lineno}: {e}") from e
                                logger.error("Prover error (no machine payload) during undermine: %s", e)
                                if stop_on_error:
                                    break
                        else:
                            logger.error("Undermine failed: one or both arguments not found ('%s', '%s').",
                                         attacker_name, target_name)
                            if strict:
                                if isolate:
                                    try:
                                        prover.close()
                                    except Exception:
                                        pass
                                else:
                                    prover.echo_notes = prev_echo
                                    prover.render_files = prev_render
                                logger.info("Finished script %s", script_path)
                                raise ProverError(f"{script_path}:{lineno}: Undermine failed: missing arguments")
                            if stop_on_error:
                                break

                    elif command.startswith('undergird ') or command.startswith('reinforce ') or command.startswith('support '):
                        verb = command.split()[0]
                        # Format: support/undergird/reinforce NEW_NAME supporter target [on <PROP...>]
                        parts = command.split()
                        if len(parts) < 4:
                            logger.error("Invalid %s command. Use: %s NEW_NAME supporter target [on PROP]", verb, verb)
                            continue
                        new_name = parts[1]
                        supporter_name = parts[2]
                        target_name = parts[3]
                        # Optional: parse 'on <prop...>'
                        on_prop = None
                        if len(parts) > 4:
                            try:
                                on_idx = parts.index('on', 4)
                                on_prop = " ".join(parts[on_idx+1:]).strip()
                            except ValueError:
                                on_prop = None

                        supporter = prover.get_argument(supporter_name)
                        target = prover.get_argument(target_name)
                        if supporter and target:
                            try:
                                if verb == 'undergird':
                                    result = supporter.undergird(target, name=new_name, on=on_prop)
                                elif verb == 'reinforce':
                                    result = supporter.reinforce(target, name=new_name, on=on_prop)
                                else:
                                    result = supporter.support(target, name=new_name, on=on_prop)
                                prover.register_argument(result)
                                logger.info("Constructed %s '%s' (target '%s' by '%s'%s).",
                                            verb, result.name, target.name, supporter.name,
                                            f" on {on_prop}" if on_prop else "")
                            except ProverError as e:
                                if strict:
                                    if isolate:
                                        try:
                                            prover.close()
                                        except Exception:
                                            pass
                                    else:
                                        prover.echo_notes = prev_echo
                                        prover.render_files = prev_render
                                    logger.info("Finished script %s", script_path)
                                    raise ProverError(f"{script_path}:{lineno}: {e}") from e
                                logger.error("Prover error during %s: %s", verb, e)
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
                                        prover.render_files = prev_render
                                    logger.info("Finished script %s", script_path)
                                    raise MachinePayloadError(f"{script_path}:{lineno}: {e}") from e
                                logger.error("Prover error (no machine payload) during %s: %s", verb, e)
                                if stop_on_error:
                                    break
                        else:
                            logger.error("%s failed: one or both arguments not found ('%s', '%s').",
                                         verb.capitalize(), supporter_name, target_name)
                            if strict:
                                if isolate:
                                    try:
                                        prover.close()
                                    except Exception:
                                        pass
                                else:
                                    prover.echo_notes = prev_echo
                                    prover.render_files = prev_render
                                logger.info("Finished script %s", script_path)
                                raise ProverError(f"{script_path}:{lineno}: {verb.capitalize()} failed: missing arguments")
                            if stop_on_error:
                                break
                    elif command.startswith('attack '):
                        # Format: attack NEW_NAME attacker target [strict] [on <PROP...>]
                        try:
                            new_name, attacker_name, target_name, on_prop, allow_strict_attack = _parse_attack_command(command)
                        except SyntaxError as e:
                            logger.error("%s", e)
                            continue
                        attacker = prover.get_argument(attacker_name)
                        target = prover.get_argument(target_name)
                        if attacker and target:
                            try:
                                result = attacker.attack(target, name=new_name, on=on_prop, allow_strict=allow_strict_attack)
                                prover.register_argument(result)
                                logger.info("Constructed attack '%s' (target '%s' by '%s'%s%s).",
                                            result.name, target.name, attacker.name,
                                            f" on {on_prop}" if on_prop else "",
                                            " allowing strict targets" if allow_strict_attack else "")
                            except ProverError as e:
                                if strict:
                                    if isolate:
                                        try:
                                            prover.close()
                                        except Exception:
                                            pass
                                    else:
                                        prover.echo_notes = prev_echo
                                        prover.render_files = prev_render
                                    logger.info("Finished script %s", script_path)
                                    raise ProverError(f"{script_path}:{lineno}: {e}") from e
                                logger.error("Prover error during attack: %s", e)
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
                                        prover.render_files = prev_render
                                    logger.info("Finished script %s", script_path)
                                    raise MachinePayloadError(f"{script_path}:{lineno}: {e}") from e
                                logger.error("Prover error (no machine payload) during attack: %s", e)
                                if stop_on_error:
                                    break
                        else:
                            logger.error("Attack failed: one or both arguments not found ('%s', '%s').",
                                         attacker_name, target_name)
                            if strict:
                                if isolate:
                                    try:
                                        prover.close()
                                    except Exception:
                                        pass
                                else:
                                    prover.echo_notes = prev_echo
                                    prover.render_files = prev_render
                                logger.info("Finished script %s", script_path)
                                raise ProverError(f"{script_path}:{lineno}: Attack failed: missing arguments")
                            if stop_on_error:
                                break
                    elif command.startswith('rebut '):
                        # Format: rebut NEW_NAME attacker target [on <PROP...>]
                        parts = command.split()
                        if len(parts) < 4:
                            logger.error("Invalid rebut command. Use: rebut NEW_NAME attacker target [on PROP]")
                            continue
                        new_name = parts[1]
                        attacker_name = parts[2]
                        target_name = parts[3]
                        # Optional: parse 'on <prop...>'
                        on_prop = None
                        if len(parts) > 4:
                            try:
                                on_idx = parts.index('on', 4)
                                on_prop = " ".join(parts[on_idx+1:]).strip()
                            except ValueError:
                                on_prop = None
                        attacker = prover.get_argument(attacker_name)
                        target = prover.get_argument(target_name)
                        if attacker and target:
                            try:
                                result = attacker.rebut(target, name=new_name, on=on_prop)
                                prover.register_argument(result)
                                logger.info("Constructed rebut '%s' (target '%s' by '%s'%s).",
                                            result.name, target.name, attacker.name,
                                            f" on {on_prop}" if on_prop else "")
                            except ProverError as e:
                                if strict:
                                    if isolate:
                                        try:
                                            prover.close()
                                        except Exception:
                                            pass
                                    else:
                                        prover.echo_notes = prev_echo
                                        prover.render_files = prev_render
                                    logger.info("Finished script %s", script_path)
                                    raise ProverError(f"{script_path}:{lineno}: {e}") from e
                                logger.error("Prover error during rebut: %s", e)
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
                                        prover.render_files = prev_render
                                    logger.info("Finished script %s", script_path)
                                    raise MachinePayloadError(f"{script_path}:{lineno}: {e}") from e
                                logger.error("Prover error (no machine payload) during rebut: %s", e)
                                if stop_on_error:
                                    break
                            except ValueError as e:
                                # e.g., when target contains an open Goal/Laog (undercut-only case)
                                if strict:
                                    if isolate:
                                        try:
                                            prover.close()
                                        except Exception:
                                            pass
                                    else:
                                        prover.echo_notes = prev_echo
                                        prover.render_files = prev_render
                                    logger.info("Finished script %s", script_path)
                                    raise ProverError(f"{script_path}:{lineno}: {e}") from e
                                logger.error("Rebut precondition failed: %s", e)
                                if stop_on_error:
                                    break
                        else:
                            logger.error("Rebut failed: one or both arguments not found ('%s', '%s').",
                                         attacker_name, target_name)
                            if strict:
                                if isolate:
                                    try:
                                        prover.close()
                                    except Exception:
                                        pass
                                else:
                                    prover.echo_notes = prev_echo
                                    prover.render_files = prev_render
                                logger.info("Finished script %s", script_path)
                                raise ProverError(f"{script_path}:{lineno}: Rebut failed: missing arguments")
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
                                    prover.render_files = prev_render
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
                                    prover.render_files = prev_render
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
    """Enables command line interaction with the wrapper.

        Paste-friendly: a pasted block is executed line by line; lines
        starting with '#' are echoed as user-facing comments, lines
        starting with '%' are ignored, blank lines are skipped - the
        conventions of .fspy scripts - and `load FILE` runs a script in
        the current session.  Line editing and history come from
        readline (emacs bindings).
        
        Syntax for commands: 
          - All fellowship commands;
          - (Custom) tactics: "tactic <TacticName> <Args>"
          - Arguments: "start argument / end argument";
          - Executing/Reducing an argument : "reduce <ArgName>"
          - Normalize an argument (silent version of reduce): "normalize <ArgName>" 
          - Chaining (Grafting) two arguments "chain <Arg1> <Arg2>" (Arg1 is rootstock, Arg2 is scion)
          - Rendering arguments (unreduced term, normal form, respectively): "render <Arg>", "render-nf <Arg>".
          - Debate graph: "graph ARG [FILE.dot] [show]", "label ARG [SEMANTICS]",
            "evaluate ARG [MODE] [SEMANTICS] [BASE]"; "explain ARG [same options]" prints
            the pipeline's stage-by-stage account of one evaluation;
            "tree ARG [nl [STYLE]|pt]" colours by the grounded labels.
          - Debate ops: undermine, undergird, reinforce, support, attack, rebut, out, tou, sub, bus, attacker, regatta.
          - Register proof terms: "register NAME [strict] : TYPE := PROOF_TERM".
          - Scripts: "load FILE" runs a .fspy file in this session.

        #TODO: implement human-oriented REPL output.
    """
    _setup_readline()
    recording = False
    current_argument = None
    try:
        while True:
            try:
                prompt = 'acdc> ' if not recording else 'acdc (recording)> '
                command = _read_line(prompt)
            except EOFError:
                print("\nEOFError: No input detected. Exiting interactive mode.")
                break
            if not command or command.startswith('%'):
                continue
            if command.startswith('#'):
                print(command[1:].lstrip())          # user-facing comment, as in scripts
                continue
            if command.lower() in ['exit', 'quit']:
                break
            if command.startswith('load '):
                path = Path(command.split(maxsplit=1)[1].strip()).expanduser()
                if not path.is_file():
                    print(f"load: no such file {path}")
                    continue
                try:
                    execute_script(prover, str(path), strict=False, stop_on_error=False, isolate=False)
                except (ProverError, MachinePayloadError) as e:
                    print(f"load: stopped: {e}")
                continue
            elif command.startswith("decorate "):
                try:
                    name, template = parse_decorate_command(command)
                    prover.register_decoration(name, template)
                    print(f"Decorated '{name}'.")
                except Exception as e:
                    print(f"Decorate failed: {e}")
                    logger.error("Decorate failed: %s", e)
            elif command.startswith("register "):
                try:
                    arg = register_argument_cmd(prover, command)
                    print(f"Registered argument '{arg.name}' with conclusion '{arg.conclusion}'.")
                except MachinePayloadError as e:
                    print(f"acdc: fatal prover communication error: {e}")
                    logger.error("Fatal prover communication error during register: %s", e)
                    break
                except Exception as e:
                    print(f"Register failed: {e}")
                    logger.error("Register failed: %s", e)
            elif command.startswith("reduce "):
                reduce_argument_cmd(prover, command.split(maxsplit=1)[1])
            elif command.startswith("render-nf "):
                    parts = command.split()
                    name = parts[1] if len(parts) >= 2 else ""
                    style = parts[2] if len(parts) >= 3 else None
                    render_argument_cmd(prover, name, True, style=style)
            elif command.startswith("render "):
                    parts = command.split()
                    name = parts[1] if len(parts) >= 2 else ""
                    style = parts[2] if len(parts) >= 3 else None
                    render_argument_cmd(prover, name, False, style=style)
            elif command.startswith("graph "):
                # Usage: graph ARG [FILE.dot] [show]
                parts = command.split()
                opts = parts[2:]
                show = "show" in opts
                dot_path = next((o for o in opts if o != "show"), None)
                graph_argument_cmd(prover, parts[1], dot_path, show=show)
            elif command in ("typecheck on", "typecheck off"):
                prover.typecheck_enabled = command.endswith("on")
                logger.info("Type checking of unfolded terms: %s", "on" if prover.typecheck_enabled else "off")
            elif command.startswith("label "):
                _dispatch_label(prover, command)
            elif command.startswith("evaluate "):
                _dispatch_evaluate(prover, command)
            elif command.startswith("explain "):
                _dispatch_explain(prover, command)
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

            elif command.startswith(('out ', 'tou ', 'sub ', 'bus ', 'attacker ', 'regatta ')):
                try:
                    result = projection_debate_cmd(prover, command)
                    print(f"Constructed {command.split()[0]} '{result.name}'.")
                    logger.info("Constructed %s '%s'.", command.split()[0], result.name)
                except Exception as e:
                    print(f"{command.split()[0].capitalize()} failed: {e}")
                    logger.error("%s failed: %s", command.split()[0].capitalize(), e)

            elif command.startswith('undermine ') or command.startswith('undercut '):
                # Format: undermine NEW_NAME attacker target
                # Legacy alias: undercut NEW_NAME attacker target
                parts = command.split()
                if len(parts) != 4:
                    print("Invalid undermine command. Use: undermine NEW_NAME attacker target")
                    logger.error("Invalid undermine command. Use: undermine NEW_NAME attacker target")
                    continue
                new_name, attacker_name, target_name = parts[1], parts[2], parts[3]
                attacker = prover.get_argument(attacker_name)
                target = prover.get_argument(target_name)
                if attacker and target:
                    try:
                        result = attacker.undercut(target, name=new_name)
                        prover.register_argument(result)
                        print(f"Constructed undermine '{result.name}' (target '{target.name}' by '{attacker.name}').")
                        logger.info("Constructed undermine '%s' (target '%s' by '%s').",
                                    result.name, target.name, attacker.name)
                    except Exception as e:
                        print(f"Prover error during undermine: {e}")
                        logger.error("Prover error during undermine: %s", e)
                else:
                    print(f"Undermine failed: one or both arguments not found ('{attacker_name}', '{target_name}').")
                    logger.error("Undermine failed: one or both arguments not found ('%s', '%s').",
                                 attacker_name, target_name)

            elif command.startswith('undergird ') or command.startswith('reinforce ') or command.startswith('support '):
                # Format: support/undergird/reinforce NEW_NAME supporter target [on PROP...]
                parts = command.split()
                verb = parts[0]
                if len(parts) < 4:
                    print(f"Invalid {verb} command. Use: {verb} NEW_NAME supporter target [on PROP]")
                    logger.error("Invalid %s command. Use: %s NEW_NAME supporter target [on PROP]", verb, verb)
                    continue
                new_name, supporter_name, target_name = parts[1], parts[2], parts[3]
                on_prop = None
                if len(parts) > 4:
                    try:
                        on_idx = parts.index('on', 4)
                        on_prop = " ".join(parts[on_idx+1:]).strip()
                    except ValueError:
                        on_prop = None
                supporter = prover.get_argument(supporter_name)
                target = prover.get_argument(target_name)
                if supporter and target:
                    try:
                        if verb == 'undergird':
                            result = supporter.undergird(target, name=new_name, on=on_prop)
                        elif verb == 'reinforce':
                            result = supporter.reinforce(target, name=new_name, on=on_prop)
                        else:
                            result = supporter.support(target, name=new_name, on=on_prop)
                        prover.register_argument(result)
                        suffix = f" on {on_prop}" if on_prop else ""
                        print(f"Constructed {verb} '{result.name}' (target '{target.name}' by '{supporter.name}'{suffix}).")
                        logger.info("Constructed %s '%s' (target '%s' by '%s'%s).",
                                    verb, result.name, target.name, supporter.name, suffix)
                    except Exception as e:
                        print(f"Prover error during {verb}: {e}")
                        logger.error("Prover error during %s: %s", verb, e)
                else:
                    print(f"{verb.capitalize()} failed: one or both arguments not found ('{supporter_name}', '{target_name}').")
                    logger.error("%s failed: one or both arguments not found ('%s', '%s').",
                                 verb.capitalize(), supporter_name, target_name)

            elif command.startswith('attack '):
                # Format: attack NEW_NAME attacker target [strict] [on PROP...]
                try:
                    new_name, attacker_name, target_name, on_prop, allow_strict_attack = _parse_attack_command(command)
                except SyntaxError as e:
                    print(str(e))
                    logger.error("%s", e)
                    continue
                attacker = prover.get_argument(attacker_name)
                target = prover.get_argument(target_name)
                if attacker and target:
                    try:
                        result = attacker.attack(target, name=new_name, on=on_prop, allow_strict=allow_strict_attack)
                        prover.register_argument(result)
                        suffix = f" on {on_prop}" if on_prop else ""
                        strict_suffix = " allowing strict targets" if allow_strict_attack else ""
                        print(f"Constructed attack '{result.name}' (target '{target.name}' by '{attacker.name}'{suffix}{strict_suffix}).")
                        logger.info("Constructed attack '%s' (target '%s' by '%s'%s%s).",
                                    result.name, target.name, attacker.name, suffix, strict_suffix)
                    except Exception as e:
                        print(f"Prover error during attack: {e}")
                        logger.error("Prover error during attack: %s", e)
                else:
                    print(f"Attack failed: one or both arguments not found ('{attacker_name}', '{target_name}').")
                    logger.error("Attack failed: one or both arguments not found ('%s', '%s').",
                                 attacker_name, target_name)

            elif command.startswith('rebut '):
                # Format: rebut NEW_NAME attacker target [on PROP...]
                parts = command.split()
                if len(parts) < 4:
                    print("Invalid rebut command. Use: rebut NEW_NAME attacker target [on PROP]")
                    logger.error("Invalid rebut command. Use: rebut NEW_NAME attacker target [on PROP]")
                    continue
                new_name, attacker_name, target_name = parts[1], parts[2], parts[3]
                on_prop = None
                if len(parts) > 4:
                    try:
                        on_idx = parts.index('on', 4)
                        on_prop = " ".join(parts[on_idx+1:]).strip()
                    except ValueError:
                        on_prop = None
                attacker = prover.get_argument(attacker_name)
                target = prover.get_argument(target_name)
                if attacker and target:
                    try:
                        result = attacker.rebut(target, name=new_name, on=on_prop)
                        prover.register_argument(result)
                        suffix = f" on {on_prop}" if on_prop else ""
                        print(f"Constructed rebut '{result.name}' (target '{target.name}' by '{attacker.name}'{suffix}).")
                        logger.info("Constructed rebut '%s' (target '%s' by '%s'%s).",
                                    result.name, target.name, attacker.name, suffix)
                    except Exception as e:
                        print(f"Rebut failed: {e}")
                        logger.error("Rebut failed: %s", e)

            # -----------------------------------------------------------------
            # Fellowship commands outside wrapper-specific commands
            # -----------------------------------------------------------------
                else:
                    print(f"Rebut failed: one or both arguments not found ('{attacker_name}', '{target_name}').")
                    logger.error("Rebut failed: one or both arguments not found ('%s', '%s').",
                                 attacker_name, target_name)

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
                try:
                    output = prover.send_command(f'antitheorem {name} : ({conclusion}).', include_ui=True)
                    _print_ui(output)
                except ProverError as e:
                    print(f"acdc: ignored command due to prover error: {e}")
                    logger.error("Prover error starting counterargument: %s", e)
                    recording = False
                    current_argument = None
                except MachinePayloadError as e:
                    print(f"acdc: fatal prover communication error: {e}")
                    logger.error("Fatal prover communication error starting counterargument: %s", e)
                    break
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
                try:
                    output = prover.send_command(f'theorem {name} : ({conclusion}).', include_ui=True)
                    _print_ui(output)
                except ProverError as e:
                    print(f"acdc: ignored command due to prover error: {e}")
                    logger.error("Prover error starting argument: %s", e)
                    recording = False
                    current_argument = None
                except MachinePayloadError as e:
                    print(f"acdc: fatal prover communication error: {e}")
                    logger.error("Fatal prover communication error starting argument: %s", e)
                    break
            elif command in {'end argument', 'end counterargument', 'end antitheorem'}:
                if not recording:
                    print("Not currently recording an argument.")
                    continue
                try:
                    out = prover.send_command('discard theorem.', include_ui=True)
                    _print_ui(out)
                except ProverError as e:
                    print(f"acdc: prover error discarding theorem: {e}")
                    logger.error("Prover error discarding theorem: %s", e)
                    # wrapper command failed; continue REPL, still recording
                    continue
                except MachinePayloadError as e:
                    print(f"acdc: fatal prover communication error: {e}")
                    logger.error("Fatal prover communication error discarding theorem: %s", e)
                    break
                # Create and execute the argument
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
                        try:
                            output = prover.send_command(command, include_ui=True, allow_incomplete=True)
                            _print_ui(output)
                            while isinstance(output, dict) and output.get('_need_more_input'):
                                more = _read_line('... ')
                                output = prover.send_command(more, include_ui=True, allow_incomplete=True)
                                _print_ui(output)
                            if isinstance(output, dict) and output.get('_need_more_input'):
                                continue
                            current_argument['instructions'].append(command)
                        except ProverError as e:
                            print(f"acdc: ignored command due to prover error: {e}")
                            logger.error("Prover error during recording: %s", e)
                            # do not record failing instruction
                        except MachinePayloadError as e:
                            print(f"acdc: fatal prover communication error: {e}")
                            logger.error("Fatal prover communication error during recording: %s", e)
                            break
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
                    try:
                        output = prover.send_command(command, include_ui=True, allow_incomplete=True)
                        _print_ui(output)
                        while isinstance(output, dict) and output.get('_need_more_input'):
                            more = _read_line('... ')
                            output = prover.send_command(more, include_ui=True, allow_incomplete=True)
                            _print_ui(output)
                    except MachinePayloadError as e:
                        # Potential prover/wrapper desync: exit interactive mode.
                        print(f"acdc: fatal prover communication error: {e}")
                        logger.error("Fatal prover communication error: %s", e)
                        break
                    except ProverError as e:
                        print(f"acdc: ignored command due to prover error: {e}")
                        logger.error("Prover error: %s", e)
                        continue
                    # print(output)
    finally:
        prover.close()
                

# ---------------------------------------------------------------------------
#  CLI helper commands                                                       
# ---------------------------------------------------------------------------


def _parse_attack_command(command: str) -> tuple[str, str, str, str | None, bool]:
    """Parse `attack NEW attacker target [strict] [on PROP...]`.

    Strict-target attacks are opt-in and can be enabled with any of:
    `strict`, `--strict`, `allow-strict`, `--allow-strict`,
    `strict-proofs`, or `--strict-proofs`.
    """
    parts = command.split()
    if len(parts) < 4:
        raise SyntaxError("Invalid attack command. Use: attack NEW_NAME attacker target [strict] [on PROP]")

    new_name, attacker_name, target_name = parts[1], parts[2], parts[3]
    strict_tokens = {
        "strict",
        "--strict",
        "allow-strict",
        "--allow-strict",
        "strict-proofs",
        "--strict-proofs",
    }
    allow_strict = False
    remainder: list[str] = []
    for token in parts[4:]:
        if token in strict_tokens:
            allow_strict = True
        else:
            remainder.append(token)

    on_prop = None
    if remainder:
        try:
            on_idx = remainder.index('on')
        except ValueError:
            on_idx = -1
        if on_idx >= 0:
            on_prop = " ".join(remainder[on_idx + 1:]).strip() or None

    return new_name, attacker_name, target_name, on_prop, allow_strict


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
    """Register an argument/theorem from a proof-term string.

    Command syntax:
        register NAME [strict] : TYPE := PROOF_TERM

    With `strict`, replay is finalized with `qed.` so Fellowship declares the
    theorem/antitheorem; otherwise the replayed theorem is discarded after the
    wrapper extracts and registers the argument state.
    """
    name, conclusion, declare_theorem, proof_term = _parse_register_command(command)
    proof_term = Argument._normalize_pt_to_unicode(proof_term)

    parsed = Grammar().parser.parse(proof_term)
    body = ProofTermTransformer().transform(parsed)

    arg = Argument(
        prover,
        name=name,
        conclusion=conclusion,
        is_anti=isinstance(body, Mutilde),
    )
    arg.body = body
    arg.execute(declare=declare_theorem, preserve_input_body=True)
    prover.register_argument(arg)

    logger.info(
        "Registered argument '%s' with conclusion '%s'%s.",
        arg.name,
        arg.conclusion,
        " and declared it in Fellowship" if declare_theorem else "",
    )
    return arg

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
    prover.register_custom_tactic('pop', pop)
    # Switch to classical logic
    #prover.send_command('lk.')
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

def render_argument_cmd(prover: ProverWrapper, name: str, normalized: bool = False, *, style: Optional[str] = None) -> None:
    """CLI for Argument Rendering.

    style can be one of: argumentation|dialectical|intuitionistic|vanilla.
    If omitted, uses the argument's configured rendering.
    """
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
        natural_language_dialectical_rendering,
        natural_language_rendering,
        pruefschema_rendering,
        vanilla_rendering,
    )
    sem_map = {
        "argumentation": natural_language_argumentative_rendering,
        "dialectical": natural_language_dialectical_rendering,
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

def _issue_term(prover: ProverWrapper, name: str):
    """Resolve NAME to (arg, issue, term): the debate term for the
    argument's issue, unfolded from the document graph (Phase C).

    Every registered atomic argument is in the document; a composed
    argument (a debate) names its host's issue.  An argument the document
    refused at registration falls back to its own term, with a notice.
    Returns (arg, issue, None) after printing a refusal.
    """
    from core.dc.unfold import unfold, UnfoldError

    arg = prover.get_argument(name)
    if not arg:
        logger.error("Argument '%s' not found.", name)
        return None, None, None
    if not arg.executed:
        arg.execute()
    issue = prover.issue_of(arg)
    document = prover.document
    if prover.logic == "lj":
        print("graph: refused: debates are classical (their scaffolds throw to a second "
              "conclusion, which LJ forbids); select lk for graph, label and evaluate.")
        logger.warning("Debate commands refused in lj for '%s'.", name)
        return arg, issue, None
    if _pipeline_logger.isEnabledFor(logging.DEBUG):
        from pres.gen import pres_str
        _pipeline_logger.debug("issue: '%s' is about %s; the document has %d edge(s)",
                               name, f"{document.nodes.get(issue[0], arg.conclusion)}[{issue[1][0]}]",
                               len(document.edges))
        artifact(_pipeline_logger, "issue: the term '%s' was registered with (before unfolding)" % name,
                 pres_str(arg.body))
    if issue not in set(document.statements()):
        logger.warning("'%s' is not in the document graph; using its own term.", name)
        _pipeline_logger.debug("issue: NOT unfolded and NOT type-checked - the issue is not in "
                               "the document, so '%s' is evaluated as it was registered", name)
        return arg, issue, arg.body
    try:
        term = unfold(document, issue)
    except UnfoldError as e:
        print(f"graph: refused: {e}")
        logger.warning("Unfolding refused for '%s': %s", name, e)
        return arg, issue, None
    if prover.typecheck_enabled:
        # The type oracle: the unfolded term must replay through Fellowship
        # (core/dc/typecheck.py).  `typecheck off` skips it.
        from core.dc.typecheck import typecheck, TypeCheckFailed
        try:
            typecheck(prover, term, name, document.nodes.get(issue[0], arg.conclusion),
                      issue[1] == "context")
        except TypeCheckFailed as e:
            print(f"graph: refused: {e}")
            logger.warning("Type check failed for '%s': %s", name, e)
            return arg, issue, None
    return arg, issue, term


def _compile_argument_graph(prover: ProverWrapper, name: str):
    """(arg, graph, term) for NAME: the issue's debate graph, compiled
    from the term unfolded out of the document graph; or, for the name
    ``document``, the document graph itself.  (arg, None, term) after
    printing the refusal - the log-and-refuse convention: compile errors
    are one-line messages, not tracebacks.
    """
    from core.dc.debate_graph import DebateCompileError, declaration_kinds
    from core.dc.strict import compile_issue
    from core.ac.ast import FirstOrderNotSupported

    if name == "document":
        _report_onus_conflicts(prover.document)
        return None, prover.document, None
    arg, issue, term = _issue_term(prover, name)
    if term is None:
        return arg, None, None
    try:
        graph = compile_issue(term, name, strict_names=prover.declarations.keys(),
                              strict_kinds=declaration_kinds(prover.declarations))
    except (DebateCompileError, FirstOrderNotSupported) as e:
        print(f"graph: refused: {e}")
        logger.warning("Debate graph compilation refused for '%s': %s", name, e)
        return arg, None, term
    _report_onus_conflicts(graph)
    return arg, graph, term


def _report_onus_conflicts(graph) -> None:
    """Warn about propositions presumed on BOTH sides.

    A presumption delegates the burden of refutation to the other side, so
    both sides presuming means neither holds it.  The compiler still
    labels such a graph; task aida-onus-delegation-polarity makes it an
    error at registration time.
    """
    from core.comp.adf_label import opposing_presumptions

    clash = opposing_presumptions(graph)
    if clash:
        names = ", ".join(graph.nodes.get(key, key) for key in clash)
        logger.warning("Opposing presumptions on %s: both sides delegate the onus "
                       "of refutation, so neither side holds it.", names)


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
    parts = command.split()
    try:
        _, semantics, _, _ = _split_eval_tokens(parts[2:])
    except ValueError as e:
        logger.error("label: %s", e)
        return
    label_argument_cmd(prover, parts[1], semantics or "grounded")


def _dispatch_evaluate(prover: ProverWrapper, command: str) -> None:
    parts = command.split()
    try:
        mode, semantics, base, witness = _split_eval_tokens(parts[2:])
    except ValueError as e:
        logger.error("evaluate: %s", e)
        return
    if witness is not None and (mode or "skeptical") != "credulous":
        logger.error("evaluate: a witness number or 'all' requires credulous mode")
        return
    evaluate_argument_cmd(prover, parts[1], mode or "skeptical", base or "cbn",
                          semantics or "preferred", witness)


def _dispatch_explain(prover: ProverWrapper, command: str) -> None:
    parts = command.split()
    if len(parts) < 2:
        logger.error("explain: needs an argument name. Use: explain ARG [MODE] [SEMANTICS] [BASE] [N|all]")
        return
    try:
        mode, semantics, base, witness = _split_eval_tokens(parts[2:])
    except ValueError as e:
        logger.error("explain: %s", e)
        return
    if witness is not None and (mode or "skeptical") != "credulous":
        logger.error("explain: a witness number or 'all' requires credulous mode")
        return
    explain_argument_cmd(prover, parts[1], mode or "skeptical", base or "cbn",
                         semantics or "preferred", witness)


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
                         witness=None) -> None:
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
        evaluate_argument_cmd(prover, name, mode, base, semantics, witness)
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
            stripped = message.lstrip()
            indent = "  " if len(message) != len(stripped) else ""
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
                       show: bool = False) -> None:
    """CLI: compile an argument's debate graph; print a summary, optionally DOT.

    Syntax:
        graph ARG [FILE.dot] [show]

    With `show`, render the graph and open it in the platform viewer;
    when Graphviz is not installed, print an indented text view instead
    (which always works and needs no dependencies).
    """
    arg, graph, _term = _compile_argument_graph(prover, name)
    if graph is None:
        return
    if arg is None:
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
    labels = None
    try:
        from core.comp.adf_label import grounded_labels
        labels = grounded_labels(graph)
    except AdfBddNotFound as e:
        # The graph itself needs no labeller; labels are an overlay.  But
        # say loudly why they are missing - there is no fallback labeller.
        print(f"graph: labels unavailable: {e}")
        logger.warning("Labels unavailable for the graph view: %s", e)
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
    """CLI: ADF labelling(s) of an argument's debate graph.

    Syntax:
        label ARG [grounded|complete|preferred|stable]

    grounded prints the one grounded labelling; the others print every
    labelling of that semantics, numbered.
    """
    from core.comp.adf_label import labellings

    arg, graph, _term = _compile_argument_graph(prover, name)
    if graph is None:
        return
    try:
        found = labellings(graph, semantics)
    except AdfBddNotFound as e:
        print(f"label: refused: {e}")
        logger.error("Labelling refused for '%s': %s", name, e)
        return
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
                          witness=None) -> None:
    """CLI: label-guided evaluation of an argument's debate term.

    Syntax:
        evaluate ARG [skeptical|credulous] [grounded|complete|preferred|stable] [cbn|cbv] [N|all]

    Options may appear in any order.  The mode ranges over the chosen
    semantics (default preferred); the base strategy resolves only critical
    pairs the witness labelling leaves open.  In credulous mode N picks
    labelling N of `label ARG SEMANTICS` as the witness and `all`
    evaluates under every labelling that accepts the issue.  The normal
    form (the last one, under `all`) is cached on the argument as
    .labelled_nf.
    """
    from core.comp.evaluate import evaluate_debate, evaluate_witnesses, EvaluationRefused
    from core.dc.debate_graph import DebateCompileError, declaration_kinds
    from core.ac.ast import FirstOrderNotSupported
    from pres.gen import ProofTermGenerationVisitor
    import copy as _copy

    if name == "document":
        logger.error("evaluate needs an argument or debate name; 'document' has no issue.")
        return
    arg, issue, term = _issue_term(prover, name)
    if term is None:
        return
    common = dict(strict_names=prover.declarations.keys(),
                  strict_kinds=declaration_kinds(prover.declarations),
                  base=base, semantics=semantics)
    try:
        if witness == "all":
            results, _ = evaluate_witnesses(term, name, **common)
            if not results:
                logger.info("Evaluated '%s' (credulous, %s, base %s): no labelling accepts the issue.",
                            name, semantics, base)
                return
            logger.info("Evaluated '%s' (credulous, %s, base %s) under %d accepting witness(es):",
                        name, semantics, base, len(results))
            for number, nf, nf_class, _sigma in results:
                pretty = ProofTermGenerationVisitor().visit(_copy.deepcopy(nf)).pres
                logger.info("  [%d] %s", number, nf_class.upper())
                logger.info("      normal form: %s", pretty)
                arg.labelled_nf = nf
            return
        nf, nf_class, sigma, graph = evaluate_debate(
            term, name, mode=mode, witness=witness, **common)
    except (EvaluationRefused, DebateCompileError, FirstOrderNotSupported, AdfBddNotFound) as e:
        print(f"evaluate: refused: {e}")
        logger.warning("Evaluation refused for '%s': %s", name, e)
        return
    arg.labelled_nf = nf
    pretty = ProofTermGenerationVisitor().visit(_copy.deepcopy(nf)).pres
    chosen = f", witness {witness}" if witness is not None else ""
    logger.info("Evaluated '%s' (%s, %s, base %s%s): %s", name, mode, semantics, base, chosen, nf_class.upper())
    logger.info("  normal form: %s", pretty)

def tree_argument_cmd(prover: ProverWrapper, name: str, fmt: str = "png", *, mode: str = "pt", nl_style: str = "argumentation") -> None:
    """CLI: render the acceptance tree (proof terms or NL), coloured by the
    grounded ADF labels of the argument's debate graph, and save it as a
    file.  If the graph is refused or adf-bdd is missing the tree is
    written uncoloured with a one-line notice."""
    from core.comp.adf_label import grounded_labels

    arg, graph, _term = _compile_argument_graph(prover, name)
    if arg is None:
        return
    labels = None
    if graph is not None:
        try:
            labels = grounded_labels(graph)
        except AdfBddNotFound as e:
            print(f"tree: labels unavailable: {e}")
            logger.warning("Tree for '%s' drawn without labels: %s", name, e)
    else:
        logger.warning("Tree for '%s' drawn without labels: debate graph refused.", name)
    if arg.normal_body is None:
        arg.normalize()
    try:
        label_mode = "proof" if mode != "nl" else "nl"
        dot = render_acceptance_tree_dot(
            arg.normal_body,
            verbose=False,
            label_mode=label_mode,
            nl_style=nl_style,
            declarations=getattr(prover, "declarations", {}),
            decorations=getattr(prover, "decorations", {}),
            labels=labels,
        )
    except Exception as e:
        logger.error("Failed to build acceptance tree for '%s': %s", name, e)
        return
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

    if source_language != "scasp":
        ap.error("Unsupported import source language '%s' (currently only 'scasp')" % source_language)
    if mode not in ("file", "interactive"):
        ap.error("Unsupported import mode '%s' (expected 'file' or 'interactive')" % mode)

    json_path = Path(source_json).expanduser()
    if not json_path.is_file():
        ap.error(f"source_json file {json_path} does not exist")

    try:
        data = json.loads(json_path.read_text())
        result = translate_json(data)
    except (OSError, json.JSONDecodeError, ScaspImportError) as e:
        ap.error(f"failed to import {json_path}: {e}")

    script_name = json_path.stem
    if mode == "file":
        target_path = Path(target_file_name).expanduser() if target_file_name else json_path.with_suffix(".fspy")
        result.write_fspy(target_path, name=script_name)
        logger.info("Imported %s JSON written to %s", source_language, target_path)
        return

    prover = setup_prover()
    with tempfile.TemporaryDirectory(prefix="scasp_import_cli_") as tmpdir:
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
