"""Statements, witnesses and citations (author, 2026-09-28; tasks.org,
aida-statements-and-witnesses).

The wrapper owns the registry of every statement and argument; Fellowship is
the kernel for STRICT proofs only.  A name Fellowship knows is a strict axiom
everywhere - in the prover and in the wrapper's compiler alike - so it must
hold exactly the strict proofs and never a defeasible one.

- ``theorem foo : A`` states a claim.  It is registered at once as the
  maximally enthymemic argument ``mu foo:A.< ?1:A || foo >``, which puts an
  obligation marker on A[t]: A is claimed and owes a proof.  The tactics that
  follow refine that enthymeme into a witness; ``qed`` demands the witness be
  strict.  Strict: Fellowship holds it as a theorem and it replaces the
  enthymeme under the same name.  Not strict: refused, the claim stays.
- ``start argument`` ... ``end argument`` records a defeasible argument.  If
  its body turns out closed, Fellowship holds it too, so a closed argument is
  citable without ceremony.  Ending it with ``qed`` demands strictness, as for
  a theorem.
- ``axiom foo`` citing a registered defeasible argument is grafted at replay
  time (core/dc/cite.py, ``Argument.execute``).

Both CLI paths - scripts, which record and replay at the end, and the REPL,
which runs every step live and then replays - call the helpers here, so the
rules exist once.
"""

import logging
import re

from core.ac.ast import Mu, Mutilde, ID, DI, Goal, Laog
from core.dc.argument import Argument
from wrap.prover import ProverError, StrictnessRefused

logger = logging.getLogger("fsp.wrapper")

_STATEMENT = re.compile(r"^\s*(theorem|antitheorem)\s+([^\s:()]+)\s*:\s*(.+?)\s*\.?\s*$")


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


def parse_statement(command: str):
    """``(is_anti, name, conclusion)`` for ``theorem NAME : (PROP).`` /
    ``antitheorem ...``, else None."""
    match = _STATEMENT.match(command)
    if not match:
        return None
    kind, name, prop = match.groups()
    return kind == "antitheorem", name, strip_outer_parens(prop)


def enthymeme(prover, name: str, conclusion: str, is_anti: bool) -> Argument:
    """The claim itself: ``mu name:A.< ?1:A || name >`` (a refutation claim
    mirrored).  In the graph an identity edge, i.e. exactly an obligation
    marker on the claimed statement."""
    if is_anti:
        body = Mutilde(DI(name, conclusion), conclusion, DI(name, conclusion), Laog("1", conclusion))
    else:
        body = Mu(ID(name, conclusion), conclusion, Goal("1", conclusion), ID(name, conclusion))
    arg = Argument(prover, name=name, conclusion=conclusion, instructions=[], is_anti=is_anti)
    arg.body = body
    arg.proof_term = f"?1:{conclusion}"
    arg.executed = True
    return arg


def start_statement(prover, is_anti: bool, name: str, conclusion: str) -> Argument:
    """Register the claim.  The name is held as a *statement*, which its
    witness may later refine."""
    # An unproved claim may be reopened: `theorem foo` again resumes it.
    prover.claim_name(name, "statement", refine=prover.names.get(name) == "statement")
    claim = enthymeme(prover, name, conclusion, is_anti)
    prover.arguments[name] = claim
    prover.document_add(claim)
    logger.info("Stated %s '%s' : %s; it owes a proof.",
                "antitheorem" if is_anti else "theorem", name, conclusion)
    return claim


def start_recording(prover, name: str, is_anti: bool) -> None:
    """Hold the name for the recording that is starting."""
    prover.claim_name(name, "recording")


def finish_recording(prover, current: dict, *, demand_strict: bool):
    """Build, replay and register the recorded argument.

    ``demand_strict`` (``qed``): the witness must be strict, or it is refused
    and nothing is registered - a statement keeps its enthymeme.  Otherwise
    (``end argument``) it is registered either way and, if closed, Fellowship
    holds it as a theorem.  Returns the registered argument, or None after a
    refusal (the reason is logged and printed by the caller's convention:
    raised as StrictnessRefused)."""
    arg = Argument(
        prover,
        name=current["name"],
        conclusion=current["conclusion"],
        instructions=current["instructions"],
        is_anti=current.get("is_anti", False),
    )
    arg.execute(declare=True if demand_strict else "auto")
    prover.register_argument(arg)
    if arg.citable:
        logger.info("'%s' registered; it is strict, so `axiom %s` cites it.", arg.name, arg.name)
    return arg


def is_qed(command: str) -> bool:
    return command.strip().rstrip(".").strip() == "qed"


__all__ = [
    "parse_statement", "enthymeme", "start_statement", "start_recording",
    "finish_recording", "is_qed", "strip_outer_parens",
    "ProverError", "StrictnessRefused",
]
