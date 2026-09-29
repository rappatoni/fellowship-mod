"""Statements, refinement and citation (author, 2026-09-28/29; tasks.org,
aida-statements-and-witnesses).

The wrapper owns the registry of every statement and argument; Fellowship is
the kernel for STRICT proofs only.  A name Fellowship knows is a strict axiom
everywhere - in the prover and in the wrapper's compiler alike - so it must
hold exactly the strict proofs and never a defeasible one.

- A *statement* - ``theorem``, ``lemma``, ``proposition`` or ``claim``, and
  their ``anti`` forms - only records a claim.  It is registered at once as
  the maximally enthymemic argument ``mu foo:A.< ?1:A || foo >``, an
  obligation marker on A[t]: A is claimed and owes a proof.  It opens nothing.
- ``refine NAME`` (``prove``, ``argue``, ``refute``, ``dispute``) reopens a
  registered argument, pre-filled with its recorded instructions, so the
  tactics that follow continue from its open goals.  Proving a statement is
  the special case of refining its enthymeme.
- A recording ends with ``end argument``, which registers the result either
  way and lets Fellowship hold it if it is closed, or with ``qed``, which
  demands a strict witness and otherwise leaves the argument as it was.
- ``cite NAME`` uses a registered argument by name; see core/dc/cite.py.

A refinement replaces the argument in place, so its position in registration
order - which is the order unfolding nests supporters and attackers - is
kept, and the document graph is rebuilt, since merging cannot take an old
edge back out.  When a refinement makes an argument strict, the arguments
that cite it are replayed too, so those that became closed are held by
Fellowship as well: strictness propagates up the citation graph.

Both CLI paths - scripts, which record and replay at the end, and the REPL,
which runs every step live and then replays - call the helpers here, so the
rules exist once.
"""

import logging
import re

from core.ac.ast import Mu, Mutilde, ID, DI, Goal, Laog
from core.dc.argument import Argument
from core.dc.cite import cited_names
from wrap.prover import ProverError, StrictnessRefused

logger = logging.getLogger("fsp.wrapper")

#: Statement keywords: a claim in favour, and its "anti" counterpart.
STATEMENT_KEYWORDS = ("theorem", "lemma", "proposition", "claim")
ANTI_KEYWORDS = tuple("anti" + k for k in STATEMENT_KEYWORDS)

#: Verbs that reopen a registered argument.  They are synonyms: the side
#: comes from the argument, not from the verb.
REFINE_VERBS = ("refine", "prove", "argue", "refute", "dispute")

_STATEMENT = re.compile(
    r"^\s*(" + "|".join(STATEMENT_KEYWORDS + ANTI_KEYWORDS) + r")\s+([^\s:()]+)\s*:\s*(.+?)\s*\.?\s*$",
    re.IGNORECASE,
)
_REFINE = re.compile(r"^\s*(" + "|".join(REFINE_VERBS) + r")\s+([^\s.]+)\s*\.?\s*$", re.IGNORECASE)


class RefinementRefused(ProverError):
    """`refine NAME` on something that cannot be reopened."""


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


# -- statements ---------------------------------------------------------------

def parse_statement(command: str):
    """``(is_anti, name, conclusion, keyword)`` for a statement, else None."""
    match = _STATEMENT.match(command)
    if not match:
        return None
    keyword, name, prop = match.groups()
    keyword = keyword.lower()
    return keyword.startswith("anti"), name, strip_outer_parens(prop), keyword


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


def state(prover, is_anti: bool, name: str, conclusion: str, keyword: str = "theorem") -> Argument:
    """Register a claim, and nothing more.  Stating an unproved claim again
    is harmless; any other reuse of the name is a NameClash."""
    prover.claim_name(name, "statement", refine=prover.names.get(name) == "statement")
    claim = enthymeme(prover, name, conclusion, is_anti)
    claim.statement_kind = keyword
    prover.arguments[name] = claim
    prover.document_add(claim)
    logger.info("Stated %s '%s' : %s; it owes a proof (`prove %s`).", keyword, name, conclusion, name)
    return claim


# -- recordings ----------------------------------------------------------------

def parse_refine(command: str):
    """``NAME`` for ``refine NAME`` and its synonyms, else None."""
    match = _REFINE.match(command)
    return match.group(2) if match else None


def start_recording(prover, name: str, is_anti: bool) -> None:
    """Hold the name for a new recording (`start argument ...`)."""
    prover.claim_name(name, "recording")


def reopen(prover, name: str) -> dict:
    """The recording state for refining ``name``: its own instructions,
    ready to be continued.  Refused for what cannot be reopened."""
    arg = prover.get_argument(name)
    if arg is None:
        raise RefinementRefused(f"there is no argument or statement '{name}' to refine")
    if getattr(arg, "composed", False):
        raise RefinementRefused(
            f"'{name}' is a debate the debate verbs composed; refine the arguments it is made of"
        )
    if getattr(arg, "citable", False):
        raise RefinementRefused(f"'{name}' is already strict; there is nothing left to refine")
    if arg.instructions is None:
        raise RefinementRefused(f"'{name}' has no recorded proof to continue")
    previous = prover.names.get(name)
    prover.names[name] = "recording"
    return {
        "name": name,
        "conclusion": arg.conclusion,
        "instructions": list(arg.instructions),
        "is_anti": getattr(arg, "is_anti", False),
        "statement": previous == "statement",
        "previous_kind": previous,
        "refining": True,
    }


def abandon(prover, current: dict) -> None:
    """A recording that ended in a refusal gives its name back: a refinement
    to what the name was before, a new recording to nobody."""
    name = current["name"]
    if current.get("previous_kind"):
        prover.names[name] = current["previous_kind"]
    elif prover.names.get(name) == "recording":
        prover.names.pop(name)


def finish_recording(prover, current: dict, *, demand_strict: bool):
    """Build, replay and register the recorded argument.

    ``demand_strict`` (``qed``): the witness must be strict, or it is refused
    and nothing changes.  Otherwise (``end argument``) it is registered
    either way and, if closed, Fellowship holds it as a theorem.  A
    replacement keeps its position, rebuilds the document graph and, if it
    newly became strict, replays its citers.  Raises ProverError subclasses
    on refusal; the caller calls ``abandon``."""
    name = current["name"]
    before = prover.get_argument(name)
    arg = Argument(
        prover,
        name=name,
        conclusion=current["conclusion"],
        instructions=current["instructions"],
        is_anti=current.get("is_anti", False),
    )
    arg.execute(declare=True if demand_strict else "auto")
    if before is not None and getattr(before, "statement_kind", None):
        arg.statement_kind = before.statement_kind
    prover.register_argument(arg)
    if before is not None:
        prover.rebuild_document()
    if arg.citable:
        logger.info("'%s' is strict: `axiom %s` and `cite %s` both use it.", name, name, name)
        if before is None or not getattr(before, "citable", False):
            propagate_strictness(prover, name)
    return arg


def propagate_strictness(prover, name: str, _seen=None) -> None:
    """``name`` just became strict.  Replay every argument that cites it: one
    that is now closed is held by Fellowship too, and its own citers follow."""
    seen = _seen if _seen is not None else {name}
    for citer_name, citer in list(prover.arguments.items()):
        if citer_name in seen or getattr(citer, "citable", False):
            continue
        if getattr(citer, "composed", False) or citer.instructions is None:
            continue
        if name not in cited_names(getattr(citer, "body", None)):
            continue
        seen.add(citer_name)
        replay = Argument(prover, name=citer_name, conclusion=citer.conclusion,
                          instructions=list(citer.instructions),
                          is_anti=getattr(citer, "is_anti", False))
        try:
            replay.execute(declare="auto")
        except ProverError as e:
            logger.warning("'%s' cites '%s' but could not be replayed: %s", citer_name, name, e)
            continue
        if getattr(citer, "statement_kind", None):
            replay.statement_kind = citer.statement_kind
        prover.register_argument(replay, replace=True)
        if replay.citable:
            logger.info("'%s' is now strict too: it only cited what is now strict.", citer_name)
            propagate_strictness(prover, citer_name, seen)
    prover.rebuild_document()


def is_qed(command: str) -> bool:
    return command.strip().rstrip(".").strip() == "qed"


__all__ = [
    "STATEMENT_KEYWORDS", "ANTI_KEYWORDS", "REFINE_VERBS", "RefinementRefused",
    "parse_statement", "enthymeme", "state", "parse_refine", "start_recording",
    "reopen", "abandon", "finish_recording", "propagate_strictness", "is_qed",
    "strip_outer_parens", "ProverError", "StrictnessRefused",
]
