"""Type checking by Fellowship replay (the type oracle).

An unfolded debate term (core/dc/unfold.py) is only as sound as the
unfolding algorithm: if every argument in the document type-checks, the
term unfolded from them must type-check too.  The prover is the ground
truth for that.  ``typecheck`` regenerates Fellowship instructions from
the term, replays them as a fresh theorem (discarded afterwards), and
compares the proof term Fellowship reconstructs with the term we sent,
up to alpha-equivalence, site numbering and proposition spelling.

Every test that unfolds a fixture goes through this; the CLI does too
unless `typecheck off` is given (FSP_TYPECHECK=0 sets the default), a
switch for production runs where the replay's cost matters.
"""

import logging
from copy import deepcopy
from itertools import count

from core.ac.ast import (
    ProofTerm, Mu, Mutilde, Lamda, Admal, Cons, Sonc,
    Goal, Laog, Deleg, Geled, ID, DI,
)
from core.ac.instructions import InstructionsGenerationVisitor
from core.comp.enrich import PropEnrichmentVisitor
from core.dc.argument import Argument
from core.dc.debate_graph import canonical_prop, _peel_eta
from core.logging_util import TRACE
from wrap.prover import ProverError

logger = logging.getLogger(__name__)


class TypeCheckFailed(Exception):
    """Fellowship rejected the term, or reconstructed a different one."""


def shape(node) -> tuple:
    """Canonical form up to alpha-equivalence, site numbering (sites are
    numbered by traversal order) and proposition spelling (canonical)."""
    binders = count(0)
    sites = count(0)

    def prop(p):
        try:
            return canonical_prop(p) if p else None
        except Exception:
            return p

    def walk(n, term_env, ctx_env):
        if n is None:
            return None
        if isinstance(n, DI):
            return ("di", term_env.get(n.name, ("free", n.name)))
        if isinstance(n, ID):
            return ("id", ctx_env.get(n.name, ("free", n.name)))
        if isinstance(n, Mu):
            i = next(binders); inner = dict(ctx_env); inner[n.id.name] = i
            return ("mu", prop(n.prop), walk(n.term, term_env, inner), walk(n.context, term_env, inner))
        if isinstance(n, Mutilde):
            i = next(binders); inner = dict(term_env); inner[n.di.name] = i
            return ("mutilde", prop(n.prop), walk(n.term, inner, ctx_env), walk(n.context, inner, ctx_env))
        if isinstance(n, Lamda):
            i = next(binders); inner = dict(term_env); inner[n.di.di.name] = i
            return ("lamda", prop(n.di.prop), walk(n.term, inner, ctx_env))
        if isinstance(n, Admal):
            i = next(binders); inner = dict(ctx_env); inner[n.id.id.name] = i
            return ("admal", prop(n.id.prop), walk(n.context, term_env, inner))
        if isinstance(n, Cons):
            return ("cons", walk(n.term, term_env, ctx_env), walk(n.context, term_env, ctx_env))
        if isinstance(n, Sonc):
            return ("sonc", walk(n.context, term_env, ctx_env), walk(n.term, term_env, ctx_env))
        for cls, tag in ((Goal, "goal"), (Laog, "laog"), (Deleg, "deleg"), (Geled, "geled")):
            if isinstance(n, cls):
                return (tag, next(sites), prop(n.prop))
        raise ValueError(f"shape: unhandled node {type(n).__name__}")

    return walk(node, {}, {})


def typecheck(prover, term: ProofTerm, name: str, conclusion: str, is_anti: bool) -> ProofTerm:
    """Replay ``term`` through Fellowship as a theorem (antitheorem if
    ``is_anti``) for ``conclusion``.  Returns the proof term Fellowship
    reconstructs; raises TypeCheckFailed otherwise."""
    import warnings
    # The replay is a check, not a step to narrate: silence the loggers that
    # would otherwise narrate it.  core.comp.enrich is here because its ~40
    # lines per replay drown the pipeline's own account at DEBUG, and the
    # enrichment below is part of the replay, not of the debate.
    quiet = [logging.getLogger("core.dc.argument"), logging.getLogger("fsp.wrapper"),
             logging.getLogger("core.comp.enrich")]
    levels = [lg.level for lg in quiet]
    for lg in quiet:
        lg.setLevel(logging.WARNING)
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")        # enrichment of scaffold wiring is not the point here
            body = PropEnrichmentVisitor(axiom_props=prover.declarations).visit(deepcopy(term))
            instructions = list(InstructionsGenerationVisitor().return_instructions(body))
    finally:
        for lg, level in zip(quiet, levels):
            lg.setLevel(level)
    logger.debug("typecheck: replaying '%s' (%s %s) as %d instruction(s)",
                 name, "antitheorem" if is_anti else "theorem", conclusion, len(instructions))
    if logger.isEnabledFor(TRACE):
        for i, instruction in enumerate(instructions, 1):
            logger.log(TRACE, "  typecheck: [%d] %s", i, instruction)
    check = Argument(prover, f"typecheck_{name}", conclusion, instructions, is_anti=is_anti)
    levels = [lg.level for lg in quiet]
    for lg in quiet:
        lg.setLevel(logging.WARNING)
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            check.execute()
    except ProverError as e:
        logger.debug("typecheck: Fellowship rejected '%s': %s", name, e)
        try:
            prover.send_command("discard theorem.")   # leave the prover clean
        except ProverError:
            pass
        raise TypeCheckFailed(
            f"Fellowship rejected the unfolded term for '{name}': {e}"
        ) from e
    finally:
        for lg, level in zip(quiet, levels):
            lg.setLevel(level)
    if shape(_peel_eta(check.body)) != shape(_peel_eta(term)):
        if logger.isEnabledFor(logging.DEBUG):
            from pres.gen import pres_str
            logger.debug("typecheck: '%s' reconstructed differently", name)
            logger.debug("  typecheck: sent        %s", pres_str(term))
            logger.debug("  typecheck: reconstructed %s", pres_str(check.body))
        raise TypeCheckFailed(
            f"Fellowship reconstructed a different term for '{name}' than the one unfolded; "
            f"the instructions do not encode the term."
        )
    logger.debug("typecheck: '%s' replayed, shape matches", name)
    return check.body
