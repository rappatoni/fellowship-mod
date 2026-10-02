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
from core.logging_util import TRACE, artifact
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


def shape_mismatch(sent, rebuilt, path=()):
    """Where two shapes first differ: (path, sent-subshape, rebuilt-subshape).

    ``path`` is the sequence of constructor slots walked to get there, e.g.
    ("mu", 2, "cons", 1), so a failure names a position in the term rather
    than only saying the two are different.  None when they agree.
    """
    if sent == rebuilt:
        return None
    if not (isinstance(sent, tuple) and isinstance(rebuilt, tuple)):
        return path, sent, rebuilt
    if not sent or not rebuilt or sent[0] != rebuilt[0]:
        return path, sent, rebuilt
    if len(sent) != len(rebuilt):
        return path, sent, rebuilt
    for i in range(1, len(sent)):
        found = shape_mismatch(sent[i], rebuilt[i], path + (sent[0], i))
        if found is not None:
            return found
    return path, sent, rebuilt


def _path_text(path) -> str:
    return "/".join(str(p) for p in path) or "the root"


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
    sent_shape, rebuilt_shape = shape(_peel_eta(term)), shape(_peel_eta(check.body))
    if sent_shape != rebuilt_shape:
        where = shape_mismatch(sent_shape, rebuilt_shape)
        if logger.isEnabledFor(logging.DEBUG):
            from pres.gen import pres_str
            logger.debug("typecheck: '%s' was reconstructed differently", name)
            artifact(logger, "typecheck: the term sent", pres_str(term))
            artifact(logger, "typecheck: what Fellowship rebuilt", pres_str(check.body))
            if where is not None:
                path, mine, theirs = where
                artifact(logger, "typecheck: first difference at %s" % _path_text(path),
                         "sent:     %s\nrebuilt:  %s" % (mine, theirs))
        detail = ""
        if where is not None:
            detail = f" The shapes first differ at {_path_text(where[0])}."
        raise TypeCheckFailed(
            f"Fellowship reconstructed a different term for '{name}' than the one unfolded; "
            f"the instructions do not encode the term.{detail}"
        )
    logger.debug("typecheck: '%s' replayed through Fellowship, the two terms agree "
                 "up to alpha, site numbering and proposition spelling", name)
    if logger.isEnabledFor(logging.DEBUG):
        from pres.gen import pres_str
        artifact(logger, "typecheck: the term sent", pres_str(term))
        artifact(logger, "typecheck: what Fellowship rebuilt", pres_str(check.body))
    return check.body


def typecheck_shared(prover, shared, name: str, checked=None) -> int:
    """Type-check a shared debate (core/dc/share.py) one definition at a
    time: replay each definition's skeleton - its body with every citation
    left as the open site of the cited statement - instead of the expanded
    term.  Returns the number of replays.

    Sound because names are transparent and typing is preserved by what
    expansion does at a citation: it puts there the cited definition's body
    (a term or context of the statement's type), a variable bound for that
    statement, or the statement's bare site.  So if every skeleton is
    well-typed, every expansion is.  The cost is the size of the document
    graph, not of the unfolded term.

    Precondition: no statement of the debate is spelled in two ways
    (``shared.spelling_clashes()`` is empty).  The graph identifies ``~A``
    with ``A -> false``, Fellowship does not, and the join of two spellings
    at a citation is in no skeleton; the caller replays the expanded term
    in that case.

    ``checked``, if a dict, maps the skeletons that already replayed in
    this document to the term Fellowship rebuilt for them, and is updated:
    declarations only grow, so a skeleton that type-checked once still
    does.  A skeleton found there is not replayed, but at DEBUG it is still
    reported with what Fellowship rebuilt then - the phase hands over its
    artifact either way.
    """
    replays = 0
    for statement, definition in shared.defs.items():
        if definition.trivial:
            continue                       # a bare site: nothing to check
        term = shared.skeleton(statement)
        key = (statement, shape(term))
        shown = f"{shared.graph.nodes.get(statement[0], statement[0])}[{statement[1][0]}]"
        if checked is not None and key in checked:
            if logger.isEnabledFor(logging.DEBUG):
                from pres.gen import pres_str
                logger.debug("typecheck: the debate about %s was replayed earlier in this "
                             "document; not replayed again", shown)
                artifact(logger, "typecheck: the term sent", pres_str(term))
                artifact(logger, "typecheck: what Fellowship rebuilt", pres_str(checked[key]))
            continue
        try:
            rebuilt = typecheck(prover, term, f"{name}_{replays + 1}",
                                shared.graph.nodes.get(statement[0], statement[0]),
                                statement[1] == "context")
        except TypeCheckFailed as e:
            raise TypeCheckFailed(f"in the debate about {shown}, which '{name}' reaches: {e}") from e
        replays += 1
        if checked is not None:
            checked[key] = rebuilt
    logger.debug("typecheck: '%s' - %d definition(s) replayed, %d already checked or bare",
                 name, replays, len(shared.defs) - replays)
    return replays
