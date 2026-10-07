"""Term-side oracle: trust level 3 of propositional-fragment-plan.org.

A minimal, self-contained normalizer implementing ONLY the four standard
reduction rules of the COMMA paper (sec. "The Matilda Calculus"):

  (->)   < lam x.v1 :: v2 * E >   -->  < v2 :: (< v1 :: E >).x mu' >
  (-)    < E1 * v :: E2 .a lam >  -->  < mu a.< v :: E2 > :: E1 >
  (mu<)  < mu a.c :: E >          -->  c[a <- E]
  (>mu)  < v :: c.x mu' >         -->  c[x <- v]

plus a capture-avoiding substitution, an alpha-equivalence check, an NF
classifier, and a site-instantiation builder for the adequacy commuting
square.  Everything here is written against the AST data classes only
(core/ac/ast.py); nothing is imported from the experimental reducers,
labellers, or graft code, whose behaviour this module exists to judge.

Commands are represented as (term, context) pairs.  Critical pairs
< mu a.c :: c'.x mu' > are resolved by the ``strategy`` parameter:
"cbv" applies (mu<) first, "cbn" applies (>mu) first, exactly as defined
in the paper.  Normalization is root-first and fuel-bounded; running out
of fuel raises instead of silently returning.
"""

import logging
from copy import deepcopy
from itertools import count

from core.ac.ast import (
    ProofTerm, Term, Context,
    Mu, Mutilde, Lamda, Admal, Hyp, Pyh, Cons, Sonc,
    Goal, Laog, Deleg, Geled, ID, DI,
    first_order_node, FirstOrderNotSupported,
)


# ---------------------------------------------------------------------------
# Names, renaming, substitution
#
# Two disjoint namespaces: term variables (DI leaves; bound by Mutilde and
# by Lamda through Hyp) and context variables (ID leaves; bound by Mu and
# by Admal through Pyh).
# ---------------------------------------------------------------------------

from core.logging_util import TRACE, artifact

logger = logging.getLogger(__name__)

def _check_propositional(node: ProofTerm, operation: str) -> None:
    found = first_order_node(node)
    if found is not None:
        raise FirstOrderNotSupported(operation, found)


def all_names(node) -> set:
    """Every variable and binder name in the tree, both namespaces pooled."""
    names = set()

    def walk(n):
        if n is None or not isinstance(n, ProofTerm):
            return
        if isinstance(n, (ID, DI)):
            names.add(n.name)
        if isinstance(n, Mu):
            names.add(n.id.name)
        if isinstance(n, Mutilde):
            names.add(n.di.name)
        if isinstance(n, Lamda):
            names.add(n.di.di.name)
        if isinstance(n, Admal):
            names.add(n.id.id.name)
        for slot in ("term", "context"):
            walk(getattr(n, slot, None))

    walk(node)
    return names


def freshen_binders(node, avoid: set, only=None):
    """A copy of ``node`` in which every binder (and its bound occurrences)
    got a fresh name outside ``avoid``.  Free occurrences are untouched.

    The placeholder ``_`` is left alone: no variable is ever called ``_``,
    so it binds nothing and can capture nothing, and the scaffold matcher
    recognises a scaffold's inner pair by it - renamed, a scaffold that
    passed through a substitution was no longer recognisable (lesson 13 of
    minicourse-evaluation.org; aida-unfold-scaffold-binders-capture).

    After this, no binder name in the result occurs in ``avoid``, and all
    binder names other than ``_`` are pairwise distinct.

    ``only``: rename just the binders whose name is in it and keep every
    other binder's name, so that a substitution leaves the names of the
    arguments it passes through alone (``substitute(minimal=True)``).
    Afterwards no binder name occurs in ``avoid & only``.
    """
    node = deepcopy(node)
    taken = set(avoid) | all_names(node)
    counter = count(1)

    def fresh() -> str:
        while True:
            name = f"b{next(counter)}"
            if name not in taken:
                taken.add(name)
                return name

    def keep_placeholder(name: str) -> str:
        if name == "_" or (only is not None and name not in only):
            return name
        return fresh()

    def walk(n, term_env: dict, ctx_env: dict):
        """term_env / ctx_env map in-scope old binder names to new ones."""
        if n is None or not isinstance(n, ProofTerm):
            return
        if isinstance(n, DI):
            if n.name in term_env:
                n.name = term_env[n.name]
            return
        if isinstance(n, ID):
            if n.name in ctx_env:
                n.name = ctx_env[n.name]
            return
        if isinstance(n, Mu):
            new = keep_placeholder(n.id.name)
            inner_ctx = dict(ctx_env); inner_ctx[n.id.name] = new
            n.id.name = new
            walk(n.term, term_env, inner_ctx)
            walk(n.context, term_env, inner_ctx)
            return
        if isinstance(n, Mutilde):
            new = keep_placeholder(n.di.name)
            inner_term = dict(term_env); inner_term[n.di.name] = new
            n.di.name = new
            walk(n.term, inner_term, ctx_env)
            walk(n.context, inner_term, ctx_env)
            return
        if isinstance(n, Lamda):
            new = keep_placeholder(n.di.di.name)
            inner_term = dict(term_env); inner_term[n.di.di.name] = new
            n.di.di.name = new
            walk(n.term, inner_term, ctx_env)
            return
        if isinstance(n, Admal):
            new = keep_placeholder(n.id.id.name)
            inner_ctx = dict(ctx_env); inner_ctx[n.id.id.name] = new
            n.id.id.name = new
            walk(n.context, term_env, inner_ctx)
            return
        for slot in ("term", "context"):
            walk(getattr(n, slot, None), term_env, ctx_env)

    walk(node, {}, {})
    return node


def _replace_free(node, kind, name: str, replacement):
    """Replace free occurrences of the named variable by copies of
    ``replacement``.  ``kind`` is DI (term variable) or ID (context
    variable).  PRECONDITION: no binder in ``node`` is called ``name`` and
    no binder in ``node`` occurs in ``replacement``'s names — use
    ``substitute`` below, which establishes this via freshen_binders."""
    if isinstance(node, kind) and node.name == name:
        return deepcopy(replacement)
    for slot in ("term", "context"):
        child = getattr(node, slot, None)
        if isinstance(child, ProofTerm):
            setattr(node, slot, _replace_free(child, kind, name, replacement))
    return node


def substitute(node, kind, name: str, replacement, minimal=False):
    """Capture-avoiding substitution [replacement / name] on a copy of node.

    ``kind`` is DI for term variables, ID for context variables.  The
    strategy is freshen-then-replace: every binder in the target is renamed
    to a fresh name outside names(replacement) + {name}; afterwards every
    remaining occurrence of ``name`` is free and plain replacement cannot
    capture.  ``minimal``: rename only the binders that could capture, i.e.
    those named like ``name`` or like a name of ``replacement``; every
    other binder keeps its name.
    """
    if kind not in (ID, DI):
        raise TypeError("kind must be the ID or DI class")
    _check_propositional(node, "Oracle substitution")
    _check_propositional(replacement, "Oracle substitution")
    avoid = all_names(replacement) | {name}
    prepared = freshen_binders(node, avoid=avoid, only=avoid if minimal else None)
    return _replace_free(prepared, kind, name, replacement)


# ---------------------------------------------------------------------------
# Alpha-equivalence
# ---------------------------------------------------------------------------

def canonical_form(node) -> tuple:
    """A nested-tuple canonical form: binders numbered by traversal order,
    free variables kept by name.  Two trees are alpha-equivalent iff their
    canonical forms are equal.  Props are compared verbatim (canonical
    proposition identity is milestone M2's business, not this function's).
    """
    counter = count(0)

    def walk(n, term_env, ctx_env):
        if n is None:
            return None
        if isinstance(n, DI):
            return ("di", term_env.get(n.name, ("free", n.name)))
        if isinstance(n, ID):
            return ("id", ctx_env.get(n.name, ("free", n.name)))
        if isinstance(n, Mu):
            i = next(counter)
            inner = dict(ctx_env); inner[n.id.name] = i
            return ("mu", n.prop,
                    walk(n.term, term_env, inner), walk(n.context, term_env, inner))
        if isinstance(n, Mutilde):
            i = next(counter)
            inner = dict(term_env); inner[n.di.name] = i
            return ("mutilde", n.prop,
                    walk(n.term, inner, ctx_env), walk(n.context, inner, ctx_env))
        if isinstance(n, Lamda):
            i = next(counter)
            inner = dict(term_env); inner[n.di.di.name] = i
            return ("lamda", n.di.prop, walk(n.term, inner, ctx_env))
        if isinstance(n, Admal):
            i = next(counter)
            inner = dict(ctx_env); inner[n.id.id.name] = i
            return ("admal", n.id.prop, walk(n.context, term_env, inner))
        if isinstance(n, Cons):
            return ("cons", walk(n.term, term_env, ctx_env), walk(n.context, term_env, ctx_env))
        if isinstance(n, Sonc):
            return ("sonc", walk(n.context, term_env, ctx_env), walk(n.term, term_env, ctx_env))
        if isinstance(n, Goal):
            return ("goal", n.number, n.prop)
        if isinstance(n, Laog):
            return ("laog", n.number, n.prop)
        if isinstance(n, Deleg):
            return ("deleg", n.number, n.prop)
        if isinstance(n, Geled):
            return ("geled", n.number, n.prop)
        raise ValueError(f"canonical_form: unhandled node {type(n).__name__}")

    _check_propositional(node, "Oracle canonical form")
    return walk(node, {}, {})


def alpha_equal(a, b) -> bool:
    return canonical_form(a) == canonical_form(b)


# ---------------------------------------------------------------------------
# The four standard reductions
# ---------------------------------------------------------------------------

class OracleFuelExhausted(RuntimeError):
    """Normalization did not reach a root normal form within the fuel bound."""


def _step_command(term, context, strategy: str, fired=None):
    """One root reduction of the command <term :: context>, or None.

    The critical pair (Mu vs Mutilde) is resolved by ``strategy``:
    "cbv" -> (mu<) first, "cbn" -> (>mu) first, per the COMMA paper.

    ``fired``, if a list, receives the name of the rule applied - the only
    way the rule reaches a caller, since the return value is the rewritten
    command.
    """
    is_mu = isinstance(term, Mu)
    is_mutilde = isinstance(context, Mutilde)

    if is_mu and is_mutilde:
        order = ("mu<", ">mu") if strategy == "cbv" else (">mu", "mu<")
    elif is_mu:
        order = ("mu<",)
    elif is_mutilde:
        order = (">mu",)
    else:
        order = ()

    for rule in order:
        if fired is not None:
            fired.append(rule)
        if rule == "mu<":
            # < mu a.c :: E >  -->  c[a <- E]
            new_term = substitute(term.term, ID, term.id.name, context)
            new_context = substitute(term.context, ID, term.id.name, context)
            return new_term, new_context
        if rule == ">mu":
            # < v :: c.x mu' >  -->  c[x <- v]
            new_term = substitute(context.term, DI, context.di.name, term)
            new_context = substitute(context.context, DI, context.di.name, term)
            return new_term, new_context

    if isinstance(term, Lamda) and isinstance(context, Cons):
        # < lam x.v1 :: v2 * E >  -->  < v2 :: (< v1 :: E >).x mu' >
        if fired is not None:
            fired.append("(->)")
        x = term.di.di.name
        prop = term.di.prop
        inner = Mutilde(DI(x, prop), prop, term.term, context.context)
        return context.term, inner

    if isinstance(term, Sonc) and isinstance(context, Admal):
        # < E1 * v :: E2 .a lam >  -->  < mu a.< v :: E2 > :: E1 >
        if fired is not None:
            fired.append("(-)")
        a = context.id.id.name
        prop = context.id.prop
        inner = Mu(ID(a, prop), prop, term.term, context.context)
        return inner, term.context

    return None


def normalize_command(term, context, strategy: str = "cbn", fuel: int = 500):
    """Iterate root reductions until none applies.  Weak reduction only: no
    reduction under binders — both sides of the adequacy square go through
    this same function, which is all the oracle needs."""
    if strategy not in ("cbn", "cbv"):
        raise ValueError(f"strategy must be 'cbn' or 'cbv', got {strategy!r}")
    _check_propositional(term, "Oracle normalization")
    _check_propositional(context, "Oracle normalization")
    term, context = deepcopy(term), deepcopy(context)
    for _ in range(fuel):
        step = _step_command(term, context, strategy)
        if step is None:
            return term, context
        term, context = step
    raise OracleFuelExhausted(
        f"No root normal form after {fuel} steps under {strategy}"
    )


def normalize_term(v, strategy: str = "cbn", fuel: int = 500):
    """Normalize the command under a root Mu/Mutilde binder, rebuilding the
    binder; other roots are returned unchanged (already weak-normal)."""
    _check_propositional(v, "Oracle normalization")
    v = deepcopy(v)
    if isinstance(v, (Mu, Mutilde)):
        t, c = normalize_command(v.term, v.context, strategy=strategy, fuel=fuel)
        v.term, v.context = t, c
    return v


def _step_anywhere(node, strategy: str, fired=None):
    """Fire ONE reduction at the leftmost-outermost command with a root
    redex.  Command positions are exactly the (term, context) pairs of
    Mu/Mutilde binders.  Returns the rewritten node, or None if the whole
    tree is in normal form.
    """
    if isinstance(node, (Mu, Mutilde)):
        step = _step_command(node.term, node.context, strategy, fired)
        if step is not None:
            node.term, node.context = step
            return node
    for slot in ("term", "context"):
        child = getattr(node, slot, None)
        if isinstance(child, ProofTerm):
            stepped = _step_anywhere(child, strategy, fired)
            if stepped is not None:
                setattr(node, slot, stepped)
                return node
    return None


def normalize_strong(v, strategy: str = "cbn", fuel: int = 2000):
    """Full normalization: the four standard rules under all congruences.

    The congruence order is fixed - leftmost-outermost, term slot before
    context slot, one step at a time with restart from the root - so the
    result is deterministic by construction (part of the T1 obligation;
    the skip rules of call-by-onus.org are unsettled theory, and this
    function fixes one order rather than deriving it).  Critical pairs
    are resolved by ``strategy`` exactly as in the weak normalizer; on
    debate terms this runs AFTER sigma has resolved the scaffolds, so
    only base-strategy pairs remain.
    """
    if strategy not in ("cbn", "cbv"):
        raise ValueError(f"strategy must be 'cbn' or 'cbv', got {strategy!r}")
    _check_propositional(v, "Oracle normalization")
    v = deepcopy(v)
    tracing = logger.isEnabledFor(TRACE)
    for step in range(1, fuel + 1):
        fired = [] if tracing else None
        stepped = _step_anywhere(v, strategy, fired)
        if stepped is None:
            logger.debug("normalise: normal form after %d step(s) under %s", step - 1, strategy)
            if logger.isEnabledFor(logging.DEBUG):
                from pres.gen import pres_tree
                artifact(logger, "normalise: the normal form", pres_tree(v))
            return v
        if tracing:
            logger.log(TRACE, "  normalise: [%d] %s", step, fired[-1] if fired else "?")
        v = stepped
    raise OracleFuelExhausted(
        f"No full normal form after {fuel} steps under {strategy}"
    )


# ---------------------------------------------------------------------------
# Normal-form classifier
# ---------------------------------------------------------------------------

def _occurs(node, kind, name: str) -> bool:
    """Free occurrence check (shadowing-aware) for the named variable."""
    if isinstance(node, kind) and node.name == name:
        return True
    if kind is ID and isinstance(node, Mu) and node.id.name == name:
        return False
    if kind is ID and isinstance(node, Admal) and node.id.id.name == name:
        return False
    if kind is DI and isinstance(node, Mutilde) and node.di.name == name:
        return False
    if kind is DI and isinstance(node, Lamda) and node.di.di.name == name:
        return False
    for slot in ("term", "context"):
        child = getattr(node, slot, None)
        if isinstance(child, ProofTerm) and _occurs(child, kind, name):
            return True
    return False


def _contains(node, kinds) -> bool:
    if isinstance(node, kinds):
        return True
    for slot in ("term", "context"):
        child = getattr(node, slot, None)
        if isinstance(child, ProofTerm) and _contains(child, kinds):
            return True
    return False


def _is_falsum(prop) -> bool:
    return isinstance(prop, str) and prop.strip() in ("false", "\u22a5", "_F_")


def _is_verum(prop) -> bool:
    return isinstance(prop, str) and prop.strip() in ("true", "\u22a4", "_T_")


def _is_affine_node(n) -> bool:
    """An affine mu/mu' that could have returned and does not.  A mu of
    type false (dually a mu' of type true) is exempt: there is nothing a
    continuation for false could be given, so leaving it unused is how
    false is inhabited - lambda h. mu _:false.< h || E > is the negation
    introduction, not an abort."""
    if isinstance(n, Mu):
        if _is_falsum(n.prop):
            return False
        name = n.id.name
        return name == "_" or not (_occurs(n.term, ID, name) or _occurs(n.context, ID, name))
    if isinstance(n, Mutilde):
        if _is_verum(n.prop):
            return False
        name = n.di.name
        return name == "_" or not (_occurs(n.term, DI, name) or _occurs(n.context, DI, name))
    return False


def _free_names(n, bound_ids=frozenset(), bound_dis=frozenset()):
    """{("id"|"di", name)} free in n, shadowing-aware."""
    out = set()
    if isinstance(n, ID):
        if n.name not in bound_ids:
            out.add(("id", n.name))
    elif isinstance(n, DI):
        if n.name not in bound_dis:
            out.add(("di", n.name))
    elif isinstance(n, Mu):
        out |= _free_names(n.term, bound_ids | {n.id.name}, bound_dis)
        out |= _free_names(n.context, bound_ids | {n.id.name}, bound_dis)
    elif isinstance(n, Mutilde):
        out |= _free_names(n.term, bound_ids, bound_dis | {n.di.name})
        out |= _free_names(n.context, bound_ids, bound_dis | {n.di.name})
    elif isinstance(n, Lamda):
        out |= _free_names(n.term, bound_ids, bound_dis | {n.di.di.name})
    elif isinstance(n, Admal):
        out |= _free_names(n.context, bound_ids | {n.id.id.name}, bound_dis)
    else:
        for slot in ("term", "context"):
            child = getattr(n, slot, None)
            if isinstance(child, ProofTerm):
                out |= _free_names(child, bound_ids, bound_dis)
    return out


def contains_uncatchable_clash(v, catchers_ids=frozenset(), catchers_dis=frozenset()) -> bool:
    """An *uncatchable clash* is an affine mu/mu' whose command no
    enclosing mu/mu' binder can catch: none of the command's free
    variables is bound by an enclosing Mu (for an ID) or Mutilde (for a
    DI).  Lambda- and Admal-bound variables cannot catch (COMMA 2026:
    "any mu-abstracted term or context binding a variable free in the
    thrown command can serve as a catch"), nor can declared names, which
    are constants.  Such a clash is the paper's abort - "an uncatchable
    exception that will consume any context to which it is passed" - and
    a term containing one is an exception whatever surrounds it."""
    if _is_affine_node(v):
        free = _free_names(v)
        caught = any((kind == "id" and name in catchers_ids) or (kind == "di" and name in catchers_dis)
                     for kind, name in free)
        if not caught:
            return True
    if isinstance(v, Mu):
        ids, dis = catchers_ids | {v.id.name}, catchers_dis
    elif isinstance(v, Mutilde):
        ids, dis = catchers_ids, catchers_dis | {v.di.name}
    else:
        ids, dis = catchers_ids, catchers_dis
    for slot in ("term", "context"):
        child = getattr(v, slot, None)
        if isinstance(child, ProofTerm) and contains_uncatchable_clash(child, ids, dis):
            return True
    return False


def classify_nf(v) -> str:
    """Classify a (root-normal) expression per the spec grammar:

    - "open":      contains an unfilled obligation (Goal/Laog).
    - "exception": contains an uncatchable clash - an affine Mu/Mutilde
      (binder does not occur in its command) that no enclosing mu/mu'
      binder can catch: the abort of the paper's Example "Contradictory
      Proof terms", which a defeated site holds as mu alpha.< t || e >.
      The root being such a node is the special case.
    - "value":     anything else.  Presumptions (Deleg/Geled) may occur in
      a value: an IN-by-default site stays indeterminate and the value is
      polynomial in it (debate-graph-spec.org, Semantics subsection).
    """
    _check_propositional(v, "Oracle NF classification")
    if _contains(v, (Goal, Laog)):
        logger.debug("classify: OPEN%s", _because_open(v))
        return "open"
    if contains_uncatchable_clash(v):
        logger.debug("classify: EXCEPTION (an uncatchable clash: the debate is "
                     "acceptable only given an inconsistency)")
        return "exception"
    logger.debug("classify: VALUE")
    return "value"


def _because_open(node) -> str:
    """The first unfilled obligation, for the classifier's log line."""
    if isinstance(node, (Goal, Laog)):
        return " (obligation %s:%s undischarged)" % (
            getattr(node, "number", "?"), getattr(node, "prop", "?"))
    for slot in ("term", "context"):
        child = getattr(node, slot, None)
        if isinstance(child, ProofTerm):
            found = _because_open(child)
            if found:
                return found
    return ""


# ---------------------------------------------------------------------------
# Conservativity (V2 of propositional-fragment-plan.org, T2-lite)
# ---------------------------------------------------------------------------

class ConservativityViolation(AssertionError):
    """A strict, closed input normalised to a term with an open leaf."""


def is_strict_closed(node) -> bool:
    """No site at all: neither obligations (Goal/Laog) nor presumptions
    (Deleg/Geled).  Such a term lives in the base category C."""
    return not _contains(node, (Goal, Laog, Deleg, Geled))


def check_conservativity(before, after, *, operation: str = "normalisation"):
    """Executable postcondition: reduction is conservative over C.

    If ``before`` has no site, ``after`` may hold no open leaf - reduction
    of a strict argument can never manufacture an obligation.  Inputs that
    already carry a site are exempt (their normal form may legitimately
    keep it, see classify_nf).  Raises ConservativityViolation."""
    if not is_strict_closed(before):
        logger.debug("conservativity: not checked (%s already carried a site)", operation)
        return
    if _contains(after, (Goal, Laog)):
        raise ConservativityViolation(
            f"{operation} of a strict, closed term produced an open obligation: "
            f"reduction is not conservative over the strict fragment here."
        )
    logger.debug("conservativity: %s held (a strict, closed input stayed closed)", operation)


# ---------------------------------------------------------------------------
# Site instantiation (the ev_m side of the adequacy square)
# ---------------------------------------------------------------------------

def make_abort_term(prop: str, inner_term, inner_context):
    """The throw term mu _:prop.< inner_term :: inner_context >.

    An affine mu discarding the demand for ``prop``: the COMMA paper's
    throw shape.  At a defeated site ``inner_context`` is the winning
    refutation and ``inner_term`` the site's original - its derivation,
    or its bare indeterminate - so the result is the *clash*
    mu alpha.< t || E >, the paper's abort.  This constructor accepts
    any well-typed pair; it does not conjure a payload, and a caller
    passing a free variable as ``inner_term`` gets a term with a free
    variable.  (An earlier docstring called this "the vacuous affine
    witness in the abort state, T -> F -> A"; that misread an ednote
    about ex contradictione quodlibet, which needs a contradiction in
    hand, not a refutation facing a hole.)"""
    return Mu(ID("_", prop), prop, inner_term, inner_context)


def make_abort_context(prop: str, inner_term, inner_context):
    """Context-side dual of make_abort_term."""
    return Mutilde(DI("_", prop), prop, inner_term, inner_context)


def instantiate_sites(node, site_map):
    """Build ev_m(node) by direct syntactic substitution at open sites.

    ``site_map`` maps a site number (the .number of a Goal/Laog/Deleg/Geled
    leaf) to a replacement node; term-side sites need Term replacements,
    context-side sites Context replacements.  Sites absent from the map are
    left indeterminate (the IN-by-default case).  Returns a copy.
    """
    _check_propositional(node, "Oracle instantiation")

    def walk(n):
        if isinstance(n, (Goal, Deleg)) and n.number in site_map:
            repl = site_map[n.number]
            if not isinstance(repl, Term):
                raise TypeError(
                    f"Site {n.number!r} is term-side; replacement must be a Term, "
                    f"got {type(repl).__name__}"
                )
            return deepcopy(repl)
        if isinstance(n, (Laog, Geled)) and n.number in site_map:
            repl = site_map[n.number]
            if not isinstance(repl, Context):
                raise TypeError(
                    f"Site {n.number!r} is context-side; replacement must be a "
                    f"Context, got {type(repl).__name__}"
                )
            return deepcopy(repl)
        for slot in ("term", "context"):
            child = getattr(n, slot, None)
            if isinstance(child, ProofTerm):
                setattr(n, slot, walk(child))
        return n

    return walk(deepcopy(node))
