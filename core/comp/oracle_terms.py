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


def freshen_binders(node, avoid: set):
    """A copy of ``node`` in which every binder (and its bound occurrences)
    got a fresh name outside ``avoid``.  Free occurrences are untouched.

    After this, no binder name in the result occurs in ``avoid``, and all
    binder names are pairwise distinct.
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
            new = fresh()
            inner_ctx = dict(ctx_env); inner_ctx[n.id.name] = new
            n.id.name = new
            walk(n.term, term_env, inner_ctx)
            walk(n.context, term_env, inner_ctx)
            return
        if isinstance(n, Mutilde):
            new = fresh()
            inner_term = dict(term_env); inner_term[n.di.name] = new
            n.di.name = new
            walk(n.term, inner_term, ctx_env)
            walk(n.context, inner_term, ctx_env)
            return
        if isinstance(n, Lamda):
            new = fresh()
            inner_term = dict(term_env); inner_term[n.di.di.name] = new
            n.di.di.name = new
            walk(n.term, inner_term, ctx_env)
            return
        if isinstance(n, Admal):
            new = fresh()
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


def substitute(node, kind, name: str, replacement):
    """Capture-avoiding substitution [replacement / name] on a copy of node.

    ``kind`` is DI for term variables, ID for context variables.  The
    strategy is freshen-then-replace: every binder in the target is renamed
    to a fresh name outside names(replacement) + {name}; afterwards every
    remaining occurrence of ``name`` is free and plain replacement cannot
    capture.
    """
    if kind not in (ID, DI):
        raise TypeError("kind must be the ID or DI class")
    _check_propositional(node, "Oracle substitution")
    _check_propositional(replacement, "Oracle substitution")
    prepared = freshen_binders(node, avoid=all_names(replacement) | {name})
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


def _step_command(term, context, strategy: str):
    """One root reduction of the command <term :: context>, or None.

    The critical pair (Mu vs Mutilde) is resolved by ``strategy``:
    "cbv" -> (mu<) first, "cbn" -> (>mu) first, per the COMMA paper.
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
        x = term.di.di.name
        prop = term.di.prop
        inner = Mutilde(DI(x, prop), prop, term.term, context.context)
        return context.term, inner

    if isinstance(term, Sonc) and isinstance(context, Admal):
        # < E1 * v :: E2 .a lam >  -->  < mu a.< v :: E2 > :: E1 >
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


def classify_nf(v) -> str:
    """Classify a (root-normal) expression per the spec grammar:

    - "open":      contains an unfilled obligation (Goal/Laog).
    - "exception": root is an affine Mu/Mutilde — a throw shape mu _.c
      (binder does not occur in its command), the abort of the paper's
      Example "Contradictory Proof terms".
    - "value":     anything else.  Presumptions (Deleg/Geled) may occur in
      a value: an IN-by-default site stays indeterminate and the value is
      polynomial in it (debate-graph-spec.org, Semantics subsection).
    """
    _check_propositional(v, "Oracle NF classification")
    if _contains(v, (Goal, Laog)):
        return "open"
    if isinstance(v, Mu):
        name = v.id.name
        if name == "_" or not (_occurs(v.term, ID, name) or _occurs(v.context, ID, name)):
            return "exception"
    if isinstance(v, Mutilde):
        name = v.di.name
        if name == "_" or not (_occurs(v.term, DI, name) or _occurs(v.context, DI, name)):
            return "exception"
    return "value"


# ---------------------------------------------------------------------------
# Site instantiation (the ev_m side of the adequacy square)
# ---------------------------------------------------------------------------

def make_abort_term(prop: str, inner_term, inner_context):
    """The throw term mu _:prop.< inner_term :: inner_context > — the
    vacuous affine witness of ``prop`` in the abort state (the abort
    morphism's syntax; see debate-graph-spec.org, Semantics subsection)."""
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
