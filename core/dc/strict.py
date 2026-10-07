"""Strictness, read off the concrete debate term.

Capturing is a debate phenomenon: in the argumentation framework a cycle
through obligations is just a cycle, but in the unfolded debate term the
opponent's assumption "not P" has been bound to the proponent's own
continuation - the trap has been sprung - and what remains may be a
closed proof.  The framework cannot see that (a demand met inside a
scope is not a claim it knows), so strictness is evaluated here, on the
term, after unfolding has applied the substitutions (author,
2026-09-17).  This is the COMMA paper's call-by-onus with its strict
phase only: strict redundancy and strict defeat decide a scaffold when
one wing is strict *in the debate* and the other is not; everything else
is delayed for the labelling.

Strictness asks what a term RESTS ON, and only that (author,
2026-09-25, tasks.org aida-strict-phase-experimental).  What a term
PRODUCES - a value or an exception - is a separate question, answered by
the normal form's class and by the strict-edge rule below.  Two rules
that confused the two were removed on that date: "a captured presumption
is an assumption" and "a subterm holding an uncatchable clash is not
strict".

- A free variable is not something a subterm rests on.  The unfolded
  term is closed as a whole (Fellowship replays it), so every variable
  free in a subterm is bound by an enclosing binder of the debate, and a
  binder is a commitment the debate has already made.  That is why a
  captured obligation counts as met; a captured presumption is the same
  situation, so the ``captured_presumption`` marker is not consulted
  here.  Classically ~P -> P gives P: a self-attacking argument derives
  its conclusion, and now does.
- A clash is not something a subterm rests on; it is what the subterm
  produces.  A closed clash is a strict proof of contradiction, so it
  decides a scaffold like any other strict wing, and it then bubbles up:
  ``classify_nf`` reports the exception at the root, which says the term
  is acceptable only given an inconsistency.  What it must NOT do is
  claim a strict edge, since it derives its statement only from that
  inconsistency - see ``strict_resolve``'s collector, which applies the
  clash test there, where it belongs.

Two notions of strictness:

- *strict in the debate* (the paper's "strict in d"): a subterm with no
  open site.  This is what decides a scaffold.
- *closed*: no open site and no free variable but declared names.  A
  closed subterm that proves (refutes) a statement and owes its
  closedness to a strictly decided scaffold is a strict derivation the
  framework did not have; it becomes a strict, source-less edge of the
  issue graph, so the labelling agrees with the term.  Peirce's law: the
  whole term is closed after its scaffolds are decided, while the inner
  proof of P is not (it uses the hypothesis f), so exactly the thesis
  gets a strict edge and P stays a claim nobody established.

Decision rules at a scaffold (orig t, supporter t2 / attacker e2; S =
strict in the debate):

    support:  S(t) -> keep t;  else S(t2) -> keep t2;  else delay
    attack:   S(e2) and not S(t) -> defeat (the clash);
              S(t) and not S(e2) -> keep t;  else delay

Both wings strict at an attack is the inconsistent case, delayed for the
labelling (CONTR); the paper says proofs can be strictly defeated there.
"""

import logging
import re
from copy import deepcopy
from dataclasses import replace

from core.ac.ast import (
    ProofTerm, Mu, Mutilde, Goal, Laog, Deleg, Geled, ID, DI,
)
from core.comp.oracle_terms import (
    _occurs, _free_names, _contains, contains_uncatchable_clash, substitute,
)
from core.dc.debate_graph import (
    DebateGraph, Edge, canonical_prop, compile_debate, _match_scaffold, _restore_scion,
    scaffold_parts, BUILTIN_LEAVES, binder_origin,
)
from core.logging_util import TRACE, artifact

logger = logging.getLogger(__name__)


def _show(statement, prop=None) -> str:
    """``Prop[t]`` / ``Prop[c]`` for a log line.  ``prop`` is the surface
    spelling when the caller has it; the canonical key is the fallback."""
    key, side = statement
    return f"{prop or key}[{side[0]}]"


def key_of(statement):
    return statement[0]


_SITES = (Goal, Laog, Deleg, Geled)


def _is_assumption(n) -> bool:
    """An open site, and nothing else.  A captured variable is not an
    assumption of the subterm: the binder that caught it is a commitment
    the debate already made, whether the site was an obligation or a
    presumption (author, 2026-09-25)."""
    return isinstance(n, _SITES)


def _contains_assumption(node) -> bool:
    if _is_assumption(node):
        return True
    for slot in ("term", "context"):
        child = getattr(node, slot, None)
        if isinstance(child, ProofTerm) and _contains_assumption(child):
            return True
    return False


def strict_in_debate(node, catchers_ids=frozenset(), catchers_dis=frozenset()) -> bool:
    """A strict proof or refutation in the debate: no open site anywhere
    in the subterm.  Nothing else - strictness is what the term rests on
    (module docstring).  A defeated derivation is still strict; it is an
    exception, which the normal form's class reports and which bars the
    strict EDGE, not the decision.  ``catchers_*`` are accepted and
    ignored: they belonged to the clash test, which now lives in
    ``strict_resolve``'s collector."""
    return not _contains_assumption(node)


#: How many rounds of substitution ``strict_in_scope`` takes at most; each
#: round replaces the scaffold variables free in the term by what stands
#: outside them, so the chain ends long before.
_SCOPE_ROUNDS = 64


def strict_in_scope(node, scope) -> bool:
    """Strict in the debate, with the scaffolds' own variables taken for
    what they stand for (aida-unfold-scaffold-binders-capture).  An
    argument's binder is a commitment the debate has made; a scaffold's
    is not.  ``scope`` maps the name of every scaffold variable in scope
    to ("b", kind, T) - the scaffold's b, standing for its supported term
    T after the outer step - or ("alt", kind, prop) - its alt, standing
    for "the statement fails", which no strictness can settle.  Each such
    variable free in ``node`` is substituted in a copy (b by T, alt by an
    open site) until none is left, and the copy is asked for an open site
    as usual.  The chain goes outward, so it ends."""
    if not scope:
        return strict_in_debate(node)
    term = node
    for _ in range(_SCOPE_ROUNDS):
        free = [(kind, name) for kind, name in _free_names(term) if name in scope]
        if not free:
            return strict_in_debate(term)
        for _kind, name in free:
            role, kind, what = scope[name]
            if role == "alt":
                # an open site stands where "the statement fails" would
                return False
            term = substitute(term, kind, name, what, minimal=True)
    raise RuntimeError("strict: scaffold variables do not resolve outward")


def _paper_beta(node):
    """(kind, name, orig) of a paper scaffold's b: the variable, its kind
    (DI on the term side, ID on the context side) and the term it is bound
    to by the outer step.  None for a legacy shape, whose inner binders
    are placeholders."""
    if isinstance(node, Mu) and isinstance(node.context, Mutilde) and node.context.di.name != "_":
        return DI, node.context.di.name, node.term
    if isinstance(node, Mutilde) and isinstance(node.term, Mu) and node.term.id.name != "_":
        return ID, node.term.id.name, node.context
    return None


def _outer_step(node, scion):
    """The scion after the scaffold's outer step, b := the original.  The
    rewrites below drop b's binder; a scion that uses b (a supporter that
    needs the statement it supports, captured by b) must not be left with
    it unbound."""
    beta = _paper_beta(node)
    if beta is None or not _occurs(scion, beta[0], beta[1]):
        return scion
    kind, name, orig = beta
    return substitute(scion, kind, name, orig, minimal=True)


def is_closed(node, strict_names=()) -> bool:
    """No open site and no free variable but declared names."""
    if not strict_in_debate(node):
        return False
    names = set(strict_names or ()) | set(BUILTIN_LEAVES)
    return all(name in names for _kind, name in _free_names(node))


# -- wing keeping (shared with core/comp/evaluate.py) -----------------------

def _eta(node_is_term, wing, alt, prop):
    """The kept wing with the scaffold's outer binder, eta-reduced away
    when the wing does not use it (a wing that captured alt keeps it)."""
    if node_is_term:
        if not _occurs(wing, ID, alt):
            return deepcopy(wing)
        return Mu(ID(alt, prop), prop, deepcopy(wing), ID(alt, prop))
    if not _occurs(wing, DI, alt):
        return deepcopy(wing)
    return Mutilde(DI(alt, prop), prop, DI(alt, prop), deepcopy(wing))


def keep_orig(node, orig, alt):
    """The original wins.  Paper shapes: inner pair resolved towards
    <beta||alpha>, then >mu at the outer pair binds beta := original:
    mu alpha.<orig || alpha> -> orig.  Legacy shapes: (mu<) then eta."""
    return _eta(isinstance(node, Mu), orig, alt, node.prop)


def keep_scion_support(node, scion, alt):
    """The supporter wins: the outer pair binds b := the original, the
    inner pair goes to <t2||alpha>: mu alpha.<t2[b:=orig] || alpha> -> t2
    (eta, when t2 does not use alpha)."""
    return _eta(isinstance(node, Mu), _outer_step(node, scion), alt, node.prop)


def keep_attack_wing(node, match):
    """The attacker wins: the site holds the real clash.

    Paper shapes: mu alpha.< orig || e2 >  /  mu'x.< t2 || orig >, the
    paper's abort under an affine binder.  Legacy M3 shapes: (>mu) only,
    leaving mu alt.< ?g || SCION_CTX > with the scion still catching alt.
    """
    result = deepcopy(node)
    if match.legacy:
        wing = node.context if isinstance(node, Mu) else node.term
        result.term = deepcopy(wing.term)
        result.context = deepcopy(wing.context)
        return result
    orig, scion = match[3], _outer_step(node, match[4])
    if isinstance(node, Mu):
        result.term = deepcopy(orig)
        result.context = deepcopy(scion)
    else:
        result.term = deepcopy(scion)
        result.context = deepcopy(orig)
    return result


# -- the strict phase ------------------------------------------------------

def strict_resolve(term: ProofTerm, strict_names=(), trace=None, edges=True):
    """Decide every scaffold strictness decides, bottom-up, and return
    (term', strict_edges): the rewritten term and the strict edges the
    closed subterms containing a decision contribute.  ``trace``, if a
    list, receives (statement, decision) per decided scaffold.  With
    ``edges=False`` only the decisions are made and no edge is collected:
    the caller resolves one instance of a shared debate and collects
    across instances itself (core/dc/instances.py).

    Works on a copy; the input is not modified.
    """
    strict_names = set(strict_names or ())
    decided = []      # nodes produced by a decision (identity)
    collect_edges, edges = edges, []
    verbose = logger.isEnabledFor(logging.DEBUG)
    if verbose:
        from pres.gen import pres_tree
        artifact(logger, "strict: the term going in", pres_tree(term))

    def binders_of(node):
        if isinstance(node, Mu):
            return {node.id.name}, set()
        if isinstance(node, Mutilde):
            return set(), {node.di.name}
        return set(), set()

    def walk(node, ids=frozenset(), dis=frozenset(), scope=None):
        """Top-down identification, bottom-up decision: a scaffold is
        recognised before its wiring, its original and scion are
        rewritten first, then it is decided.  ``ids``/``dis`` are the
        enclosing mu/mu' binders, the catchers in scope; ``scope`` the
        paper scaffolds' own variables in scope (``strict_in_scope``)."""
        if not isinstance(node, ProofTerm):
            return node
        scope = scope or {}
        own_ids, own_dis = binders_of(node)
        inner_ids, inner_dis = ids | own_ids, dis | own_dis
        match = _match_scaffold(node, strict_names)
        if match is None:
            # an argument's binder shadows a scaffold variable of that name
            inner_scope = ({k: v for k, v in scope.items() if k not in own_ids | own_dis}
                           if own_ids or own_dis else scope)
            for slot in ("term", "context"):
                child = getattr(node, slot, None)
                if isinstance(child, ProofTerm):
                    setattr(node, slot, walk(child, inner_ids, inner_dis, inner_scope))
            return node
        # the wiring binders (alpha/x and beta) are catchers for the parts
        wiring = node.context if isinstance(node, Mu) else node.term
        w_ids, w_dis = binders_of(wiring)
        part_ids, part_dis = inner_ids | w_ids, inner_dis | w_dis
        # A paper scaffold's alt binds the contrary over both parts, its b
        # the statement over the scion, bound to the original once it is
        # resolved (aida-unfold-scaffold-binders-capture).  Legacy shapes
        # keep their variables out of this: their terms never capture by
        # them.
        alt_kind = ID if isinstance(node, Mu) else DI
        orig_scope = scope if match.legacy else {**scope, match[5]: ("alt", alt_kind, match[2])}
        (o_parent, o_slot), (s_parent, s_slot) = scaffold_parts(node, match)
        setattr(o_parent, o_slot, walk(getattr(o_parent, o_slot), part_ids, part_dis, orig_scope))
        beta = None if match.legacy else _paper_beta(node)
        scion_scope = (orig_scope if beta is None
                       else {**orig_scope, beta[1]: ("b", beta[0], beta[2])})
        setattr(s_parent, s_slot, walk(getattr(s_parent, s_slot), part_ids, part_dis, scion_scope))
        match = _match_scaffold(node, strict_names)      # same shape, rewritten parts
        if match is None:
            logger.warning("strict: a scaffold no longer matches after its parts were "
                           "resolved; left for the labelling")
            return node
        role, site_side, prop, orig, scion, alt, scion_kind = match
        effective = (_restore_scion(scion, scion_kind, alt, prop, "r0")
                     if (match.legacy and role == "attacker") else scion)
        s_orig = strict_in_scope(orig, orig_scope)
        s_scion = strict_in_scope(effective, scion_scope)
        statement = (canonical_prop(prop), site_side if role == "supporter"
                     else ("context" if site_side == "term" else "term"))
        logger.log(TRACE, "  strict: %s scaffold on %s: original %s, %s %s",
                   role, _show(statement, prop),
                   "strict" if s_orig else "not strict",
                   "supporter" if role == "supporter" else "attacker",
                   "strict" if s_scion else "not strict")
        if role == "supporter":
            # The supporter first (aida-unfold-entrypoints): inside a stack
            # the supported term is a lower supporter, and when both are
            # strict the one higher in the stack - judged first by sigma
            # too - wins.  Outside a stack the supported term is a site,
            # never strict, so the order changes nothing there.
            # One exception, its own piece of semantics (author, 2026-10-06):
            # among strict terms an uncaught strict contradiction always wins,
            # so that it surfaces as an exception wherever it stands in the
            # stack.
            if (s_orig and s_scion and not contains_uncatchable_clash(scion, part_ids, part_dis)
                    and contains_uncatchable_clash(orig, part_ids, part_dis)):
                logger.debug("strict: %s both wings strict; the original holds an uncaught "
                             "contradiction, which wins", _show(statement, prop))
                kept, what = keep_orig(node, orig, alt), "original strict"
            elif s_scion:
                kept, what = keep_scion_support(node, scion, alt), "supporter strict"
            elif s_orig:
                kept, what = keep_orig(node, orig, alt), "original strict"
            else:
                # Neither wing rests on nothing: the labelling decides.
                logger.debug("strict: %s delayed for the labelling "
                             "(neither the original nor the supporter is strict)",
                             _show(statement, prop))
                return node
        else:
            if s_scion and not s_orig:
                kept, what = keep_attack_wing(node, match), "attacker strict: defeat"
            elif s_orig and not s_scion:
                kept, what = keep_orig(node, orig, alt), "original strict"
            else:
                logger.debug("strict: %s delayed for the labelling (%s)", _show(statement, prop),
                             "both wings strict, the inconsistent case"
                             if s_orig else "neither wing is strict")
                return node
        if trace is not None:
            trace.append((statement, what))
        logger.debug("strict: %s %s", _show(statement, prop), what)
        kept._strict_decision = True
        return kept

    term = walk(deepcopy(term))

    def has_decision(node):
        if not isinstance(node, ProofTerm):
            return False
        if getattr(node, "_strict_decision", False):
            return True
        return any(has_decision(getattr(node, slot, None)) for slot in ("term", "context"))

    def collect(node):
        """The OUTERMOST closed eta-wrapped subterms that owe their
        closedness to a decision -> one strict edge each.  A subterm
        holding an uncatchable clash contributes none: it derives its
        statement only from an inconsistency, which the classifier
        reports as an exception instead."""
        if not isinstance(node, ProofTerm):
            return
        statement = _eta_statement(node)
        if statement is not None and has_decision(node):
            # It owes its closedness to a decision.  Three conditions gate the
            # edge, and which one vetoed it is the interesting part.
            name = binder_origin(node)
            if not is_closed(node, strict_names):
                free = sorted({n for _kind, n in _free_names(node)}
                              - set(strict_names or ()) - set(BUILTIN_LEAVES))
                logger.debug("strict: no edge for '%s' on %s: not closed%s",
                             name, _show(statement, getattr(node, "prop", None)),
                             " (free: %s)" % ", ".join(free) if free else " (an open site remains)")
            elif contains_uncatchable_clash(node):
                logger.debug("strict: no edge for '%s' on %s: it holds an uncatchable clash, "
                             "so it derives its statement only from an inconsistency",
                             name, _show(statement, getattr(node, "prop", None)))
            else:
                edges.append(Edge(name=f"{name}*", target_key=key_of(statement),
                                  target_side=statement[1],
                                  sources=(), strict=True, role="strict", term=deepcopy(node)))
                logger.debug("strict: edge '%s*' on %s (a closed derivation the framework missed)",
                             name, _show(statement, getattr(node, "prop", None)))
                return                   # maximal: do not descend into it
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                collect(child)

    if collect_edges:
        collect(term)
    if verbose:
        from pres.gen import pres_tree
        artifact(logger, "strict: the term coming out (scaffolds strictness decided are gone)",
                 pres_tree(term))
        if collect_edges:
            artifact(logger, "strict: the strict edges it contributes",
                     "\n".join(f"{e.name}: {e.target_key}[{e.target_side[0]}] <- - (source-less, strict)"
                                for e in edges) or "none")
    return term, edges


def _eta_statement(node):
    """(canonical prop, side) of an eta-wrapped argument mu n.<t || n> /
    mu' n.<n || e>, else None."""
    if isinstance(node, Mu) and isinstance(node.context, ID) and node.context.name == node.id.name:
        return (canonical_prop(node.prop), "term")
    if isinstance(node, Mutilde) and isinstance(node.term, DI) and node.term.name == node.di.name:
        return (canonical_prop(node.prop), "context")
    return None


def compile_issue(term: ProofTerm, name: str, *, strict_names=None, strict_kinds=None) -> DebateGraph:
    """The issue graph: the argumentation framework of the term
    (compile_debate, cycles through captured obligations included) plus
    the strict edges strictness on the term contributes."""
    graph = compile_debate(term, name, strict_names=strict_names, strict_kinds=strict_kinds)
    trace = []
    _, edges = strict_resolve(term, strict_names or (), trace=trace)
    logger.debug("strict: %d decision(s), %d strict edge(s) for '%s'",
                 len(trace), len(edges), name)
    for edge in edges:
        graph.nodes.setdefault(edge.target_key, graph.nodes.get(edge.target_key, edge.target_key))
        graph.add_edge(edge)
    fold_occurrences(graph)
    return graph


def fold_occurrences(graph: DebateGraph) -> None:
    """Unfolding copies a document edge into every site of its statement,
    and the compiler makes one edge per copy (s3 and s3_2 on Peirce).
    Fold copies - same name, target, sources and strictness - into one,
    keeping the first role; a duplicate disjunct changes no label, so this
    is presentation only.

    A copy is named after the argument it came from (``binder_origin``),
    so copies share a name.  The suffix is read off a name only as a
    fallback for a term the unfolder did not mark, and only where an edge
    of the stem's own name exists with the same target and sources: a1_0
    and a1_1 are two arguments, not copies of an a1
    (aida-fold-occurrences-names)."""
    def body(edge):
        return (edge.target_key, edge.target_side, edge.strict,
                tuple((s.key, s.side, s.kind) for s in edge.sources))

    present = {(edge.name, body(edge)) for edge in graph.edges}
    seen = {}
    kept = []
    for edge in graph.edges:
        stem = re.sub(r"_\d+(?=\*?$)", "", edge.name)     # s3_2 -> s3, s3_2* -> s3*
        if stem == edge.name or (stem, body(edge)) not in present:
            stem = edge.name
        key = (stem, edge.target_key, edge.target_side, edge.strict,
               tuple((s.key, s.side, s.kind) for s in edge.sources))
        if key in seen:
            logger.debug("strict: folded the occurrence copy '%s' into '%s'", edge.name, stem)
            continue
        seen[key] = edge
        if edge.name != stem:
            logger.debug("strict: renamed the occurrence copy '%s' to '%s'", edge.name, stem)
        kept.append(replace(edge, name=stem) if edge.name != stem else edge)
    graph.edges[:] = kept
