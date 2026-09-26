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
from core.comp.oracle_terms import _occurs, _free_names, _contains, contains_uncatchable_clash
from core.dc.debate_graph import (
    DebateGraph, Edge, canonical_prop, compile_debate, _match_scaffold, _restore_scion,
    scaffold_parts, BUILTIN_LEAVES,
)
from core.logging_util import TRACE

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
    """The supporter wins: inner pair to <t2||alpha>, outer pair discards
    the original: mu alpha.<t2 || alpha> -> t2."""
    return _eta(isinstance(node, Mu), scion, alt, node.prop)


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
    orig, scion = match[3], match[4]
    if isinstance(node, Mu):
        result.term = deepcopy(orig)
        result.context = deepcopy(scion)
    else:
        result.term = deepcopy(scion)
        result.context = deepcopy(orig)
    return result


# -- the strict phase ------------------------------------------------------

def strict_resolve(term: ProofTerm, strict_names=(), trace=None):
    """Decide every scaffold strictness decides, bottom-up, and return
    (term', strict_edges): the rewritten term and the strict edges the
    closed subterms containing a decision contribute.  ``trace``, if a
    list, receives (statement, decision) per decided scaffold.

    Works on a copy; the input is not modified.
    """
    strict_names = set(strict_names or ())
    decided = []      # nodes produced by a decision (identity)
    edges = []

    def binders_of(node):
        if isinstance(node, Mu):
            return {node.id.name}, set()
        if isinstance(node, Mutilde):
            return set(), {node.di.name}
        return set(), set()

    def walk(node, ids=frozenset(), dis=frozenset()):
        """Top-down identification, bottom-up decision: a scaffold is
        recognised before its wiring, its original and scion are
        rewritten first, then it is decided.  ``ids``/``dis`` are the
        enclosing mu/mu' binders, the catchers in scope."""
        if not isinstance(node, ProofTerm):
            return node
        own_ids, own_dis = binders_of(node)
        inner_ids, inner_dis = ids | own_ids, dis | own_dis
        match = _match_scaffold(node, strict_names)
        if match is None:
            for slot in ("term", "context"):
                child = getattr(node, slot, None)
                if isinstance(child, ProofTerm):
                    setattr(node, slot, walk(child, inner_ids, inner_dis))
            return node
        # the wiring binders (alpha/x and beta) are catchers for the parts
        wiring = node.context if isinstance(node, Mu) else node.term
        w_ids, w_dis = binders_of(wiring)
        part_ids, part_dis = inner_ids | w_ids, inner_dis | w_dis
        for parent, slot in scaffold_parts(node, match):
            setattr(parent, slot, walk(getattr(parent, slot), part_ids, part_dis))
        match = _match_scaffold(node, strict_names)      # same shape, rewritten parts
        role, site_side, prop, orig, scion, alt, scion_kind = match
        effective = (_restore_scion(scion, scion_kind, alt, prop, "r0")
                     if (match.legacy and role == "attacker") else scion)
        s_orig = strict_in_debate(orig, part_ids, part_dis)
        s_scion = strict_in_debate(effective, part_ids, part_dis)
        statement = (canonical_prop(prop), site_side if role == "supporter"
                     else ("context" if site_side == "term" else "term"))
        logger.log(TRACE, "  strict: %s scaffold on %s: original %s, %s %s",
                   role, _show(statement, prop),
                   "strict" if s_orig else "not strict",
                   "supporter" if role == "supporter" else "attacker",
                   "strict" if s_scion else "not strict")
        if role == "supporter":
            if s_orig:
                kept, what = keep_orig(node, orig, alt), "original strict"
            elif s_scion:
                kept, what = keep_scion_support(node, scion, alt), "supporter strict"
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
            name = node.id.name if isinstance(node, Mu) else node.di.name
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

    collect(term)
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
    Fold copies - same name stem, target, sources and strictness - into
    one under the stem name, keeping the first role; a duplicate disjunct
    changes no label, so this is presentation only."""
    seen = {}
    kept = []
    for edge in graph.edges:
        stem = re.sub(r"_\d+$", "", edge.name)          # s3_2 -> s3: an unfolding copy
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
