"""Labelled terms: the witness labelling sigma written into the debate term
(tasks.org, aida-labelled-sites-delegation-rewrite).

After labelling and the strict phase, every statement occurrence in the
term gets its label as an attribute ``label`` ("IN", "OUT", "UNDEC"),
printed ``A{L}`` for a term-sorted occurrence of A and ``{L}A`` for a
context-sorted one (pres/gen.py):

    term-sorted     Mu, Goal, Deleg, DI, Lamda      statement (A, "term")
    context-sorted  Mutilde, Laog, Geled, ID, Admal  statement (A, "context")

The label is a function of the statement, the proposition with its side,
so every reduction step keeps it correct.  Sigma reads the labelled term
(core/comp/evaluate.py ``resolve_scaffolds``): a scion's conclusion is the
label of its root, its sources are the labels of its sites, captured
variables and lambda subarguments - the attacker's conclusion included,
whose wing root is the captured ``alt``, a context variable ``{L}S``.

Before normalisation the labels are discharged (``discharge_labels``):
every site takes its label as its name and its kind from the label,
whatever kind it had -

    IN              ->  delegation   !IN:A   /  IN:A!
    OUT or UNDEC    ->  obligation   ?OUT:A  /  OUT:A?   (UNDEC alike)

- and every other label is dropped, so normalisation, alpha-equality,
the type checker and the scaffold matcher never see one.  An obligation
labelled IN is a delegation: the onus is now on the opponent to bring an
argument the labelling did not consider.  A delegation not IN is an
obligation again: it was not established under the mode.  The class of
the normal form then reads: an exception if it holds an uncaught clash,
else a value iff every remaining site is IN.

The label is never part of ``prop``: comparisons of propositions stay
what they were.  Labelled terms are output only; neither grammar reads
them.
"""

from copy import deepcopy
from functools import lru_cache

from core.ac.ast import (
    ProofTerm, Mu, Mutilde, Lamda, Admal, Goal, Laog, Deleg, Geled, ID, DI,
)
from core.dc.debate_graph import canonical_prop, _is_affine_binder, DebateCompileError


LABELS = ("IN", "OUT", "UNDEC")

_TERM_SORTED = (Mu, Goal, Deleg, DI, Lamda)
_CONTEXT_SORTED = (Mutilde, Laog, Geled, ID, Admal)
_SITES = (Goal, Laog, Deleg, Geled)


class UnlabelledSite(DebateCompileError):
    """A site whose statement the labelling does not label: the labelling
    and the term disagree about the debate's shape."""


@lru_cache(maxsize=None)
def _key(prop: str):
    try:
        return canonical_prop(prop)
    except Exception:
        return None


def _prop_of(node):
    if isinstance(node, Lamda):
        if node.prop:
            return node.prop
        body = getattr(node.term, "prop", None)
        if node.di.prop and body:
            return f"{node.di.prop}->{body}"
        return None
    if isinstance(node, Admal):
        return node.prop
    return getattr(node, "prop", None)


def statement_of(node):
    """(key, side) of the statement an occurrence stands for, or None.

    An affine placeholder (``_``) stands for nothing referenceable; it is
    not a statement occurrence."""
    if isinstance(node, (ID, DI)) and _is_affine_binder(node.name):
        return None
    if isinstance(node, _TERM_SORTED):
        side = "term"
    elif isinstance(node, _CONTEXT_SORTED):
        side = "context"
    else:
        return None
    prop = _prop_of(node)
    if not prop:
        return None
    key = _key(prop)
    return None if key is None else (key, side)


def label_in_place(node, sigma):
    """Set ``label`` on every node of ``node`` whose statement sigma labels
    (and clear a stale one where it does not); returns ``node``."""
    if not isinstance(node, ProofTerm):
        return node
    statement = statement_of(node)
    label = sigma.get(statement) if statement is not None else None
    if label is not None:
        node.label = label
    elif hasattr(node, "label"):
        del node.label
    for slot in ("term", "context"):
        child = getattr(node, slot, None)
        if isinstance(child, ProofTerm):
            label_in_place(child, sigma)
    return node


def label_term(node, sigma):
    """A labelled copy of ``node``: every statement occurrence sigma labels
    carries its label.  Statements sigma has no node for (strict axioms,
    built-in leaves) stay unlabelled.  Idempotent."""
    return label_in_place(deepcopy(node), sigma)


def label_of(node):
    return getattr(node, "label", None)


def site_for_label(site, label):
    """The site a labelled site discharges to: named by its label, of the
    kind the label gives (IN: delegation, otherwise obligation)."""
    term_side = isinstance(site, (Goal, Deleg))
    if label == "IN":
        cls = Deleg if term_side else Geled
    else:
        cls = Goal if term_side else Laog
    return cls(label, site.prop)


def discharge_labels(node):
    """Discharge the labels of a labelled term, in place: every site
    becomes ``site_for_label``, every other label is dropped.  A site
    without a label raises ``UnlabelledSite``."""
    if not isinstance(node, ProofTerm):
        return node
    if isinstance(node, _SITES):
        label = label_of(node)
        if label is None:
            raise UnlabelledSite(
                f"No label for the site {node.number}:{node.prop}; the labelling "
                f"and the term disagree about the debate's shape.")
        return site_for_label(node, label)
    if hasattr(node, "label"):
        del node.label
    for slot in ("term", "context"):
        child = getattr(node, slot, None)
        if isinstance(child, ProofTerm):
            setattr(node, slot, discharge_labels(child))
    return node


def contains_label(node) -> bool:
    if not isinstance(node, ProofTerm):
        return False
    if hasattr(node, "label"):
        return True
    return any(contains_label(getattr(node, slot, None)) for slot in ("term", "context"))
