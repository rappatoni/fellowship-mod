"""The argument discipline: the pure level (tasks.org,
aida-argument-lj-restriction; the author, 2026-10-15).

An argument for a proposition is a *linear* term, a counterargument a
*dual-linear* context (COMMA 2026, "Linear Expressions"), and neither may
hold a debate scaffold.  Precisely:

- *linear*: every continuation - a context variable bound by ``mu`` or
  ``lambda-bar`` - occurs at most once in its scope, and no ``mu`` or
  ``lambda-bar`` stands between its binder and that occurrence.  This is
  what Fellowship's ``lj`` enforces; it is the intuitionistic fragment.
- *dual-linear*: the same for hypotheses - term variables bound by
  ``mu'`` or ``lambda`` - with ``mu'``/``lambda`` as the binders that may
  not stand between.  The dual-intuitionistic fragment: minimal logic with
  excluded middle and without non-contradiction.  A refutation through a
  *proved* negation (``mu'x:A.<t:~A || x*_F_>``) has one hypothesis and
  passes; deriving ``false`` from the hypotheses A, ~A does not.
- *no scaffold*: in a pure command ``<mu v.c || mu' w.c'>`` whose binders
  ``v`` and ``w`` are unused (the two alternatives of a support or attack
  scaffold, core/dc/unfold.py), no variable bound outside is free in both
  halves.  Linearity alone lets two of the four scaffold shapes through
  (ATT reuses a term variable, SUP~ a context variable).

Fellowship encodes negation elimination as ``mu'H:~A.<H || chain*_F_>``
(core/ac/instructions.py, ``_is_negation_elim_wrapper``): representation,
not a cut anyone performed.  The checks see through it, otherwise every
refutation through ``~A`` would carry a second hypothesis.

The generalised compiler's horn rule (core/dc/debate_graph.py) needs the
single spine this discipline gives: the binder's variable occurs in exactly
one half of its command.
"""

from copy import deepcopy

from core.ac.ast import (
    ProofTerm, Mu, Mutilde, Lamda, Admal, Cons, ID, DI,
)
from core.ac.instructions import is_primitive_negation_prop

#: Binders of a context variable (a continuation) and of a term variable
#: (a hypothesis), with the variable node type each binds.
CONTINUATION = ((Mu, Admal), ID)
HYPOTHESIS = ((Mutilde, Lamda), DI)


def binder_name(node):
    """The variable a binder node binds, or None."""
    if isinstance(node, Mu):
        return node.id.name
    if isinstance(node, Mutilde):
        return node.di.name
    if isinstance(node, Lamda):
        return node.di.di.name
    if isinstance(node, Admal):
        return node.id.id.name
    return None


def _children(node):
    for slot in ("term", "context"):
        child = getattr(node, slot, None)
        if isinstance(child, ProofTerm):
            yield child


def free_variables(node, bound=frozenset()):
    """The (kind, name) pairs free in ``node``: kind "c" for a context
    variable (ID), "t" for a term variable (DI).  Declared names count as
    free; the callers only compare the two halves of one command."""
    if not isinstance(node, ProofTerm):
        return set()
    if isinstance(node, ID):
        return set() if ("c", node.name) in bound else {("c", node.name)}
    if isinstance(node, DI):
        return set() if ("t", node.name) in bound else {("t", node.name)}
    name = binder_name(node)
    if name is not None:
        kind = "c" if isinstance(node, CONTINUATION[0]) else "t"
        bound = bound | {(kind, name)}
    out = set()
    for child in _children(node):
        out |= free_variables(child, bound)
    return out


def _is_negation_wrapper(node) -> bool:
    """``mu'H:~A.<H || chain*_F_>`` with H not used in the chain."""
    if not (isinstance(node, Mutilde) and isinstance(node.term, DI)
            and node.term.name == node.di.name and isinstance(node.context, Cons)
            and is_primitive_negation_prop(getattr(node, "prop", None))):
        return False
    tail = node.context
    while isinstance(tail, Cons):
        tail = tail.context
    return (isinstance(tail, (ID, DI)) and tail.name == "_F_"
            and ("t", node.di.name) not in free_variables(node.context))


def without_negation_wrappers(body):
    """A copy of ``body`` with every negation-elimination wrapper replaced
    by its chain."""
    def walk(node):
        if not isinstance(node, ProofTerm):
            return node
        if _is_negation_wrapper(node):
            return walk(node.context)
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                setattr(node, slot, walk(child))
        return node
    return walk(deepcopy(body))


def _occurrences(node, name, var_type, binders, between=0):
    """For each free occurrence of the variable ``name`` (of ``var_type``)
    below ``node``: how many binders of the same kind stand between."""
    if not isinstance(node, ProofTerm):
        return []
    if isinstance(node, var_type) and node.name == name:
        return [between]
    if isinstance(node, binders) and binder_name(node) == name:
        return []                                   # shadowed
    step = 1 if isinstance(node, binders) else 0
    found = []
    for child in _children(node):
        found += _occurrences(child, name, var_type, binders, between + step)
    return found


def linearity_violations(body, which) -> list:
    """``which`` is CONTINUATION (linear) or HYPOTHESIS (dual-linear): the
    variables bound more than once-used, or used under a second binder of
    the same kind, as (name, reason)."""
    binders, var_type = which
    out = []

    def walk(node):
        if not isinstance(node, ProofTerm):
            return
        if isinstance(node, binders):
            name = binder_name(node)
            if name != "_":
                found = []
                for child in _children(node):
                    found += _occurrences(child, name, var_type, binders)
                if len(found) > 1:
                    out.append((name, f"used {len(found)} times"))
                elif found and found[0] > 0:
                    kind = "continuation" if which is CONTINUATION else "hypothesis"
                    out.append((name, f"used under a second {kind} binder"))
        for child in _children(node):
            walk(child)

    walk(body)
    return out


def _affine(node) -> bool:
    return isinstance(node, (Mu, Mutilde)) and all(
        (("c" if isinstance(node, Mu) else "t"), binder_name(node)) not in free_variables(child)
        for child in _children(node))


def scaffold_violations(body) -> list:
    """Pure commands ``<mu v.c || mu' w.c'>`` (v, w unused) whose two halves
    share a variable: the shape of a debate scaffold.  Each as (names,
    the command's binder)."""
    out = []

    def walk(node):
        if not isinstance(node, ProofTerm):
            return
        if isinstance(node, (Mu, Mutilde)):
            left, right = node.term, node.context
            if isinstance(left, Mu) and isinstance(right, Mutilde) and _affine(left) and _affine(right):
                shared = free_variables(left) & free_variables(right)
                if shared:
                    out.append((sorted(name for _, name in shared), binder_name(node)))
        for child in _children(node):
            walk(child)

    walk(body)
    return out


def violations(body, anti: bool) -> list:
    """Everything that keeps ``body`` from the pure level, as messages: for
    an argument (``anti`` False) linearity, for a counterargument
    dual-linearity, and the scaffold rule for both."""
    term = without_negation_wrappers(body)
    messages = []
    if anti:
        for name, why in linearity_violations(term, HYPOTHESIS):
            messages.append(f"not dual-linear: the hypothesis '{name}' is {why}")
    else:
        for name, why in linearity_violations(term, CONTINUATION):
            messages.append(f"not linear: the continuation '{name}' is {why}")
    for names, binder in scaffold_violations(term):
        messages.append("holds a debate scaffold: the pure command under '%s' shares %s "
                        "between its two alternatives" % (binder, ", ".join(f"'{n}'" for n in names)))
    return messages


__all__ = ["violations", "linearity_violations", "scaffold_violations",
           "without_negation_wrappers", "free_variables", "CONTINUATION", "HYPOTHESIS"]
