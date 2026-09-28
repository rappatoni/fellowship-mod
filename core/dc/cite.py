"""Citation: grafting a registered argument into a site (author, 2026-09-28).

Inside a recording, `axiom foo` where ``foo`` is a registered *defeasible*
argument cannot go to Fellowship: Fellowship holds only strict proofs, and
anything it knows is a strict axiom everywhere.  The wrapper leaves that goal
open instead and, once the recording is replayed, grafts foo's body into the
open site.

Semantically this is annotation - unfolding the document graph would reach
foo by proposition identity anyway.  Concretely it is not: the citing
argument IS the term with foo inside it, obtained by grafting foo onto the
enthymeme at that site intuitionistically, without classical capture.  That
is the same distinction as between the debate verbs, concrete operations in
a concrete order, and the timeless framework they live in.

So a citing argument has two bodies:

- the concrete one, with foo grafted in, for rendering and evaluation;
- the atomic one, with the site left open as an obligation, which is its
  own contribution to the document graph.  foo's contribution is already
  there, under foo's name.

The graft is plain substitution.  The cited body's binders are renamed only
where they clash with a name the host already uses (``foo`` becomes
``foo_2``), which keeps the term readable and rules out capture in either
direction: nothing in the host can bind a variable of the cited body, and
nothing in the cited body can bind a host variable, because the cited body
is closed but for its own sites and declared names.
"""

import logging
from copy import deepcopy

from core.ac.ast import (
    ProofTerm, Mu, Mutilde, Lamda, Admal, Goal, Laog, ID, DI,
)

logger = logging.getLogger(__name__)


class CitationError(ValueError):
    """The cited argument does not fit the site it was cited for."""


def binder_names(node) -> set:
    """Every name bound anywhere in ``node``."""
    found = set()

    def walk(n):
        if not isinstance(n, ProofTerm):
            return
        if isinstance(n, Mu):
            found.add(n.id.name)
        elif isinstance(n, Mutilde):
            found.add(n.di.name)
        elif isinstance(n, Lamda):
            found.add(n.di.di.name)
        elif isinstance(n, Admal):
            found.add(n.id.id.name)
        for slot in ("term", "context"):
            walk(getattr(n, slot, None))

    walk(node)
    return found


def rename_clashing_binders(node, avoid: set):
    """A copy of ``node`` whose binders avoid ``avoid``: a clashing binder
    ``x`` becomes the first free ``x_2``, ``x_3`` ...; the others keep
    their names.  Bound occurrences follow their binder; free ones are
    untouched."""
    node = deepcopy(node)
    taken = set(avoid) | binder_names(node)

    def fresh(name):
        if name == "_" or name not in avoid:
            return name
        n = 2
        while f"{name}_{n}" in taken:
            n += 1
        taken.add(f"{name}_{n}")
        return f"{name}_{n}"

    def walk(n, term_env, ctx_env):
        if not isinstance(n, ProofTerm):
            return
        if isinstance(n, DI):
            n.name = term_env.get(n.name, n.name)
            return
        if isinstance(n, ID):
            n.name = ctx_env.get(n.name, n.name)
            return
        if isinstance(n, Mu):
            new = fresh(n.id.name)
            inner = {**ctx_env, n.id.name: new}
            n.id.name = new
            walk(n.term, term_env, inner)
            walk(n.context, term_env, inner)
            return
        if isinstance(n, Mutilde):
            new = fresh(n.di.name)
            inner = {**term_env, n.di.name: new}
            n.di.name = new
            walk(n.term, inner, ctx_env)
            walk(n.context, inner, ctx_env)
            return
        if isinstance(n, Lamda):
            new = fresh(n.di.di.name)
            inner = {**term_env, n.di.di.name: new}
            n.di.di.name = new
            walk(n.term, inner, ctx_env)
            return
        if isinstance(n, Admal):
            new = fresh(n.id.id.name)
            inner = {**ctx_env, n.id.id.name: new}
            n.id.id.name = new
            walk(n.context, term_env, inner)
            return
        for slot in ("term", "context"):
            walk(getattr(n, slot, None), term_env, ctx_env)

    walk(node, {}, {})
    return node


def graft_citation(host, site: str, cited, cited_name: str = "?"):
    """``host`` with the open site numbered ``site`` replaced by a copy of
    ``cited``.  A term site (Goal) takes a term, a context site (Laog) a
    context; anything else is a CitationError, as is a missing site.
    ``host`` is not modified."""
    host = deepcopy(host)
    replacement = rename_clashing_binders(cited, binder_names(host))
    found = []

    def fits(node):
        if isinstance(node, Goal):
            return not isinstance(replacement, Mutilde)
        return isinstance(replacement, Mutilde)

    def walk(node):
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, (Goal, Laog)) and str(child.number) == str(site):
                if not fits(child):
                    raise CitationError(
                        f"'{cited_name}' is {'a counterargument' if isinstance(replacement, Mutilde) else 'an argument'} "
                        f"and cannot fill the {'term' if isinstance(child, Goal) else 'context'} site {site}."
                    )
                setattr(node, slot, deepcopy(replacement))
                found.append(site)
            elif isinstance(child, ProofTerm):
                walk(child)

    if isinstance(host, (Goal, Laog)) and str(host.number) == str(site):
        return replacement
    walk(host)
    if not found:
        raise CitationError(f"no open site {site} in the citing term to graft '{cited_name}' into")
    logger.debug("cite: '%s' grafted into site %s", cited_name, site)
    return host


def cited_defeasible_argument(prover, instr: str):
    """The registered DEFEASIBLE argument that an `axiom NAME` / `moxia NAME`
    instruction cites, or None.  A strict argument Fellowship holds, a
    declared axiom and anything unregistered pass through to the prover."""
    parts = instr.strip().rstrip(".").split()
    if len(parts) != 2 or parts[0] not in ("axiom", "moxia"):
        return None
    name = parts[1]
    if name in getattr(prover, "declarations", {}):
        return None
    get = getattr(prover, "get_argument", None)
    cited = get(name) if get is not None else None
    if cited is None or getattr(cited, "citable", False) or getattr(cited, "body", None) is None:
        return None
    return cited
