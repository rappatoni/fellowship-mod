"""Citation by name (author, 2026-09-28/29; tasks.org,
aida-statements-and-witnesses).

``cite foo`` inside a recording uses the registered argument ``foo`` at the
focused goal.  ``axiom`` stays reserved for strict content and always goes to
Fellowship; ``cite`` works for strict and defeasible arguments alike.

The term shows the NAME at the site, exactly as an axiom would, carried by a
leaf marked ``cites = "foo"``.  The mark is what tells a citation from an
axiom or a binder of the same name; the printed term does not show it.
Whether the citation is strict is read at USE time: a leaf whose argument
Fellowship holds (it is in the prover's declarations) is a strict leaf, one
whose argument is still defeasible is an obligation on the cited conclusion.
So a citation silently becomes strict when its target is refined to strict.

That is late binding, and it is why the name and not the body goes into the
term.  A citer means "the argument foo, as it now stands": refine foo and
every citer follows, where a grafted copy would go stale.  It also keeps
terms readable once the library grows.  The full term, with every cited body
grafted in capture-free, is computed on demand by ``expand_citations``.

Semantically a citation is annotation: unfolding the document graph reaches
foo by proposition identity anyway.  The name records which argument the
author meant, concretely.
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


def parse_cite(instr: str):
    """``NAME`` for a ``cite NAME`` instruction, else None."""
    parts = instr.strip().rstrip(".").split()
    if len(parts) == 2 and parts[0] == "cite":
        return parts[1]
    return None


def citation_target(prover, instr: str):
    """The registered argument a ``cite NAME`` instruction uses, None if the
    instruction is not a citation; CitationError if NAME is not registered."""
    name = parse_cite(instr)
    if name is None:
        return None
    get = getattr(prover, "get_argument", None)
    cited = get(name) if get is not None else None
    if cited is None or getattr(cited, "body", None) is None:
        raise CitationError(f"`cite {name}`: there is no registered argument '{name}'")
    return cited


def is_strict_citation(prover, name: str) -> bool:
    """A citation of ``name`` is strict iff Fellowship holds ``name``."""
    return name in getattr(prover, "declarations", {})


def citation_leaf(name: str, prop: str, side: str):
    """The leaf a citation leaves in the term: the name, as an axiom would
    show it, marked as a citation."""
    leaf = DI(name, prop) if side == "term" else ID(name, prop)
    leaf.prop = prop
    leaf.cites = name
    return leaf


def cite_at_site(host, site: str, name: str, prop: str = None):
    """``host`` with the open site ``site`` replaced by a citation leaf for
    ``name``; CitationError if the site is not open any more.  ``prop`` is
    the goal's proposition, for a site Fellowship printed without one."""
    host = deepcopy(host)
    found = []

    def leaf_for(node):
        return citation_leaf(name, getattr(node, "prop", None) or prop,
                             "term" if isinstance(node, Goal) else "context")

    if isinstance(host, (Goal, Laog)) and str(host.number) == str(site):
        return leaf_for(host)

    def walk(node):
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, (Goal, Laog)) and str(child.number) == str(site):
                setattr(node, slot, leaf_for(child))
                found.append(site)
            elif isinstance(child, ProofTerm):
                walk(child)

    walk(host)
    if not found:
        raise CitationError(f"no open site {site} left for the citation of '{name}'")
    return host


def cited_names(body) -> set:
    """The names ``body`` cites."""
    found = set()

    def walk(node):
        if not isinstance(node, ProofTerm):
            return
        if getattr(node, "cites", None):
            found.add(node.cites)
        for slot in ("term", "context"):
            walk(getattr(node, slot, None))

    walk(body)
    return found


def mark_citations(body, is_argument):
    """Mark the FREE leaves of ``body`` that name a registered argument, as a
    term parsed from a string has lost its marks.  Bound leaves are never
    citations, whatever their name.  ``is_argument(name)`` decides what is
    registered.  Modifies and returns ``body``."""

    def walk(n, bound_terms, bound_ctxs):
        if not isinstance(n, ProofTerm):
            return
        if isinstance(n, DI):
            if n.name not in bound_terms and is_argument(n.name):
                n.cites = n.name
            return
        if isinstance(n, ID):
            if n.name not in bound_ctxs and is_argument(n.name):
                n.cites = n.name
            return
        terms, ctxs = bound_terms, bound_ctxs
        if isinstance(n, Mu):
            ctxs = ctxs | {n.id.name}
        elif isinstance(n, Mutilde):
            terms = terms | {n.di.name}
        elif isinstance(n, Lamda):
            terms = terms | {n.di.di.name}
        elif isinstance(n, Admal):
            ctxs = ctxs | {n.id.id.name}
        for slot in ("term", "context"):
            walk(getattr(n, slot, None), terms, ctxs)

    walk(body, frozenset(), frozenset())
    return body


def expand_citations(body, get_argument, _path=()):
    """``body`` with every citation replaced by the cited argument's own
    term, recursively, capture-free: the full term a citation stands for.
    A citation cycle is left as the name, and logged."""
    body = deepcopy(body)

    def expanded(name):
        if name in _path:
            logger.warning("cite: citation cycle through '%s'; left as the name", name)
            return None
        cited = get_argument(name)
        if cited is None or getattr(cited, "body", None) is None:
            return None
        return expand_citations(cited.body, get_argument, _path + (name,))

    def walk(node):
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, (DI, ID)) and getattr(child, "cites", None):
                full = expanded(child.cites)
                if full is not None:
                    setattr(node, slot, rename_clashing_binders(full, binder_names(body)))
            elif isinstance(child, ProofTerm):
                walk(child)

    if isinstance(body, (DI, ID)) and getattr(body, "cites", None):
        full = expanded(body.cites)
        return full if full is not None else body
    walk(body)
    return body
