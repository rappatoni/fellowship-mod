"""The deprecated scaffold shapes, rewritten into the current ones
(tasks.org, aida-legacy-shape-shim).

The compiler recognises two families of scaffold (the shape catalogue in
core/dc/debate_graph.py): the four paper shapes the unfolder builds, and
the M3 shapes the removed grafting verbs built.  Importers written against
the old verbs still emit M3 shapes - scasp-aida does, and it emits one
variant of its own - so a debate registered from such an importer is first
brought into the paper shapes here.  Only the paper shapes can be
unfolded from, and only they are what the round trip of a registered
debate is compared against (aida-registered-debates).

The rewrite, with ``alt`` kept as the paper's alpha/x and a fresh beta:

    M3 T-SUP  mu alt.< mu_.<ORIG||alt> || mu'_.<SCION||alt> >
          ->  mu alt.< ORIG || mu'b.< mu_.<b||alt> || mu'_.<SCION||alt> > >
    M3 T-ATT  mu alt.< mu_.<ORIG||alt> || mu'_.<?g||SCION> >
          ->  mu alt.< ORIG || mu'b.< mu_.<b||SCION'> || mu'_.<b||alt> > >
    M3 C-SUP  mu'alt.< mu_.<alt||SCION> || mu'_.<alt||ORIG> >
          ->  mu'alt.< mu b.< mu_.<alt||SCION> || mu'_.<alt||b> > || ORIG >
    M3 C-ATT  mu'alt.< mu_.<SCION||?l> || mu'_.<alt||ORIG> >
          ->  mu'alt.< mu b.< mu_.<alt||b> || mu'_.<SCION'||b> > || ORIG >

An attack's scion has the alt reroute undone first (SCION' =
``_restore_scion``: a fresh open site where alt stood), as the compiler
does when it reads an M3 attack; the stolen-value slot (``?g``, ``?l``)
has no place in the paper shapes and is dropped, as the compiler ignores
it.

scasp-aida's ``_barred`` writes the context attack with its two inner
commands exchanged:

    mu'alt.< mu_.<alt||ORIG> || mu'_.<SCION||?l> >     (ORIG, ?l open sites)

which is read as M3 C-ATT with the commands put back.  The exchange is
meaning-preserving only read as an attack, which is what the importer
means (the presumed refutation of a premise, attacked by its proof): in
<mu_.c1 || mu'_.c2> cbv takes c1 and cbn c2, so as a bare command the two
spellings reduce to different halves.
"""

import logging
from copy import deepcopy
from itertools import count

from core.ac.ast import (
    ProofTerm, Mu, Mutilde, Lamda, Admal, Goal, Laog, Deleg, Geled, ID, DI,
)
from core.dc.debate_graph import (
    _match_legacy_scaffold, _match_paper_scaffold, _restore_scion, _is_affine_binder,
)

logger = logging.getLogger(__name__)

_SITES = (Goal, Laog, Deleg, Geled)


def modernise_scaffolds(body: ProofTerm, strict_names=(), *,
                        exchanged_context_attacks: bool = True) -> ProofTerm:
    """``body`` with every M3 scaffold (and, with
    ``exchanged_context_attacks``, scasp-aida's exchanged context attack)
    rewritten into the paper shape that the compiler reads the same way.
    The input is not changed.  ``strict_names``: the declared names, which
    the M3 attack shape admits in its stolen-value slot."""
    body = deepcopy(body)
    taken_names, taken_sites = _names_and_sites(body)
    rewritten = count()

    def fresh_name(base: str) -> str:
        for n in count(1):
            name = base if n == 1 else f"{base}{n}"
            if name not in taken_names:
                taken_names.add(name)
                return name

    def fresh_site() -> str:
        numbers = [int(s) for s in taken_sites if s.isdigit()]
        site = str(max(numbers, default=0) + 1)
        taken_sites.add(site)
        return site

    def walk(node):
        if not isinstance(node, ProofTerm):
            return node
        paper = _match_paper_scaffold(node)
        if paper is not None:
            # Already current: rewrite inside its original and its scion
            # only - the wiring of a paper attack has the M3 C-SUP shape
            # and must never be read as a scaffold of its own.
            _walk_paper_parts(node, paper, walk)
            return node
        legacy = _match_legacy_scaffold(node, strict_names)
        if legacy is None and exchanged_context_attacks:
            put_back = _exchanged_context_attack(node)
            if put_back is not None:
                legacy = _match_legacy_scaffold(put_back, strict_names)
        if legacy is not None:
            next(rewritten)
            return _paper_shape(node, legacy, walk, fresh_name, fresh_site)
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                setattr(node, slot, walk(child))
        return node

    result = walk(body)
    logger.debug("shapes: %d deprecated scaffold(s) rewritten into the paper shapes",
                 next(rewritten))
    return result


def _paper_shape(node, match, walk, fresh_name, fresh_site):
    role, site_side, prop, orig, scion, alt, scion_kind = match
    if role == "attacker":
        scion = _restore_scion(scion, scion_kind, alt, prop, fresh_site())
    orig, scion = walk(orig), walk(scion)
    beta = fresh_name("b")
    if isinstance(node, Mu):                       # term side: alt is alpha
        if role == "supporter":
            wiring = (Mu(ID("_", prop), prop, DI(beta, prop), ID(alt, prop)),
                      Mutilde(DI("_", prop), prop, scion, ID(alt, prop)))
        else:
            wiring = (Mu(ID("_", prop), prop, DI(beta, prop), scion),
                      Mutilde(DI("_", prop), prop, DI(beta, prop), ID(alt, prop)))
        return Mu(ID(alt, prop), prop, orig, Mutilde(DI(beta, prop), prop, *wiring))
    if role == "supporter":                        # context side: alt is x
        wiring = (Mu(ID("_", prop), prop, DI(alt, prop), scion),
                  Mutilde(DI("_", prop), prop, DI(alt, prop), ID(beta, prop)))
    else:
        wiring = (Mu(ID("_", prop), prop, DI(alt, prop), ID(beta, prop)),
                  Mutilde(DI("_", prop), prop, scion, ID(beta, prop)))
    return Mutilde(DI(alt, prop), prop, Mu(ID(beta, prop), prop, *wiring), orig)


def _walk_paper_parts(node, match, walk):
    """Rewrite inside a paper scaffold's original and scion, in place."""
    role = match[0]
    if isinstance(node, Mu):
        node.term = walk(node.term)
        wiring = node.context
        if role == "supporter":
            wiring.context.term = walk(wiring.context.term)
        else:
            wiring.term.context = walk(wiring.term.context)
    else:
        node.context = walk(node.context)
        wiring = node.term
        if role == "supporter":
            wiring.term.context = walk(wiring.term.context)
        else:
            wiring.context.term = walk(wiring.context.term)


def _exchanged_context_attack(node):
    """scasp-aida's context attack mu'alt.< mu_.<alt||ORIG> || mu'_.<SCION||?l> >
    with its inner commands put back into the M3 order
    mu'alt.< mu_.<SCION||?l> || mu'_.<alt||ORIG> >, or None."""
    if not (isinstance(node, Mutilde) and not _is_affine_binder(node.di.name)):
        return None
    alt, prop, w1, w2 = node.di.name, node.prop, node.term, node.context
    if not (isinstance(w1, Mu) and _is_affine_binder(w1.id.name) and w1.prop == prop
            and isinstance(w2, Mutilde) and _is_affine_binder(w2.di.name) and w2.prop == prop):
        return None
    if not (isinstance(w1.term, DI) and w1.term.name == alt
            and isinstance(w1.context, (Laog, Geled))
            and isinstance(w2.context, (Laog, Geled))
            and not (isinstance(w2.term, DI) and w2.term.name == alt)):
        return None
    return Mutilde(node.di, prop,
                   Mu(w1.id, prop, w2.term, w2.context),
                   Mutilde(w2.di, prop, w1.term, w1.context))


def _names_and_sites(body):
    """Every variable name and every site number in ``body``."""
    names, sites = set(), set()

    def walk(node):
        if isinstance(node, (ID, DI)):
            names.add(node.name)
        elif isinstance(node, Mu):
            names.add(node.id.name)
        elif isinstance(node, Mutilde):
            names.add(node.di.name)
        elif isinstance(node, Lamda):
            names.add(node.di.di.name)
        elif isinstance(node, Admal):
            names.add(node.id.id.name)
        elif isinstance(node, _SITES):
            sites.add(str(node.number))
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                walk(child)

    walk(body)
    return names, sites
