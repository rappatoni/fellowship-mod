"""Debate-local names (tasks.org, aida-debate-local-arguments).

A registered debate's own declarations and arguments live in its session
(wrap/debate_session.py) under ``DEBATE__name``, so they can never clash
with the document's names - the debate's session also holds the
document's declarations it needs - and every name stays unique across the
document (the UI, the inventory, provenance).  Fellowship's identifiers
are ``letter (letter | digit | _)*``, so ``__`` is the only separator
available; the owning session reserves ``DEBATE__`` once the debate is
registered (``ProverWrapper.reserve_prefix``).

What is local in a ``register debate`` block: a proof constant
(``declare n : (P).``) and a moxia (``deny n : (P).``).  A sort -
``declare A : bool.``, ``declare iota : type.``, a predicate's type - is
shared: it goes to the document, where the debate's propositions meet the
document's arguments.  So a local name never occurs inside a proposition
and renaming leaves propositions alone.

Shown for its own debate, a local name drops the prefix, unless the
short name is taken there too (a document ``b`` beside the debate's
``b``): then it is shown as it is (``display_names``).
"""

import re
from copy import deepcopy
from typing import Dict, Iterable, List, Tuple

from core.ac.ast import ProofTerm, Mu, Mutilde, Lamda, Admal, ID, DI

SEPARATOR = "__"

_DECLARATION = re.compile(r"^\s*(declare|deny)\s+([^:]+?)\s*:\s*(.+?)\s*\.?\s*$", re.S)


def local_name(debate: str, name: str) -> str:
    return f"{debate}{SEPARATOR}{name}"


def prefix(debate: str) -> str:
    return f"{debate}{SEPARATOR}"


def parse_declaration(command: str) -> Tuple[str, List[str], str]:
    """``declare N1, N2 : T.`` / ``deny N : (P).`` as (verb, names, type),
    or ValueError.  The type may contain ':' (``forall x:iota, ...``); the
    names cannot."""
    match = _DECLARATION.match(command)
    if match is None:
        raise ValueError(f"not a declaration: `{command.strip()}`")
    verb, names, typ = match.groups()
    return verb, [n.strip() for n in names.split(",") if n.strip()], typ


def is_local(command: str) -> bool:
    """Whether a declaration in a ``register debate`` block is local to the
    debate: a proof constant or a moxia, not a sort."""
    verb, _, typ = parse_declaration(command)
    return verb == "deny" or typ.startswith("(")


def localise_declaration(command: str, debate: str) -> Tuple[str, Dict[str, str]]:
    """A local declaration with its names prefixed, and {name: local name}."""
    verb, names, typ = parse_declaration(command)
    renamed = {n: local_name(debate, n) for n in names}
    return f"{verb} {', '.join(renamed.values())} : {typ}.", renamed


def rename_term(body: ProofTerm, mapping: Dict[str, str]) -> ProofTerm:
    """A copy of ``body`` whose free leaves named in ``mapping`` are
    renamed; a leaf bound by a binder of its own kind is left alone (a
    binder named like a declaration shadows it)."""
    body = deepcopy(body)

    def walk(node, terms: frozenset, contexts: frozenset):
        if isinstance(node, DI) and node.name in mapping and node.name not in terms:
            node.name = mapping[node.name]
            return
        if isinstance(node, ID) and node.name in mapping and node.name not in contexts:
            node.name = mapping[node.name]
            return
        if isinstance(node, Mu):
            contexts = contexts | {node.id.name}
        elif isinstance(node, Mutilde):
            terms = terms | {node.di.name}
        elif isinstance(node, Lamda):
            terms = terms | {node.di.di.name}
        elif isinstance(node, Admal):
            contexts = contexts | {node.id.id.name}
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                walk(child, terms, contexts)

    walk(body, frozenset(), frozenset())
    return body


def display_names(debate: str, local: Iterable[str], taken: Iterable[str]) -> Dict[str, str]:
    """{local name: what to show for it in its debate}: the name without
    the prefix, where that name is not ``taken`` in the debate as well."""
    taken, start = set(taken), prefix(debate)
    return {n: n[len(start):] for n in local
            if n.startswith(start) and n[len(start):] not in taken}


def shown(text: str, names: Dict[str, str]) -> str:
    """``text`` with each local name in ``names`` shown as its debate
    shows it (whole identifiers only)."""
    if not names:
        return text
    pattern = re.compile(r"(?<![A-Za-z0-9_])(" + "|".join(
        re.escape(n) for n in sorted(names, key=len, reverse=True)) + r")(?![A-Za-z0-9_])")
    return pattern.sub(lambda m: names[m.group(1)], text)
