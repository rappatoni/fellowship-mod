"""Debate-local names (tasks.org, aida-debate-local-arguments).

A registered debate's own declarations and arguments live in its session
(wrap/debate_session.py) under ``DEBATE__name``, so they can never clash
with the document's names - the debate's session also holds the
document's declarations it needs - and every name stays unique across the
document (the UI, the inventory, provenance).  Fellowship's identifiers
are ``letter (letter | digit | _)*``, so ``__`` is the only separator
available; the owning session reserves ``DEBATE__`` once the debate is
registered (``ProverWrapper.reserve_prefix``).

Everything declared inside a ``register debate`` block is local - proof
constants and moxias, and sorts too (the author, 2026-10-29): what a
debate shares with the document is declared before the block, in the
document.  An importer puts the program's vocabulary there and keeps the
devices of its translation (scasp-aida's helper atoms ``H_A``) inside.
A local sort occurs inside propositions, so renaming reaches the
proposition text on every node, as well as the term's leaves; a
proposition that binds a local name as a quantifier variable is refused
rather than guessed at.

Shown for its own debate, a local name drops the prefix, unless the
short name is taken there too (a document ``b`` beside the debate's
``b``): then it is shown as it is (``display_names``).
"""

import re
from copy import deepcopy
from typing import Dict, Iterable, List, Tuple

from core.ac.ast import ProofTerm, Mu, Mutilde, Lamda, Admal, ID, DI, ConsFO, TermsPairFO
from core.ac.prop import SSym, TApp, TSym

SEPARATOR = "__"

_DECLARATION = re.compile(r"^\s*(declare|deny)\s+([^:]+?)\s*:\s*(.+?)\s*\.?\s*$", re.S)
_QUANTIFIED = re.compile(r"\b(?:forall|exists)\s+([A-Za-z][A-Za-z0-9_]*)")


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


def localise_declarations(commands: Iterable[str], debate: str) -> Tuple[List[str], Dict[str, str]]:
    """A block's declarations with every name they declare prefixed -
    where it is declared and wherever another of them mentions it - and
    {name: local name}."""
    parsed = [parse_declaration(c) for c in commands]
    mapping = {n: local_name(debate, n) for _, names, _ in parsed for n in names}
    return ([f"{verb} {', '.join(mapping[n] for n in names)} : {rename_text(typ, mapping)}."
             for verb, names, typ in parsed], mapping)


def rename_text(text: str, mapping: Dict[str, str]) -> str:
    """A proposition or a type with the identifiers in ``mapping`` renamed
    (whole identifiers only).  ValueError if it binds one of them as a
    quantifier variable: the bound occurrences are not the local name."""
    if not text or not mapping:
        return text
    bound = set(_QUANTIFIED.findall(text)) & set(mapping)
    if bound:
        raise ValueError(f"'{text}' binds {', '.join(sorted(bound))}, which the debate declares; "
                         f"rename the bound variable.")
    return shown(text, mapping)


def rename_term(body: ProofTerm, mapping: Dict[str, str]) -> ProofTerm:
    """A copy of ``body`` with the names in ``mapping`` renamed: free leaves
    (a leaf bound by a binder of its own kind is left alone - a binder named
    like a declaration shadows it), the propositions and sorts written on
    every node, and the constants of first-order terms."""
    body = deepcopy(body)

    def text(node):
        for attribute in ("prop", "sort"):
            value = getattr(node, attribute, None)
            if isinstance(value, str):
                setattr(node, attribute, rename_text(value, mapping))
            elif isinstance(value, SSym) and value.name in mapping:
                setattr(node, attribute, SSym(mapping[value.name]))

    def walk(node, terms: frozenset, contexts: frozenset):
        text(node)
        if isinstance(node, DI):
            if node.name in mapping and node.name not in terms:
                node.name = mapping[node.name]
            return
        if isinstance(node, ID):
            if node.name in mapping and node.name not in contexts:
                node.name = mapping[node.name]
            return
        if isinstance(node, Mu):
            text(node.id)
            contexts = contexts | {node.id.name}
        elif isinstance(node, Mutilde):
            text(node.di)
            terms = terms | {node.di.name}
        elif isinstance(node, Lamda):
            text(node.di), text(node.di.di)
            terms = terms | {node.di.di.name}
        elif isinstance(node, Admal):
            text(node.id), text(node.id.id)
            contexts = contexts | {node.id.id.name}
        elif isinstance(node, ConsFO):
            node.fo_term = _rename_fo(node.fo_term, mapping)
        elif isinstance(node, TermsPairFO):
            node.witness = _rename_fo(node.witness, mapping)
        for slot in ("term", "context"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                walk(child, terms, contexts)

    walk(body, frozenset(), frozenset())
    return body


def _rename_fo(term, mapping):
    if isinstance(term, TSym):
        return TSym(mapping.get(term.name, term.name))
    if isinstance(term, TApp):
        return TApp(_rename_fo(term.fun, mapping), _rename_fo(term.arg, mapping))
    return term


def display_names(debate: str, local: Iterable[str], taken: Iterable[str]) -> Dict[str, str]:
    """{local name: what to show for it in its debate}: the name without
    the prefix, where that name is not ``taken`` in the debate as well."""
    taken, start = set(taken), prefix(debate)
    return {n: n[len(start):] for n in local
            if n.startswith(start) and n[len(start):] not in taken}


def shown(text: str, names: Dict[str, str]) -> str:
    """``text`` with each name in ``names`` replaced (whole identifiers
    only): how a debate shows its own names, and how renaming spells them."""
    if not names:
        return text
    pattern = re.compile(r"(?<![A-Za-z0-9_])(" + "|".join(
        re.escape(n) for n in sorted(names, key=len, reverse=True)) + r")(?![A-Za-z0-9_])")
    return pattern.sub(lambda m: names[m.group(1)], text)
