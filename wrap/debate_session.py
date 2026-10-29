"""A debate's own session (tasks.org, aida-debate-sessions).

A registered debate (aida-registered-debates) lives in a session of its
own: a second ``ProverWrapper`` with its own Fellowship and a classical
document holding exactly what the debate is made of - the declarations it
needs and its arguments, registered there as ordinary arguments.  The
document's later changes do not reach it, and what it declares or
registers does not reach the document.  Every operation of the service
then works on it as on any session (``Service.of(debate_session.session)``).

A ``DebateSpec`` says what the session holds, as a replay: Fellowship
declarations in order, and argument bodies in registration order.  The
process is started on first use (``session``) and stopped by ``close``;
the spec can start it again, as long as the document it was made for is
still the session's.  A new document, or closing the session that owns
the debate, closes it (``ProverWrapper._close_debate_sessions``).

``scope_spec`` builds the spec for arguments of the owning document: the
declarations they mention, closed over the sorts those mention, in the
document's order, and the arguments themselves.  A held theorem they
mention that is not one of them is declared as an axiom: Fellowship has
already checked it.
"""

import logging
import re
from copy import deepcopy
from dataclasses import dataclass, field
from typing import Any, Dict, Iterable, List, Optional

from core.ac.ast import ProofTerm, ID, DI
from wrap.prover import ProverError

logger = logging.getLogger(__name__)

_IDENTIFIER = re.compile(r"[A-Za-z][A-Za-z0-9_]*")


class DebateSessionError(Exception):
    """A debate session could not be made: a declaration or an argument
    its spec replays was refused, or its document is gone."""


@dataclass
class ArgumentSpec:
    name: str
    conclusion: str
    body: Any


@dataclass
class DebateSpec:
    #: Fellowship commands (``declare``/``deny``), in the order they are sent.
    declarations: List[str] = field(default_factory=list)
    #: Arguments, in registration order (the order unfolding nests them in).
    arguments: List[ArgumentSpec] = field(default_factory=list)
    #: Wrapper-side decorations (pres/decorations.py).
    decorations: Dict[str, str] = field(default_factory=dict)


class DebateSession:
    """The session a debate lives in, started on first use."""

    def __init__(self, parent, spec: DebateSpec, *, name: str):
        self.parent = parent
        self.doc = parent.doc                  # the document the debate belongs to
        self.spec = spec
        self.name = name
        self._session = None

    @property
    def alive(self) -> bool:
        return self._session is not None

    @property
    def session(self):
        """The debate's ``ProverWrapper``, started (and its spec replayed)
        if it is not running."""
        if self._session is None:
            self._materialise()
        return self._session

    def _materialise(self) -> None:
        from wrap.service import AidaError, Service
        if self.parent.doc is not self.doc:
            raise DebateSessionError(f"debate '{self.name}': its document is gone.")
        child = self.parent.spawn_child()
        self._session = child
        self.doc.debate_sessions.append(self)
        try:
            for command in self.spec.declarations:
                try:
                    child.send_top_level(command)
                except ProverError as e:
                    raise DebateSessionError(
                        f"debate '{self.name}': `{command}` refused in its session: {e}") from e
            child.decorations.update(self.spec.decorations)
            service = Service.of(child)
            for argument in self.spec.arguments:
                try:
                    service.register_body(argument.name, argument.conclusion, deepcopy(argument.body))
                except AidaError as e:
                    raise DebateSessionError(
                        f"debate '{self.name}': argument '{argument.name}' refused in its "
                        f"session: {e}") from e
        except BaseException:
            self.close()
            raise
        logger.debug("Debate session '%s' started: %d declaration(s), %d argument(s).",
                     self.name, len(self.spec.declarations), len(self.spec.arguments))

    def close(self) -> None:
        """Stop the debate's Fellowship.  The spec stays: ``session``
        starts it again while the document is the session's."""
        session, self._session = self._session, None
        if self in self.doc.debate_sessions:
            self.doc.debate_sessions.remove(self)
        if session is not None:
            session.close()
            logger.debug("Debate session '%s' closed.", self.name)


# ---------------------------------------------------------------------------
# Specs for arguments of the owning document
# ---------------------------------------------------------------------------

def scope_spec(parent, names: Iterable[str]) -> DebateSpec:
    """The spec of a session holding the arguments ``names`` of
    ``parent``'s document (in the document's registration order), with
    the declarations they need."""
    wanted = set(names)
    arguments = []
    for name, argument in parent.arguments.items():
        if name not in wanted:
            continue
        if parent.names.get(name) == "statement":
            raise DebateSessionError(f"'{name}' is a statement; a debate session holds "
                                     f"arguments only, for now.")
        if getattr(argument, "composed", False) or getattr(argument, "body", None) is None:
            raise DebateSessionError(f"'{name}' has no term of its own to replay.")
        arguments.append(ArgumentSpec(name, argument.conclusion, argument.body))
    missing = wanted - {a.name for a in arguments}
    if missing:
        raise DebateSessionError(f"no argument {', '.join(sorted(missing))} in the document.")
    mentioned = set()
    for argument in arguments:
        mentioned |= _identifiers(argument.conclusion)
        mentioned |= mentions(argument.body)
    return DebateSpec(declarations_needed(parent.declarations, mentioned,
                                          exclude={a.name for a in arguments}),
                      arguments, dict(parent.decorations))


def declarations_needed(declarations: Dict[str, Any], mentioned: Iterable[str], *,
                        exclude: Iterable[str] = ()) -> List[str]:
    """The commands declaring the names ``mentioned`` and, transitively,
    what their own types or propositions mention, in the order of
    ``declarations`` (the document's)."""
    exclude = set(exclude)
    needed, todo = set(), [n for n in mentioned if n in declarations and n not in exclude]
    while todo:
        name = todo.pop()
        if name in needed:
            continue
        needed.add(name)
        todo.extend(n for n in _identifiers(str(declarations[name]))
                    if n in declarations and n not in exclude and n not in needed)
    return [declaration_command(name, declarations[name]) for name in declarations if name in needed]


def declaration_command(name: str, declaration) -> str:
    """The Fellowship command that declares ``name`` as the document did:
    a sort, an axiom (a held theorem is declared as one), or a moxia."""
    kind = getattr(declaration, "kind", "prop")
    if kind == "sort":
        return f"declare {name} : {declaration}."
    if kind == "moxia":
        return f"deny {name} : ({declaration})."
    return f"declare {name} : ({declaration})."


def mentions(body) -> set:
    """Every name a term mentions: its variables and the identifiers of
    the propositions and sorts written on its nodes."""
    found = set()

    def walk(node):
        if isinstance(node, (ID, DI)):
            found.add(node.name)
        for attribute in ("prop", "sort"):
            value = getattr(node, attribute, None)
            if isinstance(value, str):
                found.update(_identifiers(value))
        # term/context: the subterms; di/id: binders and hypotheses,
        # whose propositions count too
        for slot in ("term", "context", "di", "id"):
            child = getattr(node, slot, None)
            if isinstance(child, ProofTerm):
                walk(child)

    walk(body)
    return found


def _identifiers(text: str) -> set:
    return set(_IDENTIFIER.findall(text or ""))
