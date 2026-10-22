"""Discourse referents (tasks.org, aida-discourse-referents).

A discourse referent ``?x`` is a pseudo-constant local to an argument:
Fellowship holds it in the argument's goal environment, never in the
signature, and refuses ``qed`` on a theorem that has one, so no referent
is ever registered on the prover side.  An argument states its referents
in a referent environment, ``argument sup : [?x:Pub] (Dr [?x] -> false).``,
or leaves it out to have them collected from the statement.

Where an argument stating a referent fills a site that mentions a variable
bound by a universal right or an existential left, the referent stands for
that variable.
"""

import re

_REFERENT = re.compile(r"(?<![\w?!])[?!][A-Za-z][A-Za-z0-9_]*")


def is_referent(name: str) -> bool:
    """``?x`` asks for an individual, ``!x`` presumes one (tasks.org,
    aida-referents-individuals); matching ignores the sigil."""
    return len(name) > 1 and name[0] in "?!" and name[1].isalpha()


def is_presumed(name: str) -> bool:
    return is_referent(name) and name[0] == "!"


def sort_statement(sort) -> str:
    """The sort node: the statement that ``sort`` has an individual
    (tasks.org, aida-referents-individuals, decided 2026-10-21)."""
    return f"exists x:{sort}, true"


def constant(name: str) -> str:
    """The referent that stands for the variable ``name``."""
    return "?" + name


def mentioned(text) -> set:
    """The referents ``text`` mentions - a proposition, an environment or a
    Fellowship proof term (whose open goals ``?1.2`` are numbers)."""
    return set(_REFERENT.findall(text or ""))


class InContext(str):
    """A proposition as stated in a first-order context: ``scope`` lists
    the discourse referents and the variables bound around the place it
    stands, outermost first, each with its sort.  It reads as the plain
    proposition everywhere; its graph key is the statement in that
    context (core/dc/debate_graph.py, ``context_key``): the export of a
    horn and the import of an argument (tasks.org,
    aida-discourse-referents, decisions of 2026-10-20)."""

    def __new__(cls, text, scope):
        obj = super().__new__(cls, text)
        obj.scope = tuple(scope)
        return obj

    def __reduce__(self):
        return (InContext, (str(self), self.scope))

    def __copy__(self):
        return self

    def __deepcopy__(self, memo):
        return self


def plain(text):
    """``text`` without its context."""
    return str(text) if isinstance(text, str) else text


class Entry(tuple):
    """A context entry, ``(name, sort)`` as a pair, with its kind
    (tasks.org, aida-referents-instantiation, 2026-10-22):

    - ``bound``: a variable a universal right or an existential left
      binds; ``param``: a referent the argument's conclusion states.  Both
      are binder entries: an individual given from above, which owes cover;
    - ``witness``: a referent of an argument's environment its conclusion
      does not state, the argument's own open choice, which owes an
      instance.  ``owner`` is the argument: a statement about it is that
      argument's, never another's (``context_key``)."""

    def __new__(cls, name, sort, kind="bound", owner=None):
        obj = super().__new__(cls, (name, sort))
        obj.kind = kind
        obj.owner = owner
        return obj

    def __eq__(self, other):
        return (tuple.__eq__(self, other)
                and getattr(other, "kind", "bound") == self.kind
                and getattr(other, "owner", None) == self.owner)

    def __ne__(self, other):
        return not self == other

    def __hash__(self):
        return hash((tuple(self), self.kind, self.owner))

    def __reduce__(self):
        return (Entry, (self[0], self[1], self.kind, self.owner))

    def __copy__(self):
        return self

    def __deepcopy__(self, memo):
        return self

    def __repr__(self):
        return f"Entry({self[0]!r}, {self[1]!r}, {self.kind!r}, {self.owner!r})"

    def with_(self, name=None, sort=None):
        return Entry(self[0] if name is None else name, self[1] if sort is None else sort,
                     self.kind, self.owner)


def entry(item) -> Entry:
    """``item`` as an Entry: a plain pair is a bound variable's entry if
    its name is no referent, a parameter's if it is."""
    if isinstance(item, Entry):
        return item
    name, sort = item
    return Entry(name, sort, "param" if is_referent(name) else "bound")


def is_witness(item) -> bool:
    return getattr(item, "kind", None) == "witness"


def rigid(name: str, owner) -> str:
    """The name a witness entry has in keys and patterns: never a referent,
    so numbering does not rename it, and never a declared constant."""
    return f"w#{owner}:{name}"


def entries(env, conclusion, owner) -> tuple:
    """An argument's referent environment as context entries: a referent
    its conclusion states is a parameter, one it does not state a witness
    entry of ``owner``."""
    stated = mentioned(str(conclusion or ""))
    return tuple(item if isinstance(item, Entry)
                 else Entry(item[0], item[1], "param" if item[0] in stated else "witness", owner)
                 for item in env)


def witnesses_of(scope) -> tuple:
    return tuple(e for e in scope if is_witness(e))


def shown(name: str) -> str:
    """A context entry as the graph shows it: a bound variable as the
    referent standing for it."""
    return name if is_referent(name) else constant(name)


def display_name(item, presumed=False) -> str:
    """A context entry as a reader sees it: a witness entry or a
    parameter as its referent (``?u``), a variable a binder introduced
    with the onus of the statement after its name (``z?`` owed, ``z!``
    presumed; tasks.org, aida-referents-instantiation, 2026-10-22)."""
    name = item[0]
    if getattr(item, "kind", None) != "bound":
        return name
    bare = name[1:] if is_referent(name) else name
    return bare + ("!" if presumed else "?")


def env_of(payload) -> list:
    """The local environment of the first goal in a Fellowship payload,
    [(name, sort)] as Fellowship lists it."""
    found = []

    def walk(item):
        if found or not isinstance(item, (list, tuple, dict)):
            return
        if isinstance(item, dict):
            for value in item.values():
                walk(value)
            return
        if len(item) == 2 and item[0] == "env" and isinstance(item[1], list):
            entries = item[1:]
            found.append(entries)
            return
        if item and item[0] == "env":
            found.append(list(item[1:]))
            return
        for value in item:
            walk(value)

    walk(payload)
    out = []
    for entry in (found[0] if found else []):
        fields = {k: v for k, v in entry if isinstance(entry, list)} if isinstance(entry, list) else {}
        if "name" in fields:
            out.append((fields["name"], fields.get("sort")))
    return out
