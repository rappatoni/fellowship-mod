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

_REFERENT = re.compile(r"(?<![\w?])\?[A-Za-z][A-Za-z0-9_]*")


def is_referent(name: str) -> bool:
    return len(name) > 1 and name[0] == "?" and name[1].isalpha()


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


def shown(name: str) -> str:
    """A context entry as the graph shows it: a bound variable as the
    referent standing for it."""
    return name if is_referent(name) else constant(name)


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
