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
