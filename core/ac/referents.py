"""Discourse referents (tasks.org, aida-discourse-referents).

A discourse referent ``?x`` is a pseudo-constant of a sort, declared by
``referent x : SORT.``.  An argument may state it (``sup : (Dr [?x] ->
false)``); where such an argument fills a site that mentions a variable
bound by a universal right or an existential left, ``?x`` stands for that
variable.  Neither Fellowship's lexer nor the proposition grammar reads
``?``, so ``?x`` is the declared constant ``referent_x`` everywhere below
the script; ``show`` turns it back for people.
"""

import re

#: The prefix of the constant a referent is; reserved, so no user name
#: can be one.
PREFIX = "referent_"

_WRITTEN = re.compile(r"(?<![\w?])\?([A-Za-z][A-Za-z0-9_]*)")
_CONSTANT = re.compile(rf"\b{PREFIX}([A-Za-z][A-Za-z0-9_]*)")


def constant(name: str) -> str:
    """The constant of the referent ``?name``."""
    return PREFIX + name


def is_referent(name: str) -> bool:
    return name.startswith(PREFIX)


def expand(text: str) -> str:
    """``?x`` -> ``referent_x`` in script text."""
    return _WRITTEN.sub(lambda m: constant(m.group(1)), text)


def show(text: str) -> str:
    """``referent_x`` -> ``?x``, for messages and displays."""
    return _CONSTANT.sub(lambda m: "?" + m.group(1), text)


def mentioned(text) -> set:
    """The referent constants ``text`` mentions."""
    return {PREFIX + m.group(1) for m in _CONSTANT.finditer(text or "")}
