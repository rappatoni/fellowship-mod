"""Declared names and their kinds, as reported by Fellowship.

Fellowship's machine payload tags every declaration with a ``kind``::

    ((name "N")  (kind sort) (sort "type"))       a sort
    ((name "O")  (kind sort) (sort "N"))          a first-order constant
    ((name "P")  (kind sort) (sort "N->bool"))    a predicate
    ((name "A")  (kind sort) (sort "bool"))       a propositional atom
    ((name "ax") (kind prop) (prop "forall ..."))  an axiom
    ((name "mA") (kind moxia)(prop "A"))          a denied proposition

The wrapper used to discard the kind and keep only the string, which is fine
for a propositional calculus but not for a first-order one: Fellowship prints a
sort annotation and a proposition annotation identically, so ``λx:A.t`` and
``λx:N.t`` can only be told apart by knowing whether ``A`` and ``N`` were
declared as propositions or as sorts.
"""

from __future__ import annotations


class Declaration(str):
    """A declared type or proposition, tagged with the kind Fellowship gave it.

    Subclasses :class:`str` deliberately.  Callers that only ever wanted the
    rendered text -- ``pres.decorations.render_declaration``, the natural
    language renderers, every f-string that interpolates a declaration -- keep
    working untouched, while code that needs to distinguish a sort from a
    proposition can consult :attr:`kind`.
    """

    #: ``"sort"``, ``"prop"`` or ``"moxia"``.
    kind: str

    def __new__(cls, text: str, kind: str) -> "Declaration":
        obj = super().__new__(cls, text)
        obj.kind = kind
        return obj

    def __repr__(self) -> str:
        return f"Declaration({str.__repr__(self)}, kind={self.kind!r})"
