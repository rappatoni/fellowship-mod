"""Render an internal proposition for Fellowship's command syntax.

Internally, and in Fellowship's own printed output, predicate application is
written with whitespace: ``Child Bob``, ``Father Rich Bob``.  The command
parser instead requires brackets: ``Child[Bob]``, ``Father[Rich][Bob]``.

This module used to carry its own tokenizer and recursive-descent parser for
propositions -- one of two such parsers in the tree, the other in
``pres/decorations.py``.  Both have been replaced by the structured
proposition AST in :mod:`core.ac.prop`, which is the single place that knows
Fellowship's syntax.  The function below is kept as the stable entry point its
callers already use.
"""

from __future__ import annotations

from core.ac.prop import Prop, PropError

#: Retained under its original name: callers catch this, and it is now simply
#: the proposition AST's error type.
PropRenderError = PropError

__all__ = ["PropRenderError", "prop_to_command"]


def prop_to_command(prop: str | None) -> str | None:
    """Render an internal AC proposition for Fellowship command syntax.

    Predicate applications become bracketed and parentheses are placed where
    the command parser needs them, which is not always where the *printer*
    would have put them -- the two disagree about ``-`` versus ``->``.  See
    :mod:`core.ac.prop`.
    """
    if prop is None:
        return None
    text = prop.strip()
    if not text:
        return text
    return Prop.parse(text).to_command()
