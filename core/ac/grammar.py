"""Compatibility surface for the proof-term parser.

The parser itself lives in :mod:`core.ac.proof_parser`, which replaced a
Lark/Earley grammar.  Earley resolves ambiguity by silently choosing a parse,
and the proof-term syntax is genuinely ambiguous read as a context-free
grammar: the old grammar found three parses for ``λx:A.y*z``.  Fellowship's
syntax is really two mutually recursive grammars, one for terms and one for
contexts, and parsing with the target category in hand removes the ambiguity
instead of arbitrating it.

``Grammar`` and ``ProofTermTransformer`` are kept because a dozen call sites
and tests use them.  Parsing now happens in one step, so the transformer is an
identity; prefer :func:`core.ac.proof_parser.parse_proof_term` in new code.
"""

from core.ac.proof_parser import ProofTermSyntaxError, parse_proof_term

__all__ = ["Grammar", "ProofTermTransformer", "ProofTermSyntaxError", "parse_proof_term"]


class _Parser:
    """Adapter presenting the parser under the old ``.parser.parse`` shape."""

    @staticmethod
    def parse(text, start=None, **kwargs):
        return parse_proof_term(text)


class Grammar:
    """Historical entry point: ``Grammar().parser.parse(text)``."""

    def __init__(self):
        self.parser = _Parser()


class ProofTermTransformer:
    """Historical no-op: parsing already yields the AST."""

    @staticmethod
    def transform(tree):
        return tree
