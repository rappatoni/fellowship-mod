"""Compatibility surface for the parser.

Everything AIDA parses -- propositions, sorts, first-order terms and proof
terms -- is described by one grammar in :mod:`core.ac.syntax`.  ``Grammar`` and
``ProofTermTransformer`` are kept because a dozen call sites and tests use
them.  Parsing now happens in one step, so the transformer is an identity;
prefer :func:`core.ac.syntax.parse_proof_term` in new code.
"""

from core.ac.syntax import ProofTermSyntaxError, SyntaxErrorInSource, parse_proof_term

__all__ = [
    "Grammar",
    "ProofTermTransformer",
    "ProofTermSyntaxError",
    "SyntaxErrorInSource",
    "parse_proof_term",
]


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
