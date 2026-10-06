from copy import deepcopy
from typing import Optional

# Import AST nodes and the base visitor from parser to avoid circular deps
# while parser still owns the AST and base visitor.
from core.ac.ast import (
    ProofTerm, Mu, Mutilde, Lamda, Admal, Cons, Sonc, Goal, Laog, Deleg, Geled, ID, DI,
    LamdaFO, ConsFO, TermsPairFO, DestructTermsPairFO,
)
from core.comp.visitor import ProofTermVisitor


def pres_str(node) -> str:
    """Render a proof term for a message, never raising.

    ``ProofTermGenerationVisitor`` MUTATES what it visits (it hangs a ``.pres``
    string on every node), so this deep-copies first.  That makes it expensive:
    one full copy plus one full traversal per call.  Callers that build log
    messages must therefore guard with ``logger.isEnabledFor(...)`` - the cost
    is paid as soon as the arguments are evaluated, whatever the level.

    This is the one copy of a helper that was pasted into four modules under
    three names (``_pres_str``, ``_present``, ``_node_pres``); new code uses
    this one.
    """
    try:
        copy = ProofTermGenerationVisitor().visit(deepcopy(node))
        return getattr(copy, "pres", repr(node))
    except Exception:
        return repr(node)


def pres_tree(node) -> str:
    """Render a proof term as an indented tree, for a multi-line log
    artifact (``core.logging_util.artifact``), never raising.

    The vanilla rendering (``pres.nl.vanilla_rendering``) keeps the full
    proof-term syntax and only adds line breaks and tree guides.  It does
    not mutate the term, so unlike ``pres_str`` it needs no copy.  Falls
    back to ``pres_str`` on anything it cannot render.
    """
    from pres.nl import pretty_natural, vanilla_rendering
    try:
        return pretty_natural(node, vanilla_rendering)
    except Exception:
        return pres_str(node)


class ProofTermGenerationVisitor(ProofTermVisitor):
    """Generate a proof term from an (enriched or rewritten) argument body.

    verbosity levels:
      0 / False: enriched, non-verbose proof term (historical default)
      1 / True: enriched, verbose proof term with contraction labels
     -1: replay-oriented proof term that strips ordinary leaf annotations while
         preserving binder and open-placeholder annotations needed for replay
    """
    def __init__(self, verbose: bool | int = False):
        if isinstance(verbose, bool):
            self.verbosity = 1 if verbose else 0
        else:
            self.verbosity = int(verbose)
        self.verbose = self.verbosity > 0

    @staticmethod
    def _lambda_body(node) -> str:
        """A lambda body, parenthesised when it is an application.

        A lambda binds tighter than `*`, so `λh:R.b*c` reads as
        `Cons(Lamda(h,b), c)`.  Writing `Admal(h, Cons(b,c))` without
        parentheses would therefore produce a term that reads back as a
        different one -- and this matters past display, since reduce.py
        compares subtrees by their rendered string.  core.ml guards the same
        three printers the same way.
        """
        return f'({node.pres})' if isinstance(node, (Cons, ConsFO, Sonc)) else node.pres

    def _typed_leaf(self, name, prop):
        if self.verbosity < 0:
            return f'{name}'
        return f'{name}:{prop}' if prop else f'{name}'

    def _open_term(self, prefix, number, prop):
        if prop:
            return f'{prefix}{number}:{prop}'
        return f'{prefix}{number}'

    def _open_context(self, number, prop, suffix):
        if prop:
            return f'{number}:{prop}{suffix}'
        return f'{number}{suffix}'

    def visit_Mu(self, node: Mu):
        node = super().visit_Mu(node)
        if self.verbose:
            node.pres = f'μ{node.id.name}:{node.prop}.<{node.term.pres}|{node.contr}|{node.context.pres}>'
        else:
            node.pres = f'μ{node.id.name}:{node.prop}.<{node.term.pres}||{node.context.pres}>'
        return node

    def visit_Mutilde(self, node: Mutilde):
        node = super().visit_Mutilde(node)
        if self.verbose:
            node.pres = f"μ'{node.di.name}:{node.prop}.<{node.term.pres}|{node.contr}|{node.context.pres}>"
        else:
            node.pres = f"μ'{node.di.name}:{node.prop}.<{node.term.pres}||{node.context.pres}>"
        return node

    def visit_Lamda(self, node: Lamda):
        node = super().visit_Lamda(node)
        node.pres = f'λ{node.di.di.name}:{node.di.prop}.{self._lambda_body(node.term)}'
        return node

    def visit_Cons(self, node: Cons):
        node = super().visit_Cons(node)
        node.pres = f'{node.term.pres}*{node.context.pres}'
        return node

    def visit_Admal(self, node: Admal):
        node = super().visit_Admal(node)
        node.pres = f'λ{node.id.id.name}:{node.id.prop}.{self._lambda_body(node.context)}'
        return node

    def visit_Sonc(self, node: Sonc):
        node = super().visit_Sonc(node)
        node.pres = f'{node.context.pres}*{node.term.pres}'
        return node

    def visit_Goal(self, node: Goal):
        node = super().visit_Goal(node)
        node.pres = self._open_term('?', node.number, node.prop)
        return node

    def visit_Laog(self, node: Laog):
        node = super().visit_Laog(node)
        node.pres = self._open_context(node.number, node.prop, '?')
        return node

    def visit_Deleg(self, node: Deleg):
        node = super().visit_Deleg(node)
        node.pres = self._open_term('!', node.number, node.prop)
        return node

    def visit_Geled(self, node: Geled):
        node = super().visit_Geled(node)
        node.pres = self._open_context(node.number, node.prop, '!')
        return node

    @staticmethod
    def _leaf_name(node) -> str:
        """A leaf's name; a citation of a sub-debate (core/dc/share.py)
        adds what its site captures and cuts: ``d[alpha -> !:A, B:?]``."""
        return f'{node.name}{getattr(node, "bracket", "")}'

    def visit_ID(self, node: ID):
        node = super().visit_ID(node)
        node.pres = self._typed_leaf(self._leaf_name(node), node.prop)
        return node

    def visit_DI(self, node: DI):
        node = super().visit_DI(node)
        node.pres = self._typed_leaf(self._leaf_name(node), node.prop)
        return node

    # -- first-order nodes -------------------------------------------------
    #
    # The inverse of the grammar's four first-order productions.  A witness is
    # printed juxtaposed, as Fellowship prints it; the bracketed spelling is
    # only for the command parser, and belongs to instruction generation.

    def visit_LamdaFO(self, node: LamdaFO):
        node = super().visit_LamdaFO(node)
        node.pres = f'λ{node.var}:{node.sort}.{self._lambda_body(node.term)}'
        return node

    def visit_ConsFO(self, node: ConsFO):
        node = super().visit_ConsFO(node)
        node.pres = f'{node.fo_term}*{node.context.pres}'
        return node

    def visit_TermsPairFO(self, node: TermsPairFO):
        node = super().visit_TermsPairFO(node)
        node.pres = f'({node.witness},{node.term.pres})'
        return node

    def visit_DestructTermsPairFO(self, node: DestructTermsPairFO):
        node = super().visit_DestructTermsPairFO(node)
        # A destructor is a context, so its body can extend maximally.
        node.pres = f'({node.var}:{node.sort}).{node.context.pres}'
        return node

    def visit_unhandled(self, node):
        return node
