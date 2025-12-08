from copy import deepcopy
import logging
from core.comp.visitor import ProofTermVisitor
from core.ac.ast import ProofTerm, Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI
import warnings

logger = logging.getLogger(__name__)


class NegIntroRewriter(ProofTermVisitor):
    """
    Collapse the pattern
    
        μ thesis:¬A . ⟨ μ H1:¬A . ⟨ λ H2:A . μ H3:⊥ . ⟨ H2 ∥ ?k ⟩ ∥ H1 ⟩ ∥ thesis ⟩
    
    to the command that sat in the Laog/Goal “?k”.

    If the pattern is not encountered, we emit a warnings.warn and leave the AST unchanged;
    the caller can inspect the `changed` attribute afterwards.
    """
    def __init__(self):
        self.changed = False

    def visit(self, node):
        logger.debug("Rewriting negation %r to counterargument", node)
        if node is None:
            return None
        if isinstance(node, Mu) and node.id.name == "thesis" and node.prop.startswith("¬"):
            body = self._match_neg_intro(node)
            if body is not None:
                self.changed = True
                return deepcopy(body)
        new = deepcopy(node)
        for attr in ("term", "context"):
            if hasattr(node, attr) and getattr(node, attr) is not None:
                setattr(new, attr, self.visit(getattr(node, attr)))
        if isinstance(node, Lamda):  # Lamda only has .term
            new.term = self.visit(node.term)
        return new

    def _match_neg_intro(self, thesis_mu: Mu):
        inner_mu = thesis_mu.term
        if not (isinstance(inner_mu, Mu) and inner_mu.prop == thesis_mu.prop):
            logger.debug("Failed: %r is not Mu node or its prop %s does not equal the thesis prop %s",
                         inner_mu, inner_mu.prop, thesis_mu.prop)
            return None
        lam = inner_mu.term
        if not isinstance(lam, Lamda):
            logger.debug("Failed: %r is not the expected Lamda term", inner_mu.term)
            return None
        mu_bot = lam.term
        if not (isinstance(mu_bot, Mu) and mu_bot.prop == "⊥"):
            logger.debug("Failed: %r is not Mu node or its prop %s does not equal ⊥", mu_bot, inner_mu.prop)
            return None
        logger.debug("Returning negation normalized body")
        return mu_bot.context

    def rewrite(self, ast: "ProofTerm") -> "ProofTerm":
        logger.info("Starting negation-introduction rewrite")
        out = self.visit(ast)
        if not self.changed:
            warnings.warn("NegIntroRewriter: no outer negation-introduction pattern found")
        logger.info("Finished negation-introduction rewrite (changed=%s)", self.changed)
        return out
