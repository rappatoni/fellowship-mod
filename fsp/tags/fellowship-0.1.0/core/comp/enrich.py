from typing import Optional, Dict, Any
import logging, warnings
from core.comp.visitor import ProofTermVisitor
from core.ac.ast import Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI

logger = logging.getLogger(__name__)


class PropEnrichmentVisitor(ProofTermVisitor):
    """
    Replaces the old hack of parsing 'tt' from ID/DI name. 
    Instead, we rely on global knowledge:
      - assumption_mapping: {goal_number: proposition}
      - axiom_props: { 'r1': 'A->B', ...}
    which we can pass in the constructor.
    For example, for each Goal node, we set .prop = assumption_mapping[goal_number].
    For each ID or DI named 'r1', we set .prop = axiom_props['r1'], etc.
    Optionally, for Lamda, Cons, Mu, etc., we can set node.prop if we have enough info.
    """
    def __init__(self, axiom_props=None, assumptions=None, bound_vars=None, verbose: bool = False):
        self.assumptions = assumptions if assumptions else {}
        self.axiom_props = axiom_props if axiom_props else {}
        self.bound_vars = bound_vars if bound_vars else {}
        self.verbose = verbose
 
    def visit_Goal(self, node: Goal):
        node = super().visit_Goal(node)
        goal_num = node.number.strip()
        if node.prop:
            return node
        else:
            if self.assumptions.get(goal_num):
                logger.debug("Enriching Goal with type %s", self.assumptions[goal_num]["prop"])
                node.prop = self.assumptions[goal_num]["prop"]
            else:
                if getattr(self, "verbose", False):
                    warnings.warn(f'Enrichment of node {node} not possible: {goal_num} not a key in {self.assumptions}')
                else:
                    logger.debug("Enrichment skipped for Goal %s: key %s not in assumptions", node, goal_num)
            return node

    def visit_Laog(self, node: Laog):
        node = super().visit_Laog(node)
        laog_num = node.number.strip()
        if self.assumptions.get(laog_num):
            p = self.assumptions[laog_num]["prop"]
            logger.debug("Enriching Laog with type %s", p)
            node.prop = p
        return node

    def visit_ID(self, node: ID):
        node = super().visit_ID(node)
        if node.name in self.axiom_props and node.name not in self.bound_vars:
            if node.prop is None:
                logger.debug("Enriching axiom %s with type %s",
                            node.name, self.axiom_props[node.name].strip("()"))
                node.prop = self.axiom_props[node.name].strip("()")
            else:
                logger.debug("Axiom %s already enriched with type %s", node.name, node.prop)
        elif node.name in self.bound_vars and node.prop is None:
            logger.debug("Enriching bound variable %s with type %s",
                        node.name, self.bound_vars[node.name])
            node.prop = self.bound_vars[node.name]
            if node.prop.startswith("~") or node.prop.startswith("¬"):
                node.flag = "bound_negation"
                if self.verbose: pass
                logger.debug('Bound negation flagged for instruction generation.')
        elif node.prop:
            self.bound_vars[node.name] = node.prop
        elif node.name == "_F_":
            node.flag = "Falsum"
            logger.debug("Falsum flagged for instruction generation.")
        else:
            warnings.warn(f'Enrichment of node {node} not possible.')
        return node

    def visit_DI(self, node: DI):
        node = super().visit_DI(node)
        if node.name in self.axiom_props and node.name not in self.bound_vars:
            if node.prop is None:
                logger.debug("Enriching axiom %s with type %s based on declared axiom",
                            node.name, self.axiom_props[node.name].strip("()"))
                node.prop = self.axiom_props[node.name].strip("()")
            else:
                logger.debug("Axiom %s already enriched with type %s", node.name, node.prop)
        elif node.name in self.bound_vars and node.prop is None:
            logger.debug("Enriching bound variable %s with type %s based on bound variable",
                        node.name, self.bound_vars[node.name])
            node.prop = self.bound_vars[node.name]
            if node.prop.startswith("~") or node.prop.startswith("¬"):
                node.flag = "bound_negation"
                if self.verbose: pass
                logger.debug('Bound negation flagged for instruction generation.')
        elif node.name == "_F_":
            node.flag = "Falsum"
            logger.debug("Falsum flagged for instruction generation.")
        else:
            warnings.warn(f'Enrichment of node {node.name} not possible.')
        return node

    def visit_Lamda(self, node: Lamda):
        self.bound_vars[node.di.di.name] = node.di.prop
        node = super().visit_Lamda(node)
        # Optionally compute node.prop from node.di.prop + "->" + node.term.prop
        # if node.di.prop and node.term.prop exist.
        if node.prop is None and node.di.prop and node.term.prop:
            node.prop = f"{node.di.prop}->{node.term.prop}"
        return node

    def visit_Cons(self, node: Cons):
        node = super().visit_Cons(node)
        # Similarly, if node.term.prop and node.context.prop => node.prop = ...
        if node.prop is None and node.term.prop and node.context.prop:
            node.prop = f"{node.term.prop}->{node.context.prop}"
        return node

    def visit_Mu(self, node: Mu):
        self.bound_vars[node.id.name]=node.prop
        node = super().visit_Mu(node)
        node.contr = node.term.prop if node.term.prop else node.context.prop if node.context.prop else None
        return node

    def visit_Mutilde(self, node: Mutilde):
       
        self.bound_vars[node.di.name]=node.prop
        node = super().visit_Mutilde(node)
        #print("node.term, node.context", node.term.prop, node.context.prop)
        node.contr = node.term.prop if node.term.prop else node.context.prop if node.context.prop else None
        return node
