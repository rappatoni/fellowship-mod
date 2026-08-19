from typing import Optional, Dict, Any
import logging, warnings
from core.comp.visitor import ProofTermVisitor
from contextlib import contextmanager

from core.ac.ast import (
    Mu, Mutilde, Lamda, Admal, Cons, Sonc, Goal, Laog, Deleg, Geled, ID, DI,
    LamdaFO, ConsFO, TermsPairFO, DestructTermsPairFO,
)
from core.ac.prop import BinOp, PBin, PQuant, Prop, PropError, Quantifier

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
    def __init__(self, axiom_props=None, assumptions=None, delegations=None, bound_vars=None, verbose: bool = False):
        self.assumptions = assumptions if assumptions else {}
        self.delegations = delegations if delegations else {}
        self.axiom_props = axiom_props if axiom_props else {}
        self.bound_vars = bound_vars if bound_vars else {}
        self.verbose = verbose
 
    def _unwrap_outer_parens(self, prop: str) -> str:
        """Drop parentheses that wrap the whole proposition.

        str.strip("()") removes characters rather than a matching pair, so it
        mangles propositions whose leading "(" is structural: "(true-A)-B"
        becomes "true-A)-B", which no longer parses.  Only strip when the
        opening parenthesis is closed by the final character.
        """
        if not isinstance(prop, str):
            return prop
        text = prop.strip()
        while text.startswith("(") and text.endswith(")"):
            depth = 0
            for index, char in enumerate(text):
                if char == "(":
                    depth += 1
                elif char == ")":
                    depth -= 1
                    if depth == 0:
                        break
            if index != len(text) - 1:
                break  # the first "(" closes early, so it is not a wrapper
            text = text[1:-1].strip()
        return text

    # -- building propositions ---------------------------------------------
    #
    # Node propositions are strings, so these parse, build and print again.
    # That is worth the round trip: parenthesisation now follows the real
    # precedence rather than a substring test for "->" or "-", which put
    # brackets around any predicate whose name happened to contain a hyphen.

    @staticmethod
    def _as_prop(text):
        """A node's proposition as a Prop, or None if it cannot be read.

        Conjunction and disjunction are outside AIDA's fragment, so a
        declaration mentioning them will not parse; callers fall back to
        string handling rather than failing.
        """
        if text is None or isinstance(text, Prop):
            return text
        try:
            return Prop.parse(text)
        except PropError:
            return None

    def _canonical(self, prop: str) -> str:
        """A declared proposition in canonical spelling.

        Subsumes dropping the parentheses Fellowship wraps a declared type in,
        and does it by understanding the proposition rather than by matching
        brackets.  Propositions outside the fragment keep the older textual
        treatment.
        """
        parsed = self._as_prop(prop)
        return str(parsed) if parsed is not None else self._unwrap_outer_parens(prop)

    def _combine(self, left: str, op: BinOp, right: str):
        left_prop, right_prop = self._as_prop(left), self._as_prop(right)
        if left_prop is None or right_prop is None:
            return None
        return str(PBin(left_prop, op, right_prop))

    def _mk_imp(self, left: str, right: str):
        return self._combine(left, BinOp.IMP, right)

    def _mk_minus(self, left: str, right: str):
        return self._combine(left, BinOp.MINUS, right)

    def _quantify(self, quantifier: Quantifier, var: str, sort, body: str):
        body_prop = self._as_prop(body)
        if body_prop is None:
            return None
        return str(PQuant(quantifier, (var,), sort, body_prop))

    # -- scope -------------------------------------------------------------

    @contextmanager
    def _bound(self, name: str, prop):
        """Bind a variable for the duration of a subtree.

        bound_vars used to be written and never unwound, so a binder's type
        leaked into its siblings and outlived its scope.  First-order
        variables add a second namespace, which makes that worse.
        """
        missing = object()
        previous = self.bound_vars.get(name, missing)
        self.bound_vars[name] = prop
        try:
            yield
        finally:
            if previous is missing:
                self.bound_vars.pop(name, None)
            else:
                self.bound_vars[name] = previous
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
        if node.prop:
            return node
        else:
            if self.assumptions.get(laog_num):
                p = self.assumptions[laog_num]["prop"]
                logger.debug("Enriching Laog with type %s", p)
                node.prop = p
            else:
                if getattr(self, "verbose", False):
                    warnings.warn(f'Enrichment of node {node} not possible: {laog_num} not a key in {self.assumptions}')
                else:
                    logger.debug("Enrichment skipped for Goal %s: key %s not in assumptions", node, laog_num)
            return node

    def visit_Deleg(self, node: Deleg):
        node = super().visit_Deleg(node)
        deleg_num = node.number.strip()
        if node.prop:
            return node
        else:
            if self.delegations.get(deleg_num):
                logger.debug("Enriching Deleg with type %s", self.delegations[deleg_num]["prop"])
                node.prop = self.delegations[deleg_num]["prop"]
            else:
                if getattr(self, "verbose", False):
                    warnings.warn(f'Enrichment of node {node} not possible: {deleg_num} not a key in {self.delegations}')
                else:
                    logger.debug("Enrichment skipped for Deleg %s: key %s not in delegations", node, deleg_num)
            return node
        
    def visit_Geled(self, node: Geled):
        node = super().visit_Geled(node)
        geled_num = node.number.strip()
        if node.prop:
            return node
        else:
            if self.delegations.get(geled_num):
                p = self.delegations[geled_num]["prop"]
                logger.debug("Enriching Geled with type %s", p)
                node.prop = p
            else:
                if getattr(self, "verbose", False):
                    warnings.warn(f'Enrichment of node {node} not possible: {geled_num} not a key in {self.delegations}')
                else:
                    logger.debug("Enrichment skipped for Geled %s: key %s not in delegations", node, geled_num)
            return node

        
    def visit_ID(self, node: ID):
        node = super().visit_ID(node)
        if node.name in self.axiom_props and node.name not in self.bound_vars:
            if node.prop is None:
                node.prop = self._canonical(self.axiom_props[node.name])
                logger.debug("Enriching axiom %s with type %s", node.name, node.prop)
            else:
                logger.debug("Axiom %s already enriched with type %s", node.name, node.prop)
        elif node.name in self.bound_vars and node.prop is None:
            logger.debug("Enriching bound variable %s with type %s",
                        node.name, self.bound_vars[node.name])
            node.prop = self.bound_vars[node.name]
        elif node.prop:
            self.bound_vars[node.name] = node.prop
        elif node.name == "_F_":
            node.flag = "Falsum"
            logger.debug("Falsum flagged for instruction generation.")
        elif node.name == "_T_":
            node.flag = "Truth"
            logger.debug("Truth flagged for instruction generation.")
        else:
            warnings.warn(f'Enrichment of node {node} not possible.')
        return node

    def visit_DI(self, node: DI):
        node = super().visit_DI(node)
        if node.name in self.axiom_props and node.name not in self.bound_vars:
            if node.prop is None:
                node.prop = self._canonical(self.axiom_props[node.name])
                logger.debug("Enriching axiom %s with type %s based on declared axiom",
                            node.name, node.prop)
            else:
                logger.debug("Axiom %s already enriched with type %s", node.name, node.prop)
        elif node.name in self.bound_vars and node.prop is None:
            logger.debug("Enriching bound variable %s with type %s based on bound variable",
                        node.name, self.bound_vars[node.name])
            node.prop = self.bound_vars[node.name]
        elif node.name == "_F_":
            node.flag = "Falsum"
            logger.debug("Falsum flagged for instruction generation.")
        elif node.name == "_T_":
            node.flag = "Truth"
            logger.debug("Truth flagged for instruction generation.")
        else:
            warnings.warn(f'Enrichment of node {node.name} not possible.')
        return node

    def visit_Lamda(self, node: Lamda):
        with self._bound(node.di.di.name, node.di.prop):
            node = super().visit_Lamda(node)
        # Optionally compute node.prop from node.di.prop + "->" + node.term.prop
        # if node.di.prop and node.term.prop exist.
        if node.prop is None and node.di.prop and node.term.prop:
            node.prop = self._mk_imp(node.di.prop, node.term.prop)
        return node

    def visit_Admal(self, node: Admal):
        # bind context variable (ID) with its declared prop
        with self._bound(node.id.id.name, node.id.prop):
            node = super().visit_Admal(node)
        if node.prop is None and node.id.prop and node.context.prop:
            node.prop = self._mk_minus(node.id.prop, node.context.prop)
        return node

    def visit_Cons(self, node: Cons):
        node = super().visit_Cons(node)
        # Similarly, if node.term.prop and node.context.prop => node.prop = ...
        if node.prop is None and node.term.prop and node.context.prop:
            node.prop = self._mk_imp(node.term.prop, node.context.prop)
        return node

    def visit_Sonc(self, node: Sonc):
        node = super().visit_Sonc(node)
        if node.prop is None and node.term.prop and node.context.prop:
            node.prop = self._mk_minus(node.term.prop, node.context.prop)
        return node

    # -- first-order nodes -------------------------------------------------

    def visit_LamdaFO(self, node: LamdaFO):
        """Universal introduction: the body's type, quantified over the binder."""
        with self._bound(node.var, node.sort):
            node = super().visit_LamdaFO(node)
        if node.prop is None and node.term.prop:
            node.prop = self._quantify(
                Quantifier.FORALL, node.var, node.sort, node.term.prop
            )
        return node

    def visit_DestructTermsPairFO(self, node: DestructTermsPairFO):
        """Existential elimination: the dual of universal introduction."""
        with self._bound(node.var, node.sort):
            node = super().visit_DestructTermsPairFO(node)
        if node.prop is None and node.context.prop:
            node.prop = self._quantify(
                Quantifier.EXISTS, node.var, node.sort, node.context.prop
            )
        return node

    def visit_ConsFO(self, node: ConsFO):
        """Universal instantiation.  Its type cannot be built from its children.

        `Cons(t,c)` has type `t.prop -> c.prop`, because an arrow is recoverable
        from its two sides.  `ConsFO(t,c)` instead consumes a `forall x:S, P`
        where `P[t/x]` is `c.prop`, and recovering `P` from the result of a
        substitution is not determined -- several `P` give the same instance.

        The proposition is therefore left unset, and the enclosing command
        takes its type from the other side, which visit_Mu and visit_Mutilde
        already do.  Nothing downstream needs it: instruction generation emits
        `elim [t]` from the witness alone.
        """
        return super().visit_ConsFO(node)

    def visit_TermsPairFO(self, node: TermsPairFO):
        """Existential introduction.  Also not recoverable from its children.

        Fellowship's printer discards this node's binder and body
        (`core.ml:449`), so unlike ConsFO the information is absent from the
        term rather than merely non-invertible.  Same consequence.
        """
        return super().visit_TermsPairFO(node)

    def visit_Mu(self, node: Mu):
        with self._bound(node.id.name, node.prop):
            node = super().visit_Mu(node)
        node.contr = node.term.prop if node.term.prop else node.context.prop if node.context.prop else None
        return node

    def visit_Mutilde(self, node: Mutilde):
        with self._bound(node.di.name, node.prop):
            node = super().visit_Mutilde(node)
        #print("node.term, node.context", node.term.prop, node.context.prop)
        node.contr = node.term.prop if node.term.prop else node.context.prop if node.context.prop else None
        return node
