# Make this directory importable as a package root (so 'core', 'pres', etc. resolve)
import sys as _sys, pathlib as _pathlib
_BASE = _pathlib.Path(__file__).parent.resolve()
if str(_BASE) not in _sys.path:
    _sys.path.insert(0, str(_BASE))
from lark import Lark, Transformer
import re
import warnings
import collections
from copy import deepcopy
import os
import logging
from typing import Optional, Any, List, Tuple, Dict

from core.ac.ast import ProofTerm, Term, Context, Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI, Hyp
from core.ac.grammar import Grammar, ProofTermTransformer
from core.comp.visitor import ProofTermVisitor
from core.dc.graft import graft_single as _graft_single_core, graft_uniform as _graft_uniform_core
from core.comp.neg_rewrite import NegIntroRewriter
from core.ac.instructions import InstructionsGenerationVisitor
from core.comp.enrich import PropEnrichmentVisitor
logger = logging.getLogger('fsp.parser')




def pretty_natural(proof_term: "ProofTerm", semantic: "Rendering_Semantics") -> str:
    lines = []
    traverse_proof_term(semantic, proof_term, lines, indent=0)
    return '\n'.join(lines)



#Argument Reducer Helpers

def _var_occurs(name: str, node: ProofTerm) -> bool:
    """Return True iff *name* appears free inside *node*."""
    if isinstance(node, (ID, DI)):
        return node.name == name
    if isinstance(node, Lamda):
        # variable shadowing inside lamda binder (node.di.di.name)
        if node.di.di.name == name:
            return False  # bound, not free
    if isinstance(node, Mu):
        if node.id.name == name:
            return False
    if isinstance(node, Mutilde):
        if node.di.name == name:
            return False
    # recurse
    for child in getattr(node, 'term', None), getattr(node, 'context', None):
        if child and _var_occurs(name, child):
            return True
    return False

def _is_affine(varname: str, command: ProofTerm) -> bool:
    """A binder is *affine* if its bound variable occurs nowhere in the
    command body (term + context)."""
    if isinstance(command, (Mu, Mutilde)):
        return not _var_occurs(varname, command.term) and not _var_occurs(varname, command.context)
    return False

def _subst(node, name: str, replacement):
    """Return a deep‑copied version of node where every free occurrence
    of the variable `name` (an ID/DI whose .name equals `name`) is replaced
    by a deep‑copy of `replacement`.

    Note:
      - This function stops descending beneath a binder that re‑binds the
        same variable name (shadowing across λ, μ, or μ̃).
      - It does NOT α‑rename unrelated binders to avoid capture of free
        variables that might occur in `replacement`. Therefore it is intended
        to be used on closed proof terms as produced by the prover (no free
        IDs/DIs), not on arbitrary open terms.
    """
    # Atomic cases -----------------------------------------------------------
    if isinstance(node, (ID, DI)):
        return deepcopy(replacement) if node.name == name else deepcopy(node)

    # Binder blocks substitution under shadowing -----------------------------
    if isinstance(node, Lamda) and node.di.di.name == name:
        return deepcopy(node)                  # λx. …  — shadowed
    if isinstance(node, Mu) and node.id.name == name:
        return deepcopy(node)                  # μx. …  — shadowed
    if isinstance(node, Mutilde) and node.di.name == name:
        return deepcopy(node)                  # μ' x. … — shadowed

    # Recursive descent ------------------------------------------------------
    new_node = deepcopy(node)
    if hasattr(new_node, 'term') and new_node.term is not None:
        new_node.term = _subst(new_node.term, name, replacement)
    if hasattr(new_node, 'context') and new_node.context is not None:
        new_node.context = _subst(new_node.context, name, replacement)
    return new_node

    
class _ArgumentTermReducer_OLD(ProofTermVisitor):
    """
    Implements the following reduction rules (each left and right):
       - lambda
       - undercut (defeat and defence)
       - TODO: rebut
       - TODO: support
       - mu
       - TODO: eta
    """

    def __init__(self, *, verbose: bool = True, assumptions=None, axiom_props=None):
        super().__init__()
        self.verbose      = verbose
        self.assumptions  = assumptions  or {}
        self.axiom_props  = axiom_props or {}
        self._root        = None     # set by reduce()
        self._step        = 0        # counter for pretty printing
        self._trace_rows: list[tuple[int, str, str, str]] = []
        self._term_no: int = 0
        self._printed_header: bool = False
        self._w_no: int = 3
        self._w_term: int = int(os.getenv("FSP_REDUCE_TERM_WIDTH", "72"))
        self._w_rule: int = 16
        self._w_comment: int = 24
        
    def reduce(self, root: "ProofTerm") -> "ProofTerm":
        """Return the normal form; log a table of (no, term, rule, comments)."""
        #logger.info("Starting argument term reduction.")
        self._root = root
        # header + input row
        self._snapshot(rule="input", comment=None)
        out = self.visit(root)
        # final row if no further rules applicable
        try:
            if not self._has_next_redex(out):
                self._snapshot(rule="-", comment="no more rules applicable")
        except Exception:
            # best-effort; don't break reduction if heuristic fails
            # TODO: this should raise a warning that termination of reduction could not be determined and surface the exception which caused it.
            pass
        #logger.info("Finished argument term reduction.")
        return out

    # helper – snapshot current state into a table ------------------------------
    def _snapshot(self, rule: Optional[str] = None, comment: Optional[str] = None) -> None:
        if not self.verbose:
            return
        show = deepcopy(self._root)
        # TODO: is this enrichment call really necessary?
        show = PropEnrichmentVisitor(assumptions=self.assumptions,
                                     axiom_props=self.axiom_props).visit(show)
        show = ProofTermGenerationVisitor().visit(show)
        term_txt = show.pres

        def _fmt(text: str, width: int) -> str:
            s = text or ""
            return (s[:max(1, width-1)] + "…") if len(s) > width else s.ljust(width)

        # header once
        if not self._printed_header:
            logger.info("")  # spacer before table
            hdr = f"{'No'.rjust(self._w_no)} | {_fmt('Term', self._w_term)} | {_fmt('Rule', self._w_rule)} | {_fmt('Comments', self._w_comment)}"
            logger.info(hdr)
            self._printed_header = True

        # log a row and advance the counter
        row = (self._term_no, term_txt, rule or "", comment or "")
        self._trace_rows.append(row)
        line = f"{str(row[0]).rjust(self._w_no)} | {_fmt(row[1], self._w_term)} | {_fmt(row[2], self._w_rule)} | {_fmt(row[3], self._w_comment)}"
        logger.info(line)
        # additionally show the full (untruncated) proof term on DEBUG
        if logger.isEnabledFor(logging.DEBUG):
            logger.debug("Full proof term at step %d: %s", self._term_no, show.pres)
        if (rule == "-" or (comment or "") == "no more rules applicable"):
            logger.info("")  # spacer after table
        self._term_no += 1

    def _pres_str(self, n: ProofTerm) -> str:
        c = deepcopy(n)
        c = ProofTermGenerationVisitor().visit(c)
        return getattr(c, "pres", repr(c))

    def visit_Mu(self, node: Mu):
        # First, normalise the sub‑components so that the rule also fires in
        # inner positions.
        

        # λ‑rule ---------------------------------------------------------
        if isinstance(node.term, Lamda) and isinstance(node.context, Cons):
            lam: Lamda = node.term
            cons: Cons = node.context
            v      = cons.term      # the argument v
            E      = cons.context  # the continuation E (might itself be None)
            if isinstance(v, Goal):
                if self.verbose: logger.debug("Skipping λ‑rule: argument is bare Goal")
            else:
                if self.verbose: logger.debug("Applying λ‑rule")
                # Build the new µ'‑binder that will become the *context* part.
                new_context = Mutilde(
                    di_=lam.di.di,          # the variable x
                    prop=lam.di.prop,     # its declared type
                    term=deepcopy(lam.term),
                    context=deepcopy(E)
                )
                node.term    = deepcopy(v)
                node.context = new_context
                self._snapshot("lambda")
                self.visit_Mu(node)
                
                return node

        # affine helper --------------------------------------------------
        def _guards(inner_mu: Mu, ctx_mt: Mutilde):
            # both binders affine in their own commands
            if not (_is_affine(inner_mu.id.name, inner_mu) and
                    _is_affine(ctx_mt.di.name,   ctx_mt)):
                if self.verbose: logger.debug("Guard A failed")
                return False
            # both protect goals with equal prop (we ignore number for now)
            if not isinstance(inner_mu.term, Goal) and (isinstance(ctx_mt.context, Goal) or isinstance(ctx_mt.context.context.term, Goal)):
                if self.verbose: logger.debug("Guard B failed")
                return False
            if not isinstance(ctx_mt.context, Mutilde):
                if self.verbose: logger.debug("Guard C failed")
                return False
            if not node.id.name == inner_mu.context.name:
                if self.verbose: logger.debug("Guard D failed")
                return False
            #if isinstance(inner_mu.term, Goal) and isinstance(ctx_mt.context.context.term, Goal):
                #What is the point of this guard again?
             #   print(f"termprop {inner_mu.term.prop} vs Contextprop {ctx_mt.term.prop}")
              #  return inner_mu.term.prop == ctx_mt.context.context.term.prop
            if self.verbose: logger.debug("termprop %s vs contextprop %s", inner_mu.term.prop, ctx_mt.term.prop)
            return inner_mu.term.prop == ctx_mt.term.prop

        
        # affine μ‑rule --------------------------------------------
        if isinstance(node.term, Mu) and isinstance(node.context, Mutilde):
            inner_mu: Mu = node.term
            ctx_mt:   Mutilde = node.context
            if self.verbose: logger.debug("Alternative Arguments %s and %s detected.", inner_mu.id.name, ctx_mt.di.name)
            if _guards(inner_mu, ctx_mt)==True:
                if self.verbose: logger.debug("Guards passed.")

                # Case 1: ctx_mt.context is µ̃ with affine binder → throw‑away
                if self.verbose: logger.debug("Case 1? %s", _is_affine(ctx_mt.context.di.name, ctx_mt.context))
                if _is_affine(ctx_mt.context.di.name, ctx_mt.context):
                    if self.verbose: logger.debug("Applying defence rule")
                    # Apply rewrite: μ α .⟨ G || α ⟩
                    node.term    = deepcopy(inner_mu.term)
                    node.context = deepcopy(inner_mu.context)  # should be ID α
                    self._snapshot("alt-defence-mu")
                    self.visit_Mu(node)
                    return node

                # Case 2: ctx_mt.context already in normal form (no more redexes we know)
                # Rough heuristic: after recursive call, if no λ‑redex or affine‑µ pattern
                # matches in that sub‑tree, regard it as normal‑form.
                if self.verbose: logger.debug("Case 2? %s",  not self._has_next_redex(ctx_mt.context))
                if not self._has_next_redex(ctx_mt.context):
                    if self.verbose: logger.debug("Applying defeat rule")
                    node.term    = deepcopy(ctx_mt.term)
                    node.context = deepcopy(ctx_mt.context)  # t*
                    self._snapshot("alt-defeat-mu")
                    self.visit_Mu(node)
                    return node
                else:
                    if self.verbose: logger.debug("Simplifying attacker argument")
                    before = self._pres_str(ctx_mt.context)
                    ctx_mt.context = self.visit_Mutilde(ctx_mt.context)
                    after  = self._pres_str(ctx_mt.context)
                    if before == after:
                        raise RuntimeError(
                            f"Affine μ simplify made no progress at Mu node {self._pres_str(node)} "
                            f"while simplifying attacker {self._pres_str(ctx_mt.context)}; "
                            f"check guards or rewrite rules"
                        )
                    # Re-enter on this node now that attacker changed
                    return self.visit_Mu(node)


        # general μ‑rule --------------------------------------------
    # ────────────────────────────────────────────────────────────────────
        # GENERAL µ‑REDUCTION  ⟨ µ x.c  ∥  E ⟩  →  [E/x]c
        # Pattern in the AST:  node.term  is a *Mu*  (µ x.c)
        #                      node.context           is  E
        # After substitution we replace *node* by the command *c* with the
        # variable occurrences rewritten.  To stay within the current AST
        # we *mutate* *node* in‑place so that it *becomes* that command
        # (its *id/prop* remain untouched — this is adequate for our use
        # cases because outer binders are unaffected by the rewrite).
        # ────────────────────────────────────────────────────────────────────
        if isinstance(node.term, Mu):
            inner = node.term          # µ x.c
            x     = inner.id.name
            E     = node.context       # replacement
            if isinstance(E, Laog):
                if self.verbose: logger.debug("Skipping μ‑β: context is bare Laog")
            else:
                if self.verbose: logger.debug("Applying general Mu reduction rule")
                # Perform capture‑avoiding substitution inside the *command* c.
                new_term    = _subst(inner.term,    x, deepcopy(E))
                new_context = _subst(inner.context, x, deepcopy(E))

                # Splice the substituted command into *node* (drop µ x.).
                node.term    = new_term
                node.context = new_context
                self._snapshot("mu-beta")
                self.visit_Mu(node)
                return node
        if isinstance(node.context, Mutilde):
            inner = node.context       # µ x.c
            alpha     = inner.di.name
            v     = node.term       # replacement
            if isinstance(v, Goal):
                if self.verbose: logger.debug("Skipping μ̃‑β application to '%s': term is bare Goal '%s'", inner.pres, v.pres)
            else:
                if self.verbose: logger.debug("Applying general Mutilde rule")
                # Perform capture‑avoiding substitution inside the *command* c.
                new_term    = _subst(inner.term,    alpha, deepcopy(v))
                new_context = _subst(inner.context, alpha, deepcopy(v))

                # Splice the substituted command into *node* (drop µ x.).
                node.term    = new_term
                node.context = new_context
                self._snapshot("mutilde-beta")
                self.visit_Mu(node)
                return node
        #node = super().visit_Mu(node)

        node = super().visit_Mu(node)
        return node

    def visit_Mutilde(self, node: Mutilde):
        # First, normalise the sub‑components so that the rule also fires in
        # inner positions.
        # node = super().visit_Mutilde(node)

        # λ‑rule ---------------------------------------------------------
        if isinstance(node.term, Lamda) and isinstance(node.context, Cons):
            lam: Lamda = node.term
            cons: Cons = node.context

            v      = cons.term      # the argument v
            E      = cons.context  # the continuation E (might itself be None)

            if isinstance(v, Goal):
                if self.verbose: logger.debug("Skipping λ‑rule: argument is bare Goal")
            else:
                # Build the new µ'‑binder that will become the *context* part.
                new_context = Mutilde(
                    di_=lam.di.di,          # the variable x
                    prop=lam.di.prop,     # its declared type
                    term=deepcopy(lam.term),
                    context=deepcopy(E)
                )
                node.term    = deepcopy(v)
                node.context = new_context
                self._snapshot("lambda")
                self.visit_Mutilde(node)
                return node

        # affine helper --------------------------------------------------
        def _guards(inner_mu: Mu, ctx_mt: Mutilde):
            # both binders affine in their own commands
            if not (_is_affine(inner_mu.id.name, inner_mu) and
                    _is_affine(ctx_mt.di.name,   ctx_mt)):
                if self.verbose: logger.debug("Guard Atilde failed")
                return False
            # both protect goals with equal prop (we ignore number for now)
            if not (isinstance(inner_mu.context, Laog) and isinstance(ctx_mt.context, Laog)):
                if self.verbose: logger.debug("Guard Btilde failed")
                return False
            if not isinstance(inner_mu.term, Mu):
                return False
            if not node.di.name == ctx_mt.term.name:
                if self.verbose: logger.debug("Guard Ctilde failed")
                return False
            if self.verbose: logger.debug("termprop %s vs contextprop %s", inner_mu.term.prop, ctx_mt.term.prop)
            return inner_mu.context.prop == ctx_mt.context.prop
        
        # affine μ'‑rule --------------------------------------------
        if isinstance(node.term, Mu) and isinstance(node.context, Mutilde):
            inner_mu: Mu = node.term
            ctx_mt:   Mutilde = node.context
            if self.verbose: logger.debug("Alternative Counterarguments %s and %s detected.", inner_mu.id.name, ctx_mt.di.name)
            if _guards(inner_mu, ctx_mt)==True:

                # Case 1: inner_mu.term is µ̃ with affine binder → throw‑away
                if self.verbose: logger.debug("Case 1? %s", _is_affine(inner_mu.term.id.name, inner_mu.term))
                if _is_affine(inner_mu.term.id.name, inner_mu.term):
                    if self.verbose: logger.debug("Applying Mutilde defence rule")
                    # Apply rewrite: μ α .⟨ G || α ⟩
                    node.term    = deepcopy(ctx_mt.term)
                    node.context = deepcopy(ctx_mt.context)  # should be ID α
                    self._snapshot("alt-defence-mutilde")
                    self.visit_Mutilde(node)
                    return node

                # Case 2: ctx_mt.context already in normal form (no more redexes we know)
                # Rough heuristic: after recursive call, if no λ‑redex or affine‑µ pattern
                # matches in that sub‑tree, regard it as normal‑form.
                if self.verbose: logger.debug("Case 2? %s",  not self._has_next_redex(inner_mu.term))
                if not self._has_next_redex(inner_mu.term):
                    if self.verbose: logger.debug("Applying Mutilde defeat rule")
                    node.term    = deepcopy(inner_mu.term)
                    node.context = deepcopy(inner_mu.context)  # t*
                    self._snapshot("alt-defeat-mutilde")
                    self.visit_Mutilde(node)
                    return node
                else:
                    before = self._pres_str(inner_mu.term)
                    inner_mu.term = self.visit_Mu(inner_mu.term)
                    after  = self._pres_str(inner_mu.term)
                    if before == after:
                        raise RuntimeError(
                            f"Affine μ' simplify made no progress at Mutilde node {self._pres_str(node)} "
                            f"while simplifying attacker {self._pres_str(inner_mu.term)}; "
                            f"check guards or rewrite rules"
                        )
                    # Re-enter on this node now that attacker changed
                    return self.visit_Mutilde(node)

                    # self.visit_Mu(inner_mu.term)

        if isinstance(node.term, Mu):
            inner = node.term          # µ x.c
            x     = inner.id.name
            E     = node.context       # replacement

            if isinstance(E, Laog):
                if self.verbose: logger.debug("Skipping μ‑β: context is bare Laog")
            else:
                if self.verbose: logger.debug("Applying general Mu reduction rule")
                # Perform capture‑avoiding substitution inside the *command* c.
                new_term    = _subst(inner.term,    x, deepcopy(E))
                new_context = _subst(inner.context, x, deepcopy(E))

                # Splice the substituted command into *node* (drop µ x.).
                node.term    = new_term
                node.context = new_context
                self._snapshot("mu-beta")
                self.visit_Mutilde(node)
                return node

        if isinstance(node.context, Mutilde):
            inner = node.context       # µ x.c
            alpha     = inner.di.name
            v     = node.term       # replacement
            if isinstance(v, Goal):
                if self.verbose: logger.debug("Skipping μ̃‑β: term is bare Goal")
            else:
                if self.verbose: logger.debug("Applying general Mutilde rule")
                # Perform capture‑avoiding substitution inside the *command* c.
                new_term    = _subst(inner.term,    alpha, deepcopy(v))
                new_context = _subst(inner.context, alpha, deepcopy(v))

                # Splice the substituted command into *node* (drop µ x.).
                node.term    = new_term
                node.context = new_context
                self._snapshot("mutilde-beta")
                self.visit_Mutilde(node)
                return node
        node = super().visit_Mutilde(node)
        
        return node

    def _has_next_redex(self, n: ProofTerm) -> bool:
        """Lightweight check for patterns we reduce, respecting bare Goal/Laog guards."""
        if isinstance(n, Mu):
            # λ‑redex applies only if argument v is not a bare Goal
            if isinstance(n.term, Lamda) and isinstance(n.context, Cons) and not isinstance(n.context.term, Goal):
                return True
            # μ‑β applies only if the context is not a bare Laog
            if isinstance(n.term, Mu) and not isinstance(n.context, Laog):
                return True
            # μ̃‑β applies only if the term is not a bare Goal
            if isinstance(n.context, Mutilde) and not isinstance(n.term, Goal):
                return True
        if isinstance(n, Mutilde):
            # λ‑redex applies only if argument v is not a bare Goal
            if isinstance(n.term, Lamda) and isinstance(n.context, Cons) and not isinstance(n.context.term, Goal):
                return True
            # μ‑β applies only if the context is not a bare Laog
            if isinstance(n.term, Mu) and not isinstance(n.context, Laog):
                return True
            # μ̃‑β applies only if the term is not a bare Goal
            if isinstance(n.context, Mutilde) and not isinstance(n.term, Goal):
                return True
        # recurse quickly
        for child in getattr(n, 'term', None), getattr(n, 'context', None):
            if child and self._has_next_redex(child):
                return True
        return False

from core.comp.reduce import ArgumentTermReducer as _ArgumentTermReducer_OLD
ArgumentTermReducer = _ArgumentTermReducer_OLD

    
class Rendering_Semantics:
    def __init__(self, indentation, Mu, Mutilde, Lamda, Cons, Goal, ID, DI):
        self.indentation = indentation
        self.Mu = Mu
        self.Mutilde = Mutilde
        self.Lamda = Lamda
        #self.Hyp = Hyp
        self.Cons = Cons
        self.Goal = Goal
        #self.Done = Done
        self.ID = ID
        self.DI = DI

natural_language_rendering = Rendering_Semantics('   ', "we need to prove ", f"we proved ", f"assume ", f"and", f"? ", f"done ", f"by ")
natural_language_dialectical_rendering = Rendering_Semantics('   ', "Assume a refutation of ", f"Assume a proof of  ", f"assume ", f"and", f"? ", f"but then we have a contradiction, done ", f"by ")
natural_language_argumentative_rendering = Rendering_Semantics('   ', ["We will argue for ", "undercutting ", "supported by alternative ", "undercut by "], [f"We will argue against ", "using ", "by adapter"], f"assume ", f"and", f"by default ", f"done ", f"by ")

def traverse_proof_term(semantic, term, lines, indent): # TODO: change to instantiation of ProofTermVisitor
    indent_str = semantic.indentation * indent
    if isinstance(term, Mu):
        if term.id.name == "stash":
            lines.append(f"{indent_str}"+semantic.Mu[1] + f"{term.prop}"+ " in")
            traverse_proof_term(semantic, term.term, lines, indent + 1)
        elif term.id.name == "support":
            lines.append(f"{indent_str}"+semantic.Mu[0] + f"{term.prop}")
            traverse_proof_term(semantic, term.term, lines, indent + 1)
            lines.append(f"{indent_str}"+semantic.Mu[2])
            traverse_proof_term(semantic, term.context, lines, indent + 1)
        elif re.compile(r'alt\d').match(term.id.name):
            traverse_proof_term(semantic, term.term, lines, indent)
            traverse_proof_term(semantic, term.context, lines, indent)
        elif term.id.name == "undercut":
             # Note that the result is somewhat ugly due to the need for an adapter to deal with Fellowship's treatment of negation.
             lines.append(f"{indent_str}"+semantic.Mu[0] + f"{term.prop}" + f"({term.id.name})")
             traverse_proof_term(semantic, term.context, lines, indent + 1)
             lines.append(f"{indent_str}"+semantic.Mu[3])
             traverse_proof_term(semantic, term.term, lines, indent + 1)
        else:
            lines.append(f"{indent_str}"+semantic.Mu[0] + f"{term.prop}" + f"({term.id.name})")
            traverse_proof_term(semantic, term.term, lines, indent + 1)
            traverse_proof_term(semantic, term.context, lines, indent + 1)
        # lines.append(f"{indent_str}{semantic.Done}")
    elif isinstance(term, Mutilde):
        if term.di.name == "issue":
            lines.append(f"{indent_str}"+semantic.Mutilde[0] + f"{term.prop}")
            traverse_proof_term(semantic, term.term, lines, indent)
            lines.append(f"{indent_str}"+semantic.Mutilde[1])
            traverse_proof_term(semantic, term.context, lines, indent)
        elif term.di.name == "adapter":
            traverse_proof_term(semantic, term.term, lines, indent)
            lines.append(f"{indent_str}".removesuffix(semantic.indentation)+semantic.Mutilde[2])
        # For now an abandoned attempt to pretty print Fellowship's peculiar treatment of negation.
        # elif term.prop == "¬A":
        #     traverse_proof_term(semantic, term.context, lines, indent + 1)
        elif re.compile(r'alt\d').match(term.di.name):
            traverse_proof_term(semantic, term.term, lines, indent)
            traverse_proof_term(semantic, term.context, lines, indent)
        else:
            lines.append(f"{indent_str}"+semantic.Mutilde[0] + f"{term.prop} " + f"({term.di.name})")
            traverse_proof_term(semantic, term.term, lines, indent + 1)
            traverse_proof_term(semantic, term.context, lines, indent + 1)
        
            
    elif isinstance(term, Lamda):
        lines.append(f"{indent_str}"+semantic.Lamda + f"{term.di.prop}" + f"({term.di.di.name})")
        traverse_proof_term(semantic, term.term, lines, indent)
    # elif isinstance(term, Hyp):
    #     lines.append(f"{indent_str}"+semantic.Hyp + f"{term.id}")
    elif isinstance(term, Cons):
        lines.append(f"{indent_str}"+semantic.Cons)
        traverse_proof_term(semantic, term.term, lines, indent)
        traverse_proof_term(semantic, term.context, lines, indent)
    elif isinstance(term, Goal):
        lines.append(f"{indent_str}"+semantic.Goal + f"{term.number}")
    elif isinstance(term, DI):
        lines.append(f"{indent_str}" + semantic.DI + f"{term.name}")
    elif isinstance(term, ID):
        lines.append(f"{indent_str}".removesuffix(semantic.indentation) + semantic.ID)
    else:
        lines.append(f"{indent_str}Unhandled term type: {type(term)}")

# nat = pretty_natural(proof_term_ast, natural_language_argumentative_rendering )
# dia = pretty_natural(proof_term_ast, natural_language_dialectical_rendering )
# arg = pretty_natural(easy_undercut_ast, natural_language_argumentative_rendering)
# easy = pretty_natural(easy_ast, natural_language_argumentative_rendering)
# print(nat)
# print(dia)
# print(arg)
# print(easy)

def match_trees(nodeA: "ProofTerm", nodeB: "ProofTerm", mapping: Dict[str, str]) -> bool:
    # If node types are different, cannot match
    if type(nodeA) != type(nodeB):
        logger.debug("Type mismatch: %s vs %s", type(nodeA), type(nodeB))
        return False
    logger.debug("Match")
    # Handle different node types
    if isinstance(nodeA, Mu):
        # Match id_, prop, term, context
        idB = nodeB.id.name
        idA = nodeA.id.name
        if idB in mapping:
            if mapping[idB] != idA:
                return False
        else:
            mapping = mapping.copy()
            mapping[idB] = idA
        if nodeB.prop != nodeA.prop:
            return False
        if not match_trees(nodeA.term, nodeB.term, mapping):
            return False
        if not match_trees(nodeA.context, nodeB.context, mapping):
            return False
        return True

    elif isinstance(nodeA, Mutilde):
        # Match di_, prop, term, context
        diB = nodeB.di.name
        diA = nodeA.di.name
        if diB in mapping:
            if mapping[diB] != diA:
                return False
        else:
            mapping = mapping.copy()
            mapping[diB] = diA
        if nodeB.prop != nodeA.prop:
            return False
        if not match_trees(nodeA.term, nodeB.term, mapping):
            return False
        if not match_trees(nodeA.context, nodeB.context, mapping):
            return False
        return True

    elif isinstance(nodeA, Lamda):
        # Match di_, prop, term
        diB = nodeB.di.di.name
        diA = nodeA.di.di.name
        if diB in mapping:
            if mapping[diB] != diA:
                return False
        else:
            mapping = mapping.copy()
            mapping[diB] = diA
        if nodeB.prop != nodeA.prop:
            return False
        if not match_trees(nodeA.term, nodeB.term, mapping):
            return False
        return True

    elif isinstance(nodeA, Cons):
        # Match term and context
        if not match_trees(nodeA.term, nodeB.term, mapping):
            return False
        if not match_trees(nodeA.context, nodeB.context, mapping):
            return False
        return True

    elif isinstance(nodeA, Goal):
        # Goal numbers don't matter.
        # if nodeA.number != nodeB.number:
        #     return False
        return True

    elif isinstance(nodeA, ID) or isinstance(nodeA, DI):
        # Match names with mapping
        nameA = nodeA.name
        nameB = nodeB.name
        if nameB in mapping:
            if mapping[nameB] != nameA:
                return False
        else:
            mapping = mapping.copy()
            mapping[nameB] = nameA
        return True

    else:
        # Unhandled node type
        return False


def get_child_nodes(node: "ProofTerm") -> List["ProofTerm"]:
    child_nodes = []
    if isinstance(node, Mu):
        child_nodes.append(node.term)
        child_nodes.append(node.context)
    elif isinstance(node, Mutilde):
        child_nodes.append(node.term)
        child_nodes.append(node.context)
    elif isinstance(node, Lamda):
        child_nodes.append(node.term)
    elif isinstance(node, Cons):
        child_nodes.append(node.term)
        child_nodes.append(node.context)
    # For ID, DI, and Goal, there are no child nodes
    return child_nodes

def is_subargument(A: "ProofTerm", B: "ProofTerm") -> bool:
    # Returns True if B is a subargument of A up to variable renaming
    # We need to traverse A and attempt to match B starting from each node
    # Might be worth to also create a convenience method to generate all subarguments.
    nodes_to_visit = [A]
    # First we need to strip the thesis root.
    B = B.term
    while nodes_to_visit:
        current_node = nodes_to_visit.pop()
        if match_trees(current_node, B, {}):
            return True
        # Add children to the nodes_to_visit list
        child_nodes = get_child_nodes(current_node)
        nodes_to_visit.extend(child_nodes)
    return False

def contrary(A: str) -> str:
    # Deprecated!
    # Returns the contrary of assumption A
    if A.endswith('_bar'):
        return A[:-4]
    else:
        return A + '_bar'

def neg(A: str) -> str:
    return '¬'+A

def label_assumption(node: "ProofTerm", A: str, assumptions: Dict[str, Dict[str, Any]]) -> Tuple[str, "ProofTerm"]: #TODO: Implement as an instantiation of ProofTermVisitor
    #TODO: Have to first test all own assumptions that are not parts of stashed or supporting subarguments.
    # If any are OUT or UNDEC, any (intermediate) conclusion of the argument becomes UNDEC.
    
    if node is None:
        return ('UNDEC',node)

    if isinstance(node, Goal):
        # Use the goal number to retrieve the corresponding proposition
        logger.debug("ISGOAL")
        goal_number = node.number.strip()
        goal_prop = assumptions[goal_number]["prop"]
        if goal_prop is None:
            logger.debug("%s", goal_prop)
            return ('UNDEC', node)

        elif goal_prop == A:
        # A matches the goal's proposition
            return ('IN', node)
        elif goal_prop == contrary(A):
            return ('OUT',node)
        else:
            # A does not match the goal's proposition
            return ('UNDEC', node)


    elif isinstance(node, ID):
        logger.debug("ISID")
        # Rule: An assumption A is OUT if the root is an ID object with prop A
        if node.prop == A:
            return ('OUT',node)
        else:
            return ('UNDEC', node)

    elif isinstance(node, DI):
        logger.debug("ISDI")
        # Rule: An assumption A_bar is OUT if the root is a DI object with prop A
        if node.prop == contrary(A):
            return ('OUT',node)
        else:
            return ('UNDEC', node)

    elif isinstance(node, Lamda):
        logger.debug("ISLAMDA")
        label_di = label_assumption(node.di.di, A, assumptions)[0]
        label_term = label_assumption(node.term, A, assumptions)[0]

        if label_di == 'UNDEC' and label_term == 'IN' and node.prop != contrary(A):
            return ('IN', node)
        elif label_di == 'OUT' or label_term == 'OUT' or node.prop == contrary(A):
            return ('OUT',node)
        else:
            return ('UNDEC', node)

    elif isinstance(node, Cons):
        logger.debug("ISCONS")
        label_term = label_assumption(node.term, A, assumptions)[0]
        label_context = label_assumption(node.context, A, assumptions)[0]

        if (label_term == 'IN' or label_context == 'IN') and not (label_term == 'OUT' or label_context == 'OUT'):
            return ('IN', node)
        elif label_term == 'OUT' or label_context == 'OUT' or node.prop == A:
            return ('OUT',node)
        else:
            return ('UNDEC', node)

    elif isinstance(node, Mu):
        logger.debug("ISMU")
        if node.id.name == 'stash':
            logger.debug("ISSTASH")
            if node.prop == A:
            # An assumption A is IN if the root is a Mu-object with id 'stash' and prop A
                return ('IN', node)
            elif node.prop == contrary(A) or node.prop == neg(A):
                return ('OUT',node)
            else:
                return ('UNDEC', node)
        elif node.id.name == 'support':
            logger.debug("ISSUPPORT")
            if node.prop == A:
            # An assumption that gets shadowed by an alternative mu-bound continutation is UNDEC. This is because the bound continuation provides an alternative way to derive the assumption so it is no longer needed.. 
                return ('UNDEC', node)
            elif node.prop == contrary(A) or node.prop==neg(A):
                return ('OUT',node)
            else:
                label_term = label_assumption(node.term, A, assumptions)[0]
                label_context = label_assumption(node.context, A, assumptions)[0]
            
                if (label_term == 'IN' or label_context == 'IN') and not (label_term == 'OUT' or label_context == 'OUT'):
                    return ('IN', node)

                elif label_term == 'OUT' or label_context == 'OUT':
                    return ('OUT',node)

                else:
                    return ('UNDEC', node)
                
        elif node.prop != contrary(A) and node.prop != neg(A) :
            logger.debug("ISNOTCONTR")
            label_term = label_assumption(node.term, A, assumptions)[0]
            label_context = label_assumption(node.context, A, assumptions)[0]
            
            if (label_term == 'IN' or label_context == 'IN') and not (label_term == 'OUT' or label_context == 'OUT'):
                return ('IN', node)

            elif label_term == 'OUT' or label_context == 'OUT':
                return ('OUT',node)

            else:
                return ('UNDEC', node)

        else:
            
            return ('OUT',node)


    elif isinstance(node, Mutilde):
        logger.debug("ISMUTILDE")
        if node.di.name == 'stash':
            if node.prop == contrary(A) or node.prop == neg(A):
                    # An assumption A_bar is IN if the root is a Mutilde-object with di 'stash' and prop A
                    return ('IN', node)
            elif node.prop == A:
                    return ('OUT',node)
            else:
                return ('UNDEC', node)
        #TODO elif support, stash
        elif node.prop != A:
            label_term = label_assumption(node.term, A, assumptions)[0]
            label_context = label_assumption(node.context, A, assumptions)[0]

            if (label_term == 'IN' or label_context == 'IN') and not (label_term == 'OUT' or label_context == 'OUT'):
                return ('IN', node)
            
            elif label_term == 'OUT' or label_context == 'OUT':
                return ('OUT',node)

            else:
                return ('UNDEC', node)
            
        else:
            return ('OUT',node)

    else:
        return ('UNDEC', node)


class ProofTermGenerationVisitor(ProofTermVisitor):
    """Generate a proof term from an (enriched or rewritten) argument body."""

    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        pass
    
    def visit_Mu(self, node : Mu):
        node = super().visit_Mu(node)
        if self.verbose == True:
            node.pres = f'μ{node.id.name}:{node.prop}.<{node.term.pres}|{node.contr}|{node.context.pres}>'
        else:
            node.pres = f'μ{node.id.name}:{node.prop}.<{node.term.pres}||{node.context.pres}>'
        return node

    def visit_Mutilde(self, node : Mutilde):
        node = super().visit_Mutilde(node)
        if self.verbose == True:
            node.pres = f"μ'{node.di.name}:{node.prop}.<{node.term.pres}|{node.contr}|{node.context.pres}>"
        else:
            node.pres = f"μ'{node.di.name}:{node.prop}.<{node.term.pres}||{node.context.pres}>"
        return node

    def visit_Lamda(self, node : Lamda):
        node = super().visit_Lamda(node)
        node.pres = f'λ{node.di.di.name}:{node.di.prop}.{node.term.pres}'
        return node

    def visit_Cons(self, node : Cons):
        node = super().visit_Cons(node)
        node.pres = f'{node.term.pres}*{node.context.pres}'
        return node

    def visit_Goal(self, node : Goal):
        node = super().visit_Goal(node)
        node.pres = f'{node.number}:{node.prop}'
        return node
    def visit_Laog(self, node : Laog):
        node = super().visit_Laog(node)
        node.pres = f'{node.number}:{node.prop}'
        return node

    def visit_ID(self, node : ID):
        node = super().visit_ID(node)
        if node.prop:
            node.pres = f'{node.name}:{node.prop}'
        else:
            node.pres= f'{node.name}'
        return node

    def visit_DI(self, node : DI):
        node = super().visit_DI(node)
        if node.prop:
            node.pres = f'{node.name}:{node.prop}'
        else:
            node.pres= f'{node.name}'
        return node

    def visit_unhandled(self, node):
        return node




# ────────────────────────────────────────────────────────────────────────────────
#  Graft operator
#     • *graft_single*   — replace one specific open goal ?k / !k having prop P
#     • *graft_uniform*  — replace **all** open goals / laogs whose prop is P
#
#  Capturing semantics
#  -------------------
#  When we splice an argument *A* (its proof‑term AST) in place of a Goal or
#  Laog that sits underneath binders introducing variables
#
#        x₁:P₁  …  xₘ:Pₘ     (μ‑IDs capture **Laogs** )
#        y₁:Q₁  …  yₙ:Qₙ     (λ / μ̃‑DIs capture **Goals** )
#
#  then **inside the clone of A** any *open* Goal whose proposition is some Qⱼ is
#  replaced by the DI‑variable yⱼ, and any *open* Laog whose proposition equals
#  Pᵢ **or Pᵢ_bar** (wrapper convention) is replaced by the ID‑variable xᵢ.
# ────────────────────────────────────────────────────────────────────────────────

#__all__ = ["graft_single", "graft_uniform"]

# ---------------------------------------------------------------------------
#  Helper – capture‑aware substitution of Goals / Laogs by variables
# ---------------------------------------------------------------------------

# TODO(capture-avoidance / renaming during graft)
# Context:
#   Grafting currently ensures binder–binder freshness via _alpha_rename_if_needed(scion, root)
#   and performs capturing substitution of Goals/Laogs by DI/ID variables along the graft path.
#   What’s missing for full robustness is avoiding accidental capture of SCION FREE VARIABLES
#   by ROOT binders encountered along the traversal to the graft site.
#
# Why this is postponed:
#   The Fellowship prover does not allow creation of free variables during proof construction;
#   “free variables” correspond to open Goals/Laogs and not to DI/ID occurrences. Therefore
#   users cannot build terms that hit this corner case. Only hand-crafted ASTs (e.g., tests)
#   could exhibit it. We leave the implementation for when/if it becomes necessary.
#
# Intended approach (when we pick this up):
#   1) Compute the scion’s free IDs/DIs:
#        free_ids, free_dis = _free_vars(scion)
#   2) While traversing body_B toward the graft site, if a binder would capture one of those
#      free names, α‑rename that binder locally and rewrite its bound occurrences in its scope:
#        - For Mu(ID x): if x ∈ free_ids → fresh := _fresh(x, taken); rename binder + uses via _RenameIDUses
#        - For Lamda/Hyp(DI x) and Mutilde(DI x): if x ∈ free_dis → fresh := _fresh(x, taken);
#          rename binder + uses via _RenameDIUses
#   3) Maintain a “taken” set seeded from both root and scion binder names to ensure freshness.
#   4) Do NOT rewrite scion free variables; only rename root binders along the graft path.
#
# Sketch of helpers to add when implementing:
#   - _free_vars(node) -> tuple[set[str], set[str]]
#       Collect free ID/DI names respecting local bindings.
#   - _RenameIDUses(old,new) / _RenameDIUses(old,new): visitors that rename occurrences within
#       a binder scope, but stop when encountering an inner rebinding of the same name.
#   - Extend _GraftVisitor.__init__ to precompute free vars/taken names and
#     modify visit() for Mu/Lamda/Mutilde to perform local binder α-renaming if needed.
#
# See also:
#   - _alpha_rename_if_needed(scion, root): existing binder–binder freshness utility.
#   - _CapturingSubst: current capturing semantics for Goals/Laogs.
#
class _CapturingSubst(ProofTermVisitor):
    """Replace captured Goals / Laogs by the corresponding variable."""

    def __init__(self, *, goal_map: dict[str, str], laog_map: dict[str, str]):
        # prop → DI‑var  /  prop → ID‑var
        self.goal_map = goal_map
        self.laog_map = laog_map

    # generic traversal -------------------------------------------------
    def visit(self, node):
        if node is None:
            return None
        if isinstance(node, Goal):
            return self._rewrite_goal(node)
        if isinstance(node, Laog):
            return self._rewrite_laog(node)
        new_node = deepcopy(node)
        if hasattr(node, 'term') and node.term is not None:
            new_node.term = self.visit(node.term)
        if hasattr(node, 'context') and node.context is not None:
            new_node.context = self.visit(node.context)
        return new_node

    # leaves ------------------------------------------------------------
    def _rewrite_goal(self, node: Goal):
        #print("Goal Map", self.goal_map)
        var = self.goal_map.get(node.prop)
        return DI(var, node.prop) if var is not None else deepcopy(node)

    def _rewrite_laog(self, node: Laog):
        #print("Laog Map", self.laog_map)
        var = self.laog_map.get(node.prop)
        return ID(var, node.prop) if var is not None else deepcopy(node)

# ---------------------------------------------------------------------------
#  Helper – traversal that performs the grafting itself
# ---------------------------------------------------------------------------

class _GraftVisitor(ProofTermVisitor):
    """Walk *body_B* and replace matching Goal/Laog by *replacement_ast*."""

    def __init__(self, *, replacement_ast, target_prop: str,
                 target_number: str | None, target_is_goal: bool):
        self.replacement_proto = deepcopy(replacement_ast)
        self.target_prop   = target_prop
        self.target_number = target_number       # None ⇒ uniform graft
        self.target_is_goal = target_is_goal
        # Binder stacks --------------------------------------------------
        self.id_stack: list[tuple[str, str]] = []   # μ‑binders (Laog)
        self.di_stack: list[tuple[str, str]] = []   # λ / μ̃ binders (Goal)

    # binder helpers ----------------------------------------------------
    def _push_id(self, name: str, prop: str):
        self.id_stack.append((name, prop))
    def _pop_id(self):
        self.id_stack.pop()
    def _push_di(self, name: str, prop: str):
        self.di_stack.append((name, prop))
    def _pop_di(self):
        self.di_stack.pop()

    # traversal ---------------------------------------------------------
    def visit(self, node):
        if node is None:
            return None
        if isinstance(node, Goal):
            return self._maybe_graft(node, is_goal=True)
        if isinstance(node, Laog):
            return self._maybe_graft(node, is_goal=False)

        if isinstance(node, Mu):               # ID‑binder (captures Laog)
            self._push_id(node.id.name, node.prop)
            new = deepcopy(node)
            new.term    = self.visit(node.term)
            new.context = self.visit(node.context)
            self._pop_id()
            return new

        if isinstance(node, (Mutilde, Lamda)):
            varname = node.di.name if isinstance(node, Mutilde) else node.di.di.name
            varprop = node.prop  if isinstance(node, Mutilde) else node.di.prop
            self._push_di(varname, varprop)
            new = deepcopy(node)
            new.term = self.visit(node.term)
            if hasattr(node, 'context'):
                new.context = self.visit(node.context)
            self._pop_di()
            return new

        if isinstance(node, Cons):
            new = deepcopy(node)
            new.term    = self.visit(node.term)
            new.context = self.visit(node.context)
            return new

        return deepcopy(node)

    # ------------------------------------------------------------------
    def _make_maps(self):
        goal_map = {prop: name for name, prop in reversed(self.di_stack)}
        laog_map = {}
        for name, prop in reversed(self.id_stack):
            laog_map[prop] = name
        return goal_map, laog_map

    def _maybe_graft(self, node, *, is_goal: bool):
        # wrong kind? -> leave untouched
        if is_goal != self.target_is_goal:
            return deepcopy(node)
        # proposition must match
        if node.prop != self.target_prop:
            return deepcopy(node)
        # single‑site mode: number must line up
        if self.target_number is not None and node.number.strip() != self.target_number:
            return deepcopy(node)

        goal_map, laog_map = self._make_maps()
        subst   = _CapturingSubst(goal_map=goal_map, laog_map=laog_map)
        grafted = subst.visit(deepcopy(self.replacement_proto))
        return grafted

# ---------------------------------------------------------------------------
#  Utilities
# ---------------------------------------------------------------------------

def _find_target_info(ast: "ProofTerm", number: str) -> Tuple[Optional[str], Optional[bool]]:
    """Locate ?number / !number in *ast* – return (prop, is_goal)."""
    res = {'prop': None, 'kind': None}
    class _Find(ProofTermVisitor):
        def visit_Goal(self, n):
            if n.number.strip() == number:
                res['prop'], res['kind'] = n.prop, 'goal'
            return n
        def visit_Laog(self, n):
            if n.number.strip() == number:
                res['prop'], res['kind'] = n.prop, 'laog'
            return n
    _Find().visit(ast)
    if res['prop'] is None:
        return None, None
    return res['prop'], (res['kind'] == 'goal')


def _check_conclusion_matches(repl_ast: "ProofTerm", *, target_is_goal: bool, target_prop: str) -> None:
    """Ensure *repl_ast*’s root binder fits the Goal/Laog it replaces."""
    if isinstance(repl_ast, Mu):
        root_kind, root_prop = 'mu', repl_ast.prop
    elif isinstance(repl_ast, Mutilde):
        root_kind, root_prop = 'mutilde', repl_ast.prop
    else:
        raise TypeError("replacement AST must start with Mu or Mutilde binder")

    if target_is_goal:
        if root_kind != 'mu' or root_prop != target_prop:
            raise TypeError("Goal expects Mu‑binder with matching prop")
    else:
        if root_kind != 'mutilde' or root_prop != target_prop:
            raise TypeError("Laog expects Mutilde‑binder with matching prop")

# ---------------------------------------------------------------------------
#  Public API functions
# ---------------------------------------------------------------------------

def graft_single_OLD(body_B: "ProofTerm", goal_number: str, body_A: "ProofTerm") -> "ProofTerm":
    """Replace the unique Goal/Laog *goal_number* in *body_B* by *body_A*."""
    tgt_prop, is_goal = _find_target_info(body_B, goal_number)
    if tgt_prop is None:
        raise ValueError(f"no Goal/Laog numbered {goal_number} found")

    kind = "goal" if is_goal else "laog"
    logger.info("Starting graft: Replace %s %s:%s in %s by %s", kind, goal_number, tgt_prop, body_B.pres, body_A.pres)

    # type‑check replacement
    _check_conclusion_matches(body_A, target_is_goal=is_goal, target_prop=tgt_prop)

    # 2) freshness workaround: rename scion binders if they clash
    body_A = _alpha_rename_if_needed(body_A, body_B)

    visitor = _GraftVisitor(replacement_ast=body_A,
                            target_prop=tgt_prop,
                            target_number=goal_number,
                            target_is_goal=is_goal)
    return visitor.visit(body_B)


def graft_uniform_OLD(body_B: "ProofTerm", body_A: "ProofTerm") -> "ProofTerm":
    """Uniform graft: replace **all** Goals/Laogs with the conclusion of *A*."""
    logger.info("Uniformly grafting %r on %r", body_A, body_B)
    # determine conclusion of A from its *root* binder
    if isinstance(body_A, Mu):
        concl_prop      = body_A.prop
        target_is_goal  = True      # replaces Goals
    elif isinstance(body_A, Mutilde):
        concl_prop      = body_A.prop
        target_is_goal  = False     # replaces Laogs
        logger.debug("Target %s is LAOG", concl_prop)
    else:
        raise TypeError("argument A must start with Mu or Mutilde binder")

    body_A = _alpha_rename_if_needed(body_A, body_B)
    # No extra type‑check needed – we built criteria from A itself.
    visitor = _GraftVisitor(replacement_ast=body_A,
                            target_prop=concl_prop,
                            target_number=None,
                            target_is_goal=target_is_goal)
    return visitor.visit(body_B)

graft_single = _graft_single_core
graft_uniform = _graft_uniform_core


def _collect_binder_names(node: ProofTerm, names=None):
    """Return the **set** of all µ / µ̃ / λ *binder* names occurring in *node*."""
    if names is None:
        names = set()
    if isinstance(node, Mu):
        names.add(node.id.name)
    elif isinstance(node, Mutilde):
        names.add(node.di.name)
    elif isinstance(node, Lamda):
        names.add(node.di.di.name)  # di holds a Hyp → DI → name
    # recurse
    for child in (getattr(node, 'term', None), getattr(node, 'context', None)):
        if child is not None:
            _collect_binder_names(child, names)
    return names

def _fresh(prefix: str, taken: set[str]) -> str:
    """Generate a fresh variable starting with *prefix* not in *taken*."""
    if prefix not in taken:
        return prefix
    i = 2
    while f"{prefix}{i}" in taken:
        i += 1
    return f"{prefix}{i}"

def _alpha_rename_if_needed(scion: "ProofTerm", root: "ProofTerm") -> "ProofTerm":
    """Return *scion* with all **conflicting binders** α‑renamed fresh w.r.t. *root*."""
    root_names = _collect_binder_names(root)
    scion_names = _collect_binder_names(scion)
    mapping = {}
    for nm in scion_names:
        if nm in root_names:
            fresh_nm = _fresh(nm, root_names | scion_names)
            mapping[nm] = fresh_nm
    if mapping:
        scion = deepcopy(scion)
        _AlphaRename(mapping).visit(scion)
    return scion

class _AlphaRename(ProofTermVisitor):
    """Capture‑avoiding α‑renamer.  *mapping* is old‑name → new‑name."""
    def __init__(self, mapping: dict[str, str]):
        self.mapping = mapping

    # binders – rename & recurse -------------------------------------
    def visit_Mu(self, node: Mu):
        if node.id.name in self.mapping:
            node.id.name = self.mapping[node.id.name]
        return super().visit_Mu(node)

    def visit_Mutilde(self, node: Mutilde):
        if node.di.name in self.mapping:
            node.di.name = self.mapping[node.di.name]
        return super().visit_Mutilde(node)

    def visit_Lamda(self, node: Lamda):
        if node.di.di.name in self.mapping:
            node.di.di.name = self.mapping[node.di.di.name]
        return super().visit_Lamda(node)

    # leaves – replace occurrences -----------------------------------
    def visit_ID(self, node: ID):
        if node.name in self.mapping:
            node.name = self.mapping[node.name]
        return node

    def visit_DI(self, node: DI):
        if node.name in self.mapping:
            node.name = self.mapping[node.name]
        return node


# ────────────────────────────────────────────────────────────────────────────────
#  Acceptance coloring for normalized proof terms (green / red / yellow)
#    Grammar (sound and intended complete per spec)
#      Unattacked_Argument ::=
#         proof | refutation
#       | lamda . Goal
#       | Goal * Unattacked_Argument
#       | Unattacked_Argument * Laog
#       | lamda x . Unattacked_Argument
#       | Unattacked_Argument * Unattacked_Argument
#       | mu x.< Unattacked_Argument || Unattacked_Argument >
#       | mu' x.< Unattacked_Argument || Unattacked_Argument >
#      Green_Argument ::=
#         Unattacked_Argument
#       | mu x.< Goal || Red_Argument >
#       | mu' x.< Red_Argument || Laog >
#       | lamda x . Green_Argument
#       | Green_Argument * Green_Argument
#       | mu x.< Green_Argument || Green_Argument >
#       | mu' x.< Green_Argument || Green_Argument >
#      Red_Argument ::=
#         mu x.< Goal || Green_Argument >
#       | mu' x.< Green_Argument || Laog >
#       | lamda x . Red_Argument
#       | Red_Argument * Not_Red_Argument
#       | Not_Red_Argument * Red_Argument
#       | mu x.< Red_Argument || Not_Red_Argument >
#       | mu x.< Not_Red_Argument || Red_Argument >
#       | mu' x.< Red_Argument || Not_Red_Argument >
#       | mu' x.< Not_Red_Argument || Red_Argument >
#       | Red_Argument * Red_Argument
#      Yellow_Argument ::=
#         mu x.< Goal || Yellow_Argument >
#       | mu' x.< Yellow_Argument || Laog >
#       | lamda x . Yellow_Argument
#       | Yellow_Argument * Green_Argument
#       | Green_Argument * Yellow_Argument
#       | mu x.< Yellow_Argument || Green_Argument >
#       | mu x.< Green_Argument || Yellow_Argument >
#       | mu' x.< Yellow_Argument || Green_Argument >
#       | mu' x.< Green_Argument || Yellow_Argument >
#       | Yellow_Argument * Yellow_Argument
#  Notes:
#   - Assumes input is normalized (no remaining μ/μ̃/λ redexes).
#   - If no production applies, we return uncolored text (soundness over completeness).
# ────────────────────────────────────────────────────────────────────────────────
class AcceptanceColoringVisitor(ProofTermVisitor):
    ANSI = {
        "green": "\x1b[32m",
        "red": "\x1b[31m",
        "yellow": "\x1b[33m",
        "reset": "\x1b[0m",
    }
    def __init__(self, verbose: bool = False):
        super().__init__()
        self.verbose = verbose
        self._memo_unattacked: dict[int, bool] = {}
        self._memo_color: dict[int, Optional[str]] = {}

    # public API
    def render(self, node: ProofTerm) -> str:
        return self.visit(node)

    # utilities
    def _wrap(self, color: Optional[str], s: str) -> str:
        if not color:
            return s
        return f"{self.ANSI[color]}{s}{self.ANSI['reset']}"

    def _ansi(self, color: Optional[str]) -> tuple[str, str]:
        """Return (open, reset) escape codes for the given color; empty if None."""
        if not color:
            return "", ""
        return self.ANSI[color], self.ANSI["reset"]


    def _node_pres(self, n: ProofTerm) -> str:
        """Return a stable pretty string for the node (used in error messages)."""
        c = deepcopy(n)
        c = ProofTermGenerationVisitor().visit(c)
        return getattr(c, "pres", repr(c))


    def _has_open(self, n: ProofTerm) -> bool:
        if isinstance(n, (Goal, Laog)):
            return True
        for child in getattr(n, 'term', None), getattr(n, 'context', None):
            if child is not None and self._has_open(child):
                return True
        return False

    def _unattacked(self, n: ProofTerm) -> bool:
        mid = id(n)
        hit = self._memo_unattacked.get(mid)
        if hit is not None:
            return hit
        # proof | refutation: no open goals/laogs anywhere
        if not self._has_open(n):
            self._memo_unattacked[mid] = True
            return True
        # lamda . Goal
        if isinstance(n, Lamda) and isinstance(n.term, Goal):
            self._memo_unattacked[mid] = True
            return True
        # Goal * Unattacked
        if isinstance(n, Cons) and isinstance(n.term, Goal) and self._unattacked(n.context):
            self._memo_unattacked[mid] = True
            return True
        # Unattacked * Laog
        if isinstance(n, Cons) and self._unattacked(n.term) and isinstance(n.context, Laog):
            self._memo_unattacked[mid] = True
            return True
        # lamda x . Unattacked
        if isinstance(n, Lamda) and self._unattacked(n.term):
            self._memo_unattacked[mid] = True
            return True
        # Unattacked * Unattacked
        if isinstance(n, Cons) and self._unattacked(n.term) and self._unattacked(n.context):
            self._memo_unattacked[mid] = True
            return True
        # mu / mu' with unattacked children
        if isinstance(n, Mu) and self._unattacked(n.term) and self._unattacked(n.context):
            self._memo_unattacked[mid] = True
            return True
        if isinstance(n, Mutilde) and self._unattacked(n.term) and self._unattacked(n.context):
            self._memo_unattacked[mid] = True
            return True
        self._memo_unattacked[mid] = False
        return False

    def classify(self, n: ProofTerm) -> Optional[str]:
        mid = id(n)
        memo = self._memo_color.get(mid)
        if memo is not None:
            return memo
        # Green: unattacked
        if self._unattacked(n):
            self._memo_color[mid] = "green"
            return "green"
        # Lamda propagation
        if isinstance(n, Lamda):
            c = self.classify(n.term)
            self._memo_color[mid] = c
            return c
        # Cons combinations
        if isinstance(n, Cons):
            # If one side is a bare Goal/Laog, propagate the other's color
            if isinstance(n.term, Goal):
                c2 = self.classify(n.context)
                self._memo_color[mid] = c2
                return c2
            if isinstance(n.context, Laog):
                c1 = self.classify(n.term)
                self._memo_color[mid] = c1
                return c1
            c1, c2 = self.classify(n.term), self.classify(n.context)
            # Green * Green
            if c1 == "green" and c2 == "green":
                self._memo_color[mid] = "green"; return "green"
            # Red * Not_Red or Not_Red * Red
            if (c1 == "red" and c2 in {"green", "yellow"}) or (c2 == "red" and c1 in {"green", "yellow"}):
                self._memo_color[mid] = "red"; return "red"
            # Yellow * Green or Green * Yellow
            if (c1 == "yellow" and c2 == "green") or (c1 == "green" and c2 == "yellow"):
                self._memo_color[mid] = "yellow"; return "yellow"
            # Red * Red
            if c1 == "red" and c2 == "red":
                self._memo_color[mid] = "red"; return "red"
            # Yellow * Yellow
            if c1 == "yellow" and c2 == "yellow":
                self._memo_color[mid] = "yellow"; return "yellow"
            raise ValueError(
                f"Acceptance coloring incomplete for Cons node {self._node_pres(n)}: "
                f"left={c1}, right={c2}"
            )
        # Mu rules
        if isinstance(n, Mu):
            # Direct Goal on term: do not classify the Goal leaf
            if isinstance(n.term, Goal):
                c_c = self.classify(n.context)
                if c_c == "red":    self._memo_color[mid] = "green";  return "green"
                if c_c == "green":  self._memo_color[mid] = "red";    return "red"
                if c_c == "yellow": self._memo_color[mid] = "yellow"; return "yellow"
                raise ValueError(
                    f"Acceptance coloring incomplete for Mu node {self._node_pres(n)}: "
                    f"term is Goal; context_color={c_c}"
                )
            # Non‑direct cases: combine child colors
            c_t = self.classify(n.term)
            c_c = self.classify(n.context)
            if c_t == "green" and c_c == "green":
                self._memo_color[mid] = "green"; return "green"
            if (c_t == "red" and c_c in {"green","yellow"}) or (c_c == "red" and c_t in {"green","yellow"}):
                self._memo_color[mid] = "red"; return "red"
            if (c_t == "yellow" and c_c == "green") or (c_t == "green" and c_c == "yellow"):
                self._memo_color[mid] = "yellow"; return "yellow"
            raise ValueError(
                f"Acceptance coloring incomplete for Mu node {self._node_pres(n)}: "
                f"term_color={c_t}, context_color={c_c}"
            )
        # Mutilde rules
        if isinstance(n, Mutilde):
            # Direct Laog on context: do not classify the Laog leaf
            if isinstance(n.context, Laog):
                c_t = self.classify(n.term)
                if c_t == "red":    self._memo_color[mid] = "green";  return "green"
                if c_t == "green":  self._memo_color[mid] = "red";    return "red"
                if c_t == "yellow": self._memo_color[mid] = "yellow"; return "yellow"
                raise ValueError(
                    f"Acceptance coloring incomplete for Mutilde node {self._node_pres(n)}: "
                    f"context is Laog; term_color={c_t}"
                )
            # Non‑direct cases: combine child colors
            c_t = self.classify(n.term)
            c_c = self.classify(n.context)
            if c_t == "green" and c_c == "green":
                self._memo_color[mid] = "green"; return "green"
            if (c_t == "red" and c_c in {"green","yellow"}) or (c_c == "red" and c_t in {"green","yellow"}):
                self._memo_color[mid] = "red"; return "red"
            if (c_t == "yellow" and c_c == "green") or (c_t == "green" and c_c == "yellow"):
                self._memo_color[mid] = "yellow"; return "yellow"
            raise ValueError(
                f"Acceptance coloring incomplete for Mutilde node {self._node_pres(n)}: "
                f"term_color={c_t}, context_color={c_c}"
            )
        # Leaves: no rule unless covered by Unattacked (handled above)
        raise ValueError(
            f"Acceptance coloring incomplete for leaf {type(n).__name__} {self._node_pres(n)}"
        )

    # pretty rendering with color
    def visit_Mu(self, node: Mu):
        color = self.classify(node)
        openP, reset = self._ansi(color)
        t_str = self.visit(node.term)
        c_str = self.visit(node.context)
        # Color the binder and delimiters with the parent color.
        # Additionally, if the direct child is a bare Goal/Laog, color that child with the parent color.
        if isinstance(node.term, Goal):
            t_str = f"{openP}{t_str}{reset}"
        if isinstance(node.context, Laog):
            c_str = f"{openP}{c_str}{reset}"
        s = (
            f"{openP}μ{node.id.name}:{node.prop}.<"
            f"{reset}{t_str}"
            f"{openP}||"
            f"{reset}{c_str}"
            f"{openP}>"
            f"{self.ANSI['reset']}"
        )
        return s
    def visit_Mutilde(self, node: Mutilde):
        color = self.classify(node)
        openP, reset = self._ansi(color)
        t_str = self.visit(node.term)
        c_str = self.visit(node.context)
        if isinstance(node.term, Goal):
            t_str = f"{openP}{t_str}{reset}"
        if isinstance(node.context, Laog):
            c_str = f"{openP}{c_str}{reset}"
        s = (
            f"{openP}μ'{node.di.name}:{node.prop}.<"
            f"{reset}{t_str}"
            f"{openP}||"
            f"{reset}{c_str}"
            f"{openP}>"
            f"{self.ANSI['reset']}"
        )
        return s
    def visit_Lamda(self, node: Lamda):
        color = self.classify(node)
        openP, reset = self._ansi(color)
        inner = self.visit(node.term)
        if isinstance(node.term, Goal):
            inner = f"{openP}{inner}{reset}"
        s = (
            f"{openP}λ{node.di.di.name}:{node.di.prop}."
            f"{reset}{inner}"
            f"{openP}{self.ANSI['reset']}"
        )
        return s
    def visit_Cons(self, node: Cons):
        color = self.classify(node)
        openP, reset = self._ansi(color)
        left  = self.visit(node.term)
        right = self.visit(node.context)
        if isinstance(node.term, Goal):
            left = f"{openP}{left}{reset}"
        if isinstance(node.context, Laog):
            right = f"{openP}{right}{reset}"
        # Color the '*' with the Cons node’s color.
        s = f"{left}{openP}*{self.ANSI['reset']}{right}"
        return s
    def visit_Goal(self, node: Goal):
        # Leaves are not colored directly; parents determine color.
        return f"{node.number}:{node.prop}"
    def visit_Laog(self, node: Laog):
        return f"{node.number}:{node.prop}"
    def visit_ID(self, node: ID):
        s = f"{node.name}:{node.prop}" if node.prop else f"{node.name}"
        return self._wrap(self.classify(node), s)
    def visit_DI(self, node: DI):
        s = f"{node.name}:{node.prop}" if node.prop else f"{node.name}"
        return self._wrap(self.classify(node), s)
    def visit_unhandled(self, node):
        return self._wrap(self.classify(node), repr(node))

def pretty_colored_proof_term(pt: ProofTerm, verbose: bool = False) -> str:
    """Return an ANSI-colored pretty string for a normalized proof term."""
    return AcceptanceColoringVisitor(verbose=verbose).render(pt)


class AcceptanceTreeRenderer:
    """
    Build a colored binary tree of a normalized proof term:
      - Mu/Mutilde are internal nodes (parent = binder; children = term/context).
      - Non Mu/Mutilde nodes are leaves.
      - Node color is computed via AcceptanceColoringVisitor.classify; for Goal/Laog
        leaves that fail direct classification, inherit the parent color.
      - Returns Graphviz DOT source; wrapper can render to SVG/PNG or save .dot.
    """
    _gv_colors = {"green": "palegreen2", "red": "lightcoral", "yellow": "khaki1"}

    def __init__(self, verbose: bool = False, *, label_mode: str = "proof", nl_style: str = "argumentation"):
        self.color_v = AcceptanceColoringVisitor(verbose=verbose)
        self._idmap: dict[int, str] = {}
        self._seq = 0
        self.label_mode = label_mode  # "proof" or "nl"
        # pick the NL semantics
        if nl_style == "dialectical":
            self._nl_sem = natural_language_dialectical_rendering
        elif nl_style == "intuitionistic":
            self._nl_sem = natural_language_rendering
        else:
            self._nl_sem = natural_language_argumentative_rendering

    def _nid(self, n) -> str:
        k = id(n)
        v = self._idmap.get(k)
        if v is None:
            self._seq += 1
            v = f"n{self._seq}"
            self._idmap[k] = v
        return v

    def _label_leaf(self, n) -> str:
        c = deepcopy(n)
        c = ProofTermGenerationVisitor().visit(c)
        return getattr(c, "pres", repr(c))

    def _class_or_inherit(self, n, parent_color: Optional[str]) -> Optional[str]:
        try:
            return self.color_v.classify(n)
        except Exception:
            return parent_color

    def _label_and_child(self, n) -> tuple[str, Optional[ProofTerm]]:
        """
        Return (label_for_A, child_subtree) where:
          - label_for_A prints the whole term n, but for the first μ/μ' with
            a direct Goal/Laog (left-to-right), replaces the non-Goal/Laog side
            with '…' and keeps the Goal/Laog printed.
          - child_subtree is the non-Goal/Laog Argument side of that μ/μ' node.
          - If no such μ/μ' exists anywhere in n, child_subtree is None and
            label_for_A is the standard pretty proof term for n.
        """
        found = {"child": None}  # use dict to allow closure assignment

        def pp(node) -> str:
            # μ cases
            if isinstance(node, Mu):
                if found["child"] is None and isinstance(node.term, Goal):
                    goal = ProofTermGenerationVisitor().visit(deepcopy(node.term)).pres
                    found["child"] = node.context
                    return f"μ{node.id.name}:{node.prop}.<{goal}||…>"
                if found["child"] is None and isinstance(node.context, Laog):
                    laog = ProofTermGenerationVisitor().visit(deepcopy(node.context)).pres
                    found["child"] = node.term
                    return f"μ{node.id.name}:{node.prop}.<…||{laog}>"
                return f"μ{node.id.name}:{node.prop}.<{pp(node.term)}||{pp(node.context)}>"
            # μ' cases
            if isinstance(node, Mutilde):
                if found["child"] is None and isinstance(node.term, Goal):
                    goal = ProofTermGenerationVisitor().visit(deepcopy(node.term)).pres
                    found["child"] = node.context
                    return f"μ'{node.di.name}:{node.prop}.<{goal}||…>"
                if found["child"] is None and isinstance(node.context, Laog):
                    laog = ProofTermGenerationVisitor().visit(deepcopy(node.context)).pres
                    found["child"] = node.term
                    return f"μ'{node.di.name}:{node.prop}.<…||{laog}>"
                return f"μ'{node.di.name}:{node.prop}.<{pp(node.term)}||{pp(node.context)}>"
            # other nodes: reuse the same textual rendering as ProofTermGenerationVisitor
            if isinstance(node, Lamda):
                return f"λ{node.di.di.name}:{node.di.prop}.{pp(node.term)}"
            if isinstance(node, Cons):
                return f"{pp(node.term)}*{pp(node.context)}"
            if isinstance(node, Goal):
                g = deepcopy(node); g = ProofTermGenerationVisitor().visit(g); return g.pres
            if isinstance(node, Laog):
                l = deepcopy(node); l = ProofTermGenerationVisitor().visit(l); return l.pres
            if isinstance(node, ID):
                return f"{node.name}:{node.prop}" if node.prop else f"{node.name}"
            if isinstance(node, DI):
                return f"{node.name}:{node.prop}" if node.prop else f"{node.name}"
            # fallback
            return self._label_leaf(node)

        lbl = pp(n)
        return lbl, found["child"]

    def _nl_label_with_elision(self, n: ProofTerm, child: Optional[ProofTerm]) -> str:
        lines: list[str] = []
        sem = self._nl_sem
        def rec(node: ProofTerm, indent: int) -> None:
            if child is not None and node is child:
                lines.append(f"{sem.indentation * indent}...")
                return
            if isinstance(node, Mu):
                lines.append(f"{sem.indentation * indent}{sem.Mu[0]}{node.prop}")
                rec(node.term, indent + 1)
                rec(node.context, indent + 1)
                return
            if isinstance(node, Mutilde):
                lines.append(f"{sem.indentation * indent}{sem.Mutilde[0]}{node.prop}")
                rec(node.term, indent)
                lines.append(f"{sem.indentation * indent}{sem.Mutilde[1]}")
                rec(node.context, indent)
                return
            if isinstance(node, Lamda):
                lines.append(f"{sem.indentation * indent}{sem.Lamda}{node.di.prop}({node.di.di.name})")
                rec(node.term, indent)
                return
            if isinstance(node, Cons):
                lines.append(f"{sem.indentation * indent}{sem.Cons}")
                rec(node.term, indent)
                rec(node.context, indent)
                return
            if isinstance(node, Goal):
                lines.append(f"{sem.indentation * indent}{sem.Goal}{node.number}")
                return
            if isinstance(node, DI):
                lines.append(f"{sem.indentation * indent}{sem.DI}{node.name}")
                return
            if isinstance(node, ID):
                lines.append(f"{sem.indentation * indent}{sem.ID}")
                return
            c = deepcopy(node)
            c = ProofTermGenerationVisitor().visit(c)
            lines.append(f"{sem.indentation * indent}{getattr(c, 'pres', repr(c))}")
        rec(n, 0)
        return "\n".join(lines)

    def _emit(self, n, lines: list[str], parent: Optional[str], parent_color: Optional[str]) -> None:
        """
        Node = whole outer term A. If A contains (left-to-right) a μ/μ' with
        direct Goal/Laog, label A with '…' on the Argument side and add exactly
        one child = the non-Goal/Laog Argument subtree. Otherwise, A is a leaf.
        """
        # Determine this node’s label and (optional) single child
        if self.label_mode == "proof":
            label, child = self._label_and_child(n)
        else:
            # reuse child selection from proof-term variant then build NL label
            _lbl_proof, child = self._label_and_child(n)
            label = self._nl_label_with_elision(n, child)
        # Color node by acceptance color of the whole outer term A (inherit on error)
        col  = self._class_or_inherit(n, parent_color)
        fill = self._gv_colors.get(col)
        my = self._nid(n)
        attr = f'shape=box,style=filled,fillcolor="{fill}"' if fill else 'shape=box'
        # Escape label for DOT (handle newlines and quotes/backslashes)
        esc_label = label.replace("\\", "\\\\").replace("\"", "\\\"")
        lines.append(f'  {my} [label="{esc_label}", {attr}];')
        if parent:
            lines.append(f'  {my} -> {parent};')
        # If we found an Argument side, it becomes the single child; else we’re a leaf
        if child is not None:
            self._emit(child, lines, my, col)

    def to_dot(self, root: ProofTerm) -> str:
        lines: list[str] = []
        lines.append('digraph ProofTree {')
        lines.append('  graph [rankdir=BT, splines=polyline];')
        lines.append('  node  [fontname="Helvetica"];')
        lines.append('  edge  [fontname="Helvetica", color="orangered", penwidth=2, arrowhead="vee"];')
        self._emit(root, lines, parent=None, parent_color=None)
        lines.append('}')
        return "\n".join(lines)


def render_acceptance_tree_dot(pt: ProofTerm, verbose: bool = False, *, label_mode: str = "proof", nl_style: str = "argumentation") -> str:
    """
    Return Graphviz DOT source for the colored binary tree of a normalized proof term.
    """
    return AcceptanceTreeRenderer(verbose=verbose, label_mode=label_mode, nl_style=nl_style).to_dot(pt)
