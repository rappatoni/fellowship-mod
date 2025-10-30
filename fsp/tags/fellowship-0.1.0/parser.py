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
from pres.gen import ProofTermGenerationVisitor
from pres.color import AcceptanceColoringVisitor, pretty_colored_proof_term
from pres.tree import AcceptanceTreeRenderer, render_acceptance_tree_dot
logger = logging.getLogger('fsp.parser')




def pretty_natural(proof_term: "ProofTerm", semantic: "Rendering_Semantics") -> str:
    lines = []
    traverse_proof_term(semantic, proof_term, lines, indent=0)
    return '\n'.join(lines)



#Argument Reducer Helpers




    

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

# ---------------------------------------------------------------------------
#  Helper – traversal that performs the grafting itself
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
#  Utilities
# ---------------------------------------------------------------------------




# ---------------------------------------------------------------------------
#  Public API functions
# ---------------------------------------------------------------------------


graft_single = _graft_single_core
graft_uniform = _graft_uniform_core




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





