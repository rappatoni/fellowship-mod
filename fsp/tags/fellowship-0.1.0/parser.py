from lark import Lark, Transformer
import warnings
import collections
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
from pres.nl import Rendering_Semantics, traverse_proof_term, pretty_natural
from core.dc.match_utils import match_trees, get_child_nodes, is_subargument
logger = logging.getLogger('fsp.parser')
if os.getenv("FSP_DEPRECATE_PARSER", "").lower() in {"1", "true", "yes"}:
    warnings.warn(
        "parser.py is a compatibility shim; import from core.* and pres.* instead.",
        DeprecationWarning,
        stacklevel=2,
    )







#Argument Reducer Helpers




    

from core.comp.reduce import ArgumentTermReducer as _ArgumentTermReducer_OLD
ArgumentTermReducer = _ArgumentTermReducer_OLD

    

natural_language_rendering = Rendering_Semantics('   ', "we need to prove ", f"we proved ", f"assume ", f"and", f"? ", f"done ", f"by ")
natural_language_dialectical_rendering = Rendering_Semantics('   ', "Assume a refutation of ", f"Assume a proof of  ", f"assume ", f"and", f"? ", f"but then we have a contradiction, done ", f"by ")
natural_language_argumentative_rendering = Rendering_Semantics('   ', ["We will argue for ", "undercutting ", "supported by alternative ", "undercut by "], [f"We will argue against ", "using ", "by adapter"], f"assume ", f"and", f"by default ", f"done ", f"by ")


# nat = pretty_natural(proof_term_ast, natural_language_argumentative_rendering )
# dia = pretty_natural(proof_term_ast, natural_language_dialectical_rendering )
# arg = pretty_natural(easy_undercut_ast, natural_language_argumentative_rendering)
# easy = pretty_natural(easy_ast, natural_language_argumentative_rendering)
# print(nat)
# print(dia)
# print(arg)
# print(easy)












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

__all__ = [
    "ProofTerm", "Term", "Context", "Mu", "Mutilde", "Lamda", "Cons",
    "Goal", "Laog", "ID", "DI", "Hyp",
    "ProofTermTransformer",
    "ProofTermVisitor",
    "graft_single", "graft_uniform",
    "NegIntroRewriter",
    "InstructionsGenerationVisitor",
    "PropEnrichmentVisitor",
    "ProofTermGenerationVisitor",
    "AcceptanceColoringVisitor", "pretty_colored_proof_term",
    "AcceptanceTreeRenderer", "render_acceptance_tree_dot",
    "ArgumentTermReducer",
    "Rendering_Semantics", "traverse_proof_term", "pretty_natural",
    "match_trees", "get_child_nodes", "is_subargument",
]









