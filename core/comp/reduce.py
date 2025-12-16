import os
import logging
from copy import deepcopy
from typing import Optional, Any
from core.comp.visitor import ProofTermVisitor
from core.ac.ast import ProofTerm, Term, Context, Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI, Admal, Sonc
from pres.gen import ProofTermGenerationVisitor
from core.comp.enrich import PropEnrichmentVisitor
from core.comp.color import AcceptanceColoringVisitor

logger = logging.getLogger(__name__)


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

def _is_red(command: ProofTerm) -> bool:
    """Return True iff the subtree starting at this binder is colored red."""
    try:
        return AcceptanceColoringVisitor(verbose=False).classify(command) == "red"
    except Exception:
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

    
class ArgumentTermReducer(ProofTermVisitor):
    """
    Implements the following reduction rules (each left and right):
       - lambda
       - undercut (defeat and defence)
       - TODO: rebut
       - TODO: support
       - mu
       - TODO: eta
    """

    def __init__(self, *, verbose: bool = True, assumptions=None, axiom_props=None,
                 evaluation_strategy: str = "call-by-name",
                 simplify_alternative_arguments: bool = True,
                 evaluation_discipline: str = "onus-parallel",          # legacy | onus-parallel | onus (future)
                 onus_fallback: str = "none",                    # none | cbn | cbv
                 onus_stance: str = "skeptical"):                # skeptical | credulous
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
        self.evaluation_strategy = evaluation_strategy
        self.simplify_alternative_arguments = simplify_alternative_arguments
        self.evaluation_discipline = evaluation_discipline
        self.onus_fallback = onus_fallback
        self.onus_stance = onus_stance
        
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

    # ─── Call‑by‑Onus: classification and application of a single step ──────
    def _is_tprime(self, n: Optional[ProofTerm]) -> bool:
        if n is None:
            return False
        try:
            return not self._has_next_redex(n)
        except Exception:
            return False

    def _is_exception_node(self, n: ProofTerm) -> bool:
        # e ::= μ_.<t' || t> | μ'_.<t || t'>; binder affine; t' is not itself an exception or a Goal/Laog
        if isinstance(n, Mu) and _is_affine(n.id.name, n):
            tprime = n.term
            return self._is_tprime(tprime) and not isinstance(tprime, (Mu, Mutilde, Goal, Laog))
        if isinstance(n, Mutilde) and _is_affine(n.di.name, n):
            tprime = n.context
            return self._is_tprime(tprime) and not isinstance(tprime, (Mu, Mutilde, Goal, Laog))
        return False

    def _is_ap_node(self, n: ProofTerm) -> tuple[bool, bool, bool]:
        # ap ::= μ_.<t'||α> | μ'_.<t'||α>; returns (is_ap, is_default, is_defeated)
        if isinstance(n, Mu) and _is_affine(n.id.name, n) and isinstance(n.context, ID):
            tprime = n.term
            is_def = isinstance(tprime, Goal)
            is_defeated = self._is_exception_node(tprime)
            return (self._is_tprime(tprime) and not is_defeated, is_def, is_defeated)
        if isinstance(n, Mutilde) and _is_affine(n.di.name, n) and isinstance(n.term, DI):
            tprime = n.context
            is_def = isinstance(tprime, Laog)
            is_defeated = self._is_exception_node(tprime)
            return (self._is_tprime(tprime) and not is_defeated, is_def, is_defeated)
        return (False, False, False)

    def _is_ar_node(self, n: ProofTerm) -> tuple[bool, bool, bool]:
        # ar ::= μ'_.<x||t'> | μ_.<x||t'>; returns (is_ar, is_default, is_defeated)
        if isinstance(n, Mu) and _is_affine(n.id.name, n) and isinstance(n.term, ID):
            tprime = n.context
            is_def = isinstance(tprime, Laog)
            is_defeated = self._is_exception_node(tprime)
            return (self._is_tprime(tprime) and not is_defeated, is_def, is_defeated)
        if isinstance(n, Mutilde) and _is_affine(n.di.name, n) and isinstance(n.term, DI):
            tprime = n.context
            is_def = isinstance(tprime, Laog)
            is_defeated = self._is_exception_node(tprime)
            return (self._is_tprime(tprime) and not is_defeated, is_def, is_defeated)
        return (False, False, False)

    def _decide_onus(self, node: ProofTerm) -> tuple[str, str]:
        """
        Return (action, reason) with action ∈ {left-shift, right-shift, cbn, cbv, fallback}
        """
        if not isinstance(node, (Mu, Mutilde)):
            return ("fallback", "not a binder")
        left, right = node.term, node.context
        # Shifts
        if isinstance(right, Mutilde) and self._has_next_redex(getattr(right, "context", None)):
            return ("right-shift", "right-shift: ⟨ t || μ'_. c ⟩ reducible")
        if isinstance(left, Mu) and self._has_next_redex(getattr(left, "term", None)):
            return ("left-shift", "left-shift: ⟨ μ_. c || t ⟩ reducible")
        # Classifications
        L_ap, L_d, L_dap = self._is_ap_node(left)
        R_ap, R_d, R_dap = self._is_ap_node(right)
        L_ar, _,  L_dar = self._is_ar_node(left)
        R_ar, _,  R_dar = self._is_ar_node(right)
        L_e = self._is_exception_node(left)  if isinstance(left, (Mu, Mutilde)) else False
        R_e = self._is_exception_node(right) if isinstance(right, (Mu, Mutilde)) else False
        L_is_m = isinstance(left, (Mu, Mutilde))
        R_is_m = isinstance(right, (Mu, Mutilde))
        L_is_nm = not L_is_m
        R_is_nm = not R_is_m
        L_is_lam_cons = isinstance(left, Lamda) and isinstance(right, Cons)
        L_is_sonc_adm = isinstance(left, Sonc) and isinstance(right, Admal)
        credulous = (self.onus_stance.lower() == "credulous")
        # CBV
        if L_is_sonc_adm:                 return ("cbv", "⟨ sonc || admal ⟩")
        if L_is_nm and R_is_m:            return ("cbv", "⟨ !m || m ⟩")
        if L_ap and (R_e and not credulous): return ("cbv", "⟨ ap || e ⟩")
        if L_ar and R_e:                  return ("cbv", "⟨ ar || e ⟩")
        if L_d and (R_ap or R_ar or True): return ("cbv", "⟨ d || ap/ar/o ⟩")
        if R_e and not (L_ap or L_ar):    return ("cbv", "⟨ o || e ⟩")
        if L_ap and R_dap:                return ("cbv", "⟨ ap || dap ⟩")
        if L_ar and R_dar:                return ("cbv", "⟨ ar || dar ⟩")
        # CBN
        if L_e and R_ap:                  return ("cbn", "⟨ e || ap ⟩")
        if credulous and L_e and R_ar:    return ("cbv", "credulous: ⟨ e || ar ⟩→cbv")
        if (not credulous) and L_e and R_ar: return ("cbn", "⟨ e || ar ⟩")
        if L_is_m and R_is_nm:            return ("cbn", "⟨ m || !m ⟩")
        if L_is_lam_cons:                 return ("cbn", "⟨ lamda || cons ⟩")
        if L_ap and R_d:                  return ("cbn", "⟨ ap || d ⟩")
        if (not (L_ap or L_ar)) and R_d:  return ("cbn", "⟨ o || d ⟩")
        if L_ar and R_d:                  return ("cbn", "⟨ ar || d ⟩")
        if L_ap and R_dap:                return ("cbn", "⟨ ap || dap ⟩")
        # Fallback
        return ("fallback", "no onus situation matched")

    def _onus_apply_once(self, n: ProofTerm, onus_kind: str) -> ProofTerm:
        """Apply a single local step according to onus on a copy of node n."""
        node = deepcopy(n)
        if onus_kind == "left-shift" or onus_kind == "right-shift":
            return node  # shifts only decide where to reduce; they do not rewrite here
        # CBN: prefer the λ‑rule; otherwise μ‑β
        if onus_kind == "cbn":
            if isinstance(node, (Mu, Mutilde)) and isinstance(node.term, Lamda) and isinstance(node.context, Cons) and not isinstance(node.context.term, Goal):
                lam: Lamda = node.term
                cons: Cons = node.context
                v = cons.term
                E = cons.context
                new_context = Mutilde(
                    di_=lam.di.di,
                    prop=lam.di.prop,
                    term=deepcopy(lam.term),
                    context=deepcopy(E)
                )
                node.term = deepcopy(v)
                node.context = new_context
                return node
            # μ‑β if applicable in both cases
            if isinstance(node, Mu) and isinstance(node.term, Mu) and not isinstance(node.context, Laog):
                inner = node.term
                x = inner.id.name
                E = node.context
                node.term    = _subst(inner.term,    x, deepcopy(E))
                node.context = _subst(inner.context, x, deepcopy(E))
                return node
            if isinstance(node, Mutilde) and isinstance(node.term, Mu) and not isinstance(node.context, Laog):
                inner = node.term
                x = inner.id.name
                E = node.context
                node.term    = _subst(inner.term,    x, deepcopy(E))
                node.context = _subst(inner.context, x, deepcopy(E))
                return node
            return node
        # CBV: μ̃‑β if applicable
        if onus_kind == "cbv":
            # Admal step: ⟨ E*v || admal α.β ⟩ → ⟨ μ α.< v || β > || E ⟩
            if isinstance(node, (Mu, Mutilde)) and isinstance(node.term, Sonc) and isinstance(node.context, Admal):
                sonc: Sonc = node.term
                adm:  Admal = node.context
                E = deepcopy(sonc.context)
                v = deepcopy(sonc.term)
                alpha_obj = getattr(adm, "id", None)
                alpha_id  = getattr(alpha_obj, "id", alpha_obj)
                alpha_prop = getattr(alpha_obj, "prop", getattr(alpha_id, "prop", None))
                beta_ctx = deepcopy(getattr(adm, "context", None))
                if alpha_id is not None and alpha_prop is not None and beta_ctx is not None:
                    node.term = Mu(alpha_id, alpha_prop, v, beta_ctx)
                    node.context = E
                return node
            if isinstance(node, Mu) and isinstance(node.context, Mutilde) and not isinstance(node.term, Goal):
                inner = node.context
                alpha = inner.di.name
                v = node.term
                node.term    = _subst(inner.term,    alpha, deepcopy(v))
                node.context = _subst(inner.context, alpha, deepcopy(v))
                return node
            if isinstance(node, Mutilde) and isinstance(node.context, Mutilde) and not isinstance(node.term, Goal):
                inner = node.context
                alpha = inner.di.name
                v = node.term
                node.term    = _subst(inner.term,    alpha, deepcopy(v))
                node.context = _subst(inner.context, alpha, deepcopy(v))
                return node
            return node
        # Fallback: honor onus_fallback
        fb = self.onus_fallback.lower()
        if fb == "cbn":
            return self._onus_apply_once(node, "cbn")
        if fb == "cbv":
            return self._onus_apply_once(node, "cbv")
        return node

    def _maybe_warn_onus_divergence(self, onus_info, before_node: ProofTerm, after_node: ProofTerm, rule_tag: str, where_kind: str):
        """Compare onus candidate vs legacy result and warn if they diverge."""
        logger.debug("maybe warn onus called")
        if self.evaluation_discipline not in ("onus-parallel", "onus") or onus_info is None:
            logger.debug("Not in onus-parallel mode")
            return
        kind, reason, cand = onus_info
        before_pres = self._pres_str(before_node)
        after_pres  = self._pres_str(after_node)
        cand_pres   = self._pres_str(cand)
        if after_pres != before_pres and after_pres != cand_pres:
            logger.warning("call-by-onus divergence at %s (%s) on term: \n %s \n onus=%s (%s)\n  onus-candidate: %s\n  legacy-result: \n %s",
                           where_kind, rule_tag, before_pres, kind, reason, cand_pres, after_pres)
        else:
            logger.debug("No onus divergence")

    def visit_Mu(self, node: Mu):
        # First, normalise the sub‑components so that the rule also fires in
        # inner positions.
        
        # Onus parallel: prepare single-step candidate at this node
        orig_before = deepcopy(node)
        onus_info = None
        if self.evaluation_discipline in ("onus-parallel", "onus"):
            okind, oreason = self._decide_onus(node)
            ocand = self._onus_apply_once(orig_before, okind)
            onus_info = (okind, oreason, ocand)

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
                dbg_before = self._pres_str(node)
                node.term    = deepcopy(v)
                node.context = new_context
                dbg_after = self._pres_str(node)
                self._maybe_warn_onus_divergence(onus_info, orig_before, node, "lambda", "Mu")
                logger.debug("reduce.Mu lambda: before=\n%s", dbg_before)
                logger.debug("reduce.Mu lambda: after=\n%s", dbg_after)
                self._snapshot("lambda")
                self.visit_Mu(node)
                
                return node

        # admal rule: ⟨ E*v || admal α.β ⟩ → ⟨ μ α.< v || β > || E ⟩
        if isinstance(node.term, Sonc) and isinstance(node.context, Admal):
            sonc: Sonc = node.term
            adm:  Admal = node.context
            E = deepcopy(sonc.context)
            v = deepcopy(sonc.term)
            # Extract binder (α) and body (β) from Admal
            alpha_obj = getattr(adm, "id", None)  # may be ID or a Hyp with .id/.prop
            alpha_id  = getattr(alpha_obj, "id", alpha_obj)
            alpha_prop = getattr(alpha_obj, "prop", getattr(alpha_id, "prop", None))
            beta_ctx = deepcopy(getattr(adm, "context", None))
            if alpha_id is not None and alpha_prop is not None and beta_ctx is not None:
                dbg_before = self._pres_str(node)
                inner = Mu(alpha_id, alpha_prop, v, beta_ctx)
                node.term = inner
                node.context = E
                dbg_after = self._pres_str(node)
                self._maybe_warn_onus_divergence(onus_info, orig_before, node, "admal", "Mu")
                logger.debug("reduce.Mu admal: before=\n%s", dbg_before)
                logger.debug("reduce.Mu admal: after=\n%s",  dbg_after)
                self._snapshot("admal")
                return self.visit_Mu(node)

        # basic support (Goal side):
        #   μ alt . < μ aff . < Goal || ID(alt) >  ∥  μ′ aff . < Supporter || ID(alt) > >
        # Cases (when Supporter is fully simplified):
        #   A) Supporter non-affine w.r.t. aff  → keep Supporter:
        #        μ alt . < Supporter || ID(alt) >
        #   B) Supporter affine w.r.t. aff      → discard Supporter (defeated):
        #        μ alt . < Goal || ID(alt) >
        if isinstance(node.term, Mu) and isinstance(node.context, Mutilde):
            inner_mu: Mu    = node.term
            ctx_mt:  Mutilde = node.context
            # shape guards
            shape_ok = (
                isinstance(inner_mu.context, ID) and inner_mu.context.name == node.id.name and
                isinstance(ctx_mt.context, ID)   and ctx_mt.context.name   == node.id.name
            )
            logger.debug("Basic Support Shape ok? %s", shape_ok)
            if shape_ok:
                # only apply when supporter fully simplified
                if not self._has_next_redex(ctx_mt.term):
                    left      = inner_mu.term         # left-hand argument (some_arg)
                    supporter = ctx_mt.term
                    left_is_default = isinstance(left, Goal)

                    # support-keep: supporter undefeated AND (left is default OR left defeated)
                    if not _is_red(supporter) and (left_is_default or _is_red(left)):
                        dbg_before = self._pres_str(node)
                        node.term    = deepcopy(supporter)           # keep Supporter
                        node.context = deepcopy(ctx_mt.context)      # ID(alt)
                        self._maybe_warn_onus_divergence(onus_info, orig_before, node, "support-keep", "Mu")
                        dbg_after = self._pres_str(node)
                        logger.debug("reduce.Mu support-keep: before=\n%s", dbg_before)
                        logger.debug("reduce.Mu support-keep: after=\n%s",  dbg_after)
                        self._snapshot("support-keep")
                        return self.visit_Mu(node)

                    # support-discard: supporter defeated AND (left is default OR left undefeated)
                    if _is_red(supporter) and (left_is_default or not _is_red(left)):
                        dbg_before = self._pres_str(node)
                        node.term    = deepcopy(left)                 # keep left (Goal or undefeated arg)
                        node.context = deepcopy(inner_mu.context)     # ID(alt)
                        self._maybe_warn_onus_divergence(onus_info, orig_before, node, "support-discard", "Mu")
                        dbg_after = self._pres_str(node)
                        logger.debug("reduce.Mu support-discard: before=\n%s", dbg_before)
                        logger.debug("reduce.Mu support-discard: after=\n%s",  dbg_after)
                        self._snapshot("support-discard")
                        return self.visit_Mu(node)

                    # ambiguous: do not reduce (both undefeated or both defeated with non-default left)
                    if self.verbose:
                        logger.debug(
                            "reduce.Mu support guarded: no action (left=%sdefault, left_red=%s, supporter_red=%s)",
                            "" if left_is_default else "non-",
                            _is_red(left), _is_red(supporter)
                        )
                else:
                    # simplify supporter term and re-check
                    before = self._pres_str(ctx_mt.term)
                    if isinstance(ctx_mt.term, Mu):
                        ctx_mt.term = self.visit_Mu(ctx_mt.term)
                    else:
                        ctx_mt.term = self.visit(ctx_mt.term)
                    after = self._pres_str(ctx_mt.term)
                    if before != after:
                        return self.visit_Mu(node)
                    # if no progress, fall through to other rules
            else:
                if self.verbose:
                    logger.debug(
                        "support-mu shape failed: outer_alt=%s; inner_mu.context=%s(%s); ctx_mt.context=%s(%s)",
                        getattr(node.id, 'name', '?'),
                        type(inner_mu.context).__name__,
                        getattr(inner_mu.context, 'name', None),
                        type(ctx_mt.context).__name__,
                        getattr(ctx_mt.context, 'name', None),
                    )

        # affine helper --------------------------------------------------
        def _guards(inner_mu: Mu, ctx_mt: Mutilde):
            # both binders affine in their own commands
            if not (_is_affine(inner_mu.id.name, inner_mu) and _is_affine(ctx_mt.di.name, ctx_mt)):
                if self.verbose: logger.debug("Guard A failed")
                return False
            # ensure ctx_mt.context has the expected shape before dereferencing
            if not isinstance(ctx_mt.context, Mutilde):
                if self.verbose: logger.debug("Guard B failed (ctx.context is not Mutilde)")
                return False
            # both protect goals with equal prop (ignore number)
            left_goal_ok  = isinstance(inner_mu.term, Goal)
            logger.debug("Context is goal? '%s'", ctx_mt.term)
            right_goal_ok = isinstance(ctx_mt.term, Goal)
            if not (left_goal_ok and right_goal_ok):
                if self.verbose: logger.debug("Guard C failed (goal-protection) left=%s right=%s", left_goal_ok, right_goal_ok)
                return False
            # outer μ alt binder must match inner_mu.context ID(alt)
            if not (isinstance(inner_mu.context, ID) and node.id.name == inner_mu.context.name):
                if self.verbose: logger.debug("Guard D failed (alt mismatch)")
                return False
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
                if self.verbose: logger.debug("Case 1? %s", _is_red(ctx_mt.context))
                if _is_red(ctx_mt.context):
                    if self.verbose: logger.debug("Applying defence rule")
                    # Apply rewrite: μ α .⟨ G || α ⟩
                    dbg_before = self._pres_str(node)
                    node.term    = deepcopy(inner_mu.term)
                    node.context = deepcopy(inner_mu.context)  # should be ID α
                    dbg_after = self._pres_str(node)
                    self._maybe_warn_onus_divergence(onus_info, orig_before, node, "alt-defence-mu", "Mu")
                    logger.debug("reduce.Mu alt-defence: before=\n%s", dbg_before)
                    logger.debug("reduce.Mu alt-defence: after=\n%s", dbg_after)
                    self._snapshot("alt-defence-mu")
                    self.visit_Mu(node)
                    return node

                # Case 2: ctx_mt.context already in normal form (no more redexes we know)
                # Rough heuristic: after recursive call, if no λ‑redex or affine‑µ pattern
                # matches in that sub‑tree, regard it as normal‑form.
                if self.verbose: logger.debug("Case 2? %s",  not self._has_next_redex(ctx_mt.context))
                if not self._has_next_redex(ctx_mt.context):
                    if self.verbose: logger.debug("Applying defeat rule")
                    dbg_before = self._pres_str(node)
                    node.term    = deepcopy(ctx_mt.term)
                    node.context = deepcopy(ctx_mt.context)  # t*
                    dbg_after = self._pres_str(node)
                    self._maybe_warn_onus_divergence(onus_info, orig_before, node, "alt-defeat-mu", "Mu")
                    logger.debug("reduce.Mu alt-defeat: before=\n%s", dbg_before)
                    logger.debug("reduce.Mu alt-defeat: after=\n%s", dbg_after)
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
                dbg_before = self._pres_str(node)
                # Perform capture‑avoiding substitution inside the *command* c.
                new_term    = _subst(inner.term,    x, deepcopy(E))
                new_context = _subst(inner.context, x, deepcopy(E))

                # Splice the substituted command into *node* (drop µ x.).
                node.term    = new_term
                node.context = new_context
                dbg_after = self._pres_str(node)
                self._maybe_warn_onus_divergence(onus_info, orig_before, node, "mutilde-beta", "Mutilde")
                self._maybe_warn_onus_divergence(onus_info, orig_before, node, "mu-beta", "Mutilde")
                self._maybe_warn_onus_divergence(onus_info, orig_before, node, "mutilde-beta", "Mu")
                self._maybe_warn_onus_divergence(onus_info, orig_before, node, "mu-beta", "Mu")
                logger.debug("reduce.Mu mu-beta: before=\n%s", dbg_before)
                logger.debug("reduce.Mu mu-beta: after=\n%s", dbg_after)
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
                dbg_before = self._pres_str(node)
                # Perform capture‑avoiding substitution inside the *command* c.
                new_term    = _subst(inner.term,    alpha, deepcopy(v))
                new_context = _subst(inner.context, alpha, deepcopy(v))

                # Splice the substituted command into *node* (drop µ x.).
                node.term    = new_term
                node.context = new_context
                dbg_after = self._pres_str(node)
                logger.debug("reduce.Mu mutilde-beta: before=\n%s", dbg_before)
                logger.debug("reduce.Mu mutilde-beta: after=\n%s", dbg_after)
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
        orig_before = deepcopy(node)
        onus_info = None
        if self.evaluation_discipline in ("onus-parallel", "onus"):
            okind, oreason = self._decide_onus(node)
            ocand = self._onus_apply_once(orig_before, okind)
            onus_info = (okind, oreason, ocand)

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
                dbg_before = self._pres_str(node)
                node.term    = deepcopy(v)
                node.context = new_context
                dbg_after = self._pres_str(node)
                self._maybe_warn_onus_divergence(onus_info, orig_before, node, "lambda", "Mutilde")
                logger.debug("reduce.Mutilde lambda: before=\n%s", dbg_before)
                logger.debug("reduce.Mutilde lambda: after=\n%s", dbg_after)
                self._snapshot("lambda")
                self.visit_Mutilde(node)
                return node

        # admal rule: ⟨ E*v || admal α.β ⟩ → ⟨ μ α.< v || β > || E ⟩
        if isinstance(node.term, Sonc) and isinstance(node.context, Admal):
            sonc: Sonc = node.term
            adm:  Admal = node.context
            E = deepcopy(sonc.context)
            v = deepcopy(sonc.term)
            alpha_obj = getattr(adm, "id", None)  # may be ID or a Hyp with .id/.prop
            alpha_id  = getattr(alpha_obj, "id", alpha_obj)
            alpha_prop = getattr(alpha_obj, "prop", getattr(alpha_id, "prop", None))
            beta_ctx = deepcopy(getattr(adm, "context", None))
            if alpha_id is not None and alpha_prop is not None and beta_ctx is not None:
                dbg_before = self._pres_str(node)
                inner = Mu(alpha_id, alpha_prop, v, beta_ctx)
                node.term = inner
                node.context = E
                dbg_after = self._pres_str(node)
                self._maybe_warn_onus_divergence(onus_info, orig_before, node, "admal", "Mutilde")
                logger.debug("reduce.Mutilde admal: before=\n%s", dbg_before)
                logger.debug("reduce.Mutilde admal: after=\n%s",  dbg_after)
                self._snapshot("admal")
                return self.visit_Mutilde(node)

        # basic support (Laog side):
        #   μ' alt . < μ aff . < DI(alt) || Laog >  ∥  μ' aff . < DI(alt) || Supporter > >
        # Cases (when Supporter is fully simplified):
        #   A) Supporter non-affine w.r.t. aff  → keep Supporter:
        #        μ' alt . < DI(alt) || Supporter >
        #   B) Supporter affine w.r.t. aff      → discard Supporter (defeated):
        #        μ' alt . < DI(alt) || Laog >
        if isinstance(node.term, Mu) and isinstance(node.context, Mutilde):
            inner_mu: Mu     = node.term
            ctx_mt:  Mutilde = node.context
            # shape guards
            shape_ok = (
                isinstance(inner_mu.term, DI) and inner_mu.term.name == node.di.name and
                isinstance(ctx_mt.term, DI) and ctx_mt.term.name == node.di.name
            )
            logger.debug("Basic Support Shape ok? %s", shape_ok)
            if shape_ok:
                # only apply when supporter fully simplified
                if not self._has_next_redex(ctx_mt.context):
                    if not _is_red(ctx_mt.context):
                        # Case A: keep Supporter
                        dbg_before = self._pres_str(node)
                        node.term    = deepcopy(ctx_mt.term)     # DI(alt)
                        node.context = deepcopy(ctx_mt.context)  # Supporter
                        self._maybe_warn_onus_divergence(onus_info, orig_before, node, "support-keep", "Mutilde")
                        dbg_after = self._pres_str(node)
                        logger.debug("reduce.Mutilde support-keep: before=\n%s", dbg_before)
                        logger.debug("reduce.Mutilde support-keep: after=\n%s",  dbg_after)
                        self._snapshot("support-keep")
                        return self.visit_Mutilde(node)
                    else:
                        # Case B: discard Supporter (defeated)
                        dbg_before = self._pres_str(node)
                        node.term    = deepcopy(inner_mu.term)      # DI(alt)
                        node.context = deepcopy(inner_mu.context)   # Laog
                        self._maybe_warn_onus_divergence(onus_info, orig_before, node, "support-discard", "Mutilde")
                        dbg_after = self._pres_str(node)
                        logger.debug("reduce.Mutilde support-discard: before=\n%s", dbg_before)
                        logger.debug("reduce.Mutilde support-discard: after=\n%s",  dbg_after)
                        self._snapshot("support-discard")
                        return self.visit_Mutilde(node)
                else:
                    # simplify supporter subtree and re-check
                    before = self._pres_str(ctx_mt.context)
                    if isinstance(ctx_mt.context, Mu):
                        ctx_mt.context = self.visit_Mu(ctx_mt.context)
                    else:
                        ctx_mt.context = self.visit(ctx_mt.context)
                    after = self._pres_str(ctx_mt.context)
                    if before != after:
                        return self.visit_Mutilde(node)
                    # if no progress, fall through to other rules
            else:
                if self.verbose:
                    logger.debug(
                        "support-mutilde shape failed: outer_alt=%s; inner_mu.term=%s(%s); ctx_mt.term=%s(%s)",
                        getattr(node.di, 'name', '?'),
                        type(inner_mu.term).__name__,
                        getattr(inner_mu.term, 'name', None),
                        type(ctx_mt.term).__name__,
                        getattr(ctx_mt.term, 'name', None),
                    )

        # affine helper --------------------------------------------------
        def _guards(inner_mu: Mu, ctx_mt: Mutilde):
            # both binders affine in their own commands
            if not (_is_affine(inner_mu.id.name, inner_mu) and
                    _is_affine(ctx_mt.di.name, ctx_mt)):
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
                if self.verbose: logger.debug("Case 1? %s", _is_red(inner_mu.term))
                if _is_red(inner_mu.term):
                    if self.verbose: logger.debug("Applying Mutilde defence rule")
                    # Apply rewrite: μ α .⟨ G || α ⟩
                    dbg_before = self._pres_str(node)
                    node.term    = deepcopy(ctx_mt.term)
                    node.context = deepcopy(ctx_mt.context)  # should be ID α
                    dbg_after = self._pres_str(node)
                    self._maybe_warn_onus_divergence(onus_info, orig_before, node, "alt-defence-mutilde", "Mutilde")
                    logger.debug("reduce.Mutilde alt-defence: before=\n%s", dbg_before)
                    logger.debug("reduce.Mutilde alt-defence: after=\n%s", dbg_after)
                    self._snapshot("alt-defence-mutilde")
                    self.visit_Mutilde(node)
                    return node

                # Case 2: ctx_mt.context already in normal form (no more redexes we know)
                # Rough heuristic: after recursive call, if no λ‑redex or affine‑µ pattern
                # matches in that sub‑tree, regard it as normal‑form.
                if self.verbose: logger.debug("Case 2? %s",  not self._has_next_redex(inner_mu.term))
                if not self._has_next_redex(inner_mu.term):
                    if self.verbose: logger.debug("Applying Mutilde defeat rule")
                    dbg_before = self._pres_str(node)
                    node.term    = deepcopy(inner_mu.term)
                    node.context = deepcopy(inner_mu.context)  # t*
                    dbg_after = self._pres_str(node)
                    self._maybe_warn_onus_divergence(onus_info, orig_before, node, "alt-defeat-mutilde", "Mutilde")
                    logger.debug("reduce.Mutilde alt-defeat: before=\n%s", dbg_before)
                    logger.debug("reduce.Mutilde alt-defeat: after=\n%s", dbg_after)
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
                dbg_before = self._pres_str(node)
                # Perform capture‑avoiding substitution inside the *command* c.
                new_term    = _subst(inner.term,    x, deepcopy(E))
                new_context = _subst(inner.context, x, deepcopy(E))

                # Splice the substituted command into *node* (drop µ x.).
                node.term    = new_term
                node.context = new_context
                dbg_after = self._pres_str(node)
                logger.debug("reduce.Mutilde mu-beta: before=\n%s", dbg_before)
                logger.debug("reduce.Mutilde mu-beta: after=\n%s", dbg_after)
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
                dbg_before = self._pres_str(node)
                # Perform capture‑avoiding substitution inside the *command* c.
                new_term    = _subst(inner.term,    alpha, deepcopy(v))
                new_context = _subst(inner.context, alpha, deepcopy(v))

                # Splice the substituted command into *node* (drop µ x.).
                node.term    = new_term
                node.context = new_context
                dbg_after = self._pres_str(node)
                logger.debug("reduce.Mutilde mutilde-beta: before=\n%s", dbg_before)
                logger.debug("reduce.Mutilde mutilde-beta: after=\n%s", dbg_after)
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
            # support-goal redex:
            # μ alt . < μ aff . < Goal || ID(alt) >  ∥  μ′ aff . < Supporter || ID(alt) > >
            if isinstance(n.term, Mu) and isinstance(n.context, Mutilde):
                im, cm = n.term, n.context
                if (isinstance(im.context, ID) and im.context.name == n.id.name and
                    isinstance(cm.context, ID) and cm.context.name == n.id.name):
                    return True
            # admal redex: ⟨ E*v || admal α.β ⟩
            if isinstance(n.term, Sonc) and isinstance(n.context, Admal):
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
            # support-laog redex (generalized):
            # μ′ alt . < μ aff . < DI(alt) || C >  ∥  μ′ aff . < DI(alt) || Supporter > >
            if isinstance(n.term, Mu) and isinstance(n.context, Mutilde):
                im, cm = n.term, n.context
                if (isinstance(im.term, DI) and im.term.name == n.di.name and
                    isinstance(cm.term, DI) and cm.term.name == n.di.name):
                    return True
            # admal redex: ⟨ E*v || admal α.β ⟩
            if isinstance(n.term, Sonc) and isinstance(n.context, Admal):
                return True
        # recurse quickly
        for child in getattr(n, 'term', None), getattr(n, 'context', None):
            if child and self._has_next_redex(child):
                return True
        return False

class EtaReducer:
    """
    One-step η-reduction on the root node only:
      - μ α . < t || α >   → t
      - μ' x . < x || t >  → t
    """
    def __init__(self, *, verbose: bool = True):
        self.verbose = verbose

    def reduce(self, root: ProofTerm) -> ProofTerm:
        # μ α . < t || α >  →  t
        if isinstance(root, Mu) and isinstance(root.context, ID) and root.context.name == root.id.name:
            if self.verbose:
                logger.debug("eta-reduce at root: μ %s . < t || %s >", root.id.name, root.id.name)
            return deepcopy(root.term)
        # μ' x . < x || t >  →  t
        if isinstance(root, Mutilde) and isinstance(root.term, DI) and root.term.name == root.di.name:
            if self.verbose:
                logger.debug("eta-reduce at root: μ' %s . < %s || t >", root.di.name, root.di.name)
            return deepcopy(root.context)
        # no η-redex at root
        return root

class ThetaExpander(ProofTermVisitor):
    """
    Theta-expansion: uniformly expand every subterm with prop == target_prop.

    Modes:
      - mode='term'    → for Term-positions:  t:A  ↦  μ alt:A . < μ aff:A . < t || alt > ∥ AltC_t >
      - mode='context' → for Context-positions: c:A ↦  μ' alt:A . < AltT_t ∥ μ' aff:A . < alt || c > >
    """
    def __init__(self, target_prop: str, mode: str = "term", *, verbose: bool = False):
        self.target_prop = target_prop
        self.mode = mode  # 'term' | 'context'
        self.verbose = verbose
        self._i = 0
        self.changed = False

    def _fresh_label(self, kind: str) -> str:
        self._i += 1
        return f"Alt{kind}{self._i}"

    def visit(self, node):
        if node is None:
            return None
        # Term-side expansion
        logger.debug("Is instance '%s' of Term? %s", node, isinstance(node, Term))
        logger.debug("Node prop: %s vs target_prop: %s", getattr(node, "prop", None), self.target_prop)
        if (self.mode in ("term")
            and isinstance(node, Term)
            and getattr(node, "prop", None) == self.target_prop):
            logger.debug("Visiting node for theta-expansion (%s mode): %s", self.mode, getattr(node, "pres", repr(node)))
            A = self.target_prop
            t = deepcopy(node)
            inner = Mu(ID("aff", A), A, t, ID("alt", A))
            altc = Laog(self._fresh_label("C"), A)
            self.changed = True
            return Mu(ID("alt", A), A, inner, altc)
        # Context-side expansion
        logger.debug("Is instance '%s' of Context? %s", node, isinstance(node, Context))
        logger.debug("Node prop: %s vs target_prop: %s", getattr(node, "prop", None), self.target_prop)
        if (self.mode in ("context")
            and isinstance(node, Context)
            and getattr(node, "prop", None) == self.target_prop):
            logger.debug("Visiting node for theta-expansion (%s mode): %s", self.mode, getattr(node, "pres", repr(node)))
            A = self.target_prop
            c = deepcopy(node)
            altt = Goal(self._fresh_label("T"), A)
            inner = Mutilde(DI("aff", A), A, DI("alt", A), c)
            self.changed = True
            return Mutilde(DI("alt", A), A, altt, inner)
        # descend
        new = deepcopy(node)
        if hasattr(new, "term") and new.term is not None:
            new.term = self.visit(new.term)
        if hasattr(new, "context") and new.context is not None:
            new.context = self.visit(new.context)
        return new
