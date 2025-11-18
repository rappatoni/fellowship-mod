from typing import Optional, Dict, Any, Tuple
from copy import deepcopy
import logging
from core.comp.visitor import ProofTermVisitor
from core.ac.ast import ProofTerm, Mu, Mutilde, Lamda, Admal, Cons, Sonc, Goal, Laog, ID, DI
from core.comp.alpha import _collect_binder_names, _fresh, _AlphaRename

logger = logging.getLogger(__name__)

def _present(pt) -> str:
    try:
        from pres.gen import ProofTermGenerationVisitor
        c = deepcopy(pt)
        c = ProofTermGenerationVisitor().visit(c)
        return getattr(c, "pres", repr(pt))
    except Exception:
        return repr(pt)

# ---------------------------------------------------------------------------
#  Helper – alpha-rename scion binders to avoid clashes with root binders
# ---------------------------------------------------------------------------

def _alpha_rename_if_needed(scion: ProofTerm, root: ProofTerm) -> ProofTerm:
    """Return scion with all conflicting binders α-renamed fresh w.r.t. root."""
    root_names = _collect_binder_names(root)
    scion_names = _collect_binder_names(scion)
    mapping: Dict[str, str] = {}
    for nm in scion_names:
        if nm in root_names:
            fresh_nm = _fresh(nm, root_names | scion_names)
            mapping[nm] = fresh_nm
    if mapping:
        scion = deepcopy(scion)
        _AlphaRename(mapping).visit(scion)
    return scion

# ---------------------------------------------------------------------------
#  Helper – capture‑aware substitution of Goals / Laogs by variables
# ---------------------------------------------------------------------------

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
        var = self.goal_map.get(node.prop)
        return DI(var, node.prop) if var is not None else deepcopy(node)

    def _rewrite_laog(self, node: Laog):
        var = self.laog_map.get(node.prop)
        return ID(var, node.prop) if var is not None else deepcopy(node)

# ---------------------------------------------------------------------------
#  Helper – traversal that performs the grafting itself
# ---------------------------------------------------------------------------

class _GraftVisitor(ProofTermVisitor):
    """Walk body_B and replace matching Goal/Laog by replacement_ast."""

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

        if isinstance(node, Admal):          # ID-like binder (captures Laog) on context side
            self._push_id(node.id.id.name, node.id.prop)
            new = deepcopy(node)
            new.context = self.visit(node.context)
            self._pop_id()
            return new

        if isinstance(node, Sonc):
            new = deepcopy(node)
            new.context = self.visit(node.context)
            new.term    = self.visit(node.term)
            return new

        if isinstance(node, Cons):
            new = deepcopy(node)
            new.term    = self.visit(node.term)
            new.context = self.visit(node.context)
            return new

        return deepcopy(node)

    # ------------------------------------------------------------------
    def _make_maps(self):
        # Innermost binder should capture: iterate in push order so later (deeper)
        # binders override earlier ones for the same prop.
        goal_map = {}
        for name, prop in self.di_stack:
            goal_map[prop] = name  # prop → DI name (for Goals)
        laog_map = {}
        for name, prop in self.id_stack:
            laog_map[prop] = name  # prop → ID name (for Laogs)
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

def _find_target_info(ast: ProofTerm, number: str) -> Tuple[Optional[str], Optional[bool]]:
    """Locate ?number / !number in ast – return (prop, is_goal)."""
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


def _check_conclusion_matches(repl_ast: ProofTerm, *, target_is_goal: bool, target_prop: str) -> None:
    """Ensure repl_ast’s root binder fits the Goal/Laog it replaces."""
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

def graft_single(body_B: ProofTerm, goal_number: str, body_A: ProofTerm) -> ProofTerm:
    """Replace the unique Goal/Laog goal_number in body_B by body_A."""
    tgt_prop, is_goal = _find_target_info(body_B, goal_number)
    if tgt_prop is None:
        raise ValueError(f"no Goal/Laog numbered {goal_number} found")

    kind = "goal" if is_goal else "laog"
    logger.debug("graft_single #%s: scion=%s", goal_number, _present(body_A))
    logger.debug("graft_single #%s: before=\n%s", goal_number, _present(body_B))
    before_str = _present(body_B)
    logger.info(
        "Starting graft: Replace %s %s:%s in %s by %s",
        kind, goal_number, tgt_prop, _present(body_B), _present(body_A)
    )

    # type‑check replacement
    _check_conclusion_matches(body_A, target_is_goal=is_goal, target_prop=tgt_prop)

    # freshness workaround: rename scion binders if they clash
    body_A = _alpha_rename_if_needed(body_A, body_B)

    visitor = _GraftVisitor(replacement_ast=body_A,
                            target_prop=tgt_prop,
                            target_number=goal_number,
                            target_is_goal=is_goal)
    result = visitor.visit(body_B)
    after_str = _present(result)
    logger.debug("graft_single #%s: after=\n%s", goal_number, after_str)
    if before_str == after_str:
        logger.info("graft_single #%s: no-op (result identical to input)", goal_number)
    return result


def graft_uniform(body_B: ProofTerm, body_A: ProofTerm) -> ProofTerm:
    """Uniform graft: replace all Goals/Laogs with the conclusion of A."""
    logger.info("Uniformly grafting scion=%s on root=%s", _present(body_A), _present(body_B))
    logger.debug("graft_uniform: before=\n%s", _present(body_B))
    before_str = _present(body_B)
    # determine conclusion of A from its root binder
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
    visitor = _GraftVisitor(replacement_ast=body_A,
                            target_prop=concl_prop,
                            target_number=None,
                            target_is_goal=target_is_goal)
    result = visitor.visit(body_B)
    after_str = _present(result)
    logger.debug("graft_uniform: after=\n%s", after_str)
    if before_str == after_str:
        target_kind = "Goal" if target_is_goal else "Laog"
        logger.info("graft_uniform: no-op (no matching %s with prop %s)", target_kind, concl_prop)
    return result
