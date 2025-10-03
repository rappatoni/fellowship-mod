"""Simple unit‑like checks for the graft operator.

These tests are **framework‑free** so that they can be imported from the
hand‑rolled `tests.py` harness you provided.  After importing this module you
only need to register `graft_operator_test` in the global `tests` dictionary:

    from graft_operator_test import graft_operator_test
    tests["graft_operator_test"] = graft_operator_test
"""

from copy import deepcopy

from parser import *
import logging
from conftest import make_assert_log
logger = logging.getLogger("tests.graft")
_assert_log = make_assert_log(logger)

# ──────────────────────────────────────────────────────────────────────────────
# helper: tiny visitors to inspect a tree
# ──────────────────────────────────────────────────────────────────────────────

class _CountKind(ProofTermVisitor):
    """Count how many nodes of *kind* appear in a tree."""
    def __init__(self, kind):
        self.kind = kind
        self.count = 0
    def visit_Goal(self, n):
        if self.kind is Goal:
            self.count += 1
        return n
    def visit_Laog(self, n):
        if self.kind is Laog:
            self.count += 1
        return n

class _FindVar(ProofTermVisitor):
    """Whether an **ID/DI** with the given *name* occurs inside a tree."""
    def __init__(self, name, cls):
        self.name = name
        self.cls  = cls
        self.found = False
    def visit_ID(self, n):
        if self.cls is ID and n.name == self.name:
            self.found = True
        return n
    def visit_DI(self, n):
        if self.cls is DI and n.name == self.name:
            self.found = True
        return n

# ──────────────────────────────────────────────────────────────────────────────
# actual test function
# ──────────────────────────────────────────────────────────────────────────────

def graft_operator_test():
    """Check goal‑grafting **and** laog‑grafting incl. capturing substitution."""

    # ------------------------------------------------------------------
    # 1. Goal graft -----------------------------------------------------
    # ------------------------------------------------------------------
    # Body B:  λy:A. ?1   wrapped in an unrelated μ‑binder
    goal1 = Goal("1"); goal1.prop = "A"
    lam_y = Lamda(Hyp(DI("y"), "A"), goal1)
    body_B_goal = Mu(ID("thesis"), "A->A", lam_y, ID("thesis"))

    # Body A:  μx:A.< x || ?9 >   (contains another open goal with prop A)
    goal8 = Goal("8"); goal8.prop = "A"
    goal9 = Laog("9"); goal9.prop = "A"
    body_A_goal = Mu(ID("x"), "A", ID("x"), goal9); body_A_goal.term.prop = "A"
    correct_body_A_goal = Mu(ID("x"), "A", goal8, ID("x"));  correct_body_A_goal.term.prop = "A"
    enricher = PropEnrichmentVisitor()
    
    ptgenerator = ProofTermGenerationVisitor()
    body_B_goal = ptgenerator.visit(enricher.visit(body_B_goal))
    body_A_goal = ptgenerator.visit(enricher.visit(body_A_goal))
    correct_body_A_goal = ptgenerator.visit(enricher.visit(correct_body_A_goal))
    logger.info("Argument bodies (second is affine): %s | %s | %s", body_B_goal.pres, body_A_goal.pres, correct_body_A_goal.pres)
    # graft: replace ?1 in B by A
    grafted_goal = graft_single(body_B_goal, "1", body_A_goal)
    correct_grafted_goal = graft_single(body_B_goal, "1", correct_body_A_goal)

    grafted_goal = ptgenerator.visit(grafted_goal)
    correct_grafted_goal = ptgenerator.visit(correct_grafted_goal)
    logger.info("First argument: %s", grafted_goal.pres)
    logger.info("Second argument: %s", correct_grafted_goal.pres)
    # there must be **no** Goal left; goal9 had to be captured by DI("y")
    ck_goal = _CountKind(Goal)
    ck_laog = _CountKind(Laog)
    ck_goal.visit(grafted_goal)
    ck_laog.visit(grafted_goal)
    logger.debug("Counts (goal, laog) = (%d, %d)", ck_goal.count, ck_laog.count)
    _assert_log(ck_laog.count != 0, "Goal graft: false positive on y.")
    _assert_log(ck_goal.count == 0, "graft unsuccessful.")
    ck_goal.visit(correct_grafted_goal)
    ck_laog.visit(correct_grafted_goal)
    ck_laog2 = _CountKind(Laog)
    logger.debug("Laog count (post-correct) = %d", ck_laog2.count)
    _assert_log(ck_goal.count == 0, "Goal graft: residual goals present")
    _assert_log(ck_laog2.count == 0, "Substitution of y for laog 8 unsuccessful")

    fv_y = _FindVar("y", DI)
    fv_y.visit(correct_grafted_goal)
   
    generator = InstructionsGenerationVisitor()
    instructions = generator.return_instructions(grafted_goal)  
    logger.info("Generated instructions: %s", list(instructions))
    _assert_log(fv_y.found, "Goal graft: DI variable y not substituted in")
    

    

    # ------------------------------------------------------------------
    # 2. Laog graft -----------------------------------------------------
    # ------------------------------------------------------------------
    # Body B:  μx:A.< ?1 || x >   where ?1 has prop A_bar
    laog1 = Laog("1"); laog1.prop = "A_bar"
    body_B_laog = Mu(ID("x"), "A", laog1, ID("x"))

    # Body A:  μ' b:A .< ?9 || b >   (open laog inside)
    laog9 = Laog("9"); laog9.prop = "A_bar"
    body_A_laog = Mutilde(DI("b"), "A", laog9, DI("b"))

    grafted_laog = graft_single(body_B_laog, "1", body_A_laog)

    ck_laog = _CountKind(Laog)
    ck_laog.visit(grafted_laog)
    _assert_log(ck_laog.count == 0, "Laog graft: residual laogs present")

    fv_x = _FindVar("x", ID)
    fv_x.visit(grafted_laog)
    _assert_log(fv_x.found, "Laog graft: ID variable x not substituted in")

    logger.info("Graft operator test passed")

def test_graft_operator():
    graft_operator_test()
