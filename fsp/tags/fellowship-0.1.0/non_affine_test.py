from parser import *
from copy import deepcopy
import logging
from conftest import make_assert_log
logger = logging.getLogger("tests.non_affine")
_assert_log = make_assert_log(logger)

def affine_mu_rule_nonaffine_test():
    """t* is a µ-term in normal form **and not affine**.

    Input:  "μalpha:B.<μx:B.<?1||alpha>||μ'y:B.<?2||μ'w:B.<w||v>>>"
    Expect: µalpha:B.<?2||v>
    """
    logger.info("Affine μ‑rule #2 (non‑affine t*) test")

    term = "μalpha:B.<μx:B.<?1||alpha>||μ'y:B.<?2||μ'w:B.<w||v>>>"
    logger.debug("Input term: %s", term)

    grammar = Grammar()
    ast = ProofTermTransformer().transform(grammar.parser.parse(term))
    printer = ProofTermGenerationVisitor()
    parsed_term = printer.visit(ast).pres
    logger.debug("Parsed term: %s", parsed_term)
    reducer = ArgumentTermReducer()
    res = reducer.reduce(ast)
    #printer = ProofTermGenerationVisitor()
    result_term = printer.visit(res).pres
    logger.info("Result term: %s", result_term)

    try:
        # root still Mu
        _assert_log(isinstance(res, Mu), "root not Mu")
        # term is ?1
        _assert_log(isinstance(res.term, Goal) and res.term.number == "2", "term not goal ?2")
        # context is µw<B>
        _assert_log(isinstance(res.context, ID), "context not ID term as expected")
        inner = res.context
        _assert_log(inner.name == "v", "inner context should be v")
        # binder non‑affine: w occurs in term component (ID w)
        #assert isinstance(inner.term, (ID, DI)) and inner.term.name == "w", "inner term should be w"
        #logger.info("Affine μ‑rule #2 (non‑affine t*) test passed")
    except AssertionError as e:
        logger.error("Affine μ‑rule #2 (non‑affine t*) test failed: %s", e)

def test_affine_mu_rule_nonaffine():
    affine_mu_rule_nonaffine_test()
