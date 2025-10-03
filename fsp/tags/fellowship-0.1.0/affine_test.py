from parser import *

import logging
from conftest import make_assert_log
logger = logging.getLogger("tests.affine")
_assert_log = make_assert_log(logger)


def affine_mu_rule_test():
    """Test affine μ‑rule with *arbitrary* affine variable names"""
    logger.info("AFFINE μ‑RULE REDUCTION TEST")

    term_str = "μalpha:B.<μu:B.<?1.1||alpha>||μ'x:B.<?1.2||μ'beta:B.<t||?1.3>>>"
    logger.info("affine variable is named 'u': "+term_str+".")

    grammar = Grammar()
    transformer = ProofTermTransformer()
    ast = transformer.transform(grammar.parser.parse(term_str))

    reducer = ArgumentTermReducer()
    result = reducer.reduce(ast)
    printer = ProofTermGenerationVisitor()
    result_term = printer.visit(result).pres
    logger.info("Result term %s", result_term)
    try:
        _assert_log(isinstance(result, Mu), "root must be Mu")
        _assert_log(isinstance(result.term, Goal) and result.term.number == "1.1", "term should be ?1.1")
        _assert_log(isinstance(result.context, ID) and result.context.name == "alpha", "context should be alpha")
        logger.info("AFFINE μ‑RULE REDUCTION TEST PASSED")
    except AssertionError as e:
        logger.error("AFFINE μ‑RULE REDUCTION TEST FAILED: %s", e)
    except Exception:
        logger.exception("Unexpected exception during affine μ‑rule test")

def test_affine_mu_rule():
    affine_mu_rule_test()
