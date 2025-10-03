from parser import *

import logging
logger = logging.getLogger("tests.affine")


def affine_mu_rule_test():
    """Test affine μ‑rule with *arbitrary* affine variable names (no underscore
    assumption)."""
    logger.info("AFFINE μ‑RULE REDUCTION TEST")

    # same pattern as before but the affine binder is named "u" (any name).
    term_str = "μalpha:B.<μu:B.<?1.1||alpha>||μ'x:B.<?1.2||μ'beta:B.<t||?1.3>>>"

    grammar = Grammar()
    transformer = ProofTermTransformer()
    ast = transformer.transform(grammar.parser.parse(term_str))

    reducer = ArgumentTermReducer()
    result = reducer.reduce(ast)
    printer = ProofTermGenerationVisitor()
    result_term = printer.visit(result).pres
    logger.info("Result term %s", result_term)
    try:
        assert isinstance(result, Mu), "root must be Mu"
        assert isinstance(result.term, Goal) and result.term.number == "1.1", "term should be ?1.1"
        assert isinstance(result.context, ID) and result.context.name == "alpha", "context should be alpha"
        logger.info("AFFINE μ‑RULE REDUCTION TEST PASSED")
    except AssertionError as e:
        logger.error("AFFINE μ‑RULE REDUCTION TEST FAILED: %s", e)
    except Exception:
        logger.exception("Unexpected exception during affine μ‑rule test")

def test_affine_mu_rule():
    affine_mu_rule_test()
