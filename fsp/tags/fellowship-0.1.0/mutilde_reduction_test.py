from parser import Grammar, ProofTermTransformer, Goal, Laog, Mutilde, ID,  DI, Mu, ProofTermGenerationVisitor
from parser import ArgumentTermReducer  # uses integrated version in parser.py
import logging
from conftest import make_assert_log
logger = logging.getLogger("tests.mutilde")
_assert_log = make_assert_log(logger)


def mutilde_affine_defence_test():
    """Checks the *throw‑away* affine rule when the **outer binder is µ'**.

    Input:
        μ'alpha:B.<μx:B.<μz:B.<?2||t>||?1>||μ'y:B.<alpha||?3>>

    µ'y is an *affine* binder, so the entire left branch can
    be discarded.  Expected normal form:
        μ'α:B.< B:alpha || ?3 >
    """
    logger.info("Mutilde affine defence test")

    proof = "μ'alpha:B.<μx:B.<μz:B.<?2||t>||?1>||μ'y:B.<alpha||?3>>"
    logger.info("Input: %s", proof)

    grammar = Grammar()
    ast = ProofTermTransformer().transform(grammar.parser.parse(proof))

    reducer = ArgumentTermReducer()
    res = reducer.reduce(ast)
    printer = ProofTermGenerationVisitor()
    result_term = printer.visit(res).pres
    logger.info("Result term: %s", result_term)


    try:
        # root binder unchanged (µ')
        _assert_log(isinstance(res, Mutilde), "root should remain µ'")
        # term should now be the goal ?1
        logger.debug("Context number: %s", res.context.number)
        _assert_log(isinstance(res.context, Laog) and res.context.number == "3", "term should be ?3")
        # context must be ID alpha
        _assert_log(isinstance(res.term, DI) and res.term.name == "alpha", "context should be ID 'alpha'")
        logger.info("Mutilde affine defence test passed")
    except AssertionError as e:
        logger.error("Mutilde affine defence test failed: %s", e)
    except Exception:
        logger.exception("Unexpected exception during mutilde affine defence test")

def test_mutilde_affine_defence():
    mutilde_affine_defence_test()
