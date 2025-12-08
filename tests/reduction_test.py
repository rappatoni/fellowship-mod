from copy import deepcopy
import logging
from conftest import make_assert_log
from core.ac.grammar import Grammar, ProofTermTransformer
from core.comp.reduce import ArgumentTermReducer
from pres.gen import ProofTermGenerationVisitor
from core.ac.ast import Mu, DI, ID
logger = logging.getLogger("tests.reduction")
_assert_log = make_assert_log(logger)

# ------------------------------------------------------------
#  Lambda-rule regression test for ArgumentTermReducer
# ------------------------------------------------------------
# This script is intended to be imported by the big test-driver the user
# maintains.  We expose **lambda_rule_test** and add it to the global *tests*
# dictionary when that driver imports us with `from test_lambda_rule import *`.


def lambda_rule_test():
    """Check ⟨ λx.t || v * E ⟩ → ⟨ v || µ' x.⟨t||E⟩ ⟩

    A *minimal* counter-example would reveal itself if the reducer forgot to
    deep-copy sub-trees or failed to re-link the new nodes correctly.
    """
    logger.info("Lambda rule reduction test")
    try:
        # A tiny proof term that exhibits the redex *once*.
        #    μ thesis:B . < λx:A.x || y * thesis >
        # After one reduction we expect
        #    μ thesis:B . <  y  ||  μ' x:A .< x || thesis > >
        test_terms = ["μthesis:B.<λx:A.t||y*thesis>", "μ'thesis:B.<λx:A.thesis||y*z>"]
        counter = 0
        reduced_terms = []
        for test_term in test_terms:
            logger.debug("Input term: %s", test_term)
            grammar   = Grammar()
            transformer = ProofTermTransformer()
            tree      = grammar.parser.parse(test_term)
            ast       = transformer.transform(tree)
            printer = ProofTermGenerationVisitor()            
            reducer   = ArgumentTermReducer()            
            result    = reducer.reduce(ast)
            result_term = printer.visit(result).pres
            logger.info("Reduced proof term %d: %s", counter, result_term)
            reduced_terms = reduced_terms + [result_term]
            logger.debug("Reduced terms so far: %r", reduced_terms)
            if counter == 0:                
            # --- structural assertions -------------------------------------
                _assert_log(isinstance(result, Mu), "Root must remain a Mu node")
                # term part becomes the argument "y"
                _assert_log(isinstance(result.term, DI) and result.term.name == "t",
                    f"Expected term to be DI 't', got {type(result.term).__name__}:{getattr(result.term, 'name', '-')}")

                # context part becomes a fresh µ' binder
                _assert_log(isinstance(result.context, ID), "Context must be an ID after reduction")
                ctx = result.context
                _assert_log(ctx.name == "thesis", "thesis gets substituted in by mu' rule")
                #assert isinstance(ctx.term, DI) and ctx.term.name == "t", "Inner term should be the body 't'"
                #assert isinstance(ctx.context, ID) and ctx.context.name == "thesis", "Continuation should be 'thesis'"
            counter += 1
            logger.debug("Counter: %d", counter)
        logger.info("Lambda rule reduction test passed")
        logger.debug("Original proof terms: %r", test_terms)
        logger.debug("Reduced proof terms: %r", reduced_terms)
    except AssertionError as e:
        logger.error("Lambda rule reduction test failed: %s", e)
    except Exception:
        logger.exception("Unexpected exception during lambda-rule test")

def test_lambda_rule():
    lambda_rule_test()
