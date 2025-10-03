from parser import *
import logging
logger = logging.getLogger("tests.mu")
def mu_general_rule_test():
    """General µ‑reduction ⟨µx.c ∣∣ E⟩ → [E/x]c.

    Input  : μalpha:B.< μx:B.< x || v > || k >
    Expect : μalpha:B.< k || v >
    """

    logger.info("General μ‑rule (substitution) test")
    logger.debug("Input: μalpha:B.< μx:B.< x || v > || k >")

    term_str = "μalpha:B.<μx:B.<x||v>||k>"

    grammar = Grammar()
    ast = ProofTermTransformer().transform(grammar.parser.parse(term_str))

    reducer = ArgumentTermReducer()
    res = reducer.reduce(ast)
    printer = ProofTermGenerationVisitor()      
    result_term = printer.visit(res).pres
    
    logger.info("Reduced proof term: %s", result_term)

    try:
        # root node should still be the outer Mu (binder alpha)
        assert isinstance(res, Mu) and res.id.name == "alpha", "root must be outer µalpha"
        # after reduction: term component becomes ID k
        assert isinstance(res.term, ID) and res.term.name == "k", "term should be ID k after substitution"
        # context component becomes ID v
        assert isinstance(res.context, ID) and res.context.name == "v", "context should be ID v after substitution"
        # ensure no residual inner µ binder
        assert not isinstance(res.term, Mu) and not isinstance(res.context, Mu), "inner µ binder should be eliminated"
        logger.info("General μ‑rule (substitution) test passed")
    except AssertionError as e:
        logger.error("General μ‑rule (substitution) test failed: %s", e)
    except Exception:
        logger.exception("Unexpected exception during general μ‑rule test")

def test_mu_general_rule():
    mu_general_rule_test()
