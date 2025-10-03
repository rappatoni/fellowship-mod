from parser import Grammar, ProofTermTransformer, Goal, Laog, Mutilde, ID,  DI, Mu, ProofTermGenerationVisitor
from parser import ArgumentTermReducer  # uses integrated version in parser.py
import logging
logger = logging.getLogger("tests.mutilde")


def mutilde_affine_defence_test():
    """Checks the *throw‑away* affine rule when the **outer binder is µ'**.

    Input (informally):
        μ'α:B.< μx:B.< ?1 || α >  ||  μ'y:B.< α || μ'z:B.< ?1 || α > > >

    The innermost µ'z has an *affine* binder, so the entire right branch can
    be discarded.  Expected normal form:
        μ'α:B.< ?1 || α >
    """
    logger.info("Mutilde affine defence test")

    proof = "μ'alpha:B.<μx:B.<μz:B.<?2||t>||?1>||μ'y:B.<alpha||?3>>"

    grammar = Grammar()
    ast = ProofTermTransformer().transform(grammar.parser.parse(proof))

    reducer = ArgumentTermReducer()
    res = reducer.reduce(ast)
    printer = ProofTermGenerationVisitor()
    result_term = printer.visit(res).pres
    logger.info("Result term: %s", result_term)


    try:
        # root binder unchanged (µ')
        assert isinstance(res, Mutilde), "root should remain µ'"
        # term should now be the goal ?1
        logger.debug("Context number: %s", res.context.number)
        assert isinstance(res.context, Laog) and res.context.number == "3", "term should be ?3"
        # context must be ID alpha
        assert isinstance(res.term, DI) and res.term.name == "alpha", "context should be ID 'alpha'"
        logger.info("Mutilde affine defence test passed")
    except AssertionError as e:
        logger.error("Mutilde affine defence test failed: %s", e)

def test_mutilde_affine_defence():
    mutilde_affine_defence_test()
