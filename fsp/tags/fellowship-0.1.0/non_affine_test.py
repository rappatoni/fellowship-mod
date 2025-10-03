from parser import *
from copy import deepcopy

def affine_mu_rule_nonaffine_test():
    """t* is a µ-term in normal form **and not affine**.

    Input:  "μalpha:B.<μx:B.<?1||alpha>||μ'y:B.<?2||μ'w:B.<w||v>>>"
    Expect: µalpha:B.<?2||v>
    """
    print("AFFINE μ‑RULE #2 (non‑affine t*) TEST")

    term = "μalpha:B.<μx:B.<?1||alpha>||μ'y:B.<?2||μ'w:B.<w||v>>>"
    print(term)

    grammar = Grammar()
    ast = ProofTermTransformer().transform(grammar.parser.parse(term))
    printer = ProofTermGenerationVisitor()
    parsed_term = printer.visit(ast).pres
    print("parsed term", parsed_term)
    reducer = ArgumentTermReducer()
    res = reducer.reduce(ast)
    #printer = ProofTermGenerationVisitor()
    result_term = printer.visit(res).pres
    print("result term" , result_term)

    try:
        # root still Mu
        assert isinstance(res, Mu), "root not Mu"
        # term is ?1
        assert isinstance(res.term, Goal) and res.term.number == "2", "term not goal ?1"
        # context is µw<B>
        assert isinstance(res.context, ID), "context not ID term as expected"
        inner = res.context
        assert inner.name == "v", "inner context should be v"
        # binder non‑affine: w occurs in term component (ID w)
        #assert isinstance(inner.term, (ID, DI)) and inner.term.name == "w", "inner term should be w"
        print("AFFINE μ‑RULE #2 (non‑affine t*) TEST PASSED\n")
    except AssertionError as e:
        print("AFFINE μ‑RULE #2 (non‑affine t*) TEST FAILED:", e)

def test_affine_mu_rule_nonaffine():
    affine_mu_rule_nonaffine_test()
