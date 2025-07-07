from parser import *


def affine_mu_rule_test():
    """Test affine μ‑rule with *arbitrary* affine variable names (no underscore
    assumption)."""
    print("AFFINE μ‑RULE REDUCTION TEST")

    # same pattern as before but the affine binder is named "u" (any name).
    term_str = "μalpha:B.<μu:B.<?1.1||alpha>||μ'x:B.<?1.2||μ'beta:B.<t||?1.3>>>"

    grammar = Grammar()
    transformer = ProofTermTransformer()
    ast = transformer.transform(grammar.parser.parse(term_str))

    reducer = ArgumentTermReducer()
    result = reducer.reduce(ast)
    printer = ProofTermGenerationVisitor()
    result_term = printer.visit(result).pres
    print("Result term", result_term)
    try:
        assert isinstance(result, Mu), "root must be Mu"
        assert isinstance(result.term, Goal) and result.term.number == "1.1", "term should be ?1.1"
        assert isinstance(result.context, ID) and result.context.name == "alpha", "context should be alpha"
        print("AFFINE μ‑RULE REDUCTION TEST PASSED\n")
    except AssertionError as e:
        print("AFFINE μ‑RULE REDUCTION TEST FAILED:", e)
    except Exception as exc:
        print("Unexpected exception during affine μ‑rule test:", exc)
