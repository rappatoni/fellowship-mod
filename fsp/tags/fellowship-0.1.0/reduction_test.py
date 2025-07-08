from parser import *
from copy import deepcopy

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
    print("LAMBDA RULE REDUCTION TEST")
    try:
        # A tiny proof term that exhibits the redex *once*.
        #    μ thesis:B . < λx:A.x || y * thesis >
        # After one reduction we expect
        #    μ thesis:B . <  y  ||  μ' x:A .< x || thesis > >
        test_terms = ["μthesis:B.<λx:A.t||y*thesis>", "μ'thesis:B.<λx:A.thesis||y*z>"]
        counter = 0
        reduced_terms = []
        for test_term in test_terms:
            print(test_term)
            grammar   = Grammar()
            transformer = ProofTermTransformer()
            tree      = grammar.parser.parse(test_term)
            ast       = transformer.transform(tree)
            printer = ProofTermGenerationVisitor()            
            reducer   = ArgumentTermReducer()            
            result    = reducer.reduce(ast)
            result_term = printer.visit(result).pres
            print("Reduced proof term ", counter, ": ", result_term)
            reduced_terms = reduced_terms + [result_term]
            print(reduced_terms)
            if counter == 0:                
            # --- structural assertions -------------------------------------
                assert isinstance(result, Mu), "Root must remain a Mu node"
                # term part becomes the argument "y"
                assert isinstance(result.term, DI) and result.term.name == "t", (
                    f"Expected term to be DI 't', got {type(result.term).__name__}:{getattr(result.term, 'name', '-')}")

                # context part becomes a fresh µ' binder
                assert isinstance(result.context, ID), "Context must be an ID after reduction"
                ctx = result.context
                assert ctx.name == "thesis",  "thesis gets substituted in by mu' rule"
                #assert isinstance(ctx.term, DI) and ctx.term.name == "t", "Inner term should be the body 't'"
                #assert isinstance(ctx.context, ID) and ctx.context.name == "thesis", "Continuation should be 'thesis'"
            counter += 1
            print(counter)
        print("LAMBDA RULE REDUCTION TEST PASSED\n")
        print("Original proof terms:", test_terms)
        print("Reduced proof terms:", reduced_terms)
    except AssertionError as e:
        print("LAMBDA RULE REDUCTION TEST FAILED:", e)
    except Exception as exc:
        print("Unexpected exception during lambda-rule test:", exc)
