from parser import *
from wrapper import *
from parser import *
from wrapper import *
import argparse
import difflib
from reduction_test import *
from affine_test import *
from non_affine_test import *
from mu_reduction_test import *
from mutilde_reduction_test import *
from graft_test import *
from smoketest import *
import traceback
import logging
import io
import os

# Tests3
# TODO: refactor this using a standard test library such as unittest.
def test_prover():
    prover = setup_prover()
    prover.send_command('declare A,B,C,D:bool.')
    return prover

def declaration_test():
    print("DECLARATION TEST")
    prover = test_prover()
    #prover.send_command()
    test_declarations = {"A": "bool",
                         "B": "bool",
                         "C": "bool",
                         "D": "bool"}
    if prover.declarations == test_declarations:
        print("Test part 1 passed:")
        print(prover.declarations)
    else:
        print("Test part 1 failed:")
        print(prover.declarations)

    prover.send_command('declare X:(A).')
    print("Expect X:A", prover.declarations)
    prover.close
    return

def prop_enrichment_test():
    prover = test_prover()
    prover.send_command('declare r1: (A->B).')
    arg = Argument(
            prover,
            name='argA',
            conclusion='B',
            instructions=[
                'cut (A-> B) x1',
                'axiom r1.',
                'elim.',
                'next.',
                'axiom.'
            ]
        )

    try:
        arg.execute()
        # Suppose extract_assumptions found { '1.1.2': 'A' } => so we have goal #1.1.2 with prop A
        if arg.body:
            # Print each node's .prop
            print("AST after enrichment:")
            print_props(arg.body)
        else:
            print("No AST?!")

    except Exception as e:
        print("TEST FAILED:", e)
    finally:
        prover.close()

def print_props(node, indent=0):
    from parser import ProofTermVisitor

    class PrintPropVisitor(ProofTermVisitor):
        def __init__(self):
            super().__init__()
            self.indent = 0
        def visit_Mu(self, node):
            print("  "*self.indent, f"Mu id={node.id.name}, prop={node.prop}, contradiction = {node.contr}")
            self.indent += 1
            node.term = self.visit(node.term)
            node.context = self.visit(node.context)
            self.indent -= 1
            return node
        def visit_Mutilde(self, node):
            print("  "*self.indent, f"Mutilde di={node.di.name}, prop={node.prop}, contradiction = {node.contr}")
            self.indent += 1
            node.term = self.visit(node.term)
            node.context = self.visit(node.context)
            self.indent -= 1
            return node
        def visit_Lamda(self, node: Lamda):
            print("  "*self.indent, f"Lamda di={node.di.name}, prop={node.prop}")
            self.indent += 1
            node.di = self.visit(node.di)
            node.term = self.visit(node.term)
            self.indent -= 1
            return node
        def visit_Cons(self, node: Cons):
            print("  "*self.indent, f"Cons prop={node.prop}")
            node.term = self.visit(node.term)
            node.context = self.visit(node.context)
            self.indent -= 1
            return node

        def visit_Goal(self, node: Goal):
             print("  "*self.indent, f"Goal prop={node.prop}")
             return node

        def visit_ID(self, node: ID):
             print("  "*self.indent, f"ID prop={node.prop}")
             return node

        def visit_DI(self, node: DI):
             print("  "*self.indent, f"DI prop={node.prop}")
             return node

    pv = PrintPropVisitor()
    pv.visit(node)
        
def simple_test():
    print("SIMPLE TEST")
    try:
        prover = test_prover()
        prover.send_command('declare r1: (A->B).')
        #Argument
        arg_a = Argument(
            prover,
            name='argA',
            conclusion='B',
            instructions=[
                'cut (A-> B) x1',
                'axiom r1.',
                'elim.',
                'next.',
                'axiom.'
            ]
        )

        arg_a.execute()
        print("HELLO")
        print(arg_a.name)
        print(arg_a.assumptions)
        print(arg_a.conclusion)
        print(arg_a.proof_term)
        print(arg_a.representation)
        print("SIMPLE TEST PASSED")
        prover.close()
        return
    except Exception:
        print ("SIMPLE TEST FAILED WITH EXCEPTION:")
        print(Exception)
        prover.close
        return

def chain_test():
    print("CHAIN TEST")
    try:
        prover = test_prover()
        # Two rules for chaining:
        prover.send_command('declare r1: (A->B).')
        prover.send_command('declare r2: (B->C).')

    #Arguments
        arg_a = Argument(
            prover,
            name='argA',
            conclusion='B',
            instructions=[
                'cut (A-> B) x1',
                'axiom r1.',
                'elim.',
                'next.',
                'axiom.'
            ]
        )

        arg_b = Argument(
            prover,
            name='argB',
            conclusion='C',
            instructions=[
                'cut (B-> C) x2',
                'axiom r2.',
                'elim.',
                'next.',
                'axiom.'
            ]
        )
        # Chain A2 and A1
        arg_combined = arg_a.chain(arg_b)
        if arg_combined:
            print(f"Chained argument '{arg_combined.name}' created with conclusion '{arg_combined.conclusion}'.")
            print(f"Assumptions of '{arg_combined.name}': {arg_combined.get_assumptions()}")
        print("CHAIN TEST PASSED")
        prover.close()
        
    except Exception as e:
        print("CHAIN TEST FAILED WITH EXCEPTION:")
        print(str(e))
        prover.close()
        return

def simple_undercut_test():
    print("SIMPLE UNDERCUT TEST")
    try:
        prover=test_prover()
        prover.send_command('declare r1: (A->B).')
        prover.send_command('declare r2: (C->~A).')
         # Argument A: Proves ~A using C
        arg_a = Argument(
            prover,
            name='argA',
            conclusion='~A',
            instructions=[
                'cut (C-> ~A) Fresh2',
                'axiom r2.',
                'elim.',
                'next.',
                'axiom.'
            ]
        )
        # Argument B: Proves B assuming A
        arg_b = Argument(
            prover,
            name='argB',
            conclusion='B',
            instructions=[
                'cut (A-> B) Fresh',
                'axiom r1.',
                'elim.',
                'next.',
                'axiom.'
            ]
        )

        final_arg = arg_a.undercut(arg_b)

        print(final_arg.name)
        print(final_arg.assumptions)
        print(final_arg.conclusion)
        print(final_arg.proof_term)
        print(final_arg.representation)
        print("SIMPLE UNDERCUT TEST PASSED")
        prover.close()
        return
    except Exception as e:
        print("SIMPLE UNDERCUT TEST FAILED WITH EXCEPTION:")
        print(e)
        prover.close()
        return

def focussed_undercut_test(reduce:bool=False):
    reduce = str(reduce).lower() in ("true", "1", "yes")
    print("FOCUSSED UNDERCUT TEST")
    try:
        prover=test_prover()
        prover.send_command('declare r1: (A->B).')
        prover.send_command('declare r2: (C->~A).')
        print("Declarations", prover.declarations)
         # Argument A: Proves ~A using C
        arg_a = Argument(
            prover,
            name='argA',
            conclusion='~A',
            instructions=[
                'elim.',
                'cut (~A) H4.',
                'cut (C-> ~A) Fresh2.',
                'axiom r2.',
                'elim.',
                'next.',
                'axiom.',
                'elim.',
                'axiom H4.'
            ]
        )
        
        # Argument B: Proves B assuming A
        arg_b = Argument(
            prover,
            name='argB',
            conclusion='B',
            instructions=[
                'cut (A-> B) Fresh',
                'axiom r1.',
                'elim.',
                'next.',
                'axiom.'
            ]
        )

        final_arg = arg_a.focussed_undercut(arg_b)

        print(final_arg.name)
        print(final_arg.assumptions)
        print(final_arg.conclusion)
        print(final_arg.proof_term)
        print(final_arg.representation)
        print("FOCUSSED UNDERCUT TEST PASSED")
        print(reduce)
        print(type(reduce))
        if reduce == True:
            print("Reducing argument")
            final_arg.reduce()
            print("Normalized Proof Term", final_arg.normal_form)
        prover.close()
        return final_arg
    except Exception as e:
        print("FOCUSSED UNDERCUT TEST FAILED WITH EXCEPTION:")
        print(type(e).__name__,e)
        print(traceback.format_exc())
        prover.close()
        return

def reinstatement_test():
    pass
    
def arg_pop_test(): #TODO
    prover=test_prover()
    prover.send_command('declare r1: (A->B).')
    prover.send_command('declare r2: (C->~A).')
     # Argument A: Proves ~A using C
    arg_a = Argument(
        prover,
        name='argA',
        conclusion='~A',
        instructions=[
            'cut (C-> ~A) Fresh2',
            'axiom r2.',
            'elim.',
            'next.',
            'axiom.'
        ]
    )
    # Argument B: Proves B assuming A
    arg_b = Argument( prover, name='argB', conclusion='B',
                      instructions=[ 'cut (A-> B) Fresh', 'axiom r1.',
                                     'elim.', 'next.', 'axiom.'  ] )
    
    
def undercut_test():
    print("UNDERCUT AND SUPPORT TEST")
    try:
        prover=test_prover()
        prover.send_command('declare r1: (A->B).')
        prover.send_command('declare r2: (C->~A).')
        prover.send_command('declare r3: (D->C).')
        # Argument A: Proves ~A using C
        arg_a = Argument(
            prover,
            name='argA',
            conclusion='~A',
            instructions=[
                'elim.',
                'cut (~A) H4.',
                'cut (C-> ~A) Fresh2.',
                'axiom r2.',
                'elim.',
                'next.',
                'axiom.',
                'elim.',
                'axiom H4.'
            ]
        )

        # Argument B: Proves B assuming A
        arg_b = Argument(
            prover,
            name='argB',
            conclusion='B',
            instructions=[
                'cut (A-> B) Fresh',
                'axiom r1.',
                'elim.',
                'next.',
                'axiom.'
            ]
        )

        arg_c = Argument(
            prover,
            name='argC',
            conclusion='C',
            instructions=[
                'cut (D-> C) Fresh5',
                'axiom r3.',
                'elim.',
                'next.',
                'axiom.'
            ]
        )
        arg_b.execute()
        print("ARGB")
        print(arg_b.assumptions)
        undercut_arg = arg_a.focussed_undercut(arg_b)
        final_arg = arg_c.support(undercut_arg)
        print(final_arg.name)
        print(final_arg.assumptions)
        print(final_arg.conclusion)
        print(final_arg.proof_term)
        print(final_arg.representation)
        print("UNDERCUT AND SUPPORT TEST PASSED")
        print("Reducing argument")
        final_arg.reduce()
        print("Normalized Proof Term", final_arg.normal_form)
        prover.close()
        return final_arg
    except Exception as e:
        print("UNDERCUT AND SUPPORT TEST FAILED WITH EXCEPTION:")
        print("Test failed", e)
        prover.close()
        return



def subargument_test(): #TODO
    print("SUBARGUMENT TEST")
    try:
        prover=test_prover()
        prover.send_command('declare r1: (A->B).')
        prover.send_command('declare r2: (B->C).')
        superargument = Argument(
            prover,
            name='SuperArg',
            conclusion='C',
            instructions=[
                'cut (B->C) x.',
                'axiom r2.',
                'elim.',
                'cut (A->B) y.',
                'axiom r1.',
                'elim.',
                'next.',
                'axiom.',
                'axiom.'
            ]
        )
        
        subargument = Argument(
            prover,
            name='SubArg',
            conclusion='B',
            instructions=[
                'cut (A->B) y.',
                'axiom r1.',
                'elim.',
                'next.',
                'axiom.'
            ]     
        )

        subargument_renamed = Argument(
            prover,
            name='SubArg_renamed',
            conclusion='B',
            instructions=[
                'cut (A->B) z.',
                'axiom r1.',
                'elim.',
                'next.',
                'axiom.'
            ]     
        )
        print(superargument.instructions)
        superargument.execute()
        subargument.execute()
        subargument_renamed.execute()
        print(parser.is_subargument(superargument.body, subargument.body))
        print(parser.is_subargument(superargument.body, subargument_renamed.body))
        print("SUBARGUMENT TEST PASSED")
        return
    except Exception:
        print("SUBARGUMENT TEST FAILED WITH EXCEPTION:")
        print(Exception)
        prover.close()
        return


def pop_subargument_test():
    print("POP SUBARGUMENT TEST")
    try:
        prover=test_prover()
        prover.send_command('declare r1: (A->B).')
        prover.send_command('declare r2: (B->C).')
        superargument = Argument(
            prover,
            name='SuperArg',
            conclusion='C',
            instructions=[
                'cut (B->C) x.',
                'axiom r2.',
                'elim.',
                'cut (A->B) y.',
                'axiom r1.',
                'elim.',
                'next.',
                'axiom.',
                'axiom.'
            ]
        )


        subargument = Argument(
            prover,
            name='SubArg',
            conclusion='B',
            instructions=[
                'cut (A->B) z.',
                'axiom r1.',
                'elim.',
                'next.',
                'axiom.'
            ]     
        )
        print(superargument.pop_arg(subargument))
        print("POP SUBARGUMENT TEST PASSED.")
        return
    except Exception as e:
        print("POP SUBARGUMENT TEST FAILED WITH EXCEPTION:")
        print(e)
        prover.close()
        return
def label_assumptions_test(test_argument=None, test_assumption_mapping = {'1':{'prop' : 'C' , 'index' : None, 'label' : None}, '2' :{'prop' : 'B_bar' , 'index' : None, 'label' : None}}):
    print("LABEL ASSUMPTIONS TEST")
    try:
        prover=test_prover()
        prover.send_command('declare first: (A->B).')
        if test_argument == None:
            print("USING DEFAULT ARGUMENT")
            test_argument = Argument(
                prover,
                name='SubArg',
                conclusion='B',
                instructions=[
                    'cut (A->B) z.',
                    'axiom first.',
                    'elim.',
                    'next.',
                    'axiom.'
                ]     
            )
        else:
            test_argument = eval(test_argument)
            print(test_argument)
            print(f"USING PROVIDED ARGUMENT {test_argument.name}")
        test_argument.execute()
        print("Testing own assumptions (should be IN)")
        print(test_argument.assumptions)
        for key in test_argument.assumptions:
            label = parser.label_assumption(test_argument.body, test_argument.assumptions[key]["prop"], test_argument.assumptions)
            print(f"The assumption '{test_argument.assumptions[key]['prop']}' is labeled as: {label[0]} on account of {label[1]}")

        print("Testing some other assumptions (should be UNDEC and OUT, respectively)")
        # test_assumption_mapping = {'1':'C', '2' :'B_bar'}
        print(test_assumption_mapping)
        for key in test_assumption_mapping:
            label = parser.label_assumption(test_argument.body, test_assumption_mapping[key]["prop"], test_argument.assumptions)
            print(f"The assumption '{test_assumption_mapping[key]}' is labeled as: {label[0]} on account of {label[1]}")
        print("LABEL ASSUMPTIONS TEST PASSED")
        return
    except Exception as e:
        print("LABEL ASSUMPTIONS TEST FAILED WITH EXCEPTION:")
        print(e)
        prover.close()
        return


def self_attack_test():
    print("SELF ATTACK TEST")
    try:
        prover=test_prover()
        prover.send_command('declare r1: (A->B).')
        prover.send_command('declare r2: (B->~A).')
        test_argument = Argument(
                prover,
                name='SelfAttack',
                conclusion='~A',
                instructions=[
                    'cut (B->~A) y.',
                    'axiom r2.',
                    'elim.',
                    'cut (A->B) z.',
                    'axiom r1.',
                    'elim.',
                    'next.',
                    'axiom.',
                    'axiom.'
                ]     
            )

        final_arg=test_argument
        final_arg.execute()
        #final_arg.enrich()
        print("SELF ATTACKING ARGUMENT")
        print(final_arg.name)
        print(final_arg.assumptions)
        print(final_arg.conclusion)
        print(final_arg.proof_term)
        print(final_arg.enriched_proof_term)
        print(final_arg.representation)

        final_arg.label_own_assumptions()
        print("LABELLING:")
        print(final_arg.assumptions)
        for key in  final_arg.assumptions:
            print(f"Is {key} a subargument?")
            print(is_subargument(final_arg.body, final_arg.assumptions[key]["label"][1]))
        final_arg.check_self_attacks()
        print(final_arg.assumptions)
        print("SELF ATTACK TEST PASSED")
        prover.close()
        return final_arg
        
    except Exception as e:
        print("SELF ATTACK TEST FAILED WITH EXCEPTION:")
        print(e)
        prover.close()
        return

def self_labelling_test(argument=None):
    if argument == None:
        print("No argument provided.")
        pass
    else:
        argument=eval(argument)
        argument.label_own_assumptions()
        print(argument.assumptions)
        for key in argument.assumptions:
              print(is_subargument(argument.body, argument[key]["label"][1]))
        return

def generate_instructions_test():
    prover = test_prover()
    prover.send_command('declare r1: (A->B).')
    prover.send_command('declare r2: (B->C).')
    prover.send_command
    prover.send_command('declare r3: (C->~A).')
    assumptions = {"1.2.1" : {"prop": "A", "index" : None, "label": None, "attackers" : None}}
    argument_term = "μthesis:B.<μx1:B.<r1||?1.2.1*x1>||thesis>"

    tree = grammar.parser.parse(argument_term)
    transformer=ProofTermTransformer()
    body = transformer.transform(tree)
    print("BODY",body)
    enricher=PropEnrichmentVisitor(assumptions = assumptions , axiom_props = prover.declarations)
    enriched_body = enricher.visit(body)
    print("ENRICHED BODY", enriched_body)
    generator = InstructionsGenerationVisitor()
    instructions = generator.return_instructions(enriched_body)
    #instructions = generator.instructions
    print("INSTRUCTIONS", instructions)
    test_argument = Argument(prover,
                            name = 'GeneratedArg',
                            conclusion = 'B',
                            instructions = instructions
                            )
    test_argument.execute()
    print(test_argument.proof_term)
    print("Moving to second argument.")
    argument_term = "μthesis:¬A.<μH1:¬A.<λH2:A.μH3:⊥.<H2||μ'issue:A.<μstash:A.<λH:A.μFresh:B.<r1||H*Fresh>||issue*μ'affine:B.<issue||stash>>||μ'adapter:A.<μFresh2:¬A.<r3||μsupport:C.<μalt1:C.<?1.1.2.1.2.1.1.1||support>||μ'alt2:C.<μFresh5:C.<r2||?1.1.2.1.2.1.2.1.2.1*Fresh5>||support>>*Fresh2>||μ'H1:¬A.<H1||adapter*_F_>>>>||H1>||thesis>"
    assumptions = {"1.1.2.1.2.1.1.1" : {"prop" : "C", 'index' : None, 'label' : None, 'attackers' : None}, '1.1.2.1.2.1.2.1.2.1' : {'prop' : 'B', 'index' : None, 'label' : None, 'attackers' : None}}
    tree = grammar.parser.parse(argument_term)
    body = transformer.transform(tree)
    print("BODY",body)
    enriched_body = enricher.visit(body)
    print("ENRICHED BODY", enriched_body)
    generator = InstructionsGenerationVisitor()
    instructions = generator.return_instructions(enriched_body)
    #instructions = generator.instructions
    print("INSTRUCTIONS", instructions)
    test_argument = Argument(prover,
                            name = 'GeneratedArg',
                            conclusion = '~A',
                            instructions = instructions
                            )
    test_argument.execute()
    print("PROOF TERM")
    print(test_argument.proof_term)
    test_argument.generate_proof_term()
    print("ENRICHED PROOF TERM")
    print(test_argument.enriched_proof_term)
    if argument_term == test_argument.proof_term.strip():
        print("Proof terms match, TEST PASSED")
    else:
        matcher = difflib.SequenceMatcher(None, argument_term, test_argument.proof_term)
        print("Proof terms don't match:")
        for tag, i1, i2, j1, j2 in matcher.get_opcodes():
            print(f"{tag} argument_term[{i1}:{i2}] -> test_argument.proof_term.strip()[{j1}:{j2}]")
    
            # Optionally, print the differing parts:
            if tag != 'equal':
                print("  argument_term:", argument_term[i1:i2])
                print("  test_argument.proof_term:", test_argument.proof_term[j1:j2])
    prover.close
    return

def proof_term_generation_test():
    prover = test_prover()
    prover.send_command('declare r1: (A->B).')
    prover.send_command('declare r2: (B->C).')
    prover.send_command('declare c2: (C->~A).')
    argument = Argument(
        prover,
        name='SuperArg',
        conclusion='C',
        instructions=[
            'cut (B->C) x.',
            'axiom r2.',
            'elim.',
            'cut (A->B) y.',
            'axiom r1.',
            'elim.',
            'next.',
            'axiom.',
            'axiom.'
            ]
        )

    argument.generate_proof_term()
    print(argument.enriched_proof_term)
    prover.close
    return


def machine_integration_test():
    print("MACHINE INTEGRATION TEST")
    prover = test_prover()
    try:
        st = prover.send_command('idtac.')
        # Snapshot fields present
        print("machine keys:", sorted(k for k in st.keys() if k != 'raw'))
        # No errors expected on a no-op
        print("errors:", st.get('errors'))
        print("warnings:", st.get('warnings'))
        print("notes:", st.get('notes'))
        if st.get('errors'):
            raise ProverError("Unexpected errors: " + "; ".join(st['errors']))
        # Decls synced (A,B,C,D: bool from setup_prover)
        expected = {"A":"bool","B":"bool","C":"bool","D":"bool"}
        ok = all(prover.declarations.get(k) == v for k, v in expected.items())
        print("decls synced:", ok, prover.declarations)
        # Proof term present (string from machine payload)
        print("proof-term present:", bool(st.get('proof-term')))
        # Goals list (should exist; count may vary)
        print("goals count:", len(st.get('goals', [])))
        # Optional: try a bad command to see message surfacing
        try:
            prover.send_command('axiom nonexistent_rule.')
        except ProverError as e:
            print("ProverError surfaced (as expected on bad axiom):", e)
        except Exception as e:
            print("Non-ProverError exception:", type(e).__name__, e)
        else:
            print("No ProverError for bad axiom name (prover may return a note).")
    finally:
        prover.close()

        
def logger_test():
    """Capture the fsp.wrapper logger and assert that notes are logged at INFO."""
    import io, logging, os
    # Ensure the wrapper’s logger uses INFO (ProverWrapper reads env at __init__)
    old_level = os.environ.get('FSP_LOGLEVEL')
    os.environ['FSP_LOGLEVEL'] = 'INFO'
    log = logging.getLogger('fsp.wrapper')
    buf = io.StringIO()
    h = logging.StreamHandler(buf)
    h.setLevel(logging.INFO)
    h.setFormatter(logging.Formatter('%(levelname)s:%(name)s:%(message)s'))
    log.addHandler(h)
    prover = None
    try:
        # Create the prover (setup_prover also sends lk. & declares A,B,C,D)
        prover = test_prover()
        # Clear any earlier messages so the assertion targets a fresh command
        buf.truncate(0); buf.seek(0)
        # Trigger a note again (logic switch emits a note in machine messages)
        prover.send_command('lk.')
        logged = buf.getvalue()
        print("Captured logs:\n", logged)
        if ('note:' in logged) and ('Current logic:' in logged):
            print("LOGGER TEST PASSED")
        else:
            print("LOGGER TEST FAILED: expected an INFO note with 'Current logic:'")
    finally:
        if prover:
            try: prover.close()
            except Exception: pass
        log.removeHandler(h)
        h.close()
        if old_level is None:
            del os.environ['FSP_LOGLEVEL']
        else:
            os.environ['FSP_LOGLEVEL'] = old_level

def interactive_mode_test():
    prover =setup_prover()
    interactive_mode(prover)
        

if __name__ == '__main__':
    argparser = argparse.ArgumentParser(description="Run specific or all tests.")
    argparser.add_argument(
        'test', 
        nargs='?', 
        default='all', 
        help="Name of the test to run, or 'all' to run all tests (default: all)"
    )
    argparser.add_argument(
        '--args',
        nargs='*',
        help="Optional positional arguments to pass to the test function"
    )
    argparser.add_argument(
        '--kwargs',
        nargs='*',
        help="Optional keyword arguments to pass to the test function (key=value pairs)"
    )
    args = argparser.parse_args()
    tests = {
        'declaration_test': declaration_test,
        'prop_enrichment_test': prop_enrichment_test,
        'simple_test': simple_test,
        'chain_test': chain_test,
        'simple_undercut_test': simple_undercut_test,
        'focussed_undercut_test': focussed_undercut_test,
        'undercut_test': undercut_test,
        'subargument_test': subargument_test,
        'generate_instructions_test': generate_instructions_test,
        'pop_subargument_test': pop_subargument_test,
        'label_assumptions_test': label_assumptions_test,
        'self_attack_test': self_attack_test,
        'self_labelling_test': self_labelling_test,
        'proof_term_generation_test': proof_term_generation_test,
        'interactive_mode_test': interactive_mode_test,
        'machine_integration_test': machine_integration_test,
        'lambda_rule_test': lambda_rule_test,
        'affine_mu_rule_test' : affine_mu_rule_test,
        'affine_mu_rule_nonaffine_test' : affine_mu_rule_nonaffine_test,
        'mu_general_rule_test': mu_general_rule_test,
        'mutilde_affine_defence_test' : mutilde_affine_defence_test,
        'graft_operator_test': graft_operator_test,
        'test_machine_stub': test_machine_stub,
        'logger_test': logger_test
    }
    # Parse keyword arguments if provided
    kwargs_dict = {}
    if args.kwargs:
        for kwarg in args.kwargs:
            if '=' in kwarg:
                key, value = kwarg.split('=', 1)
                kwargs_dict[key] = value
            else:
                print(f"Warning: Ignoring invalid keyword argument format: {kwarg}")
    if args.test == 'all':
        for test_name, test_func in tests.items():
            print(f"Running {test_name}...")
            test_func()
    elif args.test in tests:
        print(f"Running {args.test}...")
        tests[args.test](*(args.args or []), **kwargs_dict)

    else:
        print(f"Error: Test '{args.test}' not found. Available tests are:")
        for test_name in tests.keys():
            print(f"  - {test_name}")
   
