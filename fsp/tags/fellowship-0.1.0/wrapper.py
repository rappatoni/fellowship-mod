import pexpect
import re
from lark import Lark, Transformer, v_args
import parser
import warnings
from pathlib import Path
import argparse
import copy

class ProverWrapper:
    def __init__(self, prover_cmd):
        self.prover = pexpect.spawn(prover_cmd, encoding='utf-8', timeout=5)
        self.prover.expect('fsp <')
        self.custom_tactics = {}
        self.arguments = {}  # Dictionary to store arguments by name
        self.declarations = {}

    def send_command(self, command, silent=0):
        stripped = command.strip()
        if stripped.startswith("declare "):
            # 1) parse out the declared names & type
            names, declared_type = self._parse_declare_line(stripped)
            
            # 2) send line to the prover
            try:
                self.prover.sendline(command)
                self.prover.expect('fsp <')
                output = self.prover.before
                # 3) extract names of successful declarations.
                success_names = self._extract_successfully_defined_names(output)
                # 4) For each declarations, check if it was successful and add it to the dictionary
                for nm in names:
                    if nm in success_names:
                        self.declarations[nm] = declared_type
                    else:
                        warnings.warn("Declaration not recorded.")

            except pexpect.EOF:
              print("Error: Unexpected EOF received from the prover process.")
              self.close()
              raise
            except pexpect.TIMEOUT:
              print("Error: Prover did not respond in time.")
              self.close()
              raise
          
        else:
            try:
              self.prover.sendline(command)
              self.prover.expect('fsp <')
              output = self.prover.before
              if silent == 0:
                  print(output)
              return output
            except pexpect.EOF:
              print("Error: Unexpected EOF received from the prover process.")
              self.close()
              raise
            except pexpect.TIMEOUT:
              print("Error: Prover did not respond in time.")
              self.close()
              raise

    def _parse_declare_line(self, line):
        """
        Takes something like: "declare A,B:bool." 
        or "declare test: (A)."
        returns ( ["A","B"], "bool" ) or (["test"], "(A)")
        We do not store them in self.declarations yet.
        """
        # remove leading 'declare '
        decl_str = line[len("declare"):].strip()
        # remove trailing '.' if any
        if decl_str.endswith('.'):
            decl_str = decl_str[:-1].strip()

        if ':' not in decl_str:
            return [], None  # no parse

        names_part, type_part = decl_str.split(':', 1)
        names_part = names_part.strip()
        type_part = type_part.strip()
        names = [n.strip() for n in names_part.split(',')]
        return (names, type_part)

    def _extract_successfully_defined_names(self, output):
        """
        Fellowship typically outputs lines like:
           > A defined.
           > A,B defined.
        We'll parse them with a regex capturing the part after '> '
        up to ' defined.'
        Return a set of all declared names found in these lines.
        """
        success_pattern = re.compile(r'>\s+([^>]+?)\s+defined\.')
        # e.g. match:  "> A defined." => group(1) = 'A'
        # or  "> A,B defined." => group(1) = 'A,B'
        results = set()
        for match in success_pattern.finditer(output):
            names_str = match.group(1).strip()
            # if there's a comma, e.g. "A,B", split
            for nm in names_str.split(','):
                results.add(nm.strip())
        return results

    def parse_proof_state(self, output):
        # Extract proof term
        proof_term_match = re.search(r'Proof term:\s*\n\s*(.*?)\n', output, re.DOTALL)
        proof_term = proof_term_match.group(1) if proof_term_match else None

        # Extract natural language explanation
        nl_match = re.search(r'Natural language:\s*\n\s*(.*?)\n\s*done', output, re.DOTALL)
        natural_language = nl_match.group(1) if nl_match else None

        # Extract goals
        goals_matches = re.findall(r'(\d+) goal[s]? yet to prove!', output)
        goals_match = goals_matches[-1] if len(goals_matches)>0 else None
        #goals = int(goals_match.group(1)) if goals_match else 0
        goals = int(goals_match) if goals_match else 0

        # Extract current goal
        current_goal_match = re.search(r'\|-----\s*([\d\.])\s*\n\s*([^\r\n]*)', output, re.DOTALL)
        current_goal = current_goal_match.group(2).strip() if current_goal_match else None

        return {
            'proof_term': proof_term,
            'natural_language': natural_language,
            'goals': goals,
            'current_goal': current_goal,
        }
    
    def register_custom_tactic(self, name, function):
        # Register a custom tactic with its associated function
        self.custom_tactics[name] = function

    def execute_tactic(self, tactic_name, *args):
        # Execute a tactic, either a custom or predefined tactic
        if tactic_name in self.custom_tactics:
            return self.custom_tactics[tactic_name](self, *args)
        else:
            return f"Error: Tactic '{tactic_name}' is not defined."

    def register_argument(self, argument):
        self.arguments[argument.name] = argument

    def get_argument(self, name):
        return self.arguments.get(name)

    def close(self):
        self.prover.sendline('quit.')
        self.prover.close


def check_for_errors(output,errors):
    for error in errors:
        if error in output:
            # print('Error detected: {error}.')
            raise Exception(f'Error detected: {error}')
            # return f'Error detected: {error}.'
        else:
            pass


def pop(prover, x, y, closed=True, errors=['This is not trivial. Work some more.']):

    instructions = [f'cut ({y}) stash.', f'next.', f'cut ({x}) affine.',f'next.', f'axiom.']
    # if closed == True: 
    #     instructions = [f'cut ({x}->{y}) stash.', f'elim H.',f'next.',f'elim.',f'axiom.',f'cut ({x}) affine.',f'axiom.',f'axiom.']
    # else:
    #     instructions = [f'cut ({x}->{y}) stash.', f'elim H.',f'next.',f'elim.',f'next.',f'cut ({x}) affine.',f'next.',f'axiom.']
        
    output = ''
    counter = 0
    for instruction in instructions:
        try:
            output += prover.send_command(instruction)
            check_for_errors(output, errors)
        except Exception as X:
            print(X)
            print('Undoing and aborting.')
            #Trying to undo the tactic application. Note that Fellowship's undo appears to be buggy so this will not always work.
            i=0
            while i<counter:
                output += prover.send_command(f'undo.')
                i+=1
            return output
        if instruction not in [f'next.',f'idtac.',f'prev.']:
            # Note that undo does not seem to work with idtac, but there should be no reason to use idtac in a tactic anyway.
            counter += 1
            
    return output

def execute_script(prover, script_path):
    recording = False
    current_argument = None
    with open(script_path, 'r') as script_file:
        for line in script_file:
            command = line.strip()
            if command and not command.startswith('#'):
                if command.startswith('start argument '):
                    if recording:
                        print("Already recording an argument. Please end the current recording first.")
                        continue
                    parts = command.split(' ', 3)
                    if len(parts) < 4:
                        print("Invalid command. Use: start argument name conclusion")
                        continue
                    name = parts[2]
                    conclusion = parts[3].strip()
                    current_argument = {
                        'name': name,
                        'conclusion': conclusion,
                        'instructions': []
                    }
                    recording = True
                    print(f"Started recording argument '{name}' with conclusion '{conclusion}'.")
                elif command == 'end argument':
                    if not recording:
                        print("Not currently recording an argument.")
                        continue
                    # Create and execute the argument
                    arg = Argument(
                        prover,
                        name=current_argument['name'],
                        conclusion=current_argument['conclusion'],
                        instructions=current_argument['instructions']
                    )
                    arg.execute()
                    # Store the argument for later use
                    prover.register_argument(arg)
                    print(f"Argument '{arg.name}' saved with conclusion '{arg.conclusion}'.")
                    # Reset recording state
                    recording = False
                    current_argument = None
                elif recording:
                    # Record the command as part of the argument
                    if command.startswith('tactic '):
                        # Handle tactic commands within recording
                        parts = command.split()
                        tactic_name = parts[1]
                        tactic_args = parts[2:]  # Remaining parts are arguments to the tactic
                        output = prover.execute_tactic(tactic_name, *tactic_args)
                        current_argument['instructions'].append(command)
                    else:
                        # Execute and record other commands
                        output = prover.send_command(command)
                        current_argument['instructions'].append(command)
                else:
                    # Handle commands outside of recording
                    if command.startswith('tactic '):
                        # Handle tactic commands outside of recording
                        parts = command.split()
                        tactic_name = parts[1]
                        tactic_args = parts[2:]  # Remaining parts are arguments to the tactic
                        output = prover.execute_tactic(tactic_name, *tactic_args)
                        #print(output)

                    elif command.startswith("reduce "):
                        reduce_argument_cmd(prover, command.split(maxsplit=1)[1])
                    elif command.startswith("render-nf "):
                        render_argument_cmd(prover, command.split(maxsplit=1)[1], True)
                    elif command.startswith("render "):
                        render_argument_cmd(prover, command.split(maxsplit=1)[1], False)
                    elif command.startswith("normalize "):
                        name = command.split(maxsplit=1)[1]
                        arg = prover.get_argument(name)
                        if arg: arg.normalize(); print("normal form stored in .normal_form")
                        else:   print(f"Argument '{name}' not found.")
                    elif command.startswith('chain '):
                        # Handle chaining of arguments
                        # Format: chain arg1 arg2
                        parts = command.split()
                        if len(parts) != 3:
                            print("Invalid chain command. Use: chain arg1 arg2")
                            continue
                        arg1_name = parts[1]
                        arg2_name = parts[2]
                        arg1 = prover.get_argument(arg1_name)
                        arg2 = prover.get_argument(arg2_name)
                        if arg1 and arg2:
                            combined_arg = arg1.chain(arg2)
                            if combined_arg:
                                prover.register_argument(combined_arg)
                                print(f"Arguments '{arg1_name}' and '{arg2_name}' chained into '{combined_arg.name}'.")
                            else:
                                print("Failed to chain arguments.")
                        else:
                            print("One or both arguments not found.")
                    else:
                        # Execute other commands
                        output = prover.send_command(command)
                        # print(output)


def interactive_mode(prover):
    recording = False
    current_argument = None
    try:
        while True:
            try: 
                command = input('Enter command (or "exit" to quit): ').strip()
            except EOFError:
                print("\nEOFError: No input detected. Exiting interactive mode.")
                break
            # Remove trailing dots and whitespace
            #command = command.rstrip('.').strip()
            if command.lower() in ['exit', 'quit']:
                break
            elif command.startswith("reduce "):
                reduce_argument_cmd(prover, command.split(maxsplit=1)[1])
            elif command.startswith("render-nf "):
                    render_argument_cmd(prover, command.split(maxsplit=1)[1], True)
            elif command.startswith("render "):
                    render_argument_cmd(prover, command.split(maxsplit=1)[1], False)
            elif command.startswith("normalize "):
                name = command.split(maxsplit=1)[1]
                arg = prover.get_argument(name)
                if arg: arg.normalize(); print("normal form stored in .normal_form")
                else:   print(f"Argument '{name}' not found.")

            elif command.startswith('start argument '):
                # Parse the start argument command
                if recording:
                    print("Already recording an argument. Please end the current recording first.")
                    continue
                parts = command.split(' ', 3)
                if len(parts) < 4:
                    print("Invalid command. Use: start argument name conclusion")
                    continue
                name = parts[2]
                conclusion = parts[3].strip()
                current_argument = {
                    'name': name,
                    'conclusion': conclusion,
                    'instructions': []
                }
                recording = True
                print(f"Started recording argument '{name}' with conclusion '{conclusion}'.")
                output = prover.send_command(f'theorem {name} : ({conclusion}).')
                # print(output)
            elif command == 'end argument':
                if not recording:
                    print("Not currently recording an argument.")
                    continue
                prover.send_command('discard theorem.')
                # Create and execute the argument
                arg = Argument(
                    prover,
                    name=current_argument['name'],
                    conclusion=current_argument['conclusion'],
                    instructions=current_argument['instructions']
                )
                arg.execute()
                # Store the argument for later use
                prover.register_argument(arg)
                print(f"Argument '{arg.name}' saved with conclusion '{arg.conclusion}'.")
                # Reset recording state
                recording = False
                current_argument = None
            elif recording:
                # Record the command as part of the argument
                if command:
                    if command.startswith('tactic '):
                        # Handle custom tactic invocation during recording
                        parts = command.split()
                        tactic_name = parts[1]
                        tactic_args = parts[2:]  # Remaining parts are arguments to the tactic
                        output = prover.execute_tactic(tactic_name, *tactic_args)
                        # print(output)
                        # Record the tactic command as part of the instructions
                        current_argument['instructions'].append(command)
                    else:
                        # Execute the command and record it
                        output = prover.send_command(command)
                        # print(output)
                        current_argument['instructions'].append(command)
            else:
                # Normal command execution
                if command.startswith('argument '):
                    # Handle argument definitions in one go
                    # Parse the argument definition
                    # Format: argument name conclusion instructions
                    # Example: argument argA "A" "axiom axA;"
                    parts = command.split(' ', 2)
                    if len(parts) < 3:
                        print("Invalid argument definition. Use: argument name conclusion instructions")
                        continue
                    name = parts[1]
                    rest = parts[2]
                    try:
                        conclusion_part, instructions_part = rest.split('"', 2)[1], rest.split('"', 2)[2]
                        conclusion = conclusion_part.strip()
                        instructions = [instr.strip() for instr in instructions_part.strip().split(';') if instr.strip()]
                        arg = Argument(prover, name, conclusion, instructions)
                        arg.execute()
                        print(f"Argument '{name}' defined with conclusion '{conclusion}'.")
                    except Exception as e:
                        print(f"Error parsing argument: {e}")
                elif command.startswith('tactic '):
                    # Handle custom tactic invocation outside of recording
                    parts = command.split()
                    tactic_name = parts[1]
                    tactic_args = parts[2:]  # Remaining parts are arguments to the tactic
                    output = prover.execute_tactic(tactic_name, *tactic_args)
                    # print(output)
                elif command.startswith('chain'):
                    # Handle chaining of arguments
                    # Format: chain arg1 arg2
                    parts = command.split()
                    if len(parts) != 3:
                        print("Invalid chain command. Use: chain arg1 arg2")
                        continue
                    arg1_name = parts[1]
                    arg2_name = parts[2]
                    arg1 = prover.get_argument(arg1_name)
                    arg2 = prover.get_argument(arg2_name)
                    if arg1 and arg2:
                        combined_arg = arg1.chain(arg2)
                        if combined_arg:
                            prover.register_argument(combined_arg)
                            print(f"Arguments '{arg1_name}' and '{arg2_name}' chained into '{combined_arg.name}'.")
                        else:
                            print("Failed to chain arguments.")
                    else:
                        print("One or both arguments not found.")
                else:
                    # Execute the command normally
                    output = prover.send_command(command)
                    # print(output)
    finally:
        prover.close()
                
class Argument:
    def __init__(self, prover, name: str, conclusion: str, instructions: list = None, rendering = "argumentation", enrich : str = "PROPS" ):
        self.prover = prover
        self.name = name
        self.conclusion = conclusion
        self.instructions = instructions  # List of instruction strings
        self.assumptions = {}  # Dict of Assumptions: {goal_number : {prop : some_str, goal_index : some_int, label : some_str, attackers : some_list_of_arguments}}
        self.labelling = False #Flag to check if argument has been labelled.
        self.proof_term = None  # To store the proof term if needed.
        self.enriched_proof_term = None # To store an enriched and/or rewritten proof term if needed.
        self.body = None # parsed proof term created from proof_term
        self.rendering = rendering # Which rendering should be generated?
        self.enrich = enrich
        self.representation = None # Natural language representation of the argument based on choice of rendering
        self.executed = False  # Flag to check if the argument has been executed
        #self.coloring = ## Colors the accepted, rejected and undecided parts of the argument starting from assumptions.
        self.attacks = {}  #Dict of self-attacks with corresponding subargument
        self.normal_body           = None  # reduced AST (deep‑copy)
        self.normal_form           = None  # textual proof term
        self.normal_representation = None  # NL rendering of normal form
        
    def execute(self):
        if self.executed:
            print(f"Argument '{self.name}' has already been executed.")
            return
        if self.instructions == None:
            if self.body == None:
                raise Exception("Argument instructions and body missing.")       
            else:
                generator = parser.InstructionsGenerationVisitor()
                self.instructions = generator.return_instructions(body)
        # Start the theorem
        output = self.prover.send_command(f'theorem {self.name} : ({self.conclusion}).')
        #print(output)
        # Execute each instruction
        for instr in self.instructions:
            if instr.startswith('tactic '):
                # Handle custom tactic invocation within argument execution
                parts = instr.split()
                tactic_name = parts[1]
                tactic_args = parts[2:]  # Remaining parts are arguments to the tactic
                output_instr = self.prover.execute_tactic(tactic_name, *tactic_args)
                output += output_instr
            else:
                output_instr = self.prover.send_command(instr.strip() + '.')
                output += output_instr
            #print(output)
        # Capture the assumptions (open goals)
        self.assumptions = self.extract_assumptions(output)
        #self.assumptions = number.strip() : { "prop" : assumption_text.strip(), "index" : goal_index } for (assumption_text, goal_index, number) in self.assumptions}
        # Capture the proof_term (use of idtac dangerous!)
        output = self.prover.send_command("idtac.")
        # Type check if appropriate:
        if self.proof_term:
            if self.proof_term == self.prover.parse_proof_state(output)['proof_term']:
                print("Type check passed.")
            else:
                raise Exception("Type Error: input body and generated proof term don't match")     
        else:
            self.proof_term = self.prover.parse_proof_state(output)['proof_term']
            print(self.proof_term)
        # Parse proof term
        grammar = parser.Grammar()
        # parser = Lark(proof_term_grammar, start='start')
        parsed = grammar.parser.parse(self.proof_term)
        transformer = parser.ProofTermTransformer()
        self.body = transformer.transform(parsed)
        #Generate natural language representation
        if self.rendering == "argumentation":
            self.representation = parser.pretty_natural(self.body, parser.natural_language_argumentative_rendering)
        elif self.rendering == "dialectical":
            self.representation = parser.pretty_natural(self.body, parser.natural_language_dialectical_rendering)

        elif self.rendering == "intuitionistic":
            self.representation = parser.pretty_natural(self.body, parser.natural_language_rendering)
        # Discard the theorem to prevent closing it (since it may have open goals)
        self.prover.send_command('discard theorem.')
        if self.enrich == "PROPS":    
            self.enrich_props()
            self.generate_proof_term()
        
        self.executed = True

    def extract_assumptions(self, output):
        #print("Match conclusion to assumption.")
        # Parse the output to find open goals (assumptions)
        preassumptions = []
        # temporarily ignore past proof steps. THIS IS DANGEROUS. While idtac should not change anything, it is risky to change the prover state merely to get a copy of the last output again.
        temp_output = self.prover.send_command(f'idtac.', 1)
        
        # Find the number of goals
        num_goals = self.prover.parse_proof_state(temp_output)['goals']
        if num_goals > 0 :
            # num_goals = int(goals_match.group(1))
            # Extract each goal
            goal_pattern_proof = re.compile(r'\|-----\s*(\d\.*.*\d*)\s*(\s*[^:]*:[^\s]*\s*)*\*:([^\s]*)')
            goal_pattern_refu = re.compile(r'\*:([^\s]*)\s*(\s*[^:]*:[^\s]*\s*)*\|-----\s*(\d\.*.*\d*)')
            test_match= goal_pattern_refu.findall("1.2.2.3 *:A")
            print("TEST MATCH")
            print(test_match)
            print("DONE TESTING")

            match = goal_pattern_proof.search(temp_output)
            #print("matching")
            #print(match)
            antimatch = goal_pattern_refu.search(temp_output)
            # print("HERE!")
            print(" Match, Antimatch", match, antimatch)
            preassumptions = [(match.group(3).strip(), 0, match.group(1).strip())] if match else [(antimatch.group(1).strip()+'_bar', 0, antimatch.group(3).strip())]
            i=1
            print(" Preassumptions", preassumptions)
            #Test the while loop, may be buggy.
            while i<num_goals:
                #print(i)
                #print('WARNING')
                temp_output = self.prover.send_command('next.')
                #print(temp_output)
                plus_match = goal_pattern_proof.search(temp_output)
                plus_antimatch = goal_pattern_refu.search(temp_output)
                print("Test")
                test = goal_pattern_refu.search('2 goals yet to prove! \n *:A \n issue:A \n |-----  1.1.2 ')
                print(test)
                print(plus_match)
                print('ANTI')
                print(plus_antimatch)
                #print('append attempt')
                #print(plus_antimatch.group(1).strip()+'_bar')
                if plus_match:
                    preassumptions.append((plus_match.group(3).strip(), i, plus_match.group(1).strip()))
                else:
                    preassumptions.append((plus_antimatch.group(1).strip()+'_bar', i, plus_antimatch.group(3).strip()))
                #print(assumptions)
                i+=1
        assumptions = {}
        assumptions = {number.strip() : { "prop" : assumption_text.strip(), "index" : goal_index, "label" : None } for (assumption_text, goal_index, number) in preassumptions}
        print("Assumptions extracted:")
        print(assumptions)
        return assumptions

    def enrich_props(self):
        """
        Use PropEnrichmentVisitor with assumption_mapping = self.assumptions
        and axiom_props = self.axiom_props to fill .prop for each node.
        """
        from parser import PropEnrichmentVisitor
        visitor = PropEnrichmentVisitor(assumptions=self.assumptions,
                                        axiom_props=self.prover.declarations)
        self.body = visitor.visit(self.body)
        return

    def generate_proof_term(self):
        """
        Use ProofTermGenerationVisitor to generate the string representation of an enriched proof term.
        """
        from parser import ProofTermGenerationVisitor
        visitor = ProofTermGenerationVisitor()
        self.body = visitor.visit(self.body)
        self.enriched_proof_term = self.body.pres
        return
        

    def label_own_assumptions(self):
        for key in self.assumptions:
            self.assumptions[key]["label"] = parser.label_assumption(self.body, self.assumptions[key]["prop"],self.assumptions)
            self.labelling = True
        # self.labelling = {assumption_text.strip() :  parser.label_assumption(self.body, assumption_text.strip(), self.assumptions)for (assumption_text, goal_index, number) in self.assumptions}
        return 

    def check_self_attacks(self):
        """Checks consistency up to (i.e. without) logical inference."""
        # Self-attacks are cycles of length 1. This implies that the argument is hopeless/can never be accepted.
        # Generally, a proposition A is skeptically acceptable in a set S, if there is an exhaustive (all possible counterarguments refuted) non-cyclical argument from S for A.
        # It is credulously acceptable if there is an exhaustive even-cyclical argument from S for A.
        # Compute fixpoint of supporting arguments (upper bound) counterarguments (lower bound).
        # Generate all normal-form supporting arguments (counterarguments);
        # Start with atomic arguments (alternative proofs, internally no affine mus/mutildes).
        # Label, check for cycles.
        # Then generate all counterarguments for each argument (alternative proofs, undercut).
        # Label, check for cycles.
        # Iteratively update sets of accepted supporting arguments (counterarguments).
        # If not labelled, label.

        # If not activity-labelled, activity label.
        # If any active assumption is out, the argument is self-attacking.
        # Start naively: all assumptiosn are active. No supports, no attacks.

        #An alternative approach to self-attacks: restrict interaction of mu/lambda with defaults. No affine mu/lambda abstractions over default contradictions (with no other open assumptions) in atomic arguments (See notes).
        if self.labelling == True:
            print("ALREADY LABELLED")
            pass
        else:
            print("labelling...")
            self.label_own_assumptions()
        # for assumption in labelling:
        #     if assumption[value]=="OUT":
        #         # Get subargument:
                
        #         subargument = self.body
        #         self.attacks[assumption]=
        #     else:
        print(self.assumptions)
        for key in self.assumptions:
            if self.assumptions[key]["label"][0]=="OUT":
                self.assumptions[key]["attackers"] = "Sth"
        return

    def generate_cycle(self):
        """ Converts a self-attacking argument into a cycle (for an arbitrary number of cycles, uses the first independent cycles)."""
        # Independent vs dependent cycles: a contradiction corresponds to an independent cycle, i.e. the contrary/negation of which assumption can be proven initially? The contradiction may then be used to introduce arbitrary additional cycles. These are dependent cycles. Use type of contradiction to identify independent contradictions.
        pass
         
    def pop_arg(self, subargument):
        """ Stash the current argument and pop a subargument. TODO: implement blocking."""
        # Execute args if necessary.
        if not self.executed:
            self.execute()
        if not subargument.executed:
            subargument.execute()
        print(self.body)
        print(subargument.body)
        if parser.is_subargument(self.body, subargument.body) == False:
            raise Exception("Not a subargument.")
        else:
            # Get the correct assumption (wlog we use the first assumption of the subargument).
            for key in subargument.assumptions:
                if subargument.assumptions[key]["index"] == 0:
                    pop_assumption = subargument.assumptions[key]["prop"]
            adapter = Argument(self.prover, f'pop_assumption_{pop_assumption}_of_{self.name}', pop_assumption, [f'tactic pop {pop_assumption} {self.conclusion} False'])
            print("Executing Adapter")
            adapter.execute()
            print(adapter.assumptions)
            print(self.conclusion)
            popped_arg = self.chain(adapter)
            final_arg = popped_arg.chain(subargument, True)
            return final_arg

    def match_conclusion_assumptions(self, conclusion : str, assumptions : dict):
        matching_assumption = None
        for key in assumptions:
            print(key)
            if conclusion.replace("~","¬"  ) in assumptions[key]["prop"]:
                matching_assumption = key
                print("Match found:")
                print(matching_assumption)
                break
        return matching_assumption


    def chain(self, other_argument, close=False):
        """ Chains the current argument to another argument by using it to prove an assumption of the other argument. Automatically finds the matching assumption (assuming there is only one)."""
        #TODO: Implement type checking for resulting argument.
        # Check if this argument's conclusion matches any of the other argument's assumptions
        if not self.executed:
            self.execute()
        if not other_argument.executed:
            other_argument.execute()
        # Find if other_argument has an assumption that matches self.conclusion
        matching_assumption = self.match_conclusion_assumptions(self.conclusion, other_argument.assumptions)
        print(matching_assumption)
        if matching_assumption is not None:
            # Combine the instructions and navigate to the correct goal
            combined_name = f"{self.name}_{other_argument.name}"
            combined_conclusion = other_argument.conclusion
            combined_instructions = []

            # First, execute other_argument.instructions
            combined_instructions.extend(other_argument.instructions)
            print(" HERE TOO")
            # Then, execute self_argument.instructions, but first navigate to the correct goal
            # Use 'next' commands to navigate to the goal at matching_assumption[1]
            goal_index = other_argument.assumptions[matching_assumption]["index"]
            num_goals = len(other_argument.assumptions)
            #print("GOAL INDEX")
            #print(goal_index)
            # The prover starts at the first goal (index 0), so we need to move to goal_index
            #Bug! The fsp " next"  command appears to be buggy and behaves in non-reproducable ways across arguments. The same sequence of instructions therefore does not always yield the same proof. Have to reimplement this in a principled way to replace this hack. Chain will become "Graft" and act on Argument bodies (ASTs) avoiding any ambiguity.
            if goal_index > 0:
                combined_instructions.extend(['next'] * goal_index)

            # Now append own instructions 
            combined_instructions.extend(self.instructions)

            # Optionally close available goals using assumptions propagated from other to self:
            if close==True:
                # This is an ugly and brittle hack. Will figute out something better.
                combined_instructions.extend([f'axiom.', f'next.']*(len(self.assumptions)+2))
            # Create a new Argument instance
            combined_argument = Argument(self.prover, combined_name, combined_conclusion, combined_instructions)
            # Execute the combined argument
            combined_argument.execute()
            return combined_argument
        else:
            print(f"Cannot chain arguments '{self.name}' and '{other_argument.name}' because the conclusions and assumptions do not match.")
            return None

    def undercut(self, other_argument):
        # Forward, exploratory version of undercut were the attacked argument is stashed.
        if not self.executed:
            self.execute()
        if not other_argument.executed:
            other_argument.execute()
        #Find the assumption that is to be attacked. This does not currently account for multiple assumptions of the same type.
        attacked_assumption = None
        for key in other_argument.assumptions:
            if self.conclusion.strip('~') in other_argument.assumptions[key]["prop"]:
                attacked_assumption = key
                break
        #The conclusion of the new argument is going to be the conclusion of the current argument.
        #print(attacked_assumption)

        # check if attack is not blocked. Or not? Allow endless oscillation? Even cycles should be acceptable? Determining if an assumption is blocked is itself a provability-complete problem, if arbitrary assumptions are permitted. Two solutions: a) no blocking for attacks, only block popping defeated subproofs; b) arbitrarily cut off blocking at e.g. equality. I.e. we only look for instances of the blocked formula itself. We choose option a) for now, but option b) may be required to ensure termination (or some other tie breaking mechanism).
        
        #invert the conclusion:
        issue = self.conclusion.strip('~')
        temp_concl_arg = Argument(self.prover, f'undercuts_on_{other_argument.assumptions[attacked_assumption]["prop"]}', self.conclusion, [f'elim.', f'cut ({issue}) issue', f'tactic pop {other_argument.assumptions[attacked_assumption]["prop"]} {other_argument.conclusion}', f'cut (~{issue}) adapter', f'next', f'elim', f'axiom'])
        temp_concl_arg.execute()
        #Chain the other argument to the adapter.
        adapted_argument = other_argument.chain(temp_concl_arg, close=True)
        print("ADAPTED")
        #print(adapted_argument)
        final_argument = self.chain(adapted_argument, close=True)
        return final_argument


    def focussed_undercut(self, other_argument):
        # This is a focussed, backwards reasoning version of undercut where stashing of the attacked argument is not necessary.
        if not self.executed:
            self.execute()
        if not other_argument.executed:
            other_argument.execute()
        #Find the assumption that is to be attacked. This does not currently account for multiple assumptions of the same type.
        attacked_assumption = None
        for key in other_argument.assumptions:
            if self.conclusion.strip('~') in other_argument.assumptions[key]["prop"]:
                attacked_assumption = key
                break
            #The conclusion of the new argument will be the conclusion of the attacked argument.

        #invert the conclusion:
        issue = self.conclusion.strip('~')
                adapter_arg = Argument(self.prover, f'undercuts_on_{other_argument.assumptions[attacked_assumption]["prop"]}', issue, [f'cut ({issue}) alt.', f'cut ({issue}) aff', f'next', f'axiom alt', f'cut (~{issue}) aff', f'next', f'elim'])
        adapter_arg.execute()
        #The chain method is brittle and relies on the buggy "next" command of fsp. It needs reimplementation based on grafts on the argument body.
        adapted_argument = adapter_arg.chain(other_argument)
        print("ADAPTED")
        print(adapted_argument)
        final_argument = self.chain(adapted_argument)
        return final_argument
    
    def get_assumptions(self):
        if not self.executed:
            self.execute()
        return [self.assumptions[key]["prop"] for key in self.assumptions]

    def get_conclusion(self):
        return self.conclusion

    def support(self, other_argument):
        #Akin to chain but instead of filling in an assumption, provide an alternative proof for the assumption. Could be generalized to provide an alternative proof for any intermediate derivation of an argument.
        if not self.executed:
            self.execute()
        if not other_argument.executed:
            other_argument.execute()
                # Find if other_argument has an assumption that matches self.conclusion

        matching_assumption = self.match_conclusion_assumptions(self.conclusion, other_argument.assumptions)
        # The adapter 
        alternative_proof_adapter = Argument(self.prover, f'supports_on_{other_argument.assumptions[matching_assumption]["prop"]}', other_argument.assumptions[matching_assumption]["prop"], [f'cut ({other_argument.assumptions[matching_assumption]["prop"]}) support.', f'cut ({other_argument.assumptions[matching_assumption]["prop"]}) alt1.', f'next.', f'axiom support.', f'cut ({other_argument.assumptions[matching_assumption]["prop"]}) alt2.', f'next.', f'axiom support.'])
        alternative_proof_adapter.execute()
        print("ADAPTARG")
        adapted_argument =  alternative_proof_adapter.chain(other_argument)
        final_argument = self.chain(adapted_argument)
        return final_argument

    def normalize(self, enrich: bool = True):
        """Compute and cache the normal form without mutating *self.body*."""
        if not self.executed:
            self.execute()
        #if self.normal_body is not None:
         #   return self.normal_body

        # 1. deep‑copy then reduce
        red_ast = copy.deepcopy(self.body)
        red_ast = parser.ArgumentTermReducer().reduce(red_ast)

        # 2. optionally enrich props/types
        if enrich:
            red_ast = parser.PropEnrichmentVisitor(assumptions=self.assumptions,
                                             axiom_props=self.prover.declarations).visit(red_ast)

        # 3. generate textual proof term
        red_ast = parser.ProofTermGenerationVisitor().visit(red_ast)
        self.normal_body = red_ast
        self.normal_form = red_ast.pres

        # 4. natural‑language rendering
        style = {
            "argumentation": parser.natural_language_argumentative_rendering,
            "dialectical":   parser.natural_language_dialectical_rendering,
            "intuitionistic": parser.natural_language_rendering,
        }[self.rendering]
        self.normal_representation = parser.pretty_natural(red_ast, style)
        return self.normal_body

    def render(self, normalized: bool = False):
        """Return NL rendering.  If *normalized* is True, ensure normal form
        is computed first."""
        if normalized:
            #if self.normal_representation is None:
             #   self.normalize()
            return self.normal_representation
        else:
            if self.representation is None:
                self.execute()          # fills original representation
            return self.representation

    def reduce(self):
        """Normalise and print proof term + NL; keep originals intact."""
        self.normalize()
        print("— reduction finished —\n")
        print("Normal‑form proof term:\n", self.normal_form)
        print("\nNatural language:\n", self.normal_representation)

# ---------------------------------------------------------------------------
#  CLI helper commands                                                       
# ---------------------------------------------------------------------------

def setup_prover():
    prover = ProverWrapper('./fsp')
    prover.register_custom_tactic('pop', pop)
    # Switch to classical logic
    prover.send_command('lk.')
    # Declare some booleans to work with.
    prover.send_command('declare A,B,C,D:bool.')
    return prover


def reduce_argument_cmd(prover: ProverWrapper, name: str):
    arg = prover.get_argument(name)
    if not arg:
        print(f"Argument '{name}' not found.")
        return
    print(f"Reducing argument {arg.name} with proof term {arg.proof_term}")
    arg.reduce()

def render_argument_cmd(prover: ProverWrapper, name: str, normalized=False):
    arg = prover.get_argument(name)
    if not arg:
        print(f"Argument '{name}' not found.")
        return
    if normalized==True:
        print(f"Rendering argument {arg.name} in normal form.")
    else:
        print(f"Rendering argument {arg.name}")
    print(arg.render(normalized=normalized))

def main() -> None:
    ap = argparse.ArgumentParser(
        prog='fellowship-wrapper',
        description='Run Fellowship prover in interactive or batch mode')
    g = ap.add_mutually_exclusive_group(required=True)
    g.add_argument('--interactive', action='store_true',
                   help='start an interactive Fellowship REPL')
    g.add_argument('--script', metavar='FILE',
                   help='execute commands in FILE (same grammar as interactive mode)')
    args = ap.parse_args()

    prover = setup_prover()             # ⇐ creates the ProverWrapper, 
                                        #    registers pop, declares A,B,C,D …

    if args.interactive:                # --- REPL -----------------
        interactive_mode(prover)

    else:                               # --- batch ----------------
        script = Path(args.script).expanduser()
        if not script.is_file():
            ap.error(f'script file {script} does not exist')
        execute_script(prover, script)

if __name__ == '__main__':
    main()

        
