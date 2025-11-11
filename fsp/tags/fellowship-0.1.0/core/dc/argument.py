import logging, copy
from typing import Optional, Any, Dict
from core.ac.grammar import Grammar, ProofTermTransformer
from core.ac.ast import ProofTerm, Mu, Mutilde, Goal, Laog, ID, DI
from core.ac.instructions import InstructionsGenerationVisitor
from core.comp.enrich import PropEnrichmentVisitor
from core.comp.reduce import ArgumentTermReducer
from core.dc.graft import graft_uniform, graft_single
from pres.gen import ProofTermGenerationVisitor
from pres.nl import (
    pretty_natural,
    natural_language_rendering,
    natural_language_dialectical_rendering,
    natural_language_argumentative_rendering,
)

logger = logging.getLogger(__name__)


class Argument:
    r"""Implementation of deductive arguments in \bar{\lamda}\mu\tilde{\mu} calculus. Atomic (counter-)arguments are terms of the (co-)intuitionistic (or (co-)minimal) fragment of the calculus enriched with default assumptions. Argumentation terms are constructed from atomic arguments and the undercut, rebut, support and chain operators.

       Argumentation terms are in normal form if they are \bar{\lamda}\mu\tilde{\mu} normal forms. Eta reduction is optional (TODO). 

Currently, a normalization of an argumentation Arg about issue A returns a non-affine term for A iff the top-level argument for A is skeptically accetable under admissible semantics (?) in the abstract argumentation framework induced by Arg.
    
    
    """
    
    def __init__(self, prover, name: str, conclusion: str, instructions: list = None, rendering = "argumentation", enrich : str = "PROPS", is_anti: bool = False):
        # TODO: some of these stil need type hints.
        self.prover = prover
        self.name = name
        self.conclusion = conclusion #Conclusion of the argument.
        self.instructions = instructions  # List of instruction strings
        self.assumptions = {}  # Dict of Assumptions: {goal_number : {prop : some_str, goal_index : some_int, label : some_str}
        self.proof_term = None  # To store the proof term if needed.
        self.enriched_proof_term = None # To store an enriched and/or rewritten proof term if needed.
        self.body: ProofTerm = None # parsed proof term created from proof_term
        self.rendering = rendering # Which rendering should be generated?
        self.enrich = enrich # Proof term enriched with additional type information
        self.representation = None # Natural language representation of the argument based on choice of rendering
        self.executed = False  # Flag to check if the argument has been executed
        self.normal_body           = None  # reduced AST (deep‑copy)
        self.normal_form           = None  # Normalized proof term.
        self.normal_representation = None  # NL rendering of normal form.
        # Explicit flag recording the user's intent to build a counterargument.
        # Why we need this:
        # - In recording mode we only have plain instruction strings; there is no AST
        #   to inspect when deciding between theorem and antitheorem.
        # - The prover currently prints anti‑theorems as μ … (not μ̃) at the top, so
        #   detecting “anti” from the parsed body root is unreliable.
        # - The start decision must be made before any tactic executes; tactics do not
        #   reliably encode counterargument intent.
        # - Machine state is ephemeral: you only get it after sending the start command,
        #   and you also need to replay the choice later (possibly in a new session).
        # - We need to persist and replay the choice across sessions (render/normalize/
        #   chain), and transient machine state is not available beforehand.
        self.is_anti = is_anti
        
    def execute(self) -> None:
        """Sends the sequence of instructions corresponding to an argument (possibly generated from its body) to the Fellowship prover and populates the argument's proof term, body, assumptions, and renderings from the prover output. Can be used for type-checking (TODO).
        """
        logger.info("Executing argument '%s'.", self.name)
        if self.executed:
            logger.warning("Argument '%s' has already been executed.", self.name)
            return
        if self.instructions == None:
            if self.body == None:
                raise Exception("Argument instructions and body missing.")       
            else:
                self.enrich_props()
                generator = InstructionsGenerationVisitor()
                self.instructions = generator.return_instructions(self.body)
        # Decide whether to start a theorem or an antitheorem.
        # We cannot reliably infer “anti” from available data at this point:
        # - Recording mode has no AST yet: during scripts/interactive recording we only
        #   collect strings; when we later construct the Argument (at “end argument”),
        #   there may still be no body to inspect. Without an explicit flag we would
        #   always default to theorem.
        # - Printed anti‑theorem proof‑terms are μ … at the top with current OCaml
        #   changes, not μ̃; so “detect anti by Mutilde root” is unreliable after parse.
        # - Tactics are not a signal: you must choose theorem vs antitheorem before any
        #   tactic runs, and not every counterargument will start with a distinguishing
        #   instruction.
        # - Machine state is ephemeral: you only get it after sending the start command,
        #   and you also need to replay the choice later (possibly in a new session).
        # Hence we carry user intent via `is_anti`, set by “start counterargument …”.
        logger.debug("Body '%s'", self.body)
        logger.debug("ISMUTILDE? '%s'", isinstance(self.body, Mutilde))
        if (self.body and isinstance(self.body, Mutilde)) or getattr(self, "is_anti", False):
            prop = self.body.prop if (self.body and isinstance(self.body, Mutilde)) else self.conclusion
            logger.info("Starting antitheorem '%s' for issue '%s'", self.name, prop)
            start_cmd = f'antitheorem {self.name} : ({prop}).'
        else:
            logger.info("Starting theorem '%s' for issue '%s'", self.name, self.conclusion)
            start_cmd = f'theorem {self.name} : ({self.conclusion}).'
        output = self.prover.send_command(start_cmd)
        # Execute each instruction
        for instr in self.instructions:
            if instr.startswith('tactic '):
                # Handle custom tactic invocation within argument execution
                parts = instr.split()
                tactic_name = parts[1]
                tactic_args = parts[2:]  # Remaining parts are arguments to the tactic
                output = self.prover.execute_tactic(tactic_name, *tactic_args)
                #output += output_instr
            else:
                output = self.prover.send_command(instr.strip() + '.')
                logger.trace("Prover output: %s", output)
        # Capture the assumptions (open goals)
        self._parse_proof_state(output)
        # Parse proof term
        grammar = Grammar()
        parsed = grammar.parser.parse(self.proof_term)
        transformer = ProofTermTransformer()
        self.body = transformer.transform(parsed)
        #Generate natural language representation
        if self.rendering == "argumentation":
            self.representation = pretty_natural(self.body, natural_language_argumentative_rendering)
        elif self.rendering == "dialectical":
            self.representation = pretty_natural(self.body, natural_language_dialectical_rendering)
        elif self.rendering == "intuitionistic":
            self.representation = pretty_natural(self.body, natural_language_rendering)
        # Discard the theorem to prevent closing it (since it may have open goals)
        self.prover.send_command('discard theorem.')
        logger.info("Argument '%s' executed.", self.name)
        logger.info("")  # spacer before proof term artifact
        logger.info("Argument term: '%s'.", self.proof_term)
        logger.info("")  # spacer after proof term artifact
        if self.enrich == "PROPS":    
            self.enrich_props()
            self.generate_proof_term()
        self.executed = True

    
    @staticmethod
    def _normalize_pt_to_unicode(pt_ascii: str) -> str:
        """Map Fellowship's ASCII fallbacks to the Unicode tokens your
        parser expects.  The examples observed were:
          • 'μ'   encoded as ";:"   (two chars)
          • 'μ̃'  (mu-tilde) encoded as ";:'" or ";:\'"
          • 'λ'   encoded as '\\' (backslash)
        Adjust if your printer uses slightly different sentinels.
        """
        s = pt_ascii
        # unify the two spellings we saw for mu-tilde first
        s = s.replace(";:\'", "μ'" )
        s = s.replace(";:'",  "μ'" )   # plain apostrophe variant
        # plain mu next (do *after* mu-tilde)
        s = s.replace(";:",   "μ")
        # lambda
        s = s.replace("\\",   "λ")
        s = s.replace("~", "¬")
        s = s.replace("false", "⊥")
        return s
    
    @staticmethod
    def _unquote(atom: Any) -> Any:
        """Strip surrounding double quotes from atoms like '"A"'."""
        if isinstance(atom, str) and len(atom) >= 2 and atom[0] == '"' and atom[-1] == '"':
            return atom[1:-1]
        return atom

    def _parse_proof_state(self, proof_state: Dict[str, Any]) -> None:
        """
        Build {goal_number: {"prop": some_str, "label": None}} from the
        machine payload dict produced by the wrapper.

        - goal_number := value of (meta ...)
        - prop        := value of (active-prop ...) (quotes removed)
        """
        self.proof_term = self._normalize_pt_to_unicode(proof_state.get('proof-term').strip('"'))
        res = {}
        goals = proof_state.get('goals')
    

        # Normalize: 'goals' may be a single `(goal ...)` list or a list of them.
        def _as_goal_list(gval):
            if isinstance(gval, list) and gval and gval[0] == 'goal':
                return [gval]
            if isinstance(gval, list):            
                #return [g for g in gval if isinstance(g, dict)]
                return gval
            return []

        goal_entries = _as_goal_list(goals)


        for g in goal_entries:
        #for g in goals:
            attrs = {}
            for item in g[1:]:
                if isinstance(item, list) and len(item) == 2:
                    k, v = item
                    attrs[k] = self._unquote(v)
            meta = self._unquote(attrs.get('meta')) if 'meta' in attrs else None
            prop = self._unquote(attrs.get('active-prop')) if 'active-prop' in attrs else None
            if meta and prop is not None:
                res[meta] = {"prop": prop, "label": None}
        self.assumptions = res

    # def extract_assumptions(self, output):
    #     #print("Match conclusion to assumption.")
    #     # Parse the output to find open goals (assumptions)
    #     preassumptions = []
    #     # temporarily ignore past proof steps. THIS IS DANGEROUS. While idtac should not change anything, it is risky to change the prover state merely to get a copy of the last output again.
    #     temp_output = self.prover.send_command(f'idtac.', 1)
        
    #     # Find the number of goals
    #     num_goals = self.prover.parse_proof_state(temp_output)['goals']
    #     if num_goals > 0 :
    #         # num_goals = int(goals_match.group(1))
    #         # Extract each goal
    #         goal_pattern_proof = re.compile(r'\|-----\s*(\d\.*.*\d*)\s*(\s*[^:]*:[^\s]*\s*)*\*:([^\s]*)')
    #         goal_pattern_refu = re.compile(r'\*:([^\s]*)\s*(\s*[^:]*:[^\s]*\s*)*\|-----\s*(\d\.*.*\d*)')

    #         match = goal_pattern_proof.search(temp_output)
    #         #print("matching")
    #         #print(match)
    #         antimatch = goal_pattern_refu.search(temp_output)
    #         # print("HERE!")
    #         print(" Match, Antimatch", match, antimatch)
    #         preassumptions = [(match.group(3).strip(), 0, match.group(1).strip())] if match else [(antimatch.group(1).strip()+'_bar', 0, antimatch.group(3).strip())]
    #         i=1
    #         print(" Preassumptions", preassumptions)
    #         #Test the while loop, may be buggy.
    #         while i<num_goals:
    #             #print(i)
    #             #print('WARNING')
    #             temp_output = self.prover.send_command('next.')
    #             #print(temp_output)
    #             plus_match = goal_pattern_proof.search(temp_output)
    #             plus_antimatch = goal_pattern_refu.search(temp_output)
    #             print("Test")
    #             test = goal_pattern_refu.search('2 goals yet to prove! \n *:A \n issue:A \n |-----  1.1.2 ')
    #             print(test)
    #             print(plus_match)
    #             print('ANTI')
    #             print(plus_antimatch)
    #             #print('append attempt')
    #             #print(plus_antimatch.group(1).strip()+'_bar')
    #             if plus_match:
    #                 preassumptions.append((plus_match.group(3).strip(), i, plus_match.group(1).strip()))
    #             else:
    #                 preassumptions.append((plus_antimatch.group(1).strip()+'_bar', i, plus_antimatch.group(3).strip()))
    #             #print(assumptions)
    #             i+=1
    #     assumptions = {}
    #     assumptions = {number.strip() : { "prop" : assumption_text.strip(), "index" : goal_index, "label" : None } for (assumption_text, goal_index, number) in preassumptions}
    #     print("Assumptions extracted:")
    #     print(assumptions)
    #     return assumptions

    def enrich_props(self) -> None:
        """
        Use PropEnrichmentVisitor with assumption_mapping = self.assumptions
        and axiom_props = self.axiom_props to fill .prop for each node.
        """
        logger.info("Enriching argument '%s'.", self.name)
        logger.debug("Declarations: %r", self.prover.declarations)
        visitor = PropEnrichmentVisitor(assumptions=self.assumptions,
                                        axiom_props=self.prover.declarations)
        self.body = visitor.visit(self.body)
        logger.info("Argument '%s' enriched.", self.name)
        return

    def generate_proof_term(self) -> None:
        """
        Use ProofTermGenerationVisitor to generate the string representation of an enriched proof term.
        """
        logger.info("Starting proof term generation for enriched argument '%s'", self.name)
        visitor = ProofTermGenerationVisitor()
        self.body = visitor.visit(self.body)
        self.enriched_proof_term = self.body.pres
        logger.info("Finished proof term generation for enriched argument '%s'", self.name)
        logger.info("")  # spacer before enriched proof term
        logger.info("Enriched proof term: '%s'.", self.enriched_proof_term)
        logger.info("")  # spacer after enriched proof term
        return
             
    def pop_arg(self, subargument):
        """Deprecated: Stash the current argument and pop a subargument. Currently all argumentation functionality is bottom-up, proceeding from a known conclusion. Investigating a top-down exploratory mode is future work. """
        import parser
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
            popped_arg = adapter.chain(self)
            final_arg = popped_arg.chain(subargument, True)
            return final_arg

    def match_conclusion_assumptions(self, conclusion: str, assumptions: dict) -> Optional[str]:
        matching_assumption = None
        for key in assumptions:
            logger.debug("Checking assumption key=%s", key)
            if conclusion.replace("~", "¬") in assumptions[key]["prop"]:
                matching_assumption = key
                logger.debug("Match found: %s", matching_assumption)
                break
        return matching_assumption


    def chain(self, other_argument: "Argument", close: bool = False, name: Optional[str] = None) -> "Argument":
        """ Chains the current argument to another argument by using it to prove an assumption of the other argument. Automatically finds the matching assumption (assuming there is only one)."""
        #TODO: Implement type checking for resulting argument.
        # Check if this argument's conclusion matches any of the other argument's assumptions
        if not self.executed:
            self.execute()
        if not other_argument.executed:
            other_argument.execute()
        # Combine the instructions and navigate to the correct goal
        combined_name = name or f"{self.name}_{other_argument.name}"
        logger.debug("Combined name: '%s'", combined_name)
        combined_conclusion = other_argument.conclusion
        logger.debug("Combined conclusion: '%s'", combined_conclusion)
        #combined_instructions = []
        combined_body = graft_uniform(other_argument.body, self.body)
        logger.debug("Assumptions (self, other): %r ; %r",
                     self.assumptions, other_argument.assumptions)
        enricher = PropEnrichmentVisitor(assumptions=other_argument.assumptions,
                                        axiom_props=self.prover.declarations)
        ptgenerator = ProofTermGenerationVisitor()
        generator = InstructionsGenerationVisitor()
        logger.debug("Enriching ")
        combined_body = enricher.visit(combined_body)
        logger.debug("Generating proof term")
        ptgenerator.visit(combined_body)
        combined_proof_term = combined_body.pres
        logger.debug("Proof term: '%s'", combined_proof_term)
        combined_instructions = generator.return_instructions(combined_body)
        # Optionally close available goals using assumptions propagated from other to self:
        if close==True:
            # This is an ugly and brittle hack. Will figute out something better.
            combined_instructions.extend([f'axiom.', f'next.']*(len(self.assumptions)+2))
        # Create a new Argument instance
        combined_argument = Argument(self.prover, combined_name, combined_conclusion, combined_instructions)
        combined_argument.body = combined_body
        logger.debug("Instructions '%s'", combined_instructions)
        # Execute the combined argument
        combined_argument.execute()
        
        return combined_argument

    def focussed_undercut(self, other_argument: "Argument", *, name: Optional[str], on: Optional[str] = None) -> "Argument":
        from core.ac.ast import Mu, Mutilde, Goal, Laog, ID, DI
        # Ensure both arguments are executed
        if not self.executed:
            self.execute()
        if not other_argument.executed:
            other_argument.execute()
        # Resolve attacked issue: default to the dual of our conclusion
        issue = on or self.conclusion

        #Find the assumption that is to be attacked. The uniiform graft method is used, so it does not matter which assumption is chosen if there are multiple ones with the same prop.
        attacked_assumption = None
        for key, info in other_argument.assumptions.items():
            if info["prop"].strip() == issue:
                logger.debug("Found attacked assumption key=%s", key)
                attacked_assumption = key
                break
        if attacked_assumption is None:
            raise ValueError(
                f"focussed_undercut: target assumption '{issue}' not found in other argument (exact match required)"
            )
        issue = other_argument.assumptions[attacked_assumption]["prop"]
        logger.debug("Attacked issue: %s", issue)
        if not name:
            raise ValueError("focussed_undercut requires a result name: call focussed_undercut(..., name='...')")
        # BUG: variable names may clash; consider adding a fresh‑name helper.
        adaptercontext = Mutilde(DI("aff", issue), issue, Goal("2", issue), Laog("3", issue))
        adapterterm    = Mu(ID("aff", issue), issue, Goal("1", issue), ID("alt", issue))
        adapterbody    = Mu(ID("alt", issue), issue, adapterterm, adaptercontext)
        adapter_arg = Argument(self.prover, f'adapter_{self.name}_{other_argument.name}', issue)
        logger.debug("Building adapter body")
        adapter_arg.body = adapterbody
        adapter_arg.execute()
        logger.debug("Adapter argument: '%s'", adapter_arg.proof_term)
        #print("Adapter_arg proof term",adapter_arg.proof_term)
        #print("Other arg proof term", other_argument.proof_term)
        #adapted_argument = adapter_arg.chain(other_argument)
        adapted_attacker = self.chain(adapter_arg)
        logger.debug("Adapted attacker constructed: %s", adapted_attacker.proof_term)
        final_argument = adapted_attacker.chain(other_argument, name=name)
        return final_argument

    def basic_support(self, other_argument: "Argument", name: Optional[str] = None, on: Optional[str] = None) -> "Argument":
        """
        Basic Support:
          - If supporting a Goal in other_argument:
              μ alt.< μ aff.<?1||alt> ∥ μ' aff.< supporter || alt > >
          - If supporting a Laog in other_argument:
              μ' alt.< μ aff.< alt || ?1 > ∥ μ' aff.< alt || supporter > >
        
        Steps:
          - execute both arguments if needed
          - resolve supported issue (by `on` or match self.conclusion)
          - find matching assumption in `other_argument`
          - build and execute adapter argument with the above pattern
          - adapted_supporter = self.chain(adapter)
          - result = adapted_supporter.chain(other_argument, name=name)
        """
        from core.ac.ast import Mu, Mutilde, Goal, Laog, ID, DI
        # Ensure both arguments are executed
        if not self.executed:
            self.execute()
        if not other_argument.executed:
            other_argument.execute()

        # Resolve supported issue (proposition string)
        issue = on or self.conclusion
        logger.debug("Requested support issue: %s", issue)

        # Find the supported assumption in other_argument (exact match)
        supported_key = None
        for key, info in other_argument.assumptions.items():
            if info["prop"].strip() == issue:
                supported_key = key
                break
        if supported_key is None:
            raise ValueError(
                f"basic_support: target assumption '{issue}' not found in other argument (exact match required)"
            )
        # Use canonical prop string from the assumptions dict
        issue = other_argument.assumptions[supported_key]["prop"]
        logger.debug("Supporting issue resolved to: %s (key=%s)", issue, supported_key)

        # Determine which side we are supporting in the target: Goal vs Laog
        target_is_laog = bool(getattr(other_argument, "is_anti", False))

        # Enforce supporter kind matches the supported side under pureness:
        # - supporting a Goal requires a Mu supporter
        # - supporting a Laog requires a Mutilde supporter
        if not target_is_laog and not isinstance(self.body, Mu):
            raise TypeError("basic_support: supporting a Goal requires a Mu supporter")
        if target_is_laog and not isinstance(self.body, Mutilde):
            raise TypeError("basic_support: supporting a Laog requires a Mutilde supporter")

        # Two-step construction to avoid substituting both ?1 and supporter:
        # Step 1: build adapter1 = μ' aff.< supporter || some >
        #         with 'some' of the kind captured by the next step's 'alt' binder.
        if not target_is_laog:
            # supporting a Goal → next 'alt' is Mu (captures Laogs) → some must be Laog
            supporter_placeholder = Goal("s", issue)   # only this should be replaced now
            some_node = Laog("some", issue)            # preserved; captured in step 2

            adapter1_body = Mutilde(
                DI("aff", issue), issue,
                supporter_placeholder,
                some_node
            )
        else:
            # supporting a Laog → next 'alt' is Mutilde (captures Goals) → some must be Goal
            supporter_placeholder = Laog("s", issue)   # only this should be replaced now
            some_node = Goal("some", issue)            # preserved; captured in step 2
            adapter1_body = Mutilde(
                DI("aff", issue), issue,
                supporter_placeholder,
                some_node
            )

        # adapter1_body = Mutilde(
        #     DI("aff", issue), issue,
        #     supporter_placeholder,
        #     some_node
        # )
        adapter1_name = f"adapter_support_step1_{self.name}_{other_argument.name}"
        adapter1_arg = Argument(self.prover, adapter1_name, issue)
        logger.debug("Building adapter1 (basic support step1) on issue '%s'", issue)
        adapter1_arg.body = adapter1_body
        adapter1_arg.execute()
        logger.debug("Adapter1 executed: %s", adapter1_arg.proof_term)

        # Graft supporter into adapter1 (only the supporter placeholder is replaced)
        adapted1 = self.chain(adapter1_arg)
        logger.debug("After step1: adapted1 constructed: %s", adapted1.proof_term)

        # Step 2: build adapter2 with outer 'alt' binder and a single 'second' hole
        if not target_is_laog:
            # Support a Goal:
            #   μ alt.< μ aff.<?1||alt> ∥ second >
            adapter2_term = Mu(
                ID("aff", issue), issue,
                Goal("1", issue),
                ID("alt", issue)
            )
            second_hole = Laog("second", issue)  # will be replaced by adapted1; its inner 'some' becomes ID(alt)
            adapter2_body = Mu(
                ID("alt", issue), issue,
                adapter2_term,
                second_hole
            )
        else:
            # Support a Laog:
            #   μ' alt.< μ aff.< alt || ?1 > ∥ second >
            adapter2_term = Mu(
                ID("aff", issue), issue,
                ID("alt", issue),
                Laog("1", issue)
            )
            second_hole = Laog("second", issue)  # will be replaced by adapted1; its inner 'some' becomes DI(alt)
            adapter2_body = Mutilde(
                DI("alt", issue), issue,
                adapter2_term,
                second_hole
            )

        adapter2_name = f"adapter_support_step2_{self.name}_{other_argument.name}"
        adapter2_arg = Argument(self.prover, adapter2_name, issue)
        logger.debug("Building adapter2 (basic support step2) on issue '%s'", issue)
        adapter2_arg.body = adapter2_body
        adapter2_arg.execute()
        logger.debug("Adapter2 executed: %s", adapter2_arg.proof_term)

        # Insert adapted1 into adapter2 (uniform graft replaces the single Laog('second'))
        adapted2 = adapted1.chain(adapter2_arg)
        logger.debug("After step2: adapted2 constructed: %s", adapted2.proof_term)

        # Finally graft the fully adapted supporter into the supported argument
        final_argument = adapted2.chain(other_argument, name=name)
        return final_argument
    
    def get_assumptions(self) -> list[str]:
        if not self.executed:
            self.execute()
        return [self.assumptions[key]["prop"] for key in self.assumptions]

    def get_conclusion(self) -> str:
        return self.conclusion

    def support(self, other_argument: "Argument") -> "Argument":
        #Akin to chain but instead of filling in an assumption, provide an alternative proof for the assumption. Could be generalized to provide an alternative proof for any intermediate derivation of an argument.
        from core.ac.ast import Mu, Mutilde, Goal, Laog, ID, DI
        if not self.executed:
            self.execute()
        if not other_argument.executed:
            other_argument.execute()
                # Find if other_argument has an assumption that matches self.conclusion

        matching_assumption = self.match_conclusion_assumptions(self.conclusion, other_argument.assumptions)
        issue = other_argument.assumptions[matching_assumption]["prop"]
        logger.debug("Issue: %s", issue)
        # The adapter

        adaptercontext = Mutilde(DI("aff2", issue), issue, Goal("2", issue), ID("alt1", issue))
        adapterterm = Mu(ID("aff1", issue), issue, Goal("1"), ID("alt1",issue))
        adapterbody = Mu(ID("alt1", issue), issue, adapterterm, adaptercontext)
        alternative_proof_adapter = Argument(self.prover, f'supports_on_{other_argument.assumptions[matching_assumption]["prop"]}', other_argument.assumptions[matching_assumption]["prop"])
        logger.debug("Building adapter body")
        alternative_proof_adapter.body = adapterbody

                                             #[f'cut ({other_argument.assumptions[matching_assumption]["prop"]}) support.', f'cut ({other_argument.assumptions[matching_assumption]["prop"]}) alt1.', f'next.', f'axiom support.', f'cut ({other_argument.assumptions[matching_assumption]["prop"]}) alt2.', f'next.', f'axiom support.'])
        alternative_proof_adapter.execute()
        adapted_argument =  alternative_proof_adapter.chain(other_argument)
        logger.debug("Adapter argument constructed: %s", adapted_argument.proof_term)
        final_argument = self.chain(adapted_argument)
        return final_argument

    def normalize(self, enrich: bool = True) -> ProofTerm:
        """Compute and cache the normal form *(body, term, rendering) without mutating *self.body*."""
        if not self.executed:
            self.execute()
        #if self.normal_body is not None:
         #   return self.normal_body

        # 1. deep‑copy then reduce
        red_ast = copy.deepcopy(self.body)
        red_ast = ArgumentTermReducer().reduce(red_ast)

        # 2. optionally enrich props/types
        if enrich:
            red_ast = PropEnrichmentVisitor(assumptions=self.assumptions,
                                             axiom_props=self.prover.declarations).visit(red_ast)

        # 3. generate textual proof term
        red_ast = ProofTermGenerationVisitor().visit(red_ast)
        self.normal_body = red_ast
        self.normal_form = red_ast.pres

        # 4. natural‑language rendering
        style = {
            "argumentation": natural_language_argumentative_rendering,
            "dialectical":   natural_language_dialectical_rendering,
            "intuitionistic": natural_language_rendering,
        }[self.rendering]
        self.normal_representation = pretty_natural(red_ast, style)
        return self.normal_body

    def render(self, normalized: bool = False) -> Optional[str]:
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

    def reduce(self) -> None:
        """Normalise and print proof term + NL; keep originals intact."""
        logger.info("Starting argument reduction for '%s'", self.name)
        self.normalize()
        logger.info("Reduction finished for argument '%s'", self.name)
        logger.info("")  # spacer before proof term
        logger.info("Normal‑form proof term: %s", self.normal_form)
        logger.info("")  # spacer after proof term
