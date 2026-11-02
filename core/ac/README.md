# AC (Abstract Calculus) layer

Files
- ast.py
  - Node classes for proof terms and contexts: ProofTerm, Term, Context, Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI, Hyp, and the first-order constructors.
  - Invariants: props optional pre-enrichment; binders explicit via ID/DI names; type checks on constructor.
- prop.py
  - Structured propositions, first-order terms and sorts, with capture-avoiding substitution and alpha-equivalence.
- prop_render.py
  - Renders a proposition in Fellowship's command syntax (bracketed application).
- syntax.py
  - The one Lark grammar for propositions, sorts, first-order terms and proof terms; parse_proof_term().
- grammar.py
  - Compatibility surface: Grammar (with .parser) and ProofTermTransformer over syntax.py.
- resolve.py
  - Reclassifies the constructs Fellowship prints ambiguously (propositional or first-order).
- signature.py
  - Declared names and their kinds, as reported by Fellowship.
- alt_structure.py
  - Matchers for the term shapes the natural-language renderers name (alternatives, applications, defeasible warrants).
- instructions.py
  - InstructionsGenerationVisitor: lowers AST to Fellowship commands (cut/axiom/moxia/elim/next), with scaffolds for negation-elimination.

Usage
- Parse: core.ac.syntax.parse_proof_term(str) -> AST
- Lower to commands: InstructionsGenerationVisitor().return_instructions(node)
