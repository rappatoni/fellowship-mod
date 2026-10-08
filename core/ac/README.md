# AC (Abstract Calculus) layer

Files
- ast.py
  - Node classes for proof terms and contexts: ProofTerm, Term, Context, Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI, Hyp.
  - Invariants: props optional pre-enrichment; binders explicit via ID/DI names; type checks on constructor.
- syntax.py
  - The one Lark grammar for propositions, sorts, first-order terms and proof terms; parse_proof_term().
- grammar.py
  - Compatibility shim: Grammar (with .parser) and ProofTermTransformer, kept for older call sites.
- instructions.py
  - InstructionsGenerationVisitor: lowers AST to Fellowship commands (cut/axiom/moxia/elim/next), with scaffolds for negation-elimination.

Usage
- Parse: core.ac.syntax.parse_proof_term(str) -> AST
- Lower to commands: InstructionsGenerationVisitor().return_instructions(node)
