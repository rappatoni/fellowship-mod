# AC (Abstract Calculus) layer

Files
- ast.py
  - Node classes for proof terms and contexts: ProofTerm, Term, Context, Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI, Hyp.
  - Invariants: props optional pre-enrichment; binders explicit via ID/DI names; type checks on constructor.
- grammar.py
  - Lark-based parser for Fellowship ASCII proof terms.
  - Exposes Grammar (with .parser) and ProofTermTransformer to build AST nodes.
- instructions.py
  - InstructionsGenerationVisitor: lowers AST to Fellowship commands (cut/axiom/moxia/elim/next), with scaffolds for negation-elimination.

Usage
- Parse: Grammar().parser.parse(str) -> ProofTermTransformer().transform(tree)
- Lower to commands: InstructionsGenerationVisitor().return_instructions(node)
