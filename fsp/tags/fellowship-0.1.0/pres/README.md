# Presentation layer

Files
- gen.py
  - ProofTermGenerationVisitor: generates canonical .pres strings from AST.
- nl.py
  - Natural language rendering; multiple styles (argumentation/dialectical/intuitionistic).
- color.py
  - AcceptanceColoringVisitor: classify and color normalized proof terms (green/yellow/red).
- tree.py
  - AcceptanceTreeRenderer: acceptance trees with proof/NL labels.

Notes
- Operate on normalized ASTs for stable output (normalize via Argument.normalize()).
