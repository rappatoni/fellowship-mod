# Presentation layer

Files
- gen.py
  - ProofTermGenerationVisitor: generates canonical .pres strings from AST.
- nl.py
  - Natural language rendering; multiple styles (argumentation/dialectical/intuitionistic).
- tree.py
  - AcceptanceTreeRenderer: acceptance trees with proof/NL labels, coloured
    by the grounded ADF labels passed in (core/comp/adf_label.py); the
    shape-based colouring was retired on 2026-09-16.

Notes
- Operate on normalized ASTs for stable output (normalize via Argument.normalize()).
