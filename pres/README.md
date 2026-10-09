# Presentation layer

Files
- gen.py
  - ProofTermGenerationVisitor: generates canonical .pres strings from AST, labelled sites included.
- nl.py
  - Natural language rendering in the styles argumentation, dialectical, intuitionistic, vanilla and pruefschema.
- pattern_render.py
  - Renderers for the named term shapes (core/ac/alt_structure.py) that nl.py uses.
- decorations.py
  - The `decorate NAME : "template".` command and rendering propositions through decorations.
- tree.py
  - AcceptanceTreeRenderer: acceptance trees with proof/NL labels, coloured
    by the grounded ADF labels passed in (core/comp/adf_label.py).
