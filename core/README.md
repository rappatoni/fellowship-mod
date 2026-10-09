# Core (argument calculus)

Purpose
- Houses the argument calculus (AC) AST/parsing, transformations, labelling and evaluation (COMP), and arguments and debates (DC).
- Kept import-cycle free. Import submodules directly; do not re-export heavy modules in __init__.py.

Structure
- ac/: AST, propositions and the grammar, instruction lowering to Fellowship commands.
- comp/: Visitors and transformations (enrichment, α-renaming), ADF labelling, label-guided evaluation and its normaliser.
- dc/: Arguments and debates: the Argument class, debate objects, the document graph, unfolding, strictness, citation, sharing and the type oracle.
- logging_util.py: the TRACE log level.

Notes
- Import directly: core.ac.syntax, core.comp.evaluate, core.dc.argument, etc.
- Presentation layer lives under pres/ and is imported lazily in core where needed to avoid cycles.
