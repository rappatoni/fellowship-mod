# Core (argument calculus)

Purpose
- Houses the argument calculus (AC) AST/parsing, core visitors/transformations (COMP), and debate operations (DC).
- Kept import-cycle free. Import submodules directly; do not re-export heavy modules in __init__.py.

Structure
- ac/: AST + grammar + instruction lowering to Fellowship commands.
- comp/: Visitors and transformations (enrichment, reduction, α-renaming).
- dc/: Arguments and debates: the Argument class, debate objects, the document graph, unfolding, strictness, citation, sharing and the type oracle.

Notes
- Import directly: core.ac.grammar, core.comp.reduce, core.dc.argument, etc.
- Presentation layer lives under pres/ and is imported lazily in core where needed to avoid cycles.
