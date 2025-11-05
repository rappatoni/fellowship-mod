# Fellowship Prover Mod

This repo contains an experimental wrapper for the [Fellowship prover](https://github.com/theoremprover-museum/fellowship). The wrapper implements a variant of logical (aka deductive) argumentation for classical logic by way of control operators. An exposition of the motivation and theory behind the system can be found [here](https://www8.cs.fau.de/ext/teaching/wise2024-25/oberseminar/slides-rapp.pdf).

## Usage

Prerequisites
- OCaml toolchain (to build the Fellowship prover)
- Python 3.10+ and pytest

Build the prover
- cd fsp/tags/fellowship-0.1.0
- make

Run the Python wrapper CLI
- From the same directory (fsp/tags/fellowship-0.1.0):
  - Show help: python -m wrap.cli --help
  - Run a .fspy script: python -m wrap.cli --script tests/counterarguments_and_undercut.fspy
  - Strict mode (treat parse/machine issues as errors): python -m wrap.cli --script tests/normalize_render.fspy --strict

Run tests
- cd fsp/tags/fellowship-0.1.0
- pytest
- Legacy runner (developer utility): python tests.py [test_name] within fsp/tags/fellowship-0.1.0.

Features
- Argument lifecycle: execute an argument against the prover, normalize its proof term, and render it.
- Debate operations: undercut (counterargument) is supported; support and rebut are planned.
- Presentations:
  - Proof-term rendering (ASCII .pres)
  - Natural language renderings (argumentation/dialectical/intuitionistic styles)
  - Acceptance coloring (green/yellow/red)
  - Acceptance trees (proof-term labels or NL labels)

Repository layout
- fsp/
  - tags/
    - fellowship-0.1.0/  ← main working tag (build and run here)
      - core/
        - ac/
          - ast.py — AST for proof terms (Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI, Hyp)
          - grammar.py — Lark grammar + transformer producing the AST
          - instructions.py — lowers AST to Fellowship commands (ASCII-normalized)
        - comp/
          - visitor.py — base visitor
          - enrich.py — PropEnrichmentVisitor (annotate nodes with propositions/flags)
          - reduce.py — ArgumentTermReducer (λ/μ/μ′ rules; affine cases; normalization log)
          - alpha.py — α-renaming and binder freshening
          - neg_rewrite.py — negation-introduction pattern rewriter
        - dc/
          - argument.py — Argument class (execute, normalize, chain, undercut; stubs for support/rebut)
          - graft.py — graft_single/graft_uniform with capture-aware substitution and α-freshening
          - match_utils.py — tree matching utilities (match_trees, get_child_nodes, is_subargument)
      - pres/
        - gen.py — proof-term string generation (.pres)
        - nl.py — natural-language renderings (argumentation/dialectical/intuitionistic)
        - color.py — acceptance coloring (green/yellow/red)
        - tree.py — acceptance tree renderer (proof-term or NL labels)
      - wrap/
        - cli.py — CLI entry point (python -m wrap.cli)
        - prover.py — Fellowship process integration, machine I/O, logging
      - mod/
        - store.py — global persistence for arguments
      - tests/ — pytest suite and .fspy scripts used by CLI/tests
      - conftest.py — pytest configuration/helpers
      - tests.py — legacy ad-hoc test harness (developer utility)
      - sexp_parser.py — S-expression parser for machine payloads
      - fsp (binary), core.ml, print.ml, help.ml, lexer.ml, … — OCaml prover sources
  - branches/, trunk/ — upstream variants/historical code; not used by the Python wrapper directly
- README.md — this file (wrapper and native prover usage)

CLI quick reference
- Commands operate over the scenario created by your .fspy script (declarations, arguments, operations):
  - reduce: perform reduction steps on the current proof term
  - render: render the current proof term (no normalization)
  - render-nf: normalize and then render the proof term
  - color: normalize and print a colored proof term (green/yellow/red)
  - tree [nl|pt]: render an acceptance tree (nl = natural language labels; pt = proof-term labels)
  - undercut NEW ATTACKER TARGET: creates a debate named NEW consisting of the arguments ATTACKER and TARGET. The attacked assumption(s) is computed automatically.

Examples
- Normalize and render a scenario:
  - python -m wrap.cli --script tests/normalize_render.fspy
- Show acceptance coloring:
  - python -m wrap.cli --script tests/counterarguments_and_undercut.fspy color
- Draw an acceptance tree with natural language labels:
  - python -m wrap.cli --script tests/counterarguments_and_undercut.fspy tree nl
- Perform an explicit undercut in a script:
  - python -m wrap.cli --script tests/counterarguments_and_undercut.fspy undercut U1 A "thesis:A"

# Readme for the Fellowship Prover

These are the sources for the Fellowship prover written by Florent Kirchner and Claudio Sacerdoti Coen. 
The prover is an implementation of the $\bar{\lambda}\mu\tilde{\mu}$-calculus [due to Pierre-Louis Curien and Hugo Herbelin](http://pauillac.inria.fr/~herbelin/publis/icfp-CuHer00-duality+errata.pdf).
The system and the theory behind it is described extensively in Florent Kirchner's [PhD-thesis](https://pastel.hal.science/pastel-00003192v1/document).

Note: Build and run the native prover from fsp/tags/fellowship-0.1.0 (./fsp). The Python wrapper/CLI lives alongside it and is invoked as python -m wrap.cli from the same directory.

## Setup

Clone the directory to a destination of your choosing.
Navigate to one of the available versions in the tags subdirectory, e.g. 'fellowship-0.1.0'.
Use 'make' to build (requires OCaml).
As of late 2024, the system still works on Linux (tested on WSL) and MacOS

## Usage

Use ./fsp to run the prover. Use 'help.' to get help. For further questions refer to Florent Kirchner's [PhD-thesis](https://pastel.hal.science/pastel-00003192v1/document), especially chapter 7.
The tests folder contains some examples to run.


