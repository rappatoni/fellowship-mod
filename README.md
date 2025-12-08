# Fellowship Prover Mod

This repo contains an experimental wrapper for the [Fellowship prover](https://github.com/theoremprover-museum/fellowship). The wrapper implements a variant of logical (aka deductive) argumentation for classical logic by way of control operators. An exposition of the motivation and theory behind the system can be found [here](https://www8.cs.fau.de/ext/teaching/wise2024-25/oberseminar/slides-rapp.pdf).

## Usage

Prerequisites
- OCaml toolchain (to build the Fellowship prover)
- Python 3.11.x and make

Build the prover
- make -C wrap/fellowship
- The native binary is placed at wrap/fellowship/fsp.

Run the Python wrapper CLI
- From the repo root:
  - .venv/bin/acdc --help
  - .venv/bin/acdc --script tests/normalize_render.fspy
  - Strict mode (treat parse/machine issues as errors): .venv/bin/acdc --script tests/normalize_render.fspy --strict
- Or via Makefile:
  - make cli ARGS="--help"
  - make cli ARGS="--script tests/normalize_render.fspy"

Run tests
- make test
- Manual alternative:
  - source .venv/bin/activate
  - pytest -q tests

Features
- Argument lifecycle: execute an argument against the prover, normalize its proof term, and render it.
- Debate operations: undercut (counterargument) is supported; support and rebut are planned.
- Presentations:
  - Proof-term rendering (ASCII .pres)
  - Natural language renderings (argumentation/dialectical/intuitionistic styles)
  - Acceptance coloring (green/yellow/red)
  - Acceptance trees (proof-term labels or NL labels)

Repository layout
- core/ — Python core (ac/dc/comp)
- pres/ — presentations (gen/nl/color/tree)
- wrap/ — Python wrapper (cli/prover) and native OCaml under wrap/fellowship/
- mod/ — global store
- tests/ — pytest suite and .fspy scripts
- pyproject.toml — packaging; exposes “acdc” console script
- Makefile — venv/dev tooling
- README.md — this file

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
  - .venv/bin/acdc --script tests/normalize_render.fspy
- Show acceptance coloring:
  - .venv/bin/acdc --script tests/counterarguments_and_undercut.fspy color
- Draw an acceptance tree with natural language labels:
  - .venv/bin/acdc --script tests/counterarguments_and_undercut.fspy tree nl
- Perform an explicit undercut in a script:
  - .venv/bin/acdc --script tests/counterarguments_and_undercut.fspy undercut U1 A "thesis:A"

Notas de uso (sin más cambios de código)
- Instalar para desarrollo y crear el comando local:
  - make reset-venv
  - make -C wrap/fellowship
  - .venv/bin/acdc --help
  - Opcional: make binlink; ./acdc --help
- Instalación global (opcional, sin venv local): pipx install . → acdc disponible sin activar venv.

# Readme for the Fellowship Prover

These are the sources for the Fellowship prover written by Florent Kirchner and Claudio Sacerdoti Coen. 
The prover is an implementation of the $\bar{\lambda}\mu\tilde{\mu}$-calculus [due to Pierre-Louis Curien and Hugo Herbelin](http://pauillac.inria.fr/~herbelin/publis/icfp-CuHer00-duality+errata.pdf).
The system and the theory behind it is described extensively in Florent Kirchner's [PhD-thesis](https://pastel.hal.science/pastel-00003192v1/document).

Note: Build and run the native prover under wrap/fellowship (./fsp). The Python wrapper/CLI is exposed via the acdc console script.

## Setup

Clone the directory to a destination of your choosing.
Navigate to one of the available versions in the tags subdirectory, e.g. 'fellowship-0.1.0'.
Use 'make' to build (requires OCaml).
As of late 2024, the system still works on Linux (tested on WSL) and MacOS

## Usage

Use ./fsp to run the prover. Use 'help.' to get help. For further questions refer to Florent Kirchner's [PhD-thesis](https://pastel.hal.science/pastel-00003192v1/document), especially chapter 7.
The tests folder contains some examples to run.


