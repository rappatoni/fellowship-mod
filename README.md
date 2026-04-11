# AIDA - Interactive Debate Assistant

`AIDA` is an implementation of `AC/DC` (`Argument Calculus/Debate Calculus`), a calculus for the construction, compilation and evaluation of arguments and debates. Its present manifestation is a Python wrapper and experimentation environment around the
[Fellowship prover](https://github.com/theoremprover-museum/fellowship).
It provides a workflow for constructing, composing, normalizing, and rendering
arguments and debates as proof terms over a classical control-operator calculus.

The repository combines:
- the native Fellowship prover under `wrap/fellowship/`
- a Python wrapper (`wrap/prover.py`) that talks to Fellowship in machine mode
- an argument/debate layer (`core/dc/argument.py`)
- multiple presentation layers (`pres/`) for natural language, mirror views,
  coloring, and acceptance trees

An earlier motivation/theory overview is available
[here](https://www8.cs.fau.de/ext/teaching/wise2024-25/oberseminar/slides-rapp.pdf).

## What is currently implemented

The current codebase supports:
- theorem and counterargument / antitheorem workflows
- raw Fellowship commands and wrapper-level recording commands
- argument composition by grafting / chaining
- debate operations:
  - `attack`
  - `undercut` / `undermine`
  - `rebut`
  - `support`
  - `undergird`
  - `reinforce`
- normalization of argument terms in multiple evaluation disciplines
- natural-language rendering styles:
  - `argumentation`
  - `dialectical`
  - `intuitionistic`
  - `vanilla`
- mirror renderers:
  - `mirror`
  - `mirror-tree`
- acceptance coloring of normalized proof terms
- acceptance-tree export through Graphviz (with DOT fallback)
- machine-mode integration with Fellowship, including prover state extraction

## Requirements

- Python 3.11+
- `make`
- an OCaml toolchain to build the native Fellowship binary

## Installation

### Local development install

From the repository root:

```bash
make install
make -C wrap/fellowship
```

This creates a local virtual environment in `.venv/`, installs the Python
package in editable mode, and builds the native prover binary at:

```text
wrap/fellowship/fsp
```

You can then run:

```bash
.venv/bin/acdc --help
```

### Optional global install

You can also install the Python package globally with `pipx`:

```bash
pipx install .
```

If you do that, make sure `acdc` can still find the native `fsp` binary
via one of the mechanisms below.

## Locating the Fellowship binary

The wrapper resolves `fsp` in this order:
1. `ACDC_FSP`
2. `FSP_PATH`
3. packaged path: `wrap/fellowship/fsp`
4. repo-local path from the current working directory
5. `fsp` found on `PATH`

Example:

```bash
export ACDC_FSP=/absolute/path/to/wrap/fellowship/fsp
```

Persist on macOS / Linux:

```bash
echo 'export ACDC_FSP="/absolute/path/to/your/checkout/wrap/fellowship/fsp"' >> ~/.zshrc
source ~/.zshrc
```

Verify:

```bash
test -x "$ACDC_FSP" && echo "OK: $ACDC_FSP"
```

On macOS, if a copied binary is quarantined:

```bash
xattr -d com.apple.quarantine "$ACDC_FSP"
```

## Running the CLI

### Interactive mode

```bash
.venv/bin/acdc --interactive
```

### Script mode

```bash
.venv/bin/acdc --script tests/normalize_render.fspy
```

### Via the Makefile

```bash
make cli ARGS="--help"
make cli ARGS="--script tests/normalize_render.fspy"
```

## Logging and environment variables

### CLI logging

The CLI supports:

```bash
acdc --log-level DEBUG --log-file acdc.log --script tests/normalize_render.fspy
```

You can also set the default log level with:

```bash
export FSP_LOGLEVEL=DEBUG
```

### Reduction / normalization controls

Normalization behavior is controlled by environment variables read by
`core.dc.argument.Argument.normalize()`:

- `FSP_EVAL_DISCIPLINE`
  - `legacy` (default)
  - `onus`
  - `onus-parallel`
- `FSP_ONUS_FALLBACK`
- `FSP_ONUS_STANCE`

Example:

```bash
FSP_EVAL_DISCIPLINE=onus-parallel .venv/bin/acdc --script tests/counterarguments_and_undercut.fspy
```

## Workflow overview

A typical wrapper workflow is:
1. declare prover resources
2. record an argument or counterargument
3. register it under a name
4. compose or attack/support named arguments
5. normalize the result
6. render it in one or more views

The wrapper stores named arguments internally through `ProverWrapper`, so later
commands such as `reduce`, `render`, `support`, or `attack` can refer to them by
name.

## Interactive commands

Interactive mode accepts:
- ordinary Fellowship commands
- wrapper commands for recording, normalization, rendering, and debate building

Many Fellowship commands are dot-terminated. If a command is incomplete,
`acdc` prompts for continuation with `...`.

Syntax/prover errors are non-fatal in the interactive wrapper unless the wrapper
detects a machine-mode desynchronization.

### Recording commands

- `start argument NAME CONCLUSION`
  - begin recording a theorem-backed argument
- `start counterargument NAME CONCLUSION`
- `start antitheorem NAME CONCLUSION`
  - begin recording a counterargument / antitheorem-backed argument
- `end argument`
  - finish recording, execute the proof against Fellowship, and register it

### Stored-argument commands

- `reduce ARG`
  - normalize and print the normal form
- `normalize ARG`
  - normalize silently and cache the result
- `render ARG [STYLE]`
- `render-nf ARG [STYLE]`
  - render the original or normalized term
- `color ARG`
  - show acceptance coloring for the normalized term
- `tree ARG [nl [argumentation|dialectical|intuitionistic] | pt]`
  - render an acceptance tree
- `chain ARG1 ARG2`
  - graft / chain one argument into another

### Debate commands

- `undermine NEW ATTACKER TARGET`
- `undercut NEW ATTACKER TARGET`
  - backward-compatible aliases for default-target attack
- `support NEW SUPPORTER TARGET [on PROP]`
- `undergird NEW SUPPORTER TARGET [on PROP]`
  - support restricted to default targets
- `reinforce NEW SUPPORTER TARGET [on PROP]`
  - support restricted to non-default targets
- `attack NEW ATTACKER TARGET [on PROP]`
  - generic attack operation
- `rebut NEW ATTACKER TARGET [on PROP]`
  - attack restricted to non-default targets

## Script files (`.fspy`)

Script mode uses the same general command language as the interactive wrapper.
Representative examples live in `tests/*.fspy`.

Typical script commands include:
- Fellowship commands such as `lk.`, `declare ...`, `deny ...`, `qed.`
- wrapper recording commands such as `start argument ...` / `end argument`
- normalization / rendering commands
- debate operations such as `support`, `undercut`, `attack`, `rebut`

Lines starting with:
- `#` are echoed as user-facing comments
- `%` are silent comments

## Important prover-side commands and concepts

The repository now relies on several Fellowship features that the old README did
not document.

### `theorem` and `antitheorem`

At the prover level, theorem construction starts with commands like:

```text
theorem argA : (A).
antitheorem notA : (A).
```

The wrapper-level recording commands:
- `start argument ...`
- `start counterargument ...`
- `start antitheorem ...`

are convenience front-ends for those prover workflows.

### `deny`

`deny` introduces a named refutational resource that can later be used through
`moxia`.

Examples from the current test scripts:

```text
deny mA : (A).
deny r5 : (LowballOffer - BadNegotiations).
```

### `moxia`

`moxia NAME.` invokes a denied declaration inside a proof / counterproof.
This is part of the current counterargument workflow.

Minimal example:

```text
declare A : bool.
deny mA : (A).
antitheorem notA : (A).
moxia mA.
qed.
```

See `tests/moxia_antitheorem.fspy`.

### Affine variables

Affine variables and affine uses of binders matter operationally in this codebase.
They show up both in prover commands and in the semantics of reduction / debate
encodings.

In particular:
- some proof scripts use affine cuts explicitly
- affine behavior is relevant to attacks, defaults, and control-flow-sensitive
  reductions
- several tests cover affine and non-affine cases

Examples to inspect:
- `tests/affine_test.py`
- `tests/non_affine_test.py`
- `tests/test_affine_cut.py`
- `tests/test_affine_elim.py`

## Render styles

`render ARG STYLE` and `render-nf ARG STYLE` currently support:

- `argumentation`
- `dialectical`
- `intuitionistic`
- `vanilla`
- `mirror`
- `mirror-tree`

Examples:

```bash
.venv/bin/acdc --interactive
```

Then inside the REPL:

```text
render myarg vanilla
render myarg mirror
render-nf myarg dialectical
render myarg mirror-tree
```

## Acceptance coloring and trees

### Coloring

```text
color ARG
```

This normalizes the argument if needed and prints a colored proof-term view.

### Acceptance trees

```text
tree ARG
tree ARG pt
tree ARG nl
tree ARG nl dialectical
```

Tree rendering uses Graphviz when available and otherwise writes a `.dot` file.

## Examples

### Minimal argument / normalize / render example

File: `tests/normalize_render.fspy`

```text
lk.
declare A,B:bool.
declare axA: (A).
declare axAB: (A->B).

start argument argA B
cut (A->B) h.
axiom axAB.
elim.
axiom axA.
axiom.
end argument

render argA
reduce argA
render-nf argA
```

### Counterargument and undercut example

File: `tests/counterarguments_and_undercut.fspy`

This script demonstrates:
- `start counterargument ...`
- `undercut`
- `reduce`
- `color`
- `tree ... nl`
- `deny` / `moxia`

Run it with:

```bash
.venv/bin/acdc --script tests/counterarguments_and_undercut.fspy
```

### Support example

File: `tests/basic_support.fspy`

Demonstrates:
- `support`
- `undercut`
- `render ... vanilla`
- `color`

### Mirror rendering example

File: `tests/mirror_minimal.fspy`

Demonstrates:
- `render m vanilla`
- `render m mirror`
- `render m mirror-tree`

### Antitheorem / moxia example

File: `tests/moxia_antitheorem.fspy`

```text
declare A : bool.
deny mA : (A).
antitheorem notA : (A).
moxia mA.
qed.
```

## Running tests

```bash
make test
```

Manual alternative:

```bash
source .venv/bin/activate
pytest -q tests
```

## Development commands

```bash
make reset-venv
make cli ARGS="--help"
make test
make lint
make format
make typecheck
make binlink
```

`make binlink` creates a short local `./acdc` symlink to `.venv/bin/acdc`.

## Repository layout

- `core/` — core ASTs, transformations, reduction, grafting, argument logic
- `pres/` — presentation layers (proof terms, NL, mirror, coloring, trees)
- `wrap/` — Python wrapper code and the native Fellowship subtree
- `wrap/fellowship/` — native prover sources and `fsp` binary build target
- `tests/` — pytest tests and `.fspy` / `.fsp` examples
- `pyproject.toml` — package metadata and console-script entry point
- `Makefile` — development and testing shortcuts
- `README.md` — this file

## Notes on Fellowship itself

The native prover bundled here is Fellowship, written by Florent Kirchner and
Claudio Sacerdoti Coen, implementing the
$\bar{\lambda}\mu\tilde{\mu}$-calculus.

Useful background references:
- Curien and Herbelin on the calculus:
  http://pauillac.inria.fr/~herbelin/publis/icfp-CuHer00-duality+errata.pdf
- Florent Kirchner's thesis:
  https://pastel.hal.science/pastel-00003192v1/document

For raw prover usage, build and run `wrap/fellowship/fsp` directly and use
`help.` inside the prover.
