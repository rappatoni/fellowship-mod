# AIDA - Interactive Debate Assistant

`AIDA` is an implementation of `AC/DC` (`Argument Calculus/Debate Calculus`), a calculus for the construction, compilation and evaluation of arguments and debates. Its present manifestation is a Python wrapper and experimentation environment around the
[Fellowship prover](https://github.com/theoremprover-museum/fellowship).
It provides a workflow for constructing, composing, normalizing, and rendering
arguments and debates as proof terms over a classical control-operator calculus.

The repository combines:
- the native Fellowship prover under `wrap/fellowship/`
- a Python wrapper (`wrap/prover.py`) that talks to Fellowship in machine mode
- an argument/debate layer (`core/dc/argument.py`)
- multiple presentation layers (`pres/`) for natural language and
  acceptance trees

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
- debate graphs, ADF labelling (via adf-bdd) and label-guided evaluation
- acceptance-tree export through Graphviz (with DOT fallback), coloured by
  the grounded labels
- machine-mode integration with Fellowship, including prover state extraction
- first-order propositions and proof terms: `forall` / `exists`, sorts, and
  first-order terms, through parsing, type synthesis, replay and rendering
  (see *First-order logic* below for what is and is not supported)

## Requirements

- Python 3.11+
- `make`
- an OCaml toolchain to build the native Fellowship binary
- a Rust toolchain (`cargo`) to build the `adf-bdd` solver, which the
  debate labeller requires

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

## Locating the adf-bdd binary

Debate labelling (`label`, `evaluate`, `graph ... show`) is computed by
[adf-bdd](https://github.com/ellmau/adf-obdd), a third-party solver for
abstract dialectical frameworks. `make install` builds it into
`.venv/bin/adf-bdd` (pinned version, via `cargo install`). The wrapper
resolves it in this order:

1. `ADF_BDD_BIN`
2. the active virtual environment's `bin/adf-bdd`
3. repo-local `.venv/bin/adf-bdd`
4. `adf-bdd` found on `PATH`
5. `~/.cargo/bin/adf-bdd`

There is no fallback: if the binary cannot be found, labelling refuses
with a message naming this search order. The in-tree labellers exist only
as cross-checks of adf-bdd, not as substitutes.

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
- `tree ARG [nl [argumentation|dialectical|intuitionistic] | pt]`
  - render an acceptance tree coloured by the grounded labels (see
    Debate-graph commands); drawn uncoloured if the graph is refused
- `chain ARG1 ARG2`
  - graft / chain one argument into another

### Debate-graph commands

Every atomic argument you register (`start argument ... end argument`,
`register`) adds its hyperedges to one **document graph** for the whole
file; proposition identity is global to it, so a counterargument to Q
registered anywhere contests every use of Q. The debate verbs
(`attack`, `support`, `chain`) name a debate whose issue is the host's
conclusion and check that the attacker concludes the contrary of (the
supporter concludes) a statement reachable from that issue; they add no
edge. `graph`, `label` and `evaluate` take a name, read its issue,
**unfold** the document graph from that issue into a debate term
(cycles broken by capture: a demand for P inside a proof of P->Q is the
hypothesis, a demand for a refutation of P inside a proof of P is the
continuation; a presumption is never captured), compile that
term into the issue's debate graph, label it by an ADF semantics and
evaluate it under a witness labelling. `graph document` and `label
document` show the document graph itself. Every unfolded term is
replayed through Fellowship first - the type oracle: if the arguments
type-check, so must their unfolding - and refused with a one-line
message if the prover rejects it; `typecheck off` (or `FSP_TYPECHECK=0`)
skips the replay for production runs. Debates are classical: their
scaffolds throw to a second conclusion, which LJ forbids, so the debate
commands are refused while the file is in `lj`. Derivation cycles are
admitted; `graph` reports which fragment (acyclic or cyclic) a debate
is in. They cover
the quantifier-free fragment and refuse first-order terms with a
one-line message.

- `graph ARG|document [FILE.dot] [show]`
  - print the nodes, hyperedges and default markers of ARG's issue graph
    (unfolded from the document) or of the document graph; write
    Graphviz DOT when a filename is given
  - every lambda is a subargument edge of its own; a subargument that
    captured a binder of an enclosing one is absorbed into it and listed
    under that edge with what it captured (`~ absorbed ...`)
  - `show` renders the graph and opens it in the platform viewer; without
    Graphviz installed it prints an indented text view instead
- `label ARG|document [grounded|complete|preferred|stable]`
  - print the labelling(s) of the chosen semantics (default grounded):
    one `IN` / `OUT` / `UNDEC` per proposition and side; several
    labellings are numbered
- `evaluate ARG [skeptical|credulous] [grounded|complete|preferred|stable] [cbn|cbv]`
  - label-guided evaluation; options in any order, defaults skeptical,
    preferred, cbn; prints the normal-form class (`VALUE`, `EXCEPTION`,
    `OPEN`) and the normal form, cached as `.labelled_nf`
  - credulous evaluates against one labelling of the chosen semantics in
    which the argument's issue is IN (if none exists, against the
    grounded labelling, resolved skeptically); skeptical against the
    intersection of all of them; the base strategy resolves only
    critical pairs that labelling leaves open

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
- wrapper-only decoration commands such as `decorate NAME : 'template'`

Lines starting with:
- `#` are echoed as user-facing comments
- `%` are silent comments

### Decorations

`decorate` commands attach wrapper-side natural-language metadata to names used
by renderers. They are consumed by AIDA's Python wrapper and are **not** sent to
Fellowship.

```text
decorate Bird : '@arg1 is a bird'
decorate Bird_list : "@arg1 ist eine \\sn{Liste}"
```

Decoration templates may contain positional placeholders:
- `@arg1`, `@arg2`, ... refer to the arguments of a rendered proposition.
- For example, with `decorate Bird : '@arg1 is a bird'`, the proposition
  `Bird Tweety` may render as `Tweety is a bird`.

Both single-quoted and double-quoted strings are supported intentionally:

- **Single-quoted templates** are convenient for hand-written `.fspy` scripts.
  Their contents are treated literally by the wrapper parser. This means a
  single backslash can be written directly:

  ```text
  decorate Bird_list : '@arg1 ist eine \sn{Liste}'
  ```

  The main caveat is that the current parser does not define an escaping rule
  for a literal single quote inside a single-quoted template.

- **Double-quoted templates** use JSON string syntax. They are mainly used by
  generated `.fspy` files because the importer writes them with `json.dumps`,
  which safely escapes quotes, backslashes, and other special characters.

  In double-quoted templates, these characters must be escaped as in JSON:

  | Intended runtime character | Double-quoted `.fspy` spelling |
  | --- | --- |
  | backslash `\` | `\\` |
  | double quote `"` | `\"` |
  | newline | `\n` |
  | tab | `\t` |
  | carriage return | `\r` |

  Thus the runtime template `@arg1 ist eine \sn{Liste}` is written in a
  double-quoted `.fspy` line as:

  ```text
  decorate Bird_list : "@arg1 ist eine \\sn{Liste}"
  ```

  When the wrapper reads this line, it JSON-decodes the double-quoted string and
  stores a template containing one actual backslash: `@arg1 ist eine \sn{Liste}`.

The duplication exists to support two use cases: single quotes are easier for
humans writing scripts by hand, while double quotes provide a robust,
standardized escaping format for importer-generated scripts.

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

## First-order logic

AIDA reads and replays the first-order fragment of Fellowship: universal and
existential quantification, sorts, and first-order terms.

### Declaring a signature

```text
declare N : type.                       a sort
declare O : N.                          a constant
declare S : N -> N.                     a function
declare P : N -> bool.                  a predicate
declare ax : (forall n:N, P [n]).       an axiom
```

A predicate is applied with brackets on input — `P [x] [y]`, `Even [S (S O)]` —
and the prover prints it juxtaposed, as `P x y`. Both spellings are accepted
wherever AIDA reads a proposition.

### Proposition syntax

```text
f, g ::= true | false | x | ~f | f g            atoms, negation, application
       | f -> g | f - g                          implication, subtraction
       | forall x,...,y : s, f                   universal
       | exists x,...,y : s, f                   existential
```

Precedence, loosest first: `-`, then `->`, then `\/`, then `/\`, then `~`,
then application. Implication is right associative and the rest are left
associative. A quantifier body extends as far to the right as it can, so a
quantifier used as an operand is parenthesised: `(forall x:N, P [x]) -> A`.

Conjunction and disjunction are part of Fellowship but **not** of AIDA's
fragment: there are no proof-term constructors for them here, so a proposition
using them can be declared but not argued about.

### Working with quantifiers

`elim` introduces a quantified goal and names the fresh variable; `elim [t]`
instantiates a quantified hypothesis at `t`. `focus` aborts the prover on a
first-order goal, so use its expansion — `cut (<prop>) name.` followed by
`axiom <name>.` — which is what `focus` is defined as anyway.

```text
lj.
minimal.
declare N : type.
declare O : N.
declare P : N -> bool.
declare ax : (forall n:N, P [n]).

theorem fo_forall : (P [O]).
cut (forall n:N, P [n]) th.
axiom ax.
elim [O].
axiom.
qed.
```

Worked examples: `tests/fo_forall.fspy`, and `tests/fo_exists_forall.fspy`,
which proves `(exists y, forall x. P x y) -> (forall x, exists y. P x y)` and
exercises every first-order proof-term constructor.

### What is not supported yet

Normalization and the debate operations — `reduce`, `chain`, `support`,
`attack` and the debate graph — do not handle first-order terms. The
reduction rules for first-order AC/DC are not settled, so rather than guess,
those operations raise `FirstOrderNotSupported` naming the construct they
stopped at.

## Render styles

`render ARG STYLE` and `render-nf ARG STYLE` currently support:

- `argumentation`
- `dialectical`
- `intuitionistic`
- `vanilla`

Examples:

```bash
.venv/bin/acdc --interactive
```

Then inside the REPL:

```text
render myarg vanilla
render-nf myarg dialectical
```

## Acceptance trees

```text
tree ARG
tree ARG pt
tree ARG nl
tree ARG nl dialectical
```

Tree rendering uses Graphviz when available and otherwise writes a `.dot`
file. Each box on the spine is filled by the grounded ADF label of the
statement its binder establishes (green IN, red OUT, yellow UNDEC), taken
from the argument's debate graph; the former `color` / `color-nf`
commands and their shape-based classification were removed on 2026-09-16
(they read a supported argument as defeated). Use `label` for the labels
as text.

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
- `label`
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
- `label`

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
- `pres/` — presentation layers (proof terms, NL, trees)
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

## License

AIDA is distributed under the **GNU General Public License version 3 only**
(`GPL-3.0-only`), except for third-party material that carries its own license.
See the top-level `LICENSE` file for the GPLv3 text.

The bundled Fellowship source under `wrap/fellowship/` was originally written
by Florent Kirchner and Claudio Sacerdoti Coen and is distributed under
**CeCILL v2.0**; its full license text is retained in
`wrap/fellowship/COPYING`. Files derived from that source retain their original
CeCILL notices and identify the AIDA modifications separately. The combined
modified Fellowship work in this repository is distributed under
`GPL-3.0-only`, as permitted by CeCILL v2.0 Article 5.3.4.

The manuscript source under `papers/comma2026/` has its own academic-content
license; see `papers/comma2026/LICENSE.md`. Publisher-supplied templates and
other third-party files retain their respective licenses and notices.
