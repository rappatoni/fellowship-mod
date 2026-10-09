# AIDA - Interactive Debate Assistant

`AIDA` is an implementation of `AC/DC` (`Argument Calculus/Debate Calculus`), a calculus for the construction, compilation and evaluation of arguments and debates. Its present manifestation is a Python wrapper and experimentation environment around the
[Fellowship prover](https://github.com/theoremprover-museum/fellowship).
It provides a workflow for constructing, composing, evaluating, and rendering
arguments and debates as proof terms over a classical control-operator calculus.

The repository combines:
- the native Fellowship prover under `wrap/fellowship/`
- a Python wrapper (`wrap/`): the session that talks to Fellowship in
  machine mode, the document it holds, the command syntax and its
  interpreter, the CLI, and a service layer with JSON output for programs
- the argument calculus and the debate compiler (`core/`)
- presentation layers (`pres/`) for proof terms, natural language and
  acceptance trees

An earlier motivation/theory overview is available
[here](https://www8.cs.fau.de/ext/teaching/wise2024-25/oberseminar/slides-rapp.pdf).

## What is currently implemented

The current codebase supports:
- arguments and counterarguments recorded as blocks, statements proved or
  refined later, and Fellowship's own commands and tactics
- citation of registered arguments by name (`cite NAME`)
- the document graph: every registered argument contributes its edges, and
  conflicts arise by proposition identity
- debates: named, ordered selections of a document's arguments, open or
  closed, with checked moves (`attack`, `rebut`, `undermine`/`undercut`,
  `support`, `buttress`/`reinforce`, `undergird`)
- debate graphs, ADF labelling (via adf-bdd) and label-guided evaluation,
  skeptical or credulous, call-by-name or call-by-value
- natural-language rendering styles: `argumentation`, `dialectical`,
  `intuitionistic`, `vanilla`, `pruefschema`
- acceptance-tree export through Graphviz (with DOT fallback), coloured by
  the grounded labels
- sessions and documents, a service layer for programs, whole-document
  checks with source positions, and versioned JSON
  (see *Sessions and documents* and *The service layer*)
- first-order propositions and proof terms: `forall` / `exists`, sorts, and
  first-order terms, through parsing, type synthesis, replay and rendering
  (see *First-order logic* below for what is and is not supported)

## Status

The debate compiler - unfolding a document into a debate term, compiling
and labelling it, and evaluating it under a witness labelling - is the
main line of development. It covers the quantifier-free fragment,
derivation cycles included, and is still incomplete. Known gaps, each
tracked in `tasks.org`:

- **Only sites can be attacked or supported.** The specification
  (`debate-graph-spec.org`, rational closure) lets attacks and supports
  reach subarguments and intermediate conclusions too.
- **No transposition closure.** Strict edges are not yet closed under
  transposition, so there are no premise-attacks through strict edges.
- **No first-order debates.** Normalisation and the debate operations
  refuse first-order terms (see *First-order logic*).
- **The shared route lags behind.** `pipeline shared` still builds the
  earlier scaffold shape and has not been ported to the stacked shape and
  the argument entrypoints, so the default route is `unfolded`.
- **s(CASP) import is partial.** The importer translates the positive
  parts of a justification tree; negation as failure and global
  constraints are not translated yet.

Smaller design questions are open as well.

`docs/minicourses/` holds five minicourses, each lesson by lesson with
exercises: `minicourse.org` (arguments, graphs and labels),
`minicourse-evaluation.org` (the evaluation pipeline),
`minicourse-sharing.org` (sub-debates shared by name),
`minicourse-superdeduction.org` and `minicourse-api.org` (AIDA from a
program). They are drafted by an AI agent, pinned by the tests under
`tests/minicourse/`, and still await the author's review.

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
.venv/bin/acdc --interactive --load tests/demo/01_arguments.fspy
```

The prompt has readline line editing (emacs bindings) and a history file
(`~/.acdc_history`, or `ACDC_HISTORY`). A pasted block runs one line at a
time; lines starting with `#` are echoed as narration and lines starting
with `%` are ignored, as in scripts. `load "FILE".` runs a script inside the
session, in a new document: whatever the session held before is gone.
A script loaded with `--load` or `load` stops at a `%stop` line,
so a demo file can hold its setup above the marker and the commands to
paste below it; `--script` runs the whole file. See `tests/demo/README.md`.

### Script mode

```bash
.venv/bin/acdc --script tests/debates.fspy
```

### Via the Makefile

```bash
make cli ARGS="--help"
make cli ARGS="--script tests/debates.fspy"
```

### Preloading a script into interactive mode

`--load FILE` runs a `.fspy` script (strictly, stopping on the first error)
before dropping into the REPL. It can only be combined with `--interactive`:

```bash
.venv/bin/acdc --interactive --load tests/debates.fspy
```

### Importing from other formalisms

`--import SOURCE_LANGUAGE SOURCE_JSON MODE [TARGET_FILE_NAME]` translates an
external proof/argument representation into a `.fspy` script. Importers are
separate packages that register under the `acdc.importers` entry point (see
`wrap/importers.py` for the contract); the s(CASP) importer is
[scasp-aida](https://git8.cs.fau.de/regass/explanations), source language `scasp`. `MODE` is either:
- `file` — write the translated script to `TARGET_FILE_NAME` (default:
  `SOURCE_JSON` with its extension replaced by `.fspy`) and exit
- `interactive` — translate to a temporary script, replay it strictly, and
  drop into the REPL with the result already registered

```bash
.venv/bin/acdc --import scasp path/to/proof.json file imported.fspy
.venv/bin/acdc --import scasp path/to/proof.json interactive
```

## Logging and environment variables

### CLI logging

The CLI supports:

```bash
acdc --log-level DEBUG --log-file acdc.log --script tests/debates.fspy
```

You can also set the default log level with:

```bash
export FSP_LOGLEVEL=DEBUG
```

### What the pipeline's phases are

`evaluate ARG` runs eight phases. Each hands one artifact to the next, and at
`DEBUG` each prints the artifact it produced.

The phases are described below as they act on the *unfolded* debate term,
which is what defines them and what the default route builds. `pipeline
shared` (or `FSP_PIPELINE=shared`) keeps the debate as named sub-debates
instead, and each phase works on those (see the note after phase 5 and
`share ARG`); `pipeline unfolded` switches back. The shared route has not
yet been ported to the stacked shape the unfolded route builds (see
*Status*). The unfolded route's cost grows with the number of paths
through the document graph.

1. **issue** — read the argument's own registered proof term and its issue, a
   (proposition, side) pair.
2. **unfold** — build the debate term for that issue out of the *document*
   graph: the issue's own site, wrapped in a support scaffold per deriving
   edge and an attack scaffold per edge deriving the contrary, with each
   edge's body expanded the same way. A demand for a statement that a binder
   in scope already stands for - an argument's own or a scaffold's - is
   captured by that binder instead of being expanded again, which is what
   makes this terminate. Out: one proof term standing for the whole debate.
3. **typecheck** — replay that term through Fellowship as a throwaway theorem
   and compare what Fellowship rebuilds with what was sent, up to
   alpha-equivalence, site numbering and proposition spelling. This is the
   ground truth for the unfolder: if the registered arguments type-check, so
   must their unfolding. On a mismatch the log names the position in the term
   where the two shapes first differ. The replay is done one sub-debate at a
   time: each statement's debate is replayed once, with the sub-debates it
   cites left as open sites, and not again in the same document. That is as
   strong as replaying the whole term - what goes into a site has the site's
   type - unless the debate spells a statement in two ways (`~A` and
   `A -> false` are one statement but two propositions for Fellowship); then
   the whole term is replayed. `typecheck expanded` always replays the whole
   term, whose size grows with the number of paths through the graph.
4. **compile** — turn the term into a `DebateGraph`: nodes are canonical
   propositions, hyperedges carry a name, a target statement and sources with
   their kinds, and statements get default markers. It is a separate data
   structure, not a marked-up term. The compiler finds the sub-arguments by
   matching *scaffold shapes*: the four support and attack shapes of the
   COMMA 2026 paper, which the unfolder produces. A match
   says "here is a scion and here is the original", so the scion becomes an
   edge of its own and the walk continues into the original. Out: the
   argumentation framework, printed as an indented tree.
5. **strict** — read strictness off the term. A subterm is strict in the
   debate if it rests on nothing, that is, it has no open site. Where one wing
   of a scaffold is strict and the other is not, the scaffold is decided here
   and the losing wing is dropped; everything else is delayed. In: the
   unfolded term. Out: a rewritten, usually smaller term, plus a strict,
   source-less edge for every closed derivation the framework did not have.
   The issue graph is the framework of phase 4 plus those edges.

   `graph ARG` and `label ARG` need only the issue graph, and get it without
   building the unfolded term. The debate is kept as one definition per
   statement (see `share ARG`), and phases 4 and 5 run on one *instance* of
   a sub-debate at a time: a statement together with the statements captured
   and cut in it, which is all that the copy of that sub-debate at a site
   depends on. Each instance is compiled and strictness-resolved once; a
   cited instance shows the scaffolds around it only whether a site is still
   open in it, which captured variables are still free in it, and whether a
   decision was made inside. The result is the graph the unfolded term gives
   (edges, default markers, labellings), with each edge once where the term
   has a copy per path.

   `evaluate ARG` goes on from there without unfolding either. In phase 8
   the witness labelling decides the scaffolds strictness delayed, top-down
   from the issue, and a cited sub-debate is written out only inside a wing
   that is kept; a wing that is dropped is never built. What is normalised
   has the size of the answer, not of the debate. Phase 2's artifact is then
   the debate as named sub-debates rather than one unfolded term.
6. **label** — compile one acceptance condition per statement and ask the
   adf-bdd solver for the labellings of the chosen semantics. In: the issue
   graph. Out: conditions, and a numbered list of labellings, each mapping
   every statement to IN, OUT or UNDEC.
7. **witness** — pick the single labelling σ that will guide evaluation.
   Credulous picks the first labelling in which the issue is IN, the one that
   *witnesses* its acceptability; skeptical takes the statement-wise
   intersection of them all. Choosing σ once and resolving everything against
   it is the point: resolving each scaffold against whichever extension suits
   it would mix incompatible positions.
8. **sigma, normalise, classify** — write σ into the term (every occurrence of
   a statement carries its label, printed `A{IN}` or `{OUT}A`), resolve every
   scaffold σ decides by reading those labels, keeping one wing each, then
   name every site by its label - a site is a delegation if IN, an obligation
   otherwise - and reduce the result to a normal form, classified as an
   exception if it holds an uncaught clash, open if an obligation remains,
   and a value otherwise.

Two things the log makes visible that are worth knowing. The strict phase runs
**twice** per evaluation: once inside the issue-graph compilation to collect
the strict edges, which throws the rewritten term away, and once in evaluation
to get the rewritten term, which throws the edges away. And if the issue is not
in the document graph, the argument is evaluated as registered, skipping both
the unfolding and the type check.

What each level shows for the compilation–evaluation pipeline:

- `INFO` (default): the verdict of each command, and any decision that
  contradicts what you asked for. In particular, when no labelling of the
  chosen semantics accepts the issue, credulous evaluation falls back to the
  grounded labelling resolved *skeptically*, and says so.
- `DEBUG`: the stage-by-stage account, and **the artifact each phase
  produced**: the registered term, the unfolded term, both terms the type
  check compared, the compiled framework as a tree, the term going into and
  coming out of the strict phase with the strict edges it contributed, the
  acceptance conditions, every labelling in the canonical numbering, the
  chosen σ in full, the term entering normalisation and the normal form.
  Alongside them the decisions: which statements the unfolder expanded,
  captured or left open; every edge the compiler built; every scaffold the
  strict phase decided or delayed, with the reason; the witness chosen and
  why; the wing kept at each scaffold and whether the tiebreak decided it.
  The Fellowship replay's own chatter is silenced so it cannot drown this.
- `TRACE`: per-reduction-step lines with the rule that fired, the unfolder's
  scope and spine at each decision, the compiled conditions, the exported
  ADF and the solver's raw output.

### Explaining one evaluation

`explain ARG [MODE] [SEMANTICS] [BASE] [N|all]` takes the same options as
`evaluate`, runs it once, and prints that account grouped by stage without
changing the global log level, with the verdict last:

```
  unfold         A[t] expanded, site u1 (Deleg), 1 deriving edge(s)
  unfold           A[t] +support 'argA' (alt alt1)
  compile        edge 'argA' (supporter, defeasible) A[t] <- Q[t]:pres@u2
  strict         A[t] delayed for the labelling (neither wing is strict)
  label          preferred gives 1 labelling(s)
  witness        [1] of 1 chosen, the first preferred labelling accepting A[t]
  sigma          supporter A[t] is IN -> keep the supporter
  classify       VALUE
```

### Graph files

`graph ARG show` and `tree ARG` write an image into the working directory and
open it. `ACDC_NO_OPEN=1` keeps the viewer shut; `ACDC_NO_RENDER=1` also stops
the writing, and `graph ... show` then logs an indented text view of the graph
instead, which is the more useful thing to have in a log. `execute_script` takes
`render_files=False` for the same effect on one script; the test suite sets the
environment variable for the whole session so a run leaves no files behind.

### Other environment variables

- `FSP_ECHO_NOTES` (default `1`, i.e. on) — set to `0`/`false`/`no` to
  suppress echoing Fellowship's informational notes.
- `ACDC_TIMEOUT` (default `5`) — seconds one Fellowship command may take.
- `ACDC_TRANSPORT` (default `pipe`) — how AIDA talks to Fellowship: pipes,
  where commands of any length work; `pty` restores the old terminal, whose
  lines are limited to 1024 bytes on macOS.
- `FSP_BATCH_REPLAY` (default `1`, i.e. on) — set to `0`/`false`/`no` to send
  each instruction of a recorded argument to Fellowship one at a time instead
  of batching them into fewer round-trips. Mainly useful for debugging replay
  issues.
- `FSP_QUIET_REPLAY` (default `1`, i.e. on; only applies when batch replay is
  also on) — set to `0`/`false`/`no` to keep Fellowship's per-instruction
  output during batched replay instead of suppressing all but the final one.

## Workflow overview

A typical wrapper workflow is:
1. declare prover resources
2. record arguments and counterargument; each joins the document graph
3. record debates over them, if a limited or ordered selection is wanted
4. graph, label and evaluate an argument, an issue or a debate
5. render it in one or more views

### Sessions and documents

A *session* is one running Fellowship with its settings (`typecheck`,
`pipeline`, file output); the CLI holds one, and a server can hold several,
each with a process of its own (`wrap/prover.py`, `ProverWrapper`). A
session holds one *document* at a time (`wrap/document.py`): the
declarations, names, arguments, document graph, debates and caches.

- `new document [minimal] [lk|lj].` starts a new document: Fellowship is
  reset (`discard all.`) and the logic chosen - classical (`lk`, the
  default), intuitionistic (`lj`), or either without ex falso (`minimal`).
  Session settings survive.
- `lk.`, `lj.`, `minimal.` and `full.` choose the logic at the *head* of a
  document, before its first instruction (a script's first line, say).
  Later a change is refused - Fellowship fixes the logic at the first
  instruction - and asking for the logic in force does nothing.
- A session starts with a classical document, and `load FILE` replaces the
  document with the file's.
- Debates are classical: in an `lj` or a `minimal` document the debate
  commands are refused.

### The service layer

For a program serving AIDA - the document UI, say - read
`docs/dui-integration.md`: sessions and concurrency, checking a whole
document (`Service.check`: one entry per command, with its span, status and
result), the queries, the JSON formats (`wrap/serialize.py`, described by
`wrap/schemas/aida.schema.json`), errors, and importing from s(CASP) with
provenance. `examples/http_adapter.py` is a runnable reference server.

`wrap/service.py` is what a program (a server for the document UI, a
notebook, a test) calls instead of the CLI. `Service.of(session)`
offers the documents (`new_document`, `load`, `inventory`), the settings,
the content (`command`, `register`, `record_argument`, `state`, `prove`,
`adopt`, `decorate`, `start_debate` / `move` / `close_debate`) and the
queries (`graph`, `label`, `evaluate`, `term`, `render`, `unfold`, `share`,
`tree`, which gives DOT text and writes no file). Each call holds the
session's lock, returns a result object - the graph, the labellings, the
normal form with its class and witness labelling - and carries the
warnings it logged as `diagnostics`; a refusal raises an `AidaError` with a
stable `code` and the stage that refused. The CLI prints these results.
`explain` and `expand` stay CLI-only, and proofs are recorded as whole
blocks. `Service.check(text)` replays a whole document and reports on every
command; `docs/minicourses/minicourse-api.org` walks through all of it.

## Command syntax

Scripts and the REPL share one syntax (`wrap/syntax.py`), run by one
interpreter (`wrap/interpreter.py`):

- **Every command ends with a `.`**, wrapper commands included
  (`evaluate tweety.`). A command may span several lines, and several may
  share one (`minimal. lk.`). The command lists below leave the final `.`
  out.
- **Every `.` outside double quotes ends a command**, so what contains dots
  is double-quoted: proof terms (`register t : B := "μx:B.<ax||x>".`),
  decoration templates and file names (`load "tests/demo/01_arguments.fspy".`,
  `graph d "out.dot" show.`). A single quote is an ordinary character
  (`A'`, the binder `μ'`).
- **Blocks:** `argument NAME : (PROP).` or `counterargument NAME : (PROP).`,
  then Fellowship tactics (and `cite NAME.`), closed by `dixi.` (or `cr.`,
  `case rested.`); a statement's proof, `prove NAME.` ... `qed.` (strict) or
  `dixi.` (registered either way); a debate, `debate ... : ISSUE.`, its
  moves, `cedat tempus.` (or `ct.`).
- **Fellowship's own commands and tactics** (`declare`, `deny`, `cut`,
  `axiom`, `elim`, `by default`, ...) pass through unchanged: Fellowship's
  parser stays their only grammar.
- Lines starting with `#` are narration (shown), with `%` silent; `%stop`
  ends a script.
- Forms of the earlier syntax are refused with their replacement;
  `tools/migrate_syntax.py` rewrites old files.

## Interactive commands

Interactive mode accepts the commands of scripts. A command without its
final `.` asks for more with `...`; `exit` or `quit` leaves.

Syntax/prover errors are non-fatal in the interactive wrapper unless the wrapper
detects a machine-mode desynchronization.

### Recording commands

- `argument NAME : (CONCLUSION)`
  - begin recording an argument
- `counterargument NAME : (CONCLUSION)`
  - begin recording a counterargument (an argument against CONCLUSION)
- `dixi`, also `cr` and `case rested`
  - finish recording, execute the proof against Fellowship, and register it;
    if its body is closed, Fellowship also keeps it as a theorem, so
    `axiom NAME` cites it
- `theorem NAME : (PROP).`, also `lemma`, `proposition`, `claim`, and
  `antitheorem`, `antilemma`, `antiproposition`, `anticlaim`
  - record a claim, and nothing more (see below)
- `prove NAME`, also `refine`, `argue`, `refute`, `dispute`
  - reopen a registered argument or claim where it left off; all five are
    synonyms and work for claims and counterclaims alike
- `cite NAME.` (inside a recording)
  - use the registered argument NAME at the focused goal; the term shows the
    name, as an axiom would
- `qed.`
  - end the recording and demand a strict witness; refused, leaving the
    argument as it was, while an obligation, a presumption or a defeasible
    citation remains open
- `adopt EDGE* as NAME`
  - make a strict edge found by unfolding, such as Peirce's thesis, a theorem
- `expand ARG`
  - print ARG's full term, with every cited argument's term grafted in
- `share ARG`
  - print the debate about ARG's issue as named sub-debates: the issue's
    term, then one `NAME[open sites] := term` line per sub-debate it cites

### Statements, refinement and citation

The wrapper owns the registry of every statement and argument, and Fellowship
keeps exactly the strict proofs. A name Fellowship knows is a strict axiom
everywhere, so it never holds a defeasible argument.

- **A statement only states.** `lemma foo : (A).` registers the claim as the
  maximally enthymemic argument `μfoo:A.⟨?1:A‖foo⟩`, which puts an obligation
  marker on A: A is claimed and owes a proof. It opens nothing, so claims can
  be collected first and proved later.
- **Refinement.** `prove foo` reopens foo, replaying what it has so far, so the
  proof continues from its open goals, presumptions included. It ends with
  `qed.`, which demands a strict witness, or `dixi.`, which registers
  the result either way. Proving a claim is refining its enthymeme; any
  defeasible argument can be refined the same way. The result replaces the
  argument in place, keeping its position in registration order, and the
  document graph is rebuilt. When a refinement makes an argument strict, the
  arguments that cite it are replayed, and those that became closed are held
  by Fellowship too.
- **`axiom` is for strict content, `cite` for arguments.** `axiom NAME` always
  goes to Fellowship, which refuses a defeasible NAME. `cite NAME` uses any
  registered argument. A strict one Fellowship closes itself; a defeasible one
  closes the goal for the author and leaves Fellowship's goal open. Either
  way the term shows the name at that site, like an axiom, so terms stay
  readable as the library grows. The cited argument must conclude the goal's
  proposition, on the same side. A cited site is done: a later tactic that
  would land on it is refused.
- **Citation by name is late binding.** A citer means "the argument NAME as it
  now stands". The document edge of a citer has an obligation at the cited
  conclusion, which the cited argument's own edge meets; unfolding expands the
  citation like any obligation. Refine the cited argument and every citer
  follows. `expand ARG` computes the full term on demand.
- **Sub-debates are shared by name.** The debate about an issue brings in the
  debate of every statement it reaches. `share ARG` writes a sub-debate that
  is needed in two or more places once and cites it by name - the author's
  name where one argument or debate is about that statement, `anon_1`,
  `anon_2`, ... otherwise (a reserved prefix) - and leaves one needed once in
  place. A citation shows what its site does to the cited debate:
  `d[alpha -> !:A, B:?]` reads "d, with alpha capturing its delegation of A
  and its obligation B left open"; the header `d[...] :=` lists the sites the
  sub-debate rests on. Names are transparent: writing every definition back
  in gives the term `graph`, `label` and `evaluate` work on, which for now is
  still what they compute.
- **Adopting a discovered strict edge.** `graph ARG` can show strict edges,
  named with a trailing star, that the strict phase found in the unfolded
  term. Showing them changes nothing. `adopt s1* as peirce` replays the closed
  term stored on the edge with `qed.`, so Fellowship checks it again, and
  registers it as the theorem `peirce`.
- **Names are unique per document.** Sorts, declared axioms, statements and
  arguments share one namespace, checked before anything reaches the prover;
  `new document` starts a new one. Names starting with `typecheck_`,
  `theta_expand_` or `anon_` are reserved for the names the wrapper generates.
- **Sessions start in LK.** Debates are classical, and a session starts with
  a classical document, so the type check of a debate never runs in LJ by
  accident.

### Rendering commands

A target is an argument or debate name, `issue :X` (X proved) or
`issue X:` (X refuted). `TERM` picks which term: `registered` (default),
`enriched`, `unfolded` (the debate term) or `evaluated` (the normal form
of the last `evaluate`).

- `render TARGET [STYLE] [TERM]`
  - print the term in a natural-language style (see *Render styles*)
- `tree ARG [nl [STYLE] | pt] [TERM]`
  - render an acceptance tree coloured by the grounded labels (see
    Debate-graph commands); drawn uncoloured if the graph is refused
- `unfold argument NAME`, `unfold issue :X`, `unfold issue X:`,
  `unfold debate NAME`
  - print the debate term the pipeline builds

### Debate-graph commands

Every atomic argument you register (`argument ... dixi.`,
`register`) adds its hyperedges to one **document graph** for the whole
file; proposition identity is global to it, so a counterargument to Q
registered anywhere contests every use of Q. A debate (see *Debates*)
selects and orders arguments without adding an edge. `graph`, `label`
and `evaluate` take a name, read its issue,
**unfold** the document graph from that issue into a debate term
(cycles broken by capture: a demand for P inside a proof of P->Q is the
hypothesis, a demand for a refutation of P inside a proof of P is the
continuation; a captured presumption stays a presumption source of its edge, which is how a cycle is represented), compile that
term into the issue's debate graph, label it by an ADF semantics and
evaluate it under a witness labelling. `graph document` and `label
document` show the document graph itself. Every unfolded term is
replayed through Fellowship first - the type oracle: if the arguments
type-check, so must their unfolding - and refused with a one-line
message if the prover rejects it; `typecheck off` (or `FSP_TYPECHECK=0`)
skips the replay for production runs, and `typecheck expanded` (or
`FSP_TYPECHECK=expanded`) replays the whole unfolded term instead of one
sub-debate at a time. Debates are classical: their
scaffolds throw to a second conclusion, which LJ forbids, so the debate
commands are refused while the file is in `lj`. Derivation cycles are
admitted; `graph` reports which fragment (acyclic or cyclic) a debate
is in. They cover
the quantifier-free fragment and refuse first-order terms with a
one-line message.

- `graph TARGET|document [all] ["FILE.dot"] [show]`
  - print the nodes, hyperedges and default markers of the target's issue
    graph (unfolded from the document) or of the document graph; write
    Graphviz DOT when a filename is given; `all` shows a debate's whole
    scope
  - every lambda is a subargument edge of its own; a subargument that
    captured a binder of an enclosing one records the capture as a source
    at that binder's statement, so the framework shows a sprung trap as a
    cycle; strictness is then read off the unfolded term and adds a strict
    edge (`NAME*`) for every closed derivation the framework missed
  - strict in the debate means the subterm rests on nothing, that is, it
    has no open site. A captured variable is a commitment the debate
    already made, whichever kind of site it replaced, so a self-attacking
    argument derives its conclusion. A clash rests on nothing either, so
    it decides a scaffold, but it derives its statement only from an
    inconsistency, so it claims no strict edge and surfaces as an
    `EXCEPTION` instead
  - `show` renders the graph and opens it in the platform viewer; without
    Graphviz installed it prints an indented text view instead
- `label TARGET|document [grounded|complete|preferred|stable]`
  - print the labelling(s) of the chosen semantics (default grounded):
    one `IN` / `OUT` / `UNDEC` per proposition and side; several
    labellings are numbered
  - a presumption delegates the onus of refutation to the other side, so
    a side carrying only a default marker does not contest a side that is
    actually derived: an argument for the contrary defeats a bare
    presumption outright, while two derivations still contest each other
    and a presumption's own default stays guarded
  - presuming *both* sides of one proposition means neither side holds
    the onus; that is reported as a warning and will become an error
- `explain TARGET [same options as evaluate]`
  - run one evaluation and print the pipeline's own stage-by-stage account of
    it, then the verdict; needs no change to the log level
- `evaluate TARGET [skeptical|credulous] [grounded|complete|preferred|stable] [cbn|cbv] [N|all] [favour]`
  - label-guided evaluation; options in any order, defaults skeptical,
    preferred, cbn; prints the normal-form class (`VALUE`, `EXCEPTION`,
    `OPEN`) and the normal form, cached for `render TARGET evaluated`
  - in credulous mode `N` evaluates under labelling N of `label`, `all`
    under every labelling that accepts the issue, and `favour` prefers a
    witness in which the argument's own derivation is IN
  - credulous evaluates against one labelling of the chosen semantics in
    which the argument's issue is IN (if none exists, against the
    grounded labelling, resolved skeptically); skeptical against the
    intersection of all of them; the base strategy resolves only
    critical pairs that labelling leaves open
  - scaffolds are the COMMA 2026 paper's support and attack shapes (the
    context side mirrored), so on the proponent's side credulous is
    call-by-name and skeptical call-by-value; a defeated site holds the
    clash `mu alpha.< t || E >`, the paper's abort, and a normal form
    containing an uncatchable clash is an `EXCEPTION`
  - the sites of a normal form are named by their labels: `!IN:A` is a
    delegation (the opponent must refute A), `?OUT:A` and `?UNDEC:A` are
    obligations (A was not established under the mode)

### Registering proof terms directly

- `register NAME [strict] : TYPE := "PROOF_TERM"`
  - replay a hand-written proof term against Fellowship and register the
    result as a named argument, without going through `argument` /
    `dixi.`. The term is double-quoted: it contains dots. `TYPE` is the conclusion proposition; `PROOF_TERM` is
    parsed with the same proof-term grammar used elsewhere in AIDA (see
    *First-order logic* and `pres/` for term syntax). Without `strict`, the
    replayed theorem is discarded after the wrapper extracts the argument
    state; with `strict`, replay is finalized with `qed.` so Fellowship
    actually declares the theorem/antitheorem.

  ```text
  register tester2 : A := "μtester:A.<μelur:A.<1.1.1:B!*elur:A*_T_||elur1:(true-A)-B>||tester:A>".
  ```

  (from `tests/olon.fspy`, which also shows the `μ'`-headed dual form)

### Debates

A *debate* is a named, ordered selection of the document's arguments
(`core/dc/debate.py`): who said what, in which order, in reply to whom.
It is compiled on demand and never joins the document graph, so it adds
no edge and duplicates nothing.

```
debate pro|con open|closed NAME : ISSUE.
ARG.
[VERB] ARG TARGET.
...
cedat tempus.
```

- The **onus** says who opens: `pro`, an argument for ISSUE; `con`, a
  counterargument. The opening move must match it.
- The **scope** says what the debate hears. A `closed` debate is compiled
  from its moves' arguments only, together with every argument they cite
  (`cite NAME`): a limited debate, for an agent's limited knowledge, a
  counterfactual, a checklist or a transcript. An `open` debate hears
  the whole document. The strict store (declarations and theorems) is
  always in scope. Inside the scope conflicts arise by proposition
  identity, as in the document, so a move attacks or supports wherever
  its conclusion fits, not only at its target.
- A move **adds** ARG to the scope and **orders** the term, which is
  biased towards the debate's own arguments: it is unfolded for the
  opening argument (on top of the issue's stack, its sites as it wrote
  them), and at every statement the moved arguments stand above the
  others, the last uttered outermost, so judged first.
- A **verb** only checks the move. The target must be an earlier move with
  a node the mover's conclusion can reach, of the verb's kind:

  | verb | the target needs |
  |---|---|
  | `attack` | a site of the contrary of the mover's conclusion |
  | `rebut` | ... that is an obligation |
  | `undermine` (= `undercut`) | ... that is a presumption |
  | `support` | a site of the mover's conclusion |
  | `buttress` (= `reinforce`) | ... that is an obligation |
  | `undergird` | ... that is a presumption |

  A node is a site of the target's body (its subarguments included) or
  its conclusion, which counts as an obligation unless the scope presumes
  it. Intermediate conclusions and strict leaves are not nodes yet
  (rational closure). Without a verb a move is not checked: a non
  sequitur is allowed and simply joins the scope.

Every command that takes an argument also takes a debate: `evaluate`,
`explain`, `label`, `render` (the debate's term, or `evaluated`), `tree`,
`unfold debate NAME`. `graph NAME` shows what the debate's conclusion
reaches; `graph NAME all` shows its whole scope, non sequiturs included.
While a debate is being recorded these commands compile it as it stands.
The term is cached until the document or the debate changes.

## Script files (`.fspy`)

Script mode uses the same general command language as the interactive wrapper.
Representative examples live in `tests/*.fspy`.

Typical script commands include:
- Fellowship commands such as `lk.`, `declare ...`, `deny ...`, `qed.`
- wrapper recording commands such as `argument NAME : (P).` ... `dixi.`
- queries and rendering commands
- debates: `debate pro|con open|closed NAME : ISSUE.`, moves, `cedat tempus.`
- wrapper-only decoration commands such as `decorate NAME : "template".`

Lines starting with:
- `#` are echoed as user-facing comments
- `%` are silent comments

### Decorations

`decorate` commands attach wrapper-side natural-language metadata to names used
by renderers. They are consumed by AIDA's Python wrapper and are **not** sent to
Fellowship.

```text
decorate Bird : "@arg1 is a bird".
decorate Bird_list : "@arg1 ist eine \\sn{Liste}".
```

Templates are double-quoted, like everything else that may contain a dot,
and may contain positional placeholders: `@arg1`, `@arg2`, ... refer to the
arguments of a rendered proposition, so with the first line above the
proposition `Bird Tweety` renders as `Tweety is a bird`. A single-quoted
template is refused with its double-quoted replacement.

A template uses JSON string syntax, which is also what the importers write
(with `json.dumps`). These characters must be escaped:

| Intended runtime character | `.fspy` spelling |
| --- | --- |
| backslash `\` | `\\` |
| double quote `"` | `\"` |
| newline | `\n` |
| tab | `\t` |
| carriage return | `\r` |

Thus the second line above stores the template `@arg1 ist eine \sn{Liste}`,
with one backslash.

## Important prover-side commands and concepts

AIDA relies on several Fellowship features beyond plain sequent proofs.

### `theorem` and `antitheorem`

At the prover level, theorem construction starts with commands like:

```text
theorem argA : (A).
antitheorem notA : (A).
```

The wrapper intercepts these: `theorem NAME : (P).` only states a claim, and
`prove NAME` opens its proof, which the wrapper replays and checks at `qed.`
(see "Statements, refinement and citation"). Scripts written in Fellowship's
own idiom, `theorem X : (A).` followed directly by tactics and `qed.`, need a
`prove X.` line after the statement. The blocks `argument NAME : (P).` and
`counterargument NAME : (P).` open the same kind of recording without stating
a claim.

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
refute notA.
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

The debate pipeline — unfolding, the debate graph, labelling and
evaluation — does not handle first-order terms. The
reduction rules for first-order AC/DC are not settled, so rather than guess,
those operations raise `FirstOrderNotSupported` naming the construct they
stopped at.

## Render styles

`render TARGET STYLE` supports:

- `argumentation` (the default)
- `dialectical`
- `intuitionistic`
- `vanilla`
- `pruefschema`

Examples:

```bash
.venv/bin/acdc --interactive
```

Then inside the REPL:

```text
render myarg vanilla.
render myarg dialectical evaluated.
```

## Acceptance trees

```text
tree ARG.
tree ARG pt.
tree ARG nl.
tree ARG nl dialectical.
```

Tree rendering uses Graphviz when available and otherwise writes a `.dot`
file. Each box on the spine is filled by the grounded ADF label of the
statement its binder establishes (green IN, red OUT, yellow UNDEC), taken
from the argument's debate graph. Use `label` for the labels as text.

## Examples

### Arguments, a debate and an evaluation

File: `tests/debates.fspy`: Tweety flies, being a bird (`tweety`, a
presumed rule); it does not, being a penguin (`penguin`, presumed); a photo
shows it is no penguin (`photo`, presumed). Below, the start of the file
and, after it, a debate over its arguments:

```text
lk.
declare Bird, Penguin, Flies, Photo, Sings : bool.
declare bird : (Bird).
declare penguins_dont_fly : (Penguin -> ~Flies).
declare photos_show : (Photo -> ~Penguin).
argument tweety : (Flies).
cut (Bird -> Flies) birds_fly.
by default.
next.
elim.
axiom bird.
axiom birds_fly.
dixi.
...
debate pro closed d : Flies.
tweety.
rebut penguin tweety.
undermine photo penguin.
ct.
evaluate d.
```

`evaluate tweety.` on the document gives a value: penguin attacks the
presumption that birds fly, and photo defeats penguin.
`docs/minicourses/minicourse-api.org`, lesson 4, checks this document with
the debate.

### Counterargument and undercut example

File: `tests/counterarguments_and_undercut.fspy`

This script demonstrates:
- `counterargument ... dixi.`
- debates (`debate ... cedat tempus.`) with `undercut` moves
- `evaluate`
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
- debates with `support` and `undercut` moves
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

- `core/` — the argument calculus (`ac/`), transformations, labelling and
  evaluation (`comp/`), arguments, debates and the debate compiler (`dc/`)
- `pres/` — presentation layers (proof terms, NL, trees)
- `wrap/` — the session, document, syntax, interpreter, service, JSON
  and CLI, and the native Fellowship subtree
- `wrap/fellowship/` — native prover sources and `fsp` binary build target
- `docs/` — the integration guide for the document UI and the minicourses
- `examples/` — a reference HTTP server over the service layer
- `tools/` — migration scripts for the command syntax
- `bench/` — scaling benchmarks
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
