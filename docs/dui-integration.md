# Integrating AIDA into the document UI

For the document UI (branch `DUI`): how a server uses AIDA, what it gets
back, and what to watch for. The HTTP server, its endpoints and the editor
are the UI side's; AIDA provides the service layer, the JSON formats and
this guide. `examples/http_adapter.py` is a runnable reference server
(standard library only) that does everything below; take from it what is
useful.

## What changed since the `DUI` branch was cut

The branch predates three changes that break `server.py` and `prototype.py`:

- **`core.comp.color` is gone.** Statuses come from the debate pipeline:
  a verdict per issue (`value`, `open`, `exception`) and a label per
  statement (`IN`, `OUT`, `UNDEC`), under a mode (`skeptical` /
  `credulous`) and a semantics (`grounded`, `complete`, `preferred`,
  `stable`).
- **The grafting verbs are gone** (`Argument.attack`, `.support`,
  `.undercut`, ...). Every registered argument joins the *document graph*,
  where conflicts arise by proposition identity; a *debate* names an
  ordered selection of arguments (README, *Debates*).
- **The syntax changed** (README, *Command syntax*): every command ends in
  `.`; arguments are `argument N : (P).` ... `dixi.` (or `cr.`); debates
  end with `cedat tempus.` (or `ct.`); proof terms, templates and file
  names are double-quoted. `tools/migrate_syntax.py` rewrites old files.
  The editor's own block syntax is no longer needed: send the document
  text as it is.

## Installing

From the repository root (`make install`; the README has details):

- the Python environment: `make install` (or `uv` into `.venv`);
- Fellowship: `make -C wrap/fellowship` builds `wrap/fellowship/fsp`
  (OCaml); `ACDC_FSP` points elsewhere;
- adf-bdd, the labeller: `make adf-bdd` (Rust; `ADF_BDD_BIN` points
  elsewhere). Without it there are no labels and no evaluations; graphs
  still work.

Under WSL build both natively inside WSL; the server then runs there too.

## Sessions

A *session* is one Fellowship process with its settings
(`wrap/prover.py`, `ProverWrapper`; `wrap.cli.setup_prover()` makes one). It
holds one *document* at a time (`wrap/document.py`). Sessions in one process
share nothing.

```python
from wrap.cli import setup_prover
from wrap.service import Service

session = setup_prover()           # one Fellowship process
svc = Service.of(session)          # the session's operations
...
session.close()                    # always: it ends the process
```

**Concurrency.** Every service call holds its session's lock, so calls on
one session are serialised and calls on different sessions run in
parallel (one Fellowship process each). Python threads are enough for the
I/O. The debate pipeline itself is pure Python and contends for the GIL, so
heavy concurrent use wants worker processes; sessions live in memory with
their process, so route a client to the same worker every time (sticky
sessions). Each session holds one process and its pipes: bound the number
of sessions and close idle ones.

**Transport.** AIDA talks to Fellowship over pipes: commands of any length
work. One command times out after `ACDC_TIMEOUT` seconds (default 5).

## Checking a document

The editor's main call. Send the whole text; AIDA replays it in a new
document of the session and reports on every command:

```python
report = svc.check(text)
```

`to_json(report, session)` (`wrap/serialize.py`) gives:

```json
{"aida": "1.0", "kind": "report", "ok": false, "seconds": 0.09,
 "entries": [
   {"span": {"line": 44, "col": 1, "end_line": 44, "end_col": 24},
    "text": "rebut photo penguin", "command": "move", "status": "refused",
    "message": "debate 'd': rebut: 'photo' concludes Penguin:, but ...",
    "error": {"code": "content", "stage": "debate", ...}, "result": null,
    "diagnostics": []},
   {"span": {...}, "text": "evaluate d", "command": "evaluate", "status": "ok",
    "result": {"kind": "evaluation", "verdict": "open", "sigma": [...], ...}}
 ]}
```

- `status` is `ok`, `refused` (the document refused it: underline it),
  `reported` (a query the pipeline refused: show the message), `error`
  (Fellowship refused it), `fatal` (the connection broke), `skipped`
  (a CLI-only command such as `explain`), `comment` or `stop`.
- Queries written in the text (`evaluate d.`, `label d.`, `graph d.`) carry
  their result at their own line: that is how to show "the state of the
  debate at line N".
- The check goes on after a problem, so one pass shows every problem. It
  costs a replay of the whole text each time; measured at about 0.1 s for
  a 50-command document.

## Queries

After a check, the session holds the document; queries work on it:

| Call | Result kind | Holds |
|---|---|---|
| `svc.graph(target, whole=False, labels=True)` | `graph` | nodes (statements, with grounded labels), hyperedges, default markers |
| `svc.label(target, semantics)` | `labellings` | every labelling of the semantics |
| `svc.evaluate(target, mode=, semantics=, base=, witness=, favour=)` | `evaluation` | `verdict`, `sigma` (the witness labelling), the normal form, the graph |
| `svc.term(target, which)` | `term` | a term: `registered`, `enriched`, `unfolded`, `normal`, `evaluated` |
| `svc.render(target, which, style)` | `rendering` | natural language (`argumentation`, `dialectical`, `vanilla`, `pruefschema`, ...) |
| `svc.tree(target, mode=, which=)` | `tree` | an acceptance tree as DOT text |
| `svc.inventory()` | `inventory` | declarations, arguments (with their spans), debates, names |

A target is a name (an argument or a debate), `issue :X` (X proved) or
`issue X:` (X refuted); `svc.target(text)` reads one.

**Linking back to the text.** Every edge of a graph names the argument it
came from (`argument`) and that argument's `span` in the checked text; the
inventory gives each argument's span too. A *statement* is `{prop, side,
key}`; `key` is a stable id across checks of the same document.

**What to colour with.** The editor decides; the material is: the verdict
of an issue or debate, the label of every statement (`sigma` of an
evaluation, or the grounded labels of a graph), and the mode and semantics
they hold under. AIDA keeps its own words; the colours are the editor's.

## Terms

A term is `{"pres": "...", "tree": {...}}`: the printed term and its tree,
every node `{"id", "node", ...}` (`binder`, `term`, `context` for children;
`prop`, `name`, `number` and the like as text; `label` where the term is
labelled). `wrap.serialize.term_from_tree` reads a tree back exactly.

## Errors

A refusal raises an `AidaError`; `to_json(error)` is
`{"kind": "error", "code", "stage", "message", "span", "cause",
"diagnostics"}`. Codes: `not_found`, `invalid`, `refused`, `logic`,
`unfold`, `typecheck`, `compile`, `evaluation`, `recursion`,
`adf_bdd_missing`, `content`. `diagnostics` holds the warnings AIDA logged
during the call (on results too). The reference adapter answers 404 for
`not_found`, 422 for any other refusal.

## Importing from s(CASP)

With the `scasp-aida` package installed (it registers itself as an
importer), `svc.import_document("scasp", json_data, name="imported")`
translates s(CASP) JSON, checks the resulting document, and returns its
text, the check, and the *provenance*: every block's span with the
justification node it came from (a JSON pointer into the s(CASP) output),
and every binder of the imported term with its node. Errors and labels can
then be shown against the s(CASP) source.

## JSON formats

Every object is a versioned envelope `{"aida": "1.0", "kind": ...}`,
described by `wrap/schemas/aida.schema.json` (JSON Schema 2020-12). The
version follows semver: additions raise the minor version, changes the
major one.

## Known limits

- First-order debates and debates in `lj` or minimal logic are refused
  (`code: "logic"` or a compile refusal); the document is otherwise
  checked.
- Only sites are attack and support points; intermediate conclusions and
  strict leaves are not yet (rational closure).
- `explain` (the forensic account of an evaluation) is CLI-only.
