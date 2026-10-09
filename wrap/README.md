# Wrapper (sessions, syntax, service and CLI)

Files
- prover.py
  - ProverWrapper, the session: one Fellowship process, spoken to over pipes
    (pexpect; `ACDC_TRANSPORT=pty` for a terminal), its settings and a
    re-entrant lock every send holds. It holds one document at a time
    (`doc`) and `new_document()` replaces it.
  - send_command(), machine payload parsing; the logic toggles, valid at the
    head of a document only.
  - Exceptions: ProverError, MachinePayloadError.
  - Env: ACDC_TIMEOUT (seconds per command, default 5); FSP_MACHINE=1
    enables machine mode; FSP_ECHO_NOTES toggles note logging;
    ACDC_NO_RENDER=1 stops `graph ... show` and `tree` writing image/DOT files
    (ACDC_NO_OPEN=1 only stops the viewer).
- sexp_parser.py
  - The S-expression reader for Fellowship's machine payloads.
- document.py
  - Document: everything a new document replaces - logic, declarations,
    decorations, names, arguments, the document graph, debates, caches.
    Nothing module-global: two sessions never share one.
- registry.py
  - Statements, refinement and citation: the wrapper's registry of every
    statement and argument; Fellowship keeps the strict proofs only.
- syntax.py
  - The one command syntax of scripts, the REPL and document checks:
    split() cuts a text into units with their spans, parse() reads one
    command. Every command ends in "."; what contains dots is double-quoted.
    Fellowship's own commands are opaque and pass through. Earlier forms are
    refused with their replacement.
- interpreter.py
  - Interpreter: runs parsed commands against a session - argument and
    debate blocks, statements, queries - and returns one Outcome per
    command (status, message, result, span).
- service.py
  - Service: the session's operations as calls that return result objects
    and raise AidaError (code, stage, diagnostics); the CLI is a printer over
    it. Every call holds the session lock and collects its own warnings.
    Service.check(text) replays a whole document and reports on every
    command; Service.import_document() imports from another formalism.
- serialize.py, schemas/aida.schema.json
  - Versioned JSON for every result, error and report (`{"aida": "1.0",
    "kind": ...}`), terms as text and as trees; the JSON Schema describes it.
- importers.py
  - The importer contract and the `acdc.importers` entry point (s(CASP):
    the scasp-aida package).
- cli.py
  - The CLI: setup_prover(), execute_script(), interactive_mode(), and the
    printers over the service (graph, label, evaluate, explain, render, tree,
    debates, ...).

Notes
- Programs use the service (`docs/dui-integration.md`,
  `docs/minicourses/minicourse-api.org`); `examples/http_adapter.py` is a
  reference HTTP server over it.
- Keep logging configuration in CLI helpers configurable (FSP_LOGLEVEL supports TRACE).
- The TRACE level itself lives in core/logging_util.py, not here: core modules
  log at TRACE without importing the CLI. cli.py re-exports it.
- The pipeline narrates itself at DEBUG (see the top-level README); `explain ARG`
  captures that account for one evaluation and prints it grouped by stage.
