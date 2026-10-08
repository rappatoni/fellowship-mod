# Wrapper (prover integration and CLI)

Files
- prover.py
  - ProverWrapper, the session: one Fellowship process (pexpect), its settings
    and a re-entrant lock every send holds; it holds one document at a time
    (`doc`) and `new_document()` replaces it.
  - send_command(), machine payload parsing; the logic toggles, valid at the
    head of a document only.
  - Exceptions: ProverError, MachinePayloadError.
  - Env: FSP_MACHINE=1 enables machine mode; FSP_ECHO_NOTES toggles note logging;
    ACDC_NO_RENDER=1 stops `graph ... show` and `tree` writing image/DOT files
    (ACDC_NO_OPEN=1 only stops the viewer).
- document.py
  - Document: everything a new document replaces - logic, declarations,
    decorations, names, arguments, the document graph, debates, caches.
    Nothing module-global: two sessions never share one.
- cli.py
  - CLI/task helpers: setup_prover(), execute_script(), interactive_mode(), plus the command helpers (record, register, render, reduce, tree, graph, label, evaluate, explain, debates).

Notes
- Tests and callers import from wrap.cli and wrap.prover directly (no shim).
- Keep logging configuration in CLI helpers configurable (FSP_LOGLEVEL supports TRACE).
- The TRACE level itself lives in core/logging_util.py, not here: core modules
  log at TRACE without importing the CLI. cli.py re-exports it.
- The pipeline narrates itself at DEBUG (see the top-level README); `explain ARG`
  captures that account for one evaluation and prints it grouped by stage.
