# Wrapper (prover integration and CLI)

Files
- prover.py
  - ProverWrapper: manages a Fellowship process (pexpect).
  - send_command(), machine payload parsing; per-session declarations; global arguments store via mod.store.
  - Exceptions: ProverError, MachinePayloadError.
  - Env: FSP_MACHINE=1 enables machine mode; FSP_ECHO_NOTES toggles note logging.
- cli.py
  - CLI/task helpers: setup_prover(), execute_script(), interactive_mode(), plus reduce/render/color/tree/undercut command helpers.

Notes
- Tests and callers import from wrap.cli and wrap.prover directly (no shim).
- Keep logging configuration in CLI helpers configurable (FSP_LOGLEVEL supports TRACE).
