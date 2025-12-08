# Mod (persistence)

Files
- store.py
  - Simple in-memory registries:
    - arguments: global store proxied by ProverWrapper.arguments.
    - declarations: not used by ProverWrapper (kept per session on the instance).

Notes
- conftest.py clears store.arguments automatically between tests.
