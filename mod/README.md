# Mod (persistence)

Files
- store.py
  - Simple in-memory registries:
    - arguments: global store proxied by ProverWrapper.arguments.
    - document: the session's document graph (key "graph").

Notes
- conftest.py clears store.arguments automatically between tests.
