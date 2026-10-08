# DC (Debate calculus)

Files
- argument.py
  - Argument: executes/normalizes/render arguments against a live prover.
  - Methods: execute(), normalize(), render(), reduce(); the projections out/tou/sub/bus/attacker/regatta (deprecated).
- debate.py
  - Debate, Move: debates as named, ordered, scoped selections of the document's arguments; the verb checks; unfold_debate.
- debate_graph.py
  - Debate-graph compilation (debate-graph-spec.org, layers 2-3): DebateGraph, the document graph.
- unfold.py
  - Unfolding: document graph -> debate term, for an issue or an argument entrypoint.
- strict.py
  - Strictness, read off the concrete debate term.
- typecheck.py
  - Type checking by Fellowship replay (the type oracle).
- cite.py
  - Citation by name (`cite NAME`).
- share.py, instances.py
  - Sharing: the debate of an issue as named sub-debates; the issue graph built from it without unfolding.
- graft.py
  - graft_single(body_B, number, body_A): replace a single open Goal/Laog; capture-aware; alpha-renames on collision.
  - graft_uniform(body_B, body_A): replace all matching open targets; alpha-rename before graft.
- match_utils.py
  - match_trees, get_child_nodes, is_subargument: structure matching on ASTs (used by tests/utilities).

Notes
- Replacement must match kind/prop exactly (Mu→Goal, Mutilde→Laog).
- Perform alpha-renaming before grafting if binders collide.
