# DC (Debate calculus)

Files
- argument.py
  - Argument: executes/normalizes/render arguments against a live prover.
  - Methods: execute(), normalize(), render(), reduce(), chain(), focussed_undercut(); support()/rebut() placeholders.
  - Normalization: enrichment → reduction → pres generation → NL rendering.
- graft.py
  - graft_single(body_B, number, body_A): replace a single open Goal/Laog; capture-aware; alpha-renames on collision.
  - graft_uniform(body_B, body_A): replace all matching open targets; alpha-rename before graft.
- match_utils.py
  - match_trees, get_child_nodes, is_subargument: structure matching on ASTs (used by tests/utilities).

Notes
- Replacement must match kind/prop exactly (Mu→Goal, Mutilde→Laog).
- Perform alpha-renaming before grafting if binders collide.
