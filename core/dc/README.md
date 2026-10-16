# DC (Debate calculus)

Files
- argument.py
  - Argument: a recorded or registered argument. execute() replays its instructions against Fellowship and reads back its proof term; open sites, enrichment and rendering.
- discipline.py
  - The argument discipline of the pure level: linear arguments, dual-linear counterarguments, no debate scaffold (tasks.org, aida-argument-lj-restriction); `violations(body, anti)`, enforced by wrap/registry.py at `dixi`.
- debate.py
  - Debate, Move: debates as named, ordered, scoped selections of the document's arguments; the verb checks; unfold_debate.
- debate_graph.py
  - Debate-graph compilation (debate-graph-spec.org, layers 2-3): DebateGraph, the document graph. Since 2026-10-15 by the horn rule: a binder's derivation has every horn of its command as a source, a horn that is a declared axiom or a progress node is an edge of its own, the spine (what carries the binder's own variable) gets none. First order (2026-10-17, aida-first-order-reduction): an instantiation (`ConsFO`) and an existential's introduction and elimination are spine steps, never horns; a universal introduction (`LamdaFO`) is a horn like a lambda; a site whose proposition mentions a first-order variable bound in the body is refused (only a statement can be owed), and a horn whose statement mentions one is not exposed, so every node is closed.
- unfold.py
  - Unfolding: document graph -> debate term, for an issue or an argument entrypoint; every exposed node (an axiom applied, a lemma, a lambda) becomes the debate about its statement where it stands.
- strict.py
  - Strictness, read off the concrete debate term.
- typecheck.py
  - Type checking by Fellowship replay (the type oracle).
- cite.py
  - Citation by name (`cite NAME`) and its expansion on demand (`expand ARG`).
- share.py, instances.py
  - Sharing: the debate of an issue as named sub-debates; the issue graph built from it without unfolding.
