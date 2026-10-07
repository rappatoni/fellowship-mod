# AIDA repository instructions

These instructions apply to every AI agent working in this repository.

## Authoritative repository state

Before planning or reporting progress, read:

- `README.md`
- `tasks.org`
- the relevant component README files
- the current Git status and recent Git history

Treat those files and verified build, test, and benchmark results as
authoritative. Conversation memory is not authoritative repository state.

The supervision repository is authoritative for PhD-wide priorities,
milestones, and daily planning. This repository is authoritative for detailed
AIDA implementation tasks and their technical evidence.

## Task management

- `tasks.org` is the canonical AIDA implementation backlog.
- Every actionable task needs an observable `OUTCOME` and `VERIFY` method.
- `IMPORTANT` and `URGENT` determine `EISENHOWER`; do not set contradictory
  values.
- `ICK` is an integer from 0 (pleasant) to 5 (strongly avoided).
- At most two substantial tasks may be `DOING`.
- `NEXT` means unblocked and genuinely ready to start.
- Do not mark a task `DONE` without recorded evidence.
- Keep implementation detail here. Mirror only PhD-level outcomes,
  dependencies, deadlines, or blockers into the supervision repository.
- Keep every Org `ID` globally unique. When an outcome is represented in both
  repositories, connect the two tasks with explicit reciprocal `SYNC_WITH`
  links rather than duplicating an `ID`.
- Store each reciprocal target as `SYNC_WITH: id:<other-id>`. Change TODO state
  only in the owning repository; portfolio umbrella state is reviewed manually
  from local evidence.

## Scholarly authorship

This repository contains research software and may also contain scholarly
paper material. Do not generate or substantively rewrite scholarly claims,
definitions, proofs, examples, or argumentative prose in manuscript sources.
Use critique outside the manuscript or an explicitly marked AI note instead.
The detailed authorship policy in the supervision repository controls when it
is available.

## Working rules

- Preserve unrelated user changes and inspect the dirty worktree before edits.
- Prefer focused changes that keep Python and Fellowship concerns separated.
- Run the focused tests for every change and the documented test suite when
  feasible.
- Rebuild Fellowship and run its relevant tests when OCaml sources change.
- Support performance claims with a reproducible fixture, configuration, and
  repeated timings. Record semantic equivalence checks as well as elapsed time.
- Do not edit generated or compiled artifacts manually.
- Do not push, merge, publish releases, or mutate external project state
  without explicit approval.

## End of a substantial session

1. Summarize actual changes.
2. Report verification performed and its result.
3. Identify remaining problems and blockers.
4. Propose any task-status or PhD-wide synchronization changes.
5. Show repository-state changes for review before committing or pushing.
