# Scaling benchmarks

The throwaway scripts behind the complexity analysis of 2026-09-30, kept
so the measurements can be repeated. They are **not** the harness that
`aida-scaling-benchmarks` (tasks.org) asks for: `run.sh` times a single
run, and the parser benchmark takes the best of 3. Turning them into
repeated, equivalence-checked timings is that task.

Run everything from the repository root.

| Script                 | Measures                                                      |
|------------------------|---------------------------------------------------------------|
| `gen.py KIND N`        | writes a synthetic `.fspy` (`chain`, `stated`, `double`, `decls`) |
| `run.sh KIND N [nolabel]` | one end-to-end CLI run of a generated file; files go to `bench/out/` |
| `graphbench.py [N ...]`  | graph utilities and adf-bdd grounded labelling, no Fellowship |
| `loops.py [K ...]`       | grounded / complete / preferred on k independent even loops |
| `parsebench.py`          | Earley parse time and recursion depth                       |

Examples:

```sh
for n in 20 40 80 160; do bench/run.sh stated $n nolabel; done   # refinement rebuilds
for k in 4 6 8; do bench/run.sh double $k; done                  # unfolding without sharing
.venv/bin/python bench/graphbench.py 6400 12800
.venv/bin/python bench/loops.py 2 4
```

`run.sh double 10` takes about two minutes, and `loops.py 6` runs into
adf-bdd's 120 s timeout for each of the two multi-extension semantics.
