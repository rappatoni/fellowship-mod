"""Graph-layer scaling without Fellowship: build layered DebateGraphs
directly and time the graph utilities and the adf-bdd grounded labelling.

    .venv/bin/python bench/graphbench.py [N ...]      (from the repository root)

Default sizes 100 .. 3200; larger sizes (6400 .. 25600) show the quadratic
growth of compile_conditions and of adf-bdd itself.
"""

import logging
import random
import sys
import time

sys.path.insert(0, ".")
sys.setrecursionlimit(100000)

from core.comp import adf_label
from core.dc.debate_graph import DebateGraph, Edge, Source


def build(n, fan=2, seed=0, attack_frac=0.1):
    """Layered DAG: statement i is derived from ``fan`` random earlier
    statements; about ``attack_frac`` of the edges attack (context side)."""
    rnd = random.Random(seed)
    g = DebateGraph()
    for i in range(n):
        g.nodes[f"P{i}"] = f"P{i}"
    g.mark_default("P0", "term", "presumption")
    for i in range(1, n):
        srcs = tuple(Source(f"P{j}", "term", "obligation", str(j))
                     for j in rnd.sample(range(i), min(fan, i)))
        side = "context" if rnd.random() < attack_frac else "term"
        g.add_edge(Edge(f"e{i}", f"P{i}", side, srcs, False))
        for s in srcs:
            g.mark_default(s.key, s.side, "presumption")
    return g


def timed(f):
    s = time.perf_counter()
    r = f()
    return time.perf_counter() - s, r


def main(sizes):
    logging.disable(logging.CRITICAL)
    print(f"{'n':>6} {'statements':>10} {'conds':>8} {'reach':>8} {'acyclic':>8} {'adf-bdd grd':>12}")
    for n in sizes:
        g = build(n)
        ts, stmts = timed(g.statements)
        tc, _ = timed(lambda: adf_label.compile_conditions(g))
        tr, _ = timed(lambda: g.reachable((f"P{n-1}", "term")))
        ta, _ = timed(g.is_acyclic)
        try:
            tl, _ = timed(lambda: adf_label.grounded_labels(g))
            tl = f"{tl:12.2f}"
        except Exception as e:
            tl = type(e).__name__[:12]
        print(f"{n:6d} {ts:10.3f} {tc:8.3f} {tr:8.3f} {ta:8.3f} {tl:>12}  ({len(stmts)} stmts)",
              flush=True)


if __name__ == "__main__":
    main([int(a) for a in sys.argv[1:]] or [100, 200, 400, 800, 1600, 3200])
