"""Multi-extension labelling on k independent two-node even loops
(tasks.org, aida-multi-extension-labelling-cost).

    .venv/bin/python bench/loops.py [K ...]           (from the repository root)

Each loop contributes 4 statements; complete semantics has 3^k labellings,
stable 2^k.  Measured 2026-09-30: grounded instant, complete 15-24 s at
k=4 and the 120 s solver timeout at k=6.  ``--export DIR`` writes the ADF
files instead, for running adf-bdd backends by hand.
"""

import logging
import sys
import time

sys.path.insert(0, ".")

from core.comp.adf_label import graph_to_adf, labellings
from core.comp.oracle import diamond_export
from core.dc.debate_graph import DebateGraph, Edge, Source


def loops(k):
    g = DebateGraph()
    for i in range(k):
        a, b = f"A{i}", f"B{i}"
        g.nodes[a] = a
        g.nodes[b] = b
        # A[t] <- B[c] and B[t] <- A[c], both presumed: an even loop
        # through contrariness.
        g.add_edge(Edge(f"ea{i}", a, "term", (Source(b, "context", "presumption", "1"),), False))
        g.add_edge(Edge(f"eb{i}", b, "term", (Source(a, "context", "presumption", "1"),), False))
    return g


def main(argv):
    logging.disable(logging.CRITICAL)
    export = None
    if "--export" in argv:
        i = argv.index("--export")
        export = argv[i + 1]
        argv = argv[:i] + argv[i + 2:]
    ks = [int(a) for a in argv] or [2, 4, 6]
    for k in ks:
        g = loops(k)
        if export:
            path = f"{export}/loops_{k}.adf"
            with open(path, "w") as fh:
                fh.write(diamond_export(graph_to_adf(g, guard=False))[0])
            print(f"wrote {path}")
            continue
        for semantics in ("grounded", "complete", "preferred"):
            s = time.perf_counter()
            try:
                result = f"{len(labellings(g, semantics))} labelling(s)"
            except Exception as e:
                result = f"{type(e).__name__}: {str(e)[:50]}"
            print(f"k={k} {semantics:9s} {time.perf_counter() - s:7.2f}s  {result}", flush=True)


if __name__ == "__main__":
    main(sys.argv[1:])
