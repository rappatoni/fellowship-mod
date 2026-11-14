"""adf-bdd routes on synthetic AIDA graphs: grounded and stable, naive and
hybrid backends, on support chains, layered graphs and independent even
loops; plus the stable models on the grounded-reduced ADF (solve only what
grounded leaves undecided) checked against plain `naive --stm`.

    .venv/bin/python bench/perf-2026-11/scripts/adf_routes.py
"""
import os, re, sys, time, logging, subprocess, tempfile
sys.path.insert(0, "bench"); logging.disable(logging.CRITICAL)
from core.dc.debate_graph import DebateGraph, Edge, Source
from core.comp.adf_label import graph_to_adf
from core.comp.oracle import ADF, diamond_export
from graphbench import build
from loops import loops
B = os.environ.get("ADF_BDD_BIN") or os.path.expanduser("~/.cargo/bin/adf-bdd")
LIMIT = float(os.environ.get("ROUTE_TIMEOUT", "60"))
TOK = re.compile(r"([TFu])\((\w+)\)")

def chain(n):
    g = DebateGraph(); g.nodes["P0"] = "P0"; g.add_edge(Edge("e0", "P0", "term", (), True))
    for i in range(1, n):
        g.nodes[f"P{i}"] = f"P{i}"
        g.add_edge(Edge(f"e{i}", f"P{i}", "term", (Source(f"P{i-1}", "term", "obligation", "1"),), False))
    return g

def solve(adf, lib, mode):
    text, nm = diamond_export(adf); rev = {v: k for k, v in nm.items()}
    with tempfile.NamedTemporaryFile("w", suffix=".adf", delete=False) as fh: fh.write(text); path = fh.name
    t = time.perf_counter()
    try:
        r = subprocess.run([B, "-q", "--lx", "--lib", lib, mode, path], capture_output=True, text=True, timeout=LIMIT)
    except subprocess.TimeoutExpired:
        os.unlink(path); return None, None
    os.unlink(path)
    return [frozenset((rev[a], v) for v, a in TOK.findall(l)) for l in r.stdout.splitlines() if l.strip()], time.perf_counter() - t

def simp(f, val):
    tag = f[0]
    if tag == "const": return f
    if tag == "var": return ("const", val[f[1]]) if f[1] in val else f
    if tag == "not":
        g = simp(f[1], val); return ("const", not g[1]) if g[0] == "const" else ("not", g)
    gs = [simp(g, val) for g in f[1]]
    absorb, unit = (False, True) if tag == "and" else (True, False)
    if ("const", absorb) in gs: return ("const", absorb)
    gs = [g for g in gs if g != ("const", unit)]
    return ("const", unit) if not gs else gs[0] if len(gs) == 1 else (tag, tuple(gs))

def stable_reduced(adf):
    t0 = time.perf_counter()
    (grd,), _ = solve(adf, "naive", "--grd")
    val = {s: v == "T" for s, v in grd if v != "u"}
    rest = [s for s in adf.statements if s not in val]
    fixed = frozenset((s, "T" if v else "F") for s, v in val.items())
    if not rest: return [fixed], time.perf_counter() - t0, 0
    models, _ = solve(ADF(rest, {s: simp(adf.ac[s], val) for s in rest}, guard=False), "hybrid", "--stmng")
    return (None if models is None else [m | fixed for m in models]), time.perf_counter() - t0, len(rest)

fmt = lambda r: "   timeout" if r[1] is None else f"{r[1]:7.2f}s/{len(r[0])}"
def row(name, g):
    adf = graph_to_adf(g, guard=False)
    cells = [solve(adf, "naive", "--grd"), solve(adf, "hybrid", "--grd"), solve(adf, "naive", "--stm"), solve(adf, "hybrid", "--stmng")]
    red, t_red, rest = stable_reduced(adf)
    same = "-" if cells[2][0] is None or red is None else set(cells[2][0]) == set(red)
    print(f"{name:13s} n={len(adf.statements):4d} | naive grd {fmt(cells[0])} | hybrid grd {fmt(cells[1])} | "
          f"naive stm {fmt(cells[2])} | hybrid stmng {fmt(cells[3])} | reduced ({rest} undecided) {t_red:6.2f}s same={same}", flush=True)

for n in [25, 50, 100, 200, 400, 800]: row(f"chain {n}", chain(n))
for n in [50, 100, 200, 400, 800]: row(f"layered {n}", build(n))
for k in [4, 8, 12, 16]: row(f"evenloops {k}", loops(k))
