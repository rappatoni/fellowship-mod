"""Diamond chains: level i has statements X_i and Y_i, each derived from both
X_{i-1} and Y_{i-1} (presumed rules, so every edge is defeasible); the base
X_0, Y_0 are presumed. Paths to the top double per level. Time `label` and
`evaluate` (skeptical, preferred) of the top statement, three runs each.

Usage: python scaling.py MAXN [typecheck on|off]"""
import sys, time, statistics
from aida_env import setup_prover, Service

def doc(n):
    names = [f"X{i}" for i in range(n + 1)] + [f"Y{i}" for i in range(n + 1)]
    out = ["lk.", f"declare {', '.join(names)} : bool."]
    out += ["argument bx : (X0).", "by default.", "dixi.", "argument by : (Y0).", "by default.", "dixi."]
    for i in range(1, n + 1):
        for tgt in ("X", "Y"):
            for src in ("X", "Y"):
                out += [f"argument {tgt.lower()}{i}_{src.lower()} : ({tgt}{i}).",
                        f"cut ({src}{i-1} -> {tgt}{i}) r.", "by default.", "next.", "elim.", "next.", "axiom r.", "dixi."]
    return "\n".join(out) + "\n", f"x{n}_x"

def main():
    maxn = int(sys.argv[1]); tc = sys.argv[2] if len(sys.argv) > 2 else "on"
    svc = Service.of(setup_prover()); svc.set_typecheck(tc)
    for n in range(1, maxn + 1):
        text, top = doc(n)
        times_l, times_e = [], []
        for _ in range(3):
            svc.check(text)
            t = time.monotonic(); svc.label(top, "grounded"); times_l.append(time.monotonic() - t)
            t = time.monotonic(); ev = svc.evaluate(top); times_e.append(time.monotonic() - t)
        print(f"n={n:2d} paths=2^{n} label {statistics.median(times_l):7.3f}s  evaluate {statistics.median(times_e):7.3f}s  verdict {ev.nf_class}", flush=True)

main()
