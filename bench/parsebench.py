"""Parser scaling and recursion depth (best of 3 per size).

    .venv/bin/python bench/parsebench.py                  (from the repository root)

Measured 2026-09-30: right-nested implications quadratic (203 ms at
2.4 kB), left-nested linear, proof terms linear at ~35 us/char; at the
default recursion limit the transformer fails between mu-nesting depth
200 and 400.
"""

import sys
import time

sys.path.insert(0, ".")

from core.ac.prop import Prop
from core.ac.syntax import parse_proof_term


def best(f, reps=3):
    out = float("inf")
    for _ in range(reps):
        s = time.perf_counter()
        f()
        out = min(out, time.perf_counter() - s)
    return out


def mu_chain(n):
    text = "?1:P"
    for i in range(n):
        text = f"μa{i}:P.<{text}||a{i}:P>"
    return text


def main():
    sys.setrecursionlimit(100000)
    print("right-nested implication  P0 -> P1 -> ... -> Pn")
    for n in (10, 20, 40, 80, 160, 320):
        text = " -> ".join(f"P{i}" for i in range(n))
        print(f"  n={n:4d} len={len(text):5d}  {1000 * best(lambda: Prop.parse(text)):8.1f} ms")
    print("left-nested implication  ((P0 -> P1) -> P2) ...")
    for n in (10, 40, 160, 320):
        text = "P0"
        for i in range(1, n):
            text = f"({text} -> P{i})"
        print(f"  n={n:4d} len={len(text):5d}  {1000 * best(lambda: Prop.parse(text)):8.1f} ms")
    print("proof term: nested mu chain")
    for n in (10, 20, 40, 80, 160, 320):
        text = mu_chain(n)
        print(f"  n={n:4d} len={len(text):6d}  {1000 * best(lambda: parse_proof_term(text)):8.1f} ms")
    print("depth at the default recursion limit (1000)")
    sys.setrecursionlimit(1000)
    for n in (50, 100, 200, 400):
        try:
            parse_proof_term(mu_chain(n))
            print(f"  depth {n}: ok")
        except RecursionError:
            print(f"  depth {n}: RecursionError")


if __name__ == "__main__":
    main()
