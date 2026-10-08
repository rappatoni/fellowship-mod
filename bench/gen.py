"""Synthetic .fspy documents for the scaling measurements (tasks.org,
aida-scaling-benchmarks).

    python bench/gen.py KIND N [nolabel]

KIND is
  chain   - argument a_i derives P_i from P_{i-1} through a declared rule
  stated  - the same through `claim` / `prove` (exercises refinement)
  double  - two alternative supporters per level (shared sub-debates)
  decls   - N propositions and N-1 rules, declarations only

The file ends with `label` on the last argument unless `nolabel` is given.
"""

import sys


def chain(n, stated=False, alts=1, label=True):
    lines = ["lk.", "declare " + ", ".join(f"P{i}" for i in range(n + 1)) + " : bool."]
    for i in range(1, n + 1):
        for a in range(alts):
            lines.append(f"declare r{i}_{a} : (P{i-1} -> P{i}).")
    lines += ["start argument a0 P0", "by default.", "end argument"]
    for i in range(1, n + 1):
        for a in range(alts):
            name = f"a{i}_{a}"
            if stated:
                lines += [f"claim {name}: (P{i}).", f"prove {name}"]
            else:
                lines.append(f"start argument {name} P{i}")
            lines += [f"cut (P{i-1} -> P{i}) th.", f"axiom r{i}_{a}.", "elim.", "next.", "axiom.",
                      "end argument"]
    if label:
        lines.append(f"label a{n}_0")
    return "\n".join(lines) + "\n"


def declarations(n):
    lines = ["lk."]
    lines += [f"declare P{i} : bool." for i in range(n)]
    lines += [f"declare r{i} : (P{i-1} -> P{i})." for i in range(1, n)]
    return "\n".join(lines) + "\n"


def main(argv):
    kind, n = argv[1], int(argv[2])
    label = len(argv) < 4 or argv[3] != "nolabel"
    if kind == "decls":
        text = declarations(n)
    elif kind in ("chain", "stated", "double"):
        text = chain(n, stated=(kind == "stated"), alts=(2 if kind == "double" else 1), label=label)
    else:
        raise SystemExit(f"unknown kind {kind!r}; see the module docstring")
    print(text, end="")


if __name__ == "__main__":
    main(sys.argv)
