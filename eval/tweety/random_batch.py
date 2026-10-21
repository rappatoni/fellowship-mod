"""Run compare.py (grounded only) on Tweety's 50 random ASPIC+ theories and
tabulate: is the strict part (axioms closed under strict rules)
consistent, how many literals differ from Tweety's ASPIC+ reasoner,
labelling failures, issue/document view differences, time.

Usage: python random_batch.py src/random/*.aspic
"""
import os, re, subprocess, sys, time
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import tw2fspy as T

def strict_consistent(th):
    have = {r[3] for r in th.rules if r[1] == "strict" and not r[2]}
    strict = [r for r in th.rules if r[1] == "strict" and r[2]]
    while True:
        new = {r[3] for r in strict if all(b in have for b in r[2])} - have
        if not new: break
        have |= new
    clash = sorted({a for s, a in have if s == "+" and ("-", a) in have})
    return clash

rows = []
for p in sys.argv[1:]:
    th = T.parse_aspic(open(p).read())
    clash = strict_consistent(th)
    t0 = time.monotonic()
    out = subprocess.run([sys.executable, os.path.join(HERE, "compare.py"), "aspic", p, "--sems=grounded", "--noeval"],
                         capture_output=True, text=True).stdout
    dt = time.monotonic() - t0
    n = re.search(r"== \S+: (\d+) rule instances", out)
    differ = re.search(r"(\d+) literal\(s\) differ\n", out)
    adeq = out.count("ADEQUACY MISMATCH")
    dd = re.search(r"differ on (\d+) literal/semantics", out)
    diffs = re.findall(r"^\s+(\S+)\s+AIDA GR=(\S+)\s+Tweety GR=(\S+)\s+<-- differs", out, re.M)
    rows.append((os.path.basename(p), n and int(n.group(1)), clash, differ and int(differ.group(1)), adeq,
                 dd and int(dd.group(1)), round(dt), diffs))
    r = rows[-1]
    print(f"{r[0]}: rules={r[1]} strict-clash={','.join(r[2]) or '-'} differ={r[3]} failures={r[4]} issue/doc={r[5]} {r[6]}s "
          + " ".join(f"{q}:{a}/{b}" for q, a, b in diffs), flush=True)
ok = [r for r in rows if not r[2]]
print(f"== {len(rows)} theories; {len(ok)} with a consistent strict part: "
      f"{sum(1 for r in ok if r[3] == 0)} agree on every literal, {sum(r[3] or 0 for r in ok)} literal differences, "
      f"{sum(r[4] for r in ok)} labelling failures; inconsistent strict part: {len(rows) - len(ok)} theories, "
      f"{sum(r[3] or 0 for r in rows if r[2])} literal differences, {sum(r[4] for r in rows if r[2])} labelling failures")
