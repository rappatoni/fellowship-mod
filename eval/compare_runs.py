"""Compare two harness output directories: issue labels and verdicts per
(file, target, semantics, mode).  Usage: python compare_runs.py OLD NEW"""
import os, re, sys

def parse(path):
    out, target, sem = {}, None, None
    for line in open(path):
        m = re.match(r"-- (\S+): (graph refused: .*|issue .*)", line)
        if m:
            target = m[1]
            if m[2].startswith("graph refused"):
                out[(target, "graph")] = "REFUSED " + m[2][15:90]
            continue
        m = re.match(r"   (\w+) +issue: (\[.*\])", line)
        if m:
            sem = m[1]; out[(target, sem, "labels")] = m[2]; continue
        m = re.match(r"      (skeptical|credulous) -> (\w+) +(.*?)( +<-- ADEQUACY MISMATCH)?$", line)
        if m:
            out[(target, sem, m[1])] = m[2] + (" !MISMATCH" if m[4] else "")
    return out

old, new = sys.argv[1], sys.argv[2]
changed = 0
for name in sorted(set(os.listdir(old)) | set(os.listdir(new))):
    if not name.endswith(".txt"): continue
    a = parse(os.path.join(old, name)) if os.path.exists(os.path.join(old, name)) else {}
    b = parse(os.path.join(new, name)) if os.path.exists(os.path.join(new, name)) else {}
    diffs = [(k, a.get(k, "-"), b.get(k, "-")) for k in sorted(set(a) | set(b), key=str) if a.get(k) != b.get(k)]
    if diffs:
        changed += 1
        print(f"== {name}")
        for k, x, y in diffs:
            print(f"   {' '.join(k)}: {x}  =>  {y}")
print(f"{changed} file(s) changed")
