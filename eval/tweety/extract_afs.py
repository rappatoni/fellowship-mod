"""Extract the abstract argumentation frameworks that Tweety's example
programs build in code.

Usage: python extract_afs.py JAVA_FILE...  ->  JSON on stdout:
  [{"file", "var", "args": [...], "att": [[b, a], ...]}, ...]

A heuristic reader of the example sources, not a Java parser: it follows
`Argument v = new Argument("name")`, `T t = new SomeTheory(...)`,
`t.add(v)`, `t.add(new Attack(v, w))` and `t.addAttack(v, w)` in source
order; a theory variable that is assigned anew starts a new framework.
Frameworks with support, set attacks or attacks on attacks are recorded
with their extra relation under "other" and are not plain Dung AFs.
"""
import json, re, sys

def extract(path):
    src = open(path).read()
    src = re.sub(r"//.*", "", src)
    src = re.sub(r"/\*.*?\*/", "", src, flags=re.S)
    names, theories, out = {}, {}, []
    stmt_re = re.compile(r"[^;{}]*;")
    for st in stmt_re.findall(src):
        st = " ".join(st.split())
        m = re.search(r"(\w+)\s*=\s*new Argument\(\s*\"([^\"]+)\"\s*\)", st)
        if m:
            names[m.group(1)] = m.group(2); continue
        m = re.search(r"(\w+)\s*=\s*new (\w*(Theory|Framework|DungTheory|AF|Saf|Caf|Eaf|BipolarArgumentationFramework)\w*)\s*\(", st)
        if m and "Generator" not in m.group(2) and "Reasoner" not in m.group(2):
            fr = {"file": path.split("/")[-1], "var": m.group(1), "class": m.group(2), "args": [], "att": [], "other": []}
            theories[m.group(1)] = fr; out.append(fr); continue
        m = re.search(r"(\w+)\.(?:add|addAttack|addSupport)\((.*)\)", st)
        if not m or m.group(1) not in theories:
            continue
        fr, inner = theories[m.group(1)], m.group(2)
        if ".addSupport(" in st:
            sp = re.fullmatch(r"(\w+)\s*,\s*(\w+)", inner)
            if sp and sp.group(1) in names and sp.group(2) in names:
                fr.setdefault("support", []).append([names[sp.group(1)], names[sp.group(2)]]); continue
        a = re.fullmatch(r"new Attack\(\s*(\w+)\s*,\s*(\w+)\s*\)", inner) or \
            (re.fullmatch(r"(\w+)\s*,\s*(\w+)", inner) if ".addAttack(" in st else None)
        if a and a.group(1) in names and a.group(2) in names:
            fr["att"].append([names[a.group(1)], names[a.group(2)]]); continue
        many = re.findall(r"new Attack\(\s*(\w+)\s*,\s*(\w+)\s*\)", inner)
        if many and re.fullmatch(r"(\s*new Attack\(\s*\w+\s*,\s*\w+\s*\)\s*,?)+", inner) and all(x in names and y in names for x, y in many):
            fr["att"] += [[names[x], names[y]] for x, y in many]; continue
        if inner in names:
            fr["args"].append(names[inner]); continue
        ids = [x for x in re.findall(r"\b\w+\b", inner) if x in names]
        if re.fullmatch(r"(\w+\s*,\s*)*\w+", inner) and ids and all(x.strip() in names for x in inner.split(",")):
            fr["args"] += [names[x.strip()] for x in inner.split(",")]; continue
        fr["other"].append(st)
    return [f for f in out if f["args"] or f["att"]]

if __name__ == "__main__":
    res = []
    for p in sys.argv[1:]:
        res += extract(p)
    for f in res:
        f["args"] = sorted(set(f["args"]) | {x for e in f["att"] + f.get("support", []) for x in e})
    json.dump(res, sys.stdout, indent=0)
