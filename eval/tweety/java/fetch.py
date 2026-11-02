"""Fetch Tweety module jars and their compile dependencies from Maven Central."""
import os, re, sys, urllib.request, xml.etree.ElementTree as ET
BASE = "https://repo1.maven.org/maven2"
OUT = sys.argv[1]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
seen = {}
def get(url):
    with urllib.request.urlopen(url) as r: return r.read()
def pom(g, a, v):
    return ET.fromstring(get(f"{BASE}/{g.replace('.', '/')}/{a}/{v}/{a}-{v}.pom"))
def props(root, acc):
    p = root.find("m:properties", NS)
    if p is not None:
        for c in p: acc[c.tag.split('}')[1]] = (c.text or "").strip()
    par = root.find("m:parent", NS)
    if par is not None:
        g, a, v = (par.find(f"m:{k}", NS).text for k in ("groupId", "artifactId", "version"))
        try:
            proot = pom(g, a, v); props(proot, acc)
            dm = proot.find("m:dependencyManagement/m:dependencies", NS)
            if dm is not None:
                for d in dm:
                    acc.setdefault("__dm", {})[(d.find("m:groupId", NS).text, d.find("m:artifactId", NS).text)] = d.find("m:version", NS).text
        except Exception: pass
def sub(s, acc):
    return re.sub(r"\$\{([^}]+)\}", lambda m: acc.get(m.group(1), m.group(0)), s or "")
def fetch(g, a, v):
    if (g, a) in seen: return
    seen[(g, a)] = v
    root = pom(g, a, v); acc = {"project.version": v}; props(root, acc)
    jar = f"{OUT}/{a}-{v}.jar"
    if not os.path.exists(jar):
        try: open(jar, "wb").write(get(f"{BASE}/{g.replace('.', '/')}/{a}/{v}/{a}-{v}.jar"))
        except Exception as e: print("no jar", g, a, v, e)
    deps = root.find("m:dependencies", NS)
    for d in (deps if deps is not None else []):
        sc = d.find("m:scope", NS); op = d.find("m:optional", NS)
        if (sc is not None and sc.text in ("test", "provided", "system")) or (op is not None and op.text == "true"): continue
        dg = sub(d.find("m:groupId", NS).text, acc); da = sub(d.find("m:artifactId", NS).text, acc)
        ve = d.find("m:version", NS)
        dv = sub(ve.text, acc) if ve is not None else acc.get("__dm", {}).get((dg, da))
        if dv is None or "$" in dv: print("skip", dg, da, dv); continue
        try: fetch(dg, da, dv)
        except Exception as e: print("fail", dg, da, dv, e)
os.makedirs(OUT, exist_ok=True)
for m in sys.argv[2:]:
    g, a = m.rsplit(":", 1); fetch(g, a, "1.31")
print(len(seen), "artifacts")
