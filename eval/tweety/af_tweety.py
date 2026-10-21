"""Run every abstract framework from Tweety's examples through AIDA, the
brute-force Dung reference (eval/dung.py) and Tweety's own reasoners, and
compare the extensions under grounded, complete, preferred and stable.

Usage: python af_tweety.py afs.json [APX ...]
Writes each framework as af/NAME.fspy (encoding of eval/dung.py:
an argument is a presumption, an attack b -> a a declared b -> ~a and a
counterargument on a). Frameworks with supports or attacks on attacks
are listed and skipped (see the report for their treatment).
"""
import json, os, re, subprocess, sys, time
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
from aida_env import setup_prover, Service
from core.dc.debate_graph import canonical_prop
import dung

SEMS = ["grounded", "complete", "preferred", "stable"]
ABBR = {"grounded": "GR", "complete": "CO", "preferred": "PR", "stable": "ST"}


def safe(n):
    """Argument names as AIDA atoms: letters, digits, underscore; not a keyword."""
    n = re.sub(r"\W", "_", n)
    # an argument is named n and states N: a capitalised name would clash
    return "a_" + n if not n[0].isalpha() or n[0].isupper() or n.lower() in ("all", "minimal", "lk", "nj") else n


def tweety_exts(args, att):
    out = subprocess.run([os.path.join(HERE, "ref.sh"), "af", ",".join(args), ",".join(f"{b}>{a}" for b, a in att)],
                         capture_output=True, text=True).stdout
    res = {}
    for line in out.splitlines():
        ab, exts = line.split(": ", 1)
        res[ab] = sorted(tuple(sorted(x.strip() for x in e.split(",") if x.strip())) for e in re.findall(r"\{([^{}]*)\}", exts))
    return res


def parse_apx(path):
    text = open(path).read()
    args = re.findall(r"arg\(\s*([^)\s]+)\s*\)", text)
    att = re.findall(r"att\(\s*([^,\s]+)\s*,\s*([^)\s]+)\s*\)", text)
    return args, att


def main():
    afs = json.load(open(sys.argv[1]))
    for p in sys.argv[2:]:
        args, att = parse_apx(p)
        afs.append({"file": os.path.basename(os.path.dirname(os.path.dirname(os.path.dirname(os.path.dirname(p))))) + "/" + os.path.basename(p),
                    "var": "apx", "args": args, "att": [list(e) for e in att], "other": []})
    os.makedirs(os.path.join(HERE, "af"), exist_ok=True)
    svc = Service.of(setup_prover())
    seen, total, bad = {}, 0, 0
    only = os.environ.get("ONLY")
    for f in afs:
        if only and not any(o in f["file"] for o in only.split(",")):
            continue
        name = f"{f['file'].replace('.java', '').replace('/', '_').replace('.apx', '')}_{f['var']}"
        if f.get("support") or f["other"]:
            print(f"-- {name}: skipped (supports {f.get('support')}, other {f['other']})"); continue
        args = sorted(f["args"])
        att = sorted({tuple(e) for e in f["att"]})
        key = (tuple(args), tuple(att))
        if key in seen:
            print(f"-- {name}: same framework as {seen[key]}"); continue
        seen[key] = name
        n = 1
        while os.path.exists(os.path.join(HERE, "af", f"{name}.fspy")) and n < 9 and name in [x for x in seen.values()][:-1]:
            n += 1
        sa = {a: safe(a) for a in args}
        text = f"# {f['file']} ({f['var']}): arguments {', '.join(args)}; attacks {', '.join(f'{b}->{a}' for b, a in att)}\n"
        text += dung.to_fspy([sa[a] for a in args], [(sa[b], sa[a]) for b, a in att])
        path = os.path.join(HERE, "af", f"{name}.fspy")
        open(path, "w").write(text)
        total += 1
        rep = svc.check(text)
        errs = [e for e in rep.entries if e.status in ("refused", "error", "fatal", "reported")]
        tw = tweety_exts(args, att)
        line = []
        for sem in SEMS:
            ref = sorted(tuple(sorted(a for a in args if L[a] == "IN")) for L in dung.semantics(args, att, sem))
            if errs:
                got = ["refused: " + errs[0].message]
            else:
                t0 = time.monotonic()
                try:
                    labs = svc.label("document", sem).labellings
                except Exception as ex:
                    labs = None; got = [f"{type(ex).__name__} after {time.monotonic() - t0:.0f}s"]
                if labs is not None:
                  got = sorted(tuple(sorted(a for a in args
                                          if l.get((canonical_prop(sa[a].upper()), "term")) == "IN")) for l in labs)
            ok = got == ref == tw.get(ABBR[sem])
            line.append(f"{ABBR[sem]}{'=' if ok else '!'}{len(ref)}")
            if not ok:
                bad += 1
                print(f"   {name} {sem}: Dung {ref} Tweety {tw.get(ABBR[sem])} AIDA {got}")
        print(f"-- {name}: {len(args)} args, {len(att)} attacks: {' '.join(line)}")
    print(f"== {total} distinct frameworks, {bad} semantics disagreements")


if __name__ == "__main__":
    main()
