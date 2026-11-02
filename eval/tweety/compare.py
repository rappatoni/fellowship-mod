"""Translate a Tweety theory, label it in AIDA and compare per literal with
Tweety's own reasoner.

Usage: python compare.py FORMAT SRC [--nopref]     FORMAT: aspic | delp | aba

Per literal L (each atom X and its negation ~X) and semantics, the code
is c (credulously accepted: in some extension / labelling), s
(skeptically: in all), - (neither). AIDA accepts X in a labelling when
X[t] is IN, and ~X when X[c] is IN. For DeLP, Tweety's answer is the
DeLP warrant status (YES / NO / UNDECIDED / UNKNOWN); AIDA's grounded
code is printed beside it. For ABA, Tweety's extensions (sets of
assumptions) are closed under the rules here, so that conclusions can be
compared as well.
"""
import os, re, subprocess, sys, warnings
warnings.simplefilter("ignore")   # OpposingPresumptions, expected for bare premises on both sides
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.dirname(HERE))
import tw2fspy as T
from aida_env import setup_prover, Service
from core.dc.debate_graph import canonical_prop

SEMS = ["grounded", "complete", "preferred", "stable"]
for _a in sys.argv:
    if _a.startswith("--sems="):
        SEMS = _a[7:].split(",")
ABBR = {"grounded": "GR", "complete": "CO", "preferred": "PR", "stable": "ST"}


def tweety(*args):
    env = dict(os.environ, TWEETY_SEMS=",".join({"grounded": "GR", "complete": "CO", "preferred": "PR", "stable": "ST"}[s] for s in SEMS))
    r = subprocess.run([os.path.join(HERE, "ref.sh"), *args], capture_output=True, text=True, env=env)
    if r.returncode:
        print(r.stderr[-1500:])
    return r.stdout


def doc_codes(svc, atoms):
    """The document view: atom -> sem -> (code for X, code for ~X)."""
    res = {a: {} for a in atoms}
    for sem in SEMS:
        try:
            labs = svc.label("document", sem).labellings
        except Exception:
            for a in atoms: res[a][sem] = ("T", "T", "timeout")
            continue
        for a in atoms:
            key = f"S:{a}"
            def code(side):
                ins = [any(str(k) == key and s == side and v == "IN" for (k, s), v in l.items()) for l in labs]
                return ("c" if any(ins) else "-") + ("s" if ins and all(ins) else "-")
            res[a][sem] = (code("term"), code("context"), len(labs))
    return res


def issue_codes(svc, atoms):
    """The issue view: a literal is credulously accepted when the issue of some
    argument for it is IN in some labelling of that argument's issue graph,
    skeptically when it is IN in all of them. Also evaluates every argument
    (skeptical/grounded and credulous/preferred) and checks adequacy."""
    res = {a: {s: [False, False, False, False] for s in SEMS} for a in atoms}  # [t cred, t skep, c cred, c skep]
    verdicts, problems = {}, []
    for x in svc.inventory().arguments:
        if not x.in_document:
            continue
        key, side = str(canonical_prop(x.conclusion)), ("context" if x.anti else "term")
        a = key[2:] if key.startswith("S:") else None
        for sem in SEMS:
            try:
                vals = [l.get((canonical_prop(x.conclusion), side), "absent") for l in svc.label(x.name, sem).labellings]
            except Exception as ex:
                problems.append(f"{x.name} {sem}: labelling failed: {type(ex).__name__}")
                if a in res: res[a][sem] = None
                continue
            if a in res and res[a][sem] is not None:
                i = 0 if side == "term" else 2
                res[a][sem][i] |= any(v == "IN" for v in vals)
                res[a][sem][i + 1] |= bool(vals) and all(v == "IN" for v in vals)
            for mode in (("skeptical", "credulous") if sem in ("grounded", "preferred") and "--noeval" not in sys.argv else ()):
                try:
                    cls = svc.evaluate(x.name, mode=mode, semantics=sem).nf_class
                except Exception as ex:
                    problems.append(f"{x.name} {mode}/{sem}: evaluation failed: {type(ex).__name__}: {str(ex).splitlines()[0][:120]}")
                    continue
                verdicts[(x.name, mode, sem)] = cls
                want = (all if mode == "skeptical" else any)(v == "IN" for v in vals) and bool(vals)
                if want != (cls == "value"):
                    problems.append(f"{x.name} {mode}/{sem}: issue labels {vals}, verdict {cls}")
    out = {a: {s: (("c" if r[0] else "-") + ("s" if r[1] else "-"), ("c" if r[2] else "-") + ("s" if r[3] else "-")) if r else ("T", "T")
               for s, r in m.items()} for a, m in res.items()}
    return out, verdicts, problems


def main():
    fmt, src = sys.argv[1], sys.argv[2]
    nopref = "--nopref" in sys.argv
    text = open(src).read()
    if nopref:
        text = "\n".join(l for l in text.splitlines() if not re.fullmatch(r"\s*\w+(\s*<\s*\w+)+\s*", l))
    th = {"aspic": T.parse_aspic, "delp": T.parse_delp, "aba": T.parse_aba, "rdl": T.parse_rdl}[fmt](text)
    consts = getattr(th, "constants", None) or {c for r in th.rules for x in r[2] + [r[3]] if isinstance(x[1], str)
              for c in T.args_of(x[1]) if not T.VAR.fullmatch(c)}
    g = T.reachable(T.ground(th, consts))
    full = T.ground(th, consts)
    names = {}
    for r in full.rules:
        for x in r[2] + [r[3]]:
            if isinstance(x[1], str):
                names[T.atom_name(x[1])] = x[1]
    base = os.path.splitext(os.path.basename(src))[0] + ("_nopref" if nopref else "")
    out = os.path.join(HERE, f"{base}.fspy")
    hdr = "\n".join(f"# {l}" if l.strip() else "#" for l in text.strip().splitlines())
    hdr = f"# Generated by eval/tweety/tw2fspy.py ({fmt}) from Tweety's {os.path.basename(src)}:\n" + hdr
    if th.notes:
        hdr += "\n# Not translated:\n" + "\n".join(f"#  - {n}" for n in th.notes)
    open(out, "w").write(T.emit(g, hdr))
    svc = Service.of(setup_prover())
    rep = svc.check(open(out).read())
    bad = [e for e in rep.entries if e.status in ("refused", "error", "fatal", "reported")]
    print(f"== {base}: {len(g.rules)} rule instances, check ok={rep.ok}, {len(bad)} problem(s)")
    for e in bad:
        print(f"   line {e.span.line if e.span else '?'}: {e.status}: {e.message}")
    atoms = sorted(names)
    mine, verdicts, problems = issue_codes(svc, atoms)
    doc = doc_codes(svc, atoms)
    print("   document labellings: " + " ".join(f"{ABBR[s]}={doc[atoms[0]][s][2]}" for s in SEMS) if atoms else "")
    for p in problems:
        print(f"   ADEQUACY MISMATCH {p}")
    dd = [f"{('~' if i else '') + a} {ABBR[s]}: issue {mine[a][s][i]} document {doc[a][s][i]}"
          for a in atoms for s in SEMS for i in (0, 1) if mine[a][s][i] != doc[a][s][i]]
    print(f"   issue view and document view differ on {len(dd)} literal/semantics pair(s)" + (": " + "; ".join(dd[:6]) + (" ..." if len(dd) > 6 else "") if dd else ""))
    neg = {"aspic": "!", "delp": "~", "aba": None, "rdl": "!"}[fmt]
    if fmt == "rdl":
        qs = [q for a in atoms for q in (names[a], "!" + names[a])]
        print("   -- Tweety SimpleDefaultReasoner: " + " | ".join(l for l in tweety("rdl", src, *qs).splitlines()))
        for a in atoms:
            print(f"   {names[a]:24} AIDA {' '.join(f'{ABBR[s]}={mine[a][s][0]}/{mine[a][s][1]}' for s in SEMS)}")
    if fmt in ("aspic", "delp"):
        qs = []
        for a in atoms:
            qs += [names[a], neg + names[a]]
        if fmt == "aspic":
            src2 = src
            if nopref:
                src2 = os.path.join(HERE, "src", base + ".aspic"); open(src2, "w").write(text)
            ref = dict(l.split(": ", 1) for l in tweety("aspic", src2, *qs).splitlines() if ": " in l and not l.startswith("arguments"))
        else:
            ref = {l.split(": ", 1)[0]: l.split("is: ")[-1] for l in tweety("delp", src, *qs).splitlines() if ": " in l}
        diffs = 0
        for a in atoms:
            for side, q in ((0, names[a]), (1, neg + names[a])):
                me = " ".join(f"{ABBR[s]}={mine[a][s][side]}" for s in SEMS)
                tw = ref.get(q, "?")
                if fmt == "aspic":
                    flag = "" if tw.strip() == me else "   <-- differs"
                else:
                    gr = mine[a]["grounded"][side]
                    expect = {"YES": "cs", "NO": "--", "UNDECIDED": "--", "UNKNOWN": "--"}.get(tw.strip(), "?")
                    flag = "" if gr == expect else "   <-- differs"
                diffs += bool(flag)
                print(f"   {q:24} AIDA {me}   Tweety {tw}{flag}")
        print(f"   {diffs} literal(s) differ")
    if fmt in ("delp", "aba") or "--aspic" in sys.argv:
        # the same ground theory through Tweety's ASPIC+ reasoner, no preferences
        print("   -- reference: Tweety ASPIC+ on the ground theory, no preferences")
        lines = []
        for (n, k, b, h) in g.rules:
            f = lambda l: ("!" if l[0] == "-" else "") + T.atom_name(l[1])
            lines.append(f"{', '.join(f(x) for x in b)} {'->' if k == 'strict' else '=>'} {f(h)}".strip())
        asp = os.path.join(HERE, "src", base + "_ground.aspic"); open(asp, "w").write("\n".join(lines) + "\n")
        qs = [q for a in atoms for q in (a, "!" + a)]
        ref = dict(l.split(": ", 1) for l in tweety("aspic", asp, *qs).splitlines() if ": " in l and not l.startswith("arguments"))
        diffs = 0
        for a in atoms:
            for side, q in ((0, a), (1, "!" + a)):
                me = " ".join(f"{ABBR[s]}={mine[a][s][side]}" for s in SEMS)
                tw = ref.get(q, "?").strip()
                if tw != me:
                    diffs += 1; print(f"   {q:24} AIDA {me}   ASPIC {tw}")
        print(f"   {diffs} literal(s) differ from ASPIC+ without preferences")
    if fmt == "aba":
        import aba
        out = tweety("aba", src)
        print("   -- Tweety FlatAbaReasoner: " + (" | ".join(out.splitlines()[1:]) or "refused (theory not flat)"))
        A = [h[1] for (n, k, b, h) in g.rules if n.startswith("asm_")]
        contrary = {h[1]: b[0][1] for (n, k, b, h) in g.rules if n.startswith("contrary_")}
        rules = [(h[1], [x[1] for x in b]) for (n, k, b, h) in g.rules if not n.startswith(("asm_", "contrary_"))]
        diffs = 0
        for sem in SEMS:
            exts, codes = aba.accepted(rules, A, contrary, sem)
            print(f"   -- ABA reference {ABBR[sem]}: " + (", ".join("{" + ",".join(sorted(e)) + "}" for e in exts) or "none"))
            for (sign, x), tw in sorted(codes.items()):
                a = T.atom_name(x)
                if a not in mine: continue
                me = mine[a][sem][0 if sign == "+" else 1]
                if me != tw:
                    diffs += 1
                    print(f"   {('~' if sign == '-' else '') + x:10} {ABBR[sem]}: AIDA {me} ABA {tw}")
        print(f"   {diffs} literal/semantics pair(s) differ from the ABA reference")

if __name__ == "__main__":
    main()
