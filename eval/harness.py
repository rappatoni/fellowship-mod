"""Run a .fspy file through AIDA's service and tabulate labels and verdicts.

Usage: python eval/harness.py FILE.fspy [--args a,b] [--sem grounded,preferred,stable]

For every argument and debate of the document (or the ones named) it prints
the labelling of the issue under each semantics and the verdict of
evaluation in each mode, and flags an *adequacy mismatch*: credulous
evaluation should be a value exactly when some labelling of the semantics
has the issue IN, skeptical evaluation exactly when every labelling has.
"""
import argparse, json, sys, time, traceback
from aida_env import setup_prover, Service
from core.dc.debate_graph import canonical_prop

def pretty(term):
    import copy
    from pres.gen import ProofTermGenerationVisitor
    return ProofTermGenerationVisitor().visit(copy.deepcopy(term)).pres


SEMS = ["grounded", "complete", "preferred", "stable"]


def show(graph, key, side):
    return f"{graph.nodes.get(key, key)}[{side[0]}]"


def lab_str(graph, lab):
    return " ".join(f"{show(graph, k, s)}={v}" for (k, s), v in sorted(lab.items(), key=lambda x: (x[0][0], x[0][1])))


def issue_of(info):
    side = "context" if info.anti else "term"
    return canonical_prop(info.conclusion), side


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("file")
    ap.add_argument("--args", default=None)
    ap.add_argument("--sem", default="grounded,preferred,stable")
    ap.add_argument("--labels", action="store_true", help="print full labellings")
    ap.add_argument("--json", default=None)
    ap.add_argument("--typecheck", default=None, help="on|off|expanded")
    ap.add_argument("--modes", default="skeptical,credulous")
    a = ap.parse_args()
    sems = a.sem.split(",")
    svc = Service.of(setup_prover())
    if a.typecheck:
        svc.set_typecheck(a.typecheck)
    t0 = time.monotonic()
    rep = svc.check(open(a.file).read())
    print(f"== {a.file}: check ok={rep.ok} ({rep.seconds:.1f}s)")
    for e in rep.entries:
        if e.status in ("refused", "error", "fatal", "reported"):
            print(f"   line {e.span.line if e.span else '?'}: {e.status}: {e.message}")
    inv = svc.inventory()
    targets = [(x.name, issue_of(x)) for x in inv.arguments if x.in_document]
    for d in inv.debates:
        targets.append((d.name, None))
    if a.args:
        want = a.args.split(",")
        targets = [t for t in targets if t[0] in want]
    out = []
    for name, issue in targets:
        row = {"target": name, "labels": {}, "verdicts": {}, "mismatch": []}
        try:
            g = svc.graph(name).graph
        except Exception as ex:
            print(f"-- {name}: graph refused: {type(ex).__name__}: {ex}")
            row["error"] = str(ex); out.append(row); continue
        if issue is None:
            dbg = [d for d in inv.debates if d.name == name][0]
            # a debate's issue: the statement it is about
            iss = dbg.issue
            issue = (canonical_prop(iss), "context" if dbg.onus == "con" else "term")
        print(f"-- {name}: issue {show(g, *issue)}; {len(g.edges)} edges")
        for sem in sems:
            try:
                labs = svc.label(name, sem).labellings
            except Exception as ex:
                print(f"   label {sem}: {type(ex).__name__}: {ex}"); continue
            vals = [l.get(issue, "absent") for l in labs]
            row["labels"][sem] = vals
            print(f"   {sem:9s} issue: {vals}")
            if a.labels:
                for i, l in enumerate(labs, 1):
                    print(f"      [{i}] {lab_str(g, l)}")
            for mode in a.modes.split(","):
                try:
                    ev = svc.evaluate(name, mode=mode, semantics=sem)
                    cls = ev.nf_class
                    nf = pretty(ev.normal_form)
                except Exception as ex:
                    cls, nf = "ERROR", f"{type(ex).__name__}: {ex}"
                row["verdicts"][f"{mode}/{sem}"] = cls
                expect_value = (all(v == "IN" for v in vals) if mode == "skeptical" else any(v == "IN" for v in vals)) and vals
                flag = ""
                if cls != "ERROR" and bool(expect_value) != (cls == "value"):
                    flag = "   <-- ADEQUACY MISMATCH"
                    row["mismatch"].append(f"{mode}/{sem}: labels {vals} but {cls}")
                print(f"      {mode:9s} -> {cls:9s} {nf if len(nf) < 220 else nf[:220] + '...'}{flag}")
        out.append(row)
    print(f"== done in {time.monotonic() - t0:.1f}s")
    if a.json:
        json.dump(out, open(a.json, "w"), indent=1)


if __name__ == "__main__":
    main()
