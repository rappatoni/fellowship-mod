"""Tweety's small ASP programs: parse, ground, compute answer sets by brute
force, translate to .fspy and compare the answer sets with AIDA's stable
labellings of the document (the correspondence of
aida-scasp-stable-correspondence: a rule a :- b, not c is a declared
implication B -> (C -> false) -> A, b an obligation, not c the presumed
refutation of C; a fact a declared axiom; a classical literal -q the
refutation of Q).

Usage: python asp.py FILE.asp ...
Not translated (reported per file): aggregates, arithmetic, optimisation,
#show and queries. A disjunctive head a | b is shifted into
a :- not b and b :- not a (equivalent on head-cycle-free programs), a
choice {q(X) : p(X)} into q :- p, not nq and nq :- p, not q with an
auxiliary atom nq. Integrity constraints are checked on the answer sets
only; AIDA has no counterpart and they are listed.
"""
import itertools, os, re, sys
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE); sys.path.insert(0, os.path.dirname(HERE))
import tw2fspy as T


def split_lits(s):
    return [x.strip() for x in re.split(r",\s*(?![^()]*\))", s) if x.strip()]


def parse(text):
    text = re.sub(r"%\*.*?\*%", "", text, flags=re.S)
    text = re.sub(r"%.*", "", text)
    rules, cons, notes = [], [], []
    for cl in (c.strip() for c in re.split(r"\.(?=\s|$)", text) if c.strip()):
        if cl.startswith("#") or cl.endswith("?"):
            notes.append(f"`{cl}` (directive or query)"); continue
        if "#" in cl:
            notes.append(f"`{cl}` (aggregate)"); continue
        if cl.startswith("{"):
            m = re.fullmatch(r"\{\s*(.*?)\s*:\s*(.*?)\s*\}", cl)
            head, cond = m.group(1), split_lits(m.group(2))
            aux = "n_" + head
            rules.append(([head], cond + ["not " + aux]))
            rules.append(([aux], cond + ["not " + head]))
            notes.append(f"choice `{cl}` written with the auxiliary atom {aux}"); continue
        head, _, body = cl.partition(":-")
        body = split_lits(body)
        if not head.strip():
            cons.append(body); continue
        heads = [h.strip() for h in re.split(r"\|| v ", head)]
        if len(heads) > 1:
            for h in heads:
                rules.append(([h], body + ["not " + o for o in heads if o != h]))
            notes.append(f"disjunction `{cl}` shifted"); continue
        rules.append((heads, body))
    return rules, cons, notes


def ground(rules, cons):
    consts = {c for r in rules for x in r[0] + r[1] for c in T.args_of(x.replace("not ", "").lstrip("-"))
              if not T.VAR.fullmatch(c)}
    out = []
    for hs, body in rules + [([], c) for c in cons]:
        vs = sorted({v for x in hs + body for v in T.args_of(x.replace("not ", "").lstrip("-")) if T.VAR.fullmatch(v)})
        for combo in itertools.product(sorted(consts), repeat=len(vs)):
            sub = dict(zip(vs, combo))
            g = lambda x: re.sub(r"\b[A-Z]\w*\b", lambda m: sub.get(m.group(0), m.group(0)), x)
            out.append(([g(h) for h in hs], [g(b) for b in body]))
    return [r for r in out if r[0]], [r[1] for r in out if not r[0]]


def answer_sets(rules, cons):
    atoms = sorted({x.replace("not ", "") for hs, b in rules for x in hs + b})
    res = []
    for k in range(len(atoms) + 1):
        for S in itertools.combinations(atoms, k):
            S = set(S)
            red = [(hs[0], [x for x in b if not x.startswith("not ")]) for hs, b in rules
                   if not any(x.startswith("not ") and x[4:] in S for x in b)]
            M = set()
            while True:
                new = {h for h, b in red if all(x in M for x in b)} - M
                if not new: break
                M |= new
            if M != S: continue
            if any(a in S and "-" + a in S for a in atoms): continue
            if any(all((x[4:] not in S) if x.startswith("not ") else (x in S) for x in c) for c in cons): continue
            res.append(S)
    return res


def lit(x):
    if x.startswith("not -"): return ("n-", x[5:].strip())
    if x.startswith("not "): return ("n", x[4:].strip())
    if x.startswith("-"): return ("-", x[1:].strip())
    return ("+", x)


def main():
    from aida_env import setup_prover, Service
    from core.dc.debate_graph import canonical_prop
    svc = Service.of(setup_prover())
    os.makedirs(os.path.join(HERE, "asp"), exist_ok=True)
    for path in sys.argv[1:]:
        text = open(path).read()
        rules, cons, notes = parse(text)
        g, gc = ground(rules, cons)
        ans = answer_sets(g, gc)
        th = T.Theory()
        for i, (hs, b) in enumerate(g):
            th.add(f"r{i}", "strict", [lit(x) for x in b], lit(hs[0]))
        th = T.reachable(th)
        if gc: notes.append("integrity constraints " + "; ".join(":- " + ", ".join(c) for c in gc) + " (checked on the answer sets only)")
        hdr = f"# Tweety lp-asp {os.path.basename(path)}:\n" + "\n".join("# " + l for l in text.strip().splitlines())
        if notes: hdr += "\n# Translation notes:\n" + "\n".join("#  - " + n for n in notes)
        name = os.path.splitext(os.path.basename(path))[0]
        out = os.path.join(HERE, "asp", name + ".fspy")
        open(out, "w").write(T.emit(th, hdr))
        rep = svc.check(open(out).read())
        bad = [e for e in rep.entries if e.status in ("refused", "error", "fatal", "reported")]
        atoms = sorted({x.replace("not ", "").lstrip("-") for hs, b in g for x in hs + b})
        # a refutation the program does not state, from a presumed `not x`:
        # counted separately (AIDA has one negation for both)
        classical = {x.replace("not ", "") for hs, b in g for x in hs + b if x.replace("not ", "").startswith("-")}
        def view(S):
            # an AIDA refutation of x counts as -x only where the program has -x;
            # elsewhere it is the presumed `not x` and the answer set just lacks x
            return tuple(sorted(x for x in S if not x.lstrip("-").startswith("n_")
                                and (not x.startswith("-") or x in classical)))
        ref = sorted(view(S) for S in ans)
        labs = svc.label("document", "stable").labellings
        got = []
        for l in labs:
            S = set()
            for a in atoms:
                key = canonical_prop(T.atom_name(a))
                if l.get((key, "term")) == "IN": S.add(a)
                if l.get((key, "context")) == "IN": S.add("-" + a)
            got.append(view(S))
        got = sorted(got)
        print(f"== {name}: {len(bad)} problem(s); answer sets {ref}")
        for e in bad: print(f"   {e.status}: {e.message}")
        print(f"   AIDA stable {got} -> {'agree' if got == ref else 'DIFFER'}")
        for n in notes: print(f"   note: {n}")


if __name__ == "__main__":
    main()
