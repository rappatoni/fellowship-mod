"""Translate TweetyProject example theories (ASPIC+, DeLP, ABA, default
logic) into .fspy documents.

Usage: python tw2fspy.py FORMAT IN [OUT]     FORMAT: aspic | delp | aba | rdl

Encoding (the one of eval/literature, generated):
- every rule instance is one argument whose body literals are obligations
  (open sites, filled by capture from the arguments for them);
- a strict rule is a declared implication, a defeasible rule a presumed one;
- a strict fact is a declared axiom, a defeasible premise / ABA assumption
  a bare presumption;
- a rule with conclusion ~X is a counterargument on X (it proves ~X and
  applies it), since an argument for ~X is not contested by one for X; a
  defeasible premise ~X is a bare presumption of that refutation;
- a negative body literal ~X is owed as the refutation of X (X[c]), met
  by the counterarguments on X; default negation `not X` (ASP) is the
  presumed refutation of X;
- the ABA contrary x of an assumption a is the strict rule x -> ~a;
- an ASPIC undercutter (conclusion !d for a defeasible rule d) is a
  counterargument on the implication statement of d.
Variables are grounded over the constants of the theory; ground rules
whose body is not forward-reachable are dropped.
"""
import itertools, re, sys

class Theory:
    def __init__(self):
        self.rules = []      # (name, kind, body, head); kind strict|defeasible
        self.notes = []      # what the translation leaves out

    def add(self, name, kind, body, head):
        self.rules.append((name, kind, list(body), head))

# ---- literals: ("+", atom) | ("-", atom) | ("n", atom) default negation
#      | ("-", ("rule", name)) ----

def atom_name(a):
    a = a.strip()
    m = re.fullmatch(r"(\w+)\s*\((.*)\)", a)
    if m:
        args = [x.strip() for x in m.group(2).split(",") if x.strip()]
        a = "_".join([m.group(1)] + args)
    a = re.sub(r"\W", "_", a)
    return a[0].upper() + a[1:]

def lit(s, neg_marks=("!", "~", "-")):
    s = s.strip()
    neg = False
    while s and s[0] in neg_marks:
        neg, s = not neg, s[1:].strip()
    return ("-" if neg else "+", s)

# ---- grounding ----

VAR = re.compile(r"\b[A-Z]\w*\b")

def args_of(a):
    m = re.fullmatch(r"\w+\s*\((.*)\)", a.strip())
    return [x.strip() for x in m.group(1).split(",")] if m else []

def ground(th, constants):
    out = Theory(); out.notes = th.notes
    for (name, kind, body, head) in th.rules:
        atoms = [x[1] for x in body + [head] if isinstance(x[1], str)]
        vs = sorted({v for a in atoms for v in args_of(a) if VAR.fullmatch(v)})
        if not vs:
            out.add(name, kind, body, head); continue
        for combo in itertools.product(sorted(constants), repeat=len(vs)):
            sub = dict(zip(vs, combo))
            g = lambda l: (l[0], re.sub(r"\b[A-Z]\w*\b", lambda m: sub.get(m.group(0), m.group(0)), l[1])) if isinstance(l[1], str) else l
            out.add(f"{name}_{'_'.join(combo)}", kind, [g(b) for b in body], g(head))
    return out

def reachable(th):
    """Keep the rules whose body literals can all be derived (forward)."""
    have, keep, changed = set(), [], True
    rules = list(th.rules)
    while changed:
        changed = False
        for r in rules:
            if r in keep: continue
            if all(b in have or b[0] in ("n", "n-") for b in r[2]):
                keep.append(r); have.add(r[3]); changed = True
    out = Theory(); out.notes = th.notes
    out.rules = [r for r in th.rules if r in keep]
    return out

# ---- parsers ----

def parse_aspic(text):
    th = Theory(); names = {}
    lines = [l.strip() for l in text.splitlines() if l.strip() and not l.strip().startswith("%")]
    for l in lines:
        if re.fullmatch(r"\w+(\s*<\s*\w+)+", l):
            th.notes.append(f"rule preference `{l}` (no preferences in AIDA)"); continue
        m = re.fullmatch(r"(?:(\w+)\s*:)?\s*(.*?)\s*(=>|->)\s*(.*)", l)
        if not m:
            th.notes.append(f"unparsed `{l}`"); continue
        name, body, arrow, head = m.groups()
        kind = "defeasible" if arrow == "=>" else "strict"
        body = [lit(b) for b in re.split(r",\s*(?![^()]*\))", body) if b.strip()]
        th.add(name or f"{'p' if kind == 'defeasible' else 'k'}{len(th.rules)}", kind, body, lit(head))
    rnames = {r[0] for r in th.rules if r[1] == "defeasible"}
    th.rules = [(n, k, b, ("-", ("rule", h[1])) if h[0] == "-" and h[1] in rnames else h) for (n, k, b, h) in th.rules]
    return th

def parse_delp(text):
    th = Theory()
    text = re.sub(r"%.*", "", text)
    for i, cl in enumerate(c.strip() for c in text.split(".") if c.strip()):
        m = re.fullmatch(r"(.*?)\s*(<-|-<)\s*(.*)", cl, re.S)
        if m:
            head, arrow, body = m.groups()
            kind = "strict" if arrow == "<-" else "defeasible"
            body = [lit(b) for b in re.split(r",\s*(?![^()]*\))", body) if b.strip()]
        else:
            head, kind, body = cl, "strict", []
        th.add(f"r{i}", kind, body, lit(head, ("~",)))
    return th

def parse_aba(text):
    th = Theory(); assumptions = []
    for l in (l.strip() for l in text.splitlines()):
        if not l or l.startswith("%"): continue
        if l.startswith("{"):
            inner = l.strip().strip("{}")
            assumptions = [a.strip() for a in (inner.split(";") if ";" in inner else inner.split(",")) if a.strip()]
            for a in assumptions:
                th.add(f"asm_{re.sub(r'[^a-z0-9]', '', a.lower())}", "defeasible", [], ("+", a))
        elif l.startswith("not "):
            a, x = (s.strip() for s in l[4:].split("=", 1))
            th.add(f"contrary_{re.sub(r'[^a-z0-9]', '', a.lower())}", "strict", [("+", x)], ("-", a))
        elif "<-" in l:
            head, body = (s.strip() for s in l.split("<-", 1))
            body = [("+", b.strip()) for b in re.split(r"[;,]\s*(?![^()]*\))", body) if b.strip()]
            th.add(f"r{len(th.rules)}", "strict", body, ("+", head))
        else:
            th.notes.append(f"ignored line `{l}`")
    return th

def parse_rdl(text):
    th = Theory(); consts = set()
    for l in (l.strip() for l in text.splitlines()):
        if not l: continue
        m = re.fullmatch(r"\w+\s*=\s*\{(.*)\}", l)
        if m:
            consts |= {c.strip() for c in m.group(1).split(",")}; continue
        if l.startswith("type("): continue
        if "::" in l:
            pre, rest = l.split("::", 1); just, concl = rest.split("/", 1)
            if just.strip() != concl.strip():
                th.notes.append(f"non-normal default `{l}`")
            body = [lit(b) for b in pre.split("&&") if b.strip()]
            th.add(f"d{len(th.rules)}", "defeasible", body, lit(concl))
        elif l.startswith("exists") or l.startswith("forall"):
            th.notes.append(f"quantified fact `{l}` (left out)")
        else:
            th.add(f"f{len(th.rules)}", "strict", [], lit(l))
    th.constants = consts
    return th

# ---- emitter ----

def formula(l, stmts):
    sign, a = l
    f = f"({stmts[a[1]]})" if isinstance(a, tuple) else atom_name(a)
    return f"~{f}" if sign == "-" else f"({f} -> false)" if sign == "n" else f"(~{f} -> false)" if sign == "n-" else f

def emit(th, header=""):
    atoms = sorted({atom_name(x[1]) for r in th.rules for x in r[2] + [r[3]] if isinstance(x[1], str)})
    stmts = {}
    pending = list(th.rules)
    while pending:   # a rule's statement; an undercutter's needs the undercut rule's first
        rest = []
        for (n, k, b, h) in pending:
            refs = [x[1][1] for x in b + [h] if isinstance(x[1], tuple)]
            if all(r in stmts for r in refs):
                stmts[n] = " -> ".join([formula(x, stmts) for x in b] + [formula(h, stmts)])
            else:
                rest.append((n, k, b, h))
        if len(rest) == len(pending):
            raise ValueError(f"undercut of an unknown rule in {rest}")
        pending = rest
    out = [header.rstrip(), "lk.", f"declare {', '.join(atoms)} : bool."] if header else ["lk.", f"declare {', '.join(atoms)} : bool."]
    used = set()
    def ident(n):
        i = re.sub(r"\W", "_", n).lower()
        i = "x_" + i if not i[0].isalpha() else i
        while i in used: i += "_"
        used.add(i); return i
    decls, args = [], []
    for (n, k, b, h) in th.rules:
        name = ident(n)
        s = " -> ".join([formula(x, stmts) for x in b] + [formula(h, stmts)])
        if k == "strict":
            decls.append(f"declare {name}_rule : ({s}).")
        neg = h[0] == "-"
        target = formula(("+", h[1]), stmts)
        body = []
        if b:
            body.append(f"cut ({s}) th.")
            body += [f"axiom {name}_rule."] if k == "strict" else ["by default.", "next."]
            for i, x in enumerate(b):
                if x[0] == "-" and not isinstance(x[1], tuple):
                    # a negative premise ~X is owed as the refutation of X (the
                    # context side X[c]), so that a counterargument on X meets it;
                    # an obligation on the node ~X[t] would be met only by an
                    # argument for ~X, which no derivation of X contests
                    body += ["elim.", f"elim h{i}.", f"cut ({formula(('+', x[1]), stmts)}) k{i}.", "axiom.", "next."]
                elif x[0] in ("n", "n-"):   # default negation: the presumed refutation of the literal
                    body += ["elim.", f"elim h{i}.", f"cut ({formula(('+' if x[0] == 'n' else '-', x[1]), stmts)}) k{i}.", f"axiom h{i}.", "by default.", "next."]
                else:
                    body += ["elim.", "next."]
            body.append("axiom th.")
        else:
            body += [f"axiom {name}_rule."] if k == "strict" else ["by default."]
        if neg and not b and k != "strict":
            # a bare presumption of the refutation, symmetric to a bare
            # presumption of X (derived, it would win the contest outright)
            args += [f"# {n} ({k}): {s}", f"counterargument {name} : ({target}).", "by default.", "dixi."]
        elif neg:
            args += [f"# {n} ({k}): {s}", f"counterargument {name} : ({target}).", f"cut (~{target}) x."] + body + ["elim.", "axiom.", "dixi."]
        else:
            args += [f"# {n} ({k}): {s}", f"argument {name} : ({target})."] + body + ["dixi."]
    return "\n".join(out + decls + args) + "\n"

def main():
    fmt, src = sys.argv[1], sys.argv[2]
    text = open(src).read()
    th = {"aspic": parse_aspic, "delp": parse_delp, "aba": parse_aba, "rdl": parse_rdl}[fmt](text)
    consts = getattr(th, "constants", None) or {c for r in th.rules for x in r[2] + [r[3]] if isinstance(x[1], str)
                                                for c in args_of(x[1]) if not VAR.fullmatch(c)}
    th = reachable(ground(th, consts))
    hdr = "\n".join(f"# {l}" if l.strip() else "#" for l in text.strip().splitlines())
    hdr = f"# Generated by eval/tweety/tw2fspy.py ({fmt}) from Tweety's {src.split('/')[-1]}:\n" + hdr
    if th.notes:
        hdr += "\n# Not translated:\n" + "\n".join(f"#  - {n}" for n in th.notes)
    res = emit(th, hdr)
    if len(sys.argv) > 3:
        open(sys.argv[3], "w").write(res)
    else:
        print(res)

if __name__ == "__main__":
    main()
