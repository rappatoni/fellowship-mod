"""The IRIS comparison material in ASPIC+, evaluated with PyArg
(python-argumentation 2.0.2).

The mapping from AIDA (see ../comparison.org, section ASPIC+):
- a fact is an axiom;
- a delegation (presumption) is an ordinary premise, so it can be
  undermined;
- a rule AIDA presumes is a defeasible rule; a strict rule stays strict;
- an obligation (an owed premise) is a premise that is not in the
  knowledge base;
- AIDA's helper with one denial per requirement becomes the helper as an
  ordinary premise, one strict rule from it to each requirement, and the
  requirement's refutation as an ordinary premise where AIDA presumes it;
- strict rules are closed under transposition (ASPIC+'s own condition for
  its rationality postulates);
- no preferences are given. Two variants are reported: "plain", where
  every attack is a defeat, and "lastlink", PyArg's default (last-link
  elitist ordering with empty preferences), under which an argument whose
  last link is a defeasible rule is weaker than one resting on premises;
  and "burden", last-link elitist with premise preferences that put the
  premise of each counterargument above the presumption it attacks, as
  AIDA's burden allocation does (BURDEN below).

Run: python aspic.py (prints one line per theory: the target's status
under grounded, and under stable skeptically and credulously).
"""

import os

from py_arg.abstract_argumentation_classes.abstract_argumentation_framework import \
    AbstractArgumentationFramework
from py_arg.algorithms.semantics.get_grounded_extension import get_grounded_extension
from py_arg.algorithms.semantics.get_stable_extensions import get_stable_extensions
from py_arg.aspic_classes.argumentation_system import ArgumentationSystem
from py_arg.aspic_classes.argumentation_theory import ArgumentationTheory
from py_arg.aspic_classes.defeasible_rule import DefeasibleRule
from py_arg.aspic_classes.literal import Literal
from py_arg.aspic_classes.orderings.preference_preorder import PreferencePreorder
from py_arg.aspic_classes.strict_rule import StrictRule


def neg(x):
    return x[1:] if x.startswith("-") else "-" + x


def parse(rule):
    """'a, b -> c' or 'a, b => c' to (antecedents, consequent, strict)."""
    strict = "->" in rule
    body, head = rule.split("->" if strict else "=>")
    ants = [a.strip() for a in body.split(",") if a.strip()]
    return ants, head.strip(), strict


def transpositions(ants, head):
    for i, a in enumerate(ants):
        yield ants[:i] + [neg(head)] + ants[i + 1:], neg(a)


def theory(axioms, premises, rules, prefs=()):
    strict, defeasible = [], []
    for r in rules:
        ants, head, is_strict = parse(r)
        if is_strict:
            strict.append((ants, head))
            strict.extend(transpositions(ants, head))
        else:
            defeasible.append((ants, head))
    atoms = set(axioms) | set(premises)
    for ants, head in strict + defeasible:
        atoms |= set(ants) | {head}
    atoms |= {neg(a) for a in atoms}
    lang = {a: Literal(a) for a in atoms}
    contraries = {a: {lang[neg(a)]} for a in atoms}
    srules = [StrictRule(f"s{i}", {lang[a] for a in ants}, lang[h])
              for i, (ants, h) in enumerate(strict)]
    drules = [DefeasibleRule(f"d{i}", {lang[a] for a in ants}, lang[h])
              for i, (ants, h) in enumerate(defeasible)]
    system = ArgumentationSystem(lang, contraries, srules, drules)
    order = PreferencePreorder.create_reflexive_preorder([lang[p] for p in premises])
    for weaker, stronger in prefs:
        order.append((lang[weaker], lang[stronger]))
    return ArgumentationTheory(system, [lang[a] for a in axioms],
                               [lang[p] for p in premises], order), lang


def stable_extensions(af):
    """Stable extensions of PyArg's framework. With ASPARTIX set to the
    directory of the ASPARTIX encodings, clingo computes them with
    stable.dl (PyArg's own routine took 166 s on rq7-a); otherwise PyArg's."""
    aspartix = os.environ.get("ASPARTIX")
    if not aspartix:
        return [set(e) for e in get_stable_extensions(af)]
    import clingo
    args = list(af.arguments)
    index = {a: i for i, a in enumerate(args)}
    facts = "".join(f"arg(a{i})." for i in range(len(args)))
    facts += "".join(f"att(a{index[d.from_argument]},a{index[d.to_argument]})." for d in af.defeats)
    ctl = clingo.Control(["0", "--warn=none"])
    ctl.load(os.path.join(aspartix, "stable.dl"))
    ctl.add("base", [], facts)
    ctl.ground([("base", [])])
    out = []
    with ctl.solve(yield_=True) as handle:
        for model in handle:
            out.append({args[int(str(s.arguments[0])[1:])] for s in model.symbols(atoms=True)
                        if s.name == "in"})
    return out


def status(axioms, premises, rules, target, show=(), variant="plain", prefs=()):
    th, lang = theory(axioms, premises, rules, prefs if variant == "burden" else ())
    if variant == "plain":
        af = AbstractArgumentationFramework("af", th.all_arguments, th.recompute_all_defeats(None))
    else:
        af = th.create_abstract_argumentation_framework("af")
    by_name = {str(a): a for a in th.all_arguments}

    def holds(ext, lit=target):
        return any(str(a.conclusion) == lit for a in ext)

    grounded = get_grounded_extension(af)
    stable = stable_extensions(af)
    for i, e in enumerate(stable):
        if show:
            print(f"        stable [{i + 1}]: " + " ".join(
                f"{lit}:{'IN' if holds(e, lit) else '-'}" for lit in show))
    g = "IN" if holds(grounded) else "-"
    sk = "IN" if stable and all(holds(e) for e in stable) else "-"
    cr = "IN" if any(holds(e) for e in stable) else "-"
    return (f"args={len(by_name):3d} stable={len(stable)}  grounded:{g:3s}"
            f" stable-skeptical:{sk:3s} stable-credulous:{cr}")


# --- RQ 3, fragment (g) ----------------------------------------------------
FRAG_G = dict(
    axioms=["consent1071", "created_without"],
    premises=["excluded", "powerless"],
    rules=["need -> registrable",
           "excluded -> -registrable",
           "need -> -excluded",              # the restraint's exception
           "consent1071 => codet",
           "codet => need",
           "powerless -> -codet",
           "constitutive -> -powerless",     # the presumption's exception
           "consent1071 => constitutive",
           "created_without => -constitutive"])

# --- RQ 4 ------------------------------------------------------------------
DUTY = ["metanorm, passed -> duty"]
POS = ["written_only, no_written -> -metanorm"]
ANCHOR = ["presupposed -> -written_only"]
RQ4 = {
    "rq4-a": dict(axioms=[], premises=["metanorm", "passed"], rules=DUTY),
    "rq4-b": dict(axioms=[], premises=["passed"], rules=DUTY),
    "rq4-c": dict(axioms=["no_written"], premises=["metanorm", "passed", "written_only"],
                  rules=DUTY + POS),
    "rq4-d": dict(axioms=["no_written", "presupposed"],
                  premises=["metanorm", "passed", "written_only"], rules=DUTY + POS + ANCHOR),
}

# --- RQ 6 ------------------------------------------------------------------
REQ = ["passed -> registrable", "passed -> submitted", "passed -> correct",
       "statute -> registrable"]
P6 = ["metanorm", "passed", "correct", "-registrable", "-submitted"]
DOUBT = ["conflict => -correct"]
PROOF = ["proved -> correct"]
RQ6 = {
    "rq6-a": dict(axioms=["statute", "submitted"], premises=P6, rules=DUTY + REQ),
    "rq6-b": dict(axioms=["statute"], premises=P6, rules=DUTY + REQ),
    "rq6-c": dict(axioms=["statute", "submitted", "conflict"], premises=P6,
                  rules=DUTY + REQ + DOUBT),
    "rq6-d": dict(axioms=["statute", "submitted", "conflict", "proved"], premises=P6,
                  rules=DUTY + REQ + DOUBT + PROOF),
}

# --- RQ 7 ------------------------------------------------------------------
CYCLE = ["duty -> claim", "claim -> duty",
         "demanded, proper, passed -> submitted_contract",
         "claim -> limited", "limited, unrequired -> -proper"]
A7 = ["statute", "submitted", "demanded", "unrequired"]
P7 = P6 + ["proper", "-submitted_contract"]
RQ7 = {
    "rq7-a": dict(axioms=A7, premises=P7, rules=DUTY + REQ + CYCLE),
    "rq7-b": dict(axioms=A7, premises=P7 + ["limited"], rules=DUTY + REQ + CYCLE),
    "rq7-c": dict(axioms=A7 + ["conflict"], premises=P7, rules=DUTY + REQ + CYCLE + DOUBT),
}

# premise preferences (weaker, stronger) for the "burden" variant
BURDEN = {
    "rq4-c": [("metanorm", "written_only")],
    "rq4-d": [("metanorm", "written_only")],
    **{n: [("passed", "-submitted"), ("passed", "-registrable")] for n in RQ6},
    **{n: [("passed", "-submitted"), ("passed", "-registrable"),
           ("passed", "proper"), ("passed", "-submitted_contract")] for n in RQ7},
}
BURDEN["rq7-b"] = BURDEN["rq7-a"] + [("proper", "limited")]

if __name__ == "__main__":
    SHOW_G = ("registrable", "-registrable", "codet", "-codet", "constitutive", "-constitutive")
    SHOW_7 = ("claim", "duty", "passed", "-passed", "limited", "proper", "-proper")
    for variant in ("plain", "lastlink", "burden"):
        print(f"=== variant {variant}")
        print(f"{'frag_g':7s} target registrable   ",
              status(**FRAG_G, target="registrable", show=SHOW_G, variant=variant))
        for name, t in {**RQ4, **RQ6}.items():
            print(f"{name:7s} target duty          ", status(**t, target="duty", variant=variant,
                                                                      prefs=BURDEN.get(name, ())))
        for name, t in RQ7.items():
            print(f"{name:7s} target claim         ",
                  status(**t, target="claim", show=SHOW_7, variant=variant,
                         prefs=BURDEN.get(name, ())))
