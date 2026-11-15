"""RQ 6 processing in Arg2P: which order is lawful, as query bindings.

The counterpart of AIDA's layers 2b and 2c (../../rq4-6-7-metanorm.org):
- the declared orders o_entry, o_submit, o_prove (layer 2b);
- an order computed from a finding, interim(D) for a missing document D
  (layer 2c, constructive);
- the case split on the duty: AIDA's argued Peirce step with the "not
  owed" site presumed has its nearest Arg2P counterpart in weak negation,
  ~(duty), "unless the duty is shown" (layer 2c, classical). Arg2P has no
  single classical argument for it.

For each scenario and labelling the query lawful(O) gives the accepted
instances, and for each the argument behind it: the rules it uses, its
ordinary premises (Arg2P's presumptions) and the weak-negation conditions
of its rules (what it assumes is not shown).

Scenarios 6c and 6d need a burden of persuasion and a superiority, as
found in run.py: under the aspic profile the doubt and the face-value
premise deadlock (6c: bp(correct), bpGrounded), and the proof deadlocks
with the doubt (6d: sup(evidence, doubt) under aspicPlus).

Run: python processing.py (writes NAME.arg2p and prints the bindings).
"""

import re
from pathlib import Path

from arg2p import Arg2p, Flags, Semantics

HERE = Path(__file__).resolve().parent

BASE = """\
f_meta :=> metanorm.
meta : metanorm, passed -> duty.
exam : registrable(kp_c), submitted(d_appl), correct(kp_c) => passed.
a_stat :-> statute(kp_c).
reg_stat : statute(F) -> registrable(F).
face_value :=> correct(kp_c).
"""
SUBMITTED = "a_sub :-> submitted(d_appl).\n"
DOUBT = "a_conf :-> conflict(kp_c).\ndoubt : conflict(F) => -correct(F).\n"
PROOF = "a_proved :-> proved(kp_c).\nevidence : proved(F) => correct(F).\n"

# layer 2b: declared orders, excluding each other as in AIDA
ORDERS = """\
lawful_entry : duty => lawful(o_entry).
lawful_submit : ~(submitted(d_appl)), ~(duty) => lawful(o_submit).
lawful_prove : conflict(kp_c), ~(duty) => lawful(o_prove).
"""
# layer 2c, constructive: the order computed from the finding
COMPUTED = """\
finding :=> missing(d_appl).
moot : submitted(D) => -missing(D).
interim_rule : missing(D) => lawful(interim(D)).
"""

SCENARIOS = {
    # name: (theory body, profile, semantics)
    "6p-a": (BASE + SUBMITTED + ORDERS, "aspic", Semantics.GROUNDED),
    "6p-b": (BASE + ORDERS, "aspic", Semantics.GROUNDED),
    "6p-c": (BASE + SUBMITTED + DOUBT + ORDERS + "bp(correct(kp_c)).\n", "aspic",
             Semantics.BP_GROUNDED),
    "6p-d": (BASE + SUBMITTED + DOUBT + PROOF + ORDERS + "sup(evidence, doubt).\n",
             "aspicPlus", Semantics.GROUNDED),
    "6w-a": (BASE + SUBMITTED + COMPUTED, "aspic", Semantics.GROUNDED),
    "6w-b": (BASE + COMPUTED, "aspic", Semantics.GROUNDED),
}

WEAK = re.compile(r"~\(([^()]*(?:\([^()]*\))?[^()]*)\)")


def rule_bodies(theory):
    """rule name -> the weak-negation conditions in its body."""
    out = {}
    for line in theory.splitlines():
        m = re.match(r"\s*(\w+)\s*:\s*(.*?)\s*=>", line)
        if m:
            out[m.group(1)] = WEAK.findall(m.group(2))
    return out


def run(name, body, profile, semantics):
    theory = f"graphBuildMode({profile}).\n" + body
    (HERE / f"proc_{name}.arg2p").write_text(theory)
    premises = {m.group(1) for m in re.finditer(r"^(\w+)\s*:=>", body, re.M)}
    weak = rule_bodies(body)
    arg2p = Arg2p.of(theory, Flags.defaults().argument_labelling_mode(semantics))
    answers = arg2p.answer("lawful(O)")
    graphs = list(arg2p.graphs())
    lines = []
    for k, answer in enumerate(answers):
        accepted = [render(s) for s in answer.in_statements] or "nothing"
        lines.append(f"  labelling {k + 1}: lawful(O) in for {accepted}")
        if k < len(graphs):
            for arg in graphs[k].arguments:
                if arg.conclusion.startswith("lawful(") and arg.conclusion in in_conclusions(graphs[k]):
                    pres = sorted(r for r in arg.rules if r in premises)
                    assumes = sorted({w for r in arg.rules for w in weak.get(r, [])})
                    lines.append(f"     {arg.conclusion}: rules {sorted(arg.rules)}; "
                                 f"presumed premises {pres}; assumes not shown {assumes}")
    print(f"{name} ({profile}, {semantics.name.lower()})")
    print("\n".join(lines) or "  no answer")


def render(term):
    if hasattr(term, "functor"):
        args = getattr(term, "args", ())
        return f"{term.functor}({', '.join(render(a) for a in args)})" if args else term.functor
    return str(term)


def in_conclusions(graph):
    try:
        return {a.conclusion for a in graph.labelled("in")}
    except Exception:  # noqa: BLE001 - an older API without labelled(label)
        return {a.conclusion for a in graph.arguments}


if __name__ == "__main__":
    for name, (body, profile, semantics) in SCENARIOS.items():
        run(name, body, profile, semantics)
