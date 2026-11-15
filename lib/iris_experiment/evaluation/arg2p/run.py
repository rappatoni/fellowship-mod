"""The IRIS comparison material in Arg2P (arg2p 0.26.5, profile aspic:
no preference counts, every attack succeeds), evaluated under grounded, stable and, where a burden of
persuasion is declared, bpGrounded.

The mapping from AIDA (see ../comparison.org, section Arg2P):
- a fact is an axiom (`:->`);
- a delegation (presumption) is an ordinary premise (`:=>`), or an
  exception written with weak negation (`~(x)`: "unless x is shown");
- a rule AIDA presumes is defeasible (`=>`), a strict one strict (`->`);
  the profile refuses a contrary to the conclusion of a strict rule, so
  rules whose conclusion is contested are defeasible here;
- an obligation (an owed premise) is a premise that is not stated;
- AIDA's helper with one denial per requirement becomes one rule whose
  body lists the requirements (a closed conjunction);
- a burden of persuasion (`bp(x)`) is Arg2P's own construct, which AIDA
  expresses as which side presumes; scenarios with `bp` are marked.

Run: python run.py (writes the theories as NAME.arg2p and prints the
target's label per semantics; one label per stable labelling).
"""

from pathlib import Path

from arg2p import Arg2p, Flags, Semantics

HERE = Path(__file__).resolve().parent

FRAG_G = """\
a1 :-> consent1071.
a2 :-> created_without.
m_need : need => registrable.
koe_restraint : ~(need) => -registrable.
old_codet_1071 : consent1071 => codet.
old_need_codet : codet => need.
koe_no_codet : ~(constitutive) => -codet.
stg_constitutive : consent1071 => constitutive.
koe_not_constitutive : created_without => -constitutive.
"""

DUTY = "meta : metanorm, passed -> duty.\n"
RQ4 = {
    "rq4-a": "f1 :=> metanorm.\nf2 :=> passed.\n" + DUTY,
    "rq4-b": "f2 :=> passed.\n" + DUTY,
    "rq4-c": "f1 :=> metanorm.\nf2 :=> passed.\nf3 :=> written_only.\na1 :-> no_written.\n"
             "positivist : written_only, no_written -> -metanorm.\n" + DUTY,
    "rq4-d": "f1 :=> metanorm.\nf2 :=> passed.\nf3 :=> written_only.\na1 :-> no_written.\n"
             "a2 :-> presupposed.\npositivist : written_only, no_written -> -metanorm.\n"
             "anchor : presupposed -> -written_only.\n" + DUTY,
}

R6 = ("f1 :=> metanorm.\n" + DUTY
      + "exam : registrable, submitted, correct => passed.\n"
        "a1 :-> statute.\nreg_stat : statute -> registrable.\n"
        "face_value :=> correct.\n")
DOUBT = "a3 :-> conflict.\ndoubt : conflict => -correct.\n"
PROOF = "a4 :-> proved.\nevidence : proved => correct.\n"
RQ6 = {
    "rq6-a": R6 + "a2 :-> submitted.\n",
    "rq6-b": R6,
    "rq6-c": R6 + "a2 :-> submitted.\n" + DOUBT,
    "rq6-c-bp": R6 + "a2 :-> submitted.\n" + DOUBT + "bp(correct).\n",
    "rq6-d": R6 + "a2 :-> submitted.\n" + DOUBT + PROOF,
}

R7 = (R6.replace("exam : registrable, submitted, correct => passed.",
                 "exam : registrable, submitted, correct, ~(demand_unmet) => passed.")
      + "a2 :-> submitted.\n"
        "corr1 : duty -> claim.\ncorr2 : claim -> duty.\n"
        "a5 :-> demanded.\na6 :-> unrequired.\n"
        "court_extra : demanded, ~(improper) => demand_unmet.\n"
        "bgh_rule : claim -> limited.\n"
        "bgh_limit : limited, unrequired -> improper.\n")
RQ7 = {
    "rq7-a": R7,
    "rq7-a-bp": R7 + "bp(claim).\n",
    "rq7-b": R7 + "scope_n7 :=> limited.\n",
    "rq7-c": R7 + DOUBT,
}


def labels(theory, target, semantics):
    arg2p = Arg2p.of(theory, Flags.defaults().argument_labelling_mode(semantics))
    out = []
    for answer in arg2p.answer(target):
        if target in answer.in_statements:
            out.append("in")
        elif target in answer.out_statements:
            out.append("out")
        elif target in answer.und_statements:
            out.append("und")
        else:
            out.append("-")
    return ",".join(out) or "none"


def run(name, theory, target, profile="aspic"):
    theory = f"graphBuildMode({profile}).\n" + theory
    (HERE / f"{name}.arg2p").write_text(theory)
    sems = [("grounded", Semantics.GROUNDED), ("stable", Semantics.STABLE)]
    if "bp(" in theory:
        sems.append(("bpGrounded", Semantics.BP_GROUNDED))
    cols = "  ".join(f"{s}:{labels(theory, target, sem)}" for s, sem in sems)
    print(f"{name:9s} {target:12s} {cols}", flush=True)


if __name__ == "__main__":
    run("frag_g", FRAG_G, "registrable")
    run("frag_g-bp", FRAG_G + "bp(codet).\n", "registrable")
    run("frag_g-bpr", FRAG_G + "bp(registrable).\n", "registrable")
    for name, th in {**RQ4, **RQ6}.items():
        run(name, th, "duty")
    # the proof prevails over the doubt only through a superiority
    run("rq6-d-sup", RQ6["rq6-d"] + "sup(evidence, doubt).\n", "duty", profile="aspicPlus")
    for name, th in RQ7.items():
        run(name, th, "claim")
