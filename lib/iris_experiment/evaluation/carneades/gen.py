"""Write the Carneades 4 (CAES) encodings of the IRIS comparison material.

The mapping from AIDA (see ../comparison.org, section Carneades):
- a fact is an assumption (Carneades accepts assumptions outright);
- a delegation (a presumption) is an argument without premises, under the
  scheme `presumption` (constant weight 0.5), so a counterargument of
  weight 1.0 defeats it;
- an argument whose rule AIDA presumes also gets weight 0.5; one resting
  only on facts and strict rules gets the default weight 1.0;
- an obligation (an owed premise) is a premise nobody argues for;
- AIDA's helper with one denial per requirement becomes one linked
  argument whose premises are the requirements (a closed conjunction);
- a refutation is the statement prefixed with "¬", in an issue with it.

Run: python gen.py (writes the *.yml files next to this script).
"""

from pathlib import Path

HERE = Path(__file__).resolve().parent
PRES = {"premises": [], "scheme": "presumption"}


def yaml_value(v):
    if isinstance(v, list):
        return "[" + ", ".join(v) + "]"
    return str(v)


def write(name, title, statements, arguments, assumptions, issues=()):
    lines = [f"meta: {{title: '{title}'}}",
             "argument_schemes:",
             "  - id: presumption",
             "    weight: {constant: 0.5}",
             "statements:"]
    for key, text in statements.items():
        lines.append(f"  {key}: {text}")
    if issues:
        lines.append("issues:")
        for i, (pos, neg) in enumerate(issues):
            lines.append(f"  i{i}: {{positions: [{pos}, {neg}]}}")
    lines.append("arguments:")
    for key, arg in arguments.items():
        fields = ", ".join(f"{k}: {yaml_value(v)}" for k, v in arg.items())
        lines.append(f"  {key}: {{{fields}}}")
    lines.append(f"assumptions: [{', '.join(assumptions)}]")
    (HERE / f"{name}.yml").write_text("\n".join(lines) + "\n")


def arg(conclusion, *premises, **extra):
    return {"conclusion": conclusion, "premises": list(premises), **extra}


def pres(conclusion):
    return {"conclusion": conclusion, **PRES}


# --- RQ 3, fragment (g) --------------------------------------------------

G_ST = {
    "registrable": "the usufruct is registrable",
    "¬registrable": "the usufruct is not registrable",
    "excluded": "registration is excluded (restraint)",
    "need": "legal dealings need the information",
    "codet": "the usufructuary co-determines the partnership's fate",
    "¬codet": "the usufructuary does not co-determine",
    "consent1071": "the partner's measures need the usufructuary's consent (§ 1071 BGB)",
    "powerless": "the usufructuary has no influence",
    "constitutive": "the consent conditions the validity of the measure",
    "¬constitutive": "the consent does not condition the measure's validity",
    "created_without": "the usufruct can be created without the partnership",
}
write("frag_g", "IRIS RQ 3, fragment (g): the core of the dispute", G_ST, {
    "m_need": arg("registrable", "need"),
    "pres_excluded": pres("excluded"),
    "koe_restraint": arg("¬registrable", "excluded", undercutter="need"),
    "old_need_codet": arg("need", "codet", scheme="presumption"),
    "old_codet_1071": arg("codet", "consent1071", scheme="presumption"),
    "pres_powerless": pres("powerless"),
    "koe_no_codet": arg("¬codet", "powerless", undercutter="constitutive"),
    "stg_constitutive": arg("constitutive", "consent1071", scheme="presumption"),
    "koe_not_constitutive": arg("¬constitutive", "created_without", scheme="presumption"),
}, ["consent1071", "created_without"],
    issues=[("registrable", "¬registrable"), ("codet", "¬codet"),
            ("constitutive", "¬constitutive")])

# --- RQ 4 -----------------------------------------------------------------

M_ST = {
    "duty": "the register court owes the applicants the entry",
    "metanorm": "the unwritten meta-norm is in force",
    "¬metanorm": "the meta-norm is not in force",
    "passed": "the examination of the entry is passed",
}
POS_ST = {
    "written_only": "only a written norm can bind the register court",
    "¬written_only": "a norm the written rules presuppose can bind the court",
    "no_written": "no written norm orders the entry",
    "presupposed": "the written rules presuppose the meta-norm",
}
DUTY = {"duty_arg": arg("duty", "metanorm", "passed")}
POSITIVIST = {"pres_written_only": pres("written_only"),
              "positivist": arg("¬metanorm", "written_only", "no_written")}
ANCHOR = {"anchor": arg("¬written_only", "presupposed")}
META_ISSUES = [("metanorm", "¬metanorm"), ("written_only", "¬written_only")]

write("rq4-a", "IRIS RQ 4, 4a: the meta-norm presumed", M_ST,
      {**DUTY, "pres_meta": pres("metanorm"), "pres_passed": pres("passed")}, [],
      META_ISSUES[:1])
write("rq4-b", "IRIS RQ 4, 4b: the meta-norm an ordinary premise", M_ST,
      {**DUTY, "pres_passed": pres("passed")}, [], META_ISSUES[:1])
write("rq4-c", "IRIS RQ 4, 4c: the positivist objection", {**M_ST, **POS_ST},
      {**DUTY, "pres_meta": pres("metanorm"), "pres_passed": pres("passed"), **POSITIVIST},
      ["no_written"], META_ISSUES)
write("rq4-d", "IRIS RQ 4, 4d: the objection answered", {**M_ST, **POS_ST},
      {**DUTY, "pres_meta": pres("metanorm"), "pres_passed": pres("passed"), **POSITIVIST,
       **ANCHOR}, ["no_written", "presupposed"], META_ISSUES)

# --- RQ 6 -----------------------------------------------------------------

R6_ST = {
    "duty": "the register court owes the applicants the entry",
    "metanorm": "the unwritten meta-norm is in force",
    "passed": "the examination of the entry is passed",
    "registrable": "the entry is abstractly registrable",
    "statute": "the entry is provided by statute",
    "submitted": "the application in certified form is submitted",
    "correct": "the reported change is correct",
    "¬correct": "the reported change is not correct",
    "conflict": "the register contradicts the reported change",
    "proved": "the applicants have proved the change",
}
R6 = {
    **DUTY,
    "pres_meta": pres("metanorm"),
    "exam": arg("passed", "registrable", "submitted", "correct"),
    "reg_stat": arg("registrable", "statute"),
    "face_value": pres("correct"),
}
DOUBT = {"doubt": arg("¬correct", "conflict", scheme="presumption")}
EVIDENCE = {"evidence": arg("correct", "proved")}
COR = [("correct", "¬correct")]

write("rq6-a", "IRIS RQ 6, 6a: complete application", R6_ST, R6,
      ["statute", "submitted"], COR)
write("rq6-b", "IRIS RQ 6, 6b: the application is missing", R6_ST, R6, ["statute"], COR)
write("rq6-c", "IRIS RQ 6, 6c: a justified doubt", R6_ST, {**R6, **DOUBT},
      ["statute", "submitted", "conflict"], COR)
write("rq6-d", "IRIS RQ 6, 6d: the doubt answered by proof", R6_ST,
      {**R6, **DOUBT, **EVIDENCE}, ["statute", "submitted", "conflict", "proved"], COR)

# --- RQ 7 -----------------------------------------------------------------

R7_ST = {
    **R6_ST,
    "claim": "the applicants have a claim to the entry",
    "demand_unmet": "a demand of the court within the scope is unmet",
    "demanded": "the court demands the transfer agreement",
    "contract_missing": "the transfer agreement is not submitted",
    "improper": "the demand is outside the scope of examination",
    "limited": "the examination is limited to what the law requires",
    "unrequired": "the law does not require the transfer agreement",
}
R7 = {
    **R6,
    "exam": arg("passed", "registrable", "submitted", "correct", undercutter="demand_unmet"),
    "corr1": arg("claim", "duty"),
    "corr2": arg("duty", "claim"),
    "court_extra": arg("demand_unmet", "demanded", "contract_missing",
                       undercutter="improper", scheme="presumption"),
    "bgh_rule": arg("limited", "claim"),
    "bgh_limit": arg("improper", "limited", "unrequired"),
}
F7 = ["statute", "submitted", "demanded", "contract_missing", "unrequired"]

write("rq7-a", "IRIS RQ 7, 7a: the claim-scope cycle", R7_ST, R7, F7, COR)
write("rq7-b", "IRIS RQ 7, 7b: the scope principle as anchor", R7_ST,
      {**R7, "scope_n7": pres("limited")}, F7, COR)
write("rq7-c", "IRIS RQ 7, 7c: a justified doubt on the cycle", R7_ST, {**R7, **DOUBT},
      F7 + ["conflict"], COR)
