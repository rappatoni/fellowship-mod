"""Checks of the Arg2P features the AIDA/Arg2P feature comparison relies on
(../comparison.org, "AIDA and Arg2P, feature by feature"; arg2p 0.26.5).

Each check prints what Arg2P answered, on the IRIS material where it fits.
Run: python features.py (output saved as features.txt).
"""

from arg2p import AnswerMode, Arg2p, Flags, Semantics


def show(title, theory, query, flags=None):
    arg2p = Arg2p.of(theory, flags or Flags.defaults())
    out = []
    for a in arg2p.answer(query):
        out.append(f"in={[str(s) for s in a.in_statements]} out={[str(s) for s in a.out_statements]} "
                   f"und={[str(s) for s in a.und_statements]}")
    print(f"- {title}: {query} -> " + " | ".join(out))
    return arg2p


print("1. Transposition of strict rules (autoTransposition, on under aspicPlus)")
# meta-norm as a strict rule; a refutation of its consequent attacks a premise
th = """f_meta :=> metanorm.
f_passed :=> passed.
meta : metanorm, passed -> duty.
r_no : objection => -duty.
f_obj :=> objection.
"""
for label, text in (("on (default)", th), ("off", "autoTransposition(off).\n" + th)):
    (graph,) = Arg2p.of(text).graphs()
    transposed = [a.descriptor for a in graph.arguments if a.conclusion.replace(" ", "") == "-metanorm"]
    print(f"- autoTransposition {label}: arguments for -metanorm: {transposed or 'none'}")

print("2. Obligations and permissions (deontic conflict model)")
th = """r_duty : complete => o(enter).
r_perm : [] => p(-enter).
f :=> complete.
sup(r_duty, r_perm).
"""
show("an obligation against a permission not to", th, "o(enter)")

print("3. Causality (NESS and but-for) on the processing example")
th = """finding :=> missing(d_appl).
interim_rule : missing(D) => lawful(interim(D)).
"""
arg2p = Arg2p.of(th)
sol = list(arg2p.solve("causality::butFor(missing(d_appl), lawful(interim(d_appl)))"))
print(f"- butFor(missing(d_appl), lawful(interim(d_appl))): {[s.is_yes for s in sol][:1]}")
sol = list(arg2p.solve("causality::ness(X, missing(d_appl), lawful(interim(d_appl)))"))
print(f"- ness(X, missing(d_appl), lawful(interim(d_appl))): "
      f"{[str(s.value('X')) for s in sol if s.is_yes][:2]}")

print("4. Arguable rule applicability (metaRules): the threshold of the need (RQ 8)")
th = """metaRules.
f_co :=> codetermines.
r_need : codetermines => need.
reading_substantial :=> applicable(r_need).
reading_compelling :=> -applicable(r_need).
"""
show("which test applies to the need", th, "need", Flags.defaults().argument_labelling_mode(Semantics.PREFERRED))

print("5. Incremental addition (add)")
arg2p = Arg2p.of("f :=> codetermines.\nr : codetermines => need.\n")
print(f"- before: need in = {[str(s) for s in arg2p.answer('need')[0].in_statements]}")
arg2p.add("r_k : powerless => -need.\nf2 :=> powerless.\n")
print(f"- after adding a counterargument: need -> "
      f"in={[str(s) for s in arg2p.answer('need')[0].in_statements]} "
      f"und={[str(s) for s in arg2p.answer('need')[0].und_statements]}")

print("6. answerMode across labellings (fragment (g) in stable)")
g = """graphBuildMode(aspic).
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
for mode in (AnswerMode.CREDULOUS, AnswerMode.SKEPTICAL):
    show(f"registrable, {mode.name.lower()}", g, "registrable",
         Flags.defaults().argument_labelling_mode(Semantics.STABLE).answer_mode(mode))
