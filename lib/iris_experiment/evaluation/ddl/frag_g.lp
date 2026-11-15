% IRIS RQ 3, fragment (g), in Defeasible Logic (gvdgdo/Defeasible-Deontic-Logic).
% Every rule is defeasible; no superiority is stated, as AIDA states none.
atom(registrable; excluded; need; codet; consent1071; powerless; constitutive; created_without).
fact(consent1071). fact(created_without).
constitutiveRule(m_need, registrable).           body(m_need, need).
constitutiveRule(p_excluded, excluded).
constitutiveRule(excl_exception, non(excluded)). body(excl_exception, need).
constitutiveRule(koe_restraint, non(registrable)). body(koe_restraint, excluded).
constitutiveRule(old_codet_1071, codet).         body(old_codet_1071, consent1071).
constitutiveRule(old_need_codet, need).          body(old_need_codet, codet).
constitutiveRule(p_powerless, powerless).
constitutiveRule(pw_exception, non(powerless)).  body(pw_exception, constitutive).
constitutiveRule(koe_no_codet, non(codet)).      body(koe_no_codet, powerless).
constitutiveRule(stg_constitutive, constitutive). body(stg_constitutive, consent1071).
constitutiveRule(koe_not_constitutive, non(constitutive)). body(koe_not_constitutive, created_without).
% an exception overrides the presumption it answers
superior(excl_exception, p_excluded).
superior(pw_exception, p_powerless).
#show defeasible/1.
#show refuted/1.
