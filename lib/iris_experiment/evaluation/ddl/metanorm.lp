% IRIS RQ 4/6/7 in Defeasible Deontic Logic. The court's duty to enter is
% the obligation obligation(enter), from a prescriptive rule. Scenario
% switches are constants: clingo -c scenario=NAME.
#const scenario = rq4a.
atom(metanorm; passed; written_only; no_written; presupposed;
     registrable; statute; submitted; correct; conflict; proved;
     claim; limited; proper; demanded; unrequired; submitted_contract; enter).

layer4 :- scenario = (rq4a; rq4c; rq4d).
layer6 :- scenario = (rq6a; rq6b; rq6c; rq6d).
layer7 :- scenario = (rq7a; rq7b; rq7c).

% the meta-norm: a presumption (rule without body); in rq4b it is missing
constitutiveRule(p_meta, metanorm) :- scenario != rq4b.
prescriptiveRule(meta, enter). body(meta, metanorm). body(meta, passed).
% the examination is presumed passed (the helper)
constitutiveRule(h_passed, passed).

% RQ 4: the positivist objection and its answer
fact(no_written) :- scenario = (rq4c; rq4d).
constitutiveRule(p_written_only, written_only) :- scenario = (rq4c; rq4d).
constitutiveRule(positivist, non(metanorm)) :- scenario = (rq4c; rq4d).
body(positivist, written_only). body(positivist, no_written).
superior(positivist, p_meta).
fact(presupposed) :- scenario = rq4d.
constitutiveRule(anchor, non(written_only)) :- scenario = rq4d.
body(anchor, presupposed).
superior(anchor, p_written_only).

% RQ 6: one defeating rule per requirement, superior to the helper
fact(statute) :- layer6. fact(statute) :- layer7.
constitutiveRule(reg_stat, registrable). body(reg_stat, statute).
constitutiveRule(p_not_registrable, non(registrable)).          % applicant must show
constitutiveRule(p_not_submitted, non(submitted)).              % applicant must show
superior(reg_stat, p_not_registrable).
fact(submitted) :- scenario = (rq6a; rq6c; rq6d; rq7a; rq7b; rq7c).
constitutiveRule(no_reg, non(passed)) :- layer6. constitutiveRule(no_reg, non(passed)) :- layer7.
body(no_reg, non(registrable)).
constitutiveRule(no_doc, non(passed)) :- layer6. constitutiveRule(no_doc, non(passed)) :- layer7.
body(no_doc, non(submitted)).
constitutiveRule(face_value, correct).                           % court must refute
constitutiveRule(no_cor, non(passed)) :- layer6. constitutiveRule(no_cor, non(passed)) :- layer7.
body(no_cor, non(correct)).
superior(no_reg, h_passed). superior(no_doc, h_passed). superior(no_cor, h_passed).
fact(conflict) :- scenario = (rq6c; rq6d; rq7c).
constitutiveRule(doubt, non(correct)). body(doubt, conflict).
superior(doubt, face_value).
fact(proved) :- scenario = rq6d.
constitutiveRule(evidence, correct). body(evidence, proved).
superior(evidence, doubt).

% RQ 7: correlativity and the claim-scope cycle
constitutiveRule(corr1, claim) :- layer7. body(corr1, obl(enter)).
prescriptiveRule(corr2, enter) :- layer7. body(corr2, claim).
fact(demanded) :- layer7. fact(unrequired) :- layer7.
constitutiveRule(p_proper, proper) :- layer7.
constitutiveRule(p_contract_missing, non(submitted_contract)) :- layer7.
constitutiveRule(court_extra, non(passed)) :- layer7.
body(court_extra, demanded). body(court_extra, proper). body(court_extra, non(submitted_contract)).
superior(court_extra, h_passed).
constitutiveRule(bgh_rule, limited) :- layer7. body(bgh_rule, claim).
constitutiveRule(scope_n7, limited) :- scenario = rq7b.
constitutiveRule(bgh_limit, non(proper)) :- layer7.
body(bgh_limit, limited). body(bgh_limit, unrequired).
superior(bgh_limit, p_proper).

#show obligation/1.
#show defeasible/1.
