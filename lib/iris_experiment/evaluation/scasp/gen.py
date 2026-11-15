"""Write the s(CASP) encodings of the IRIS comparison material.

s(CASP) is goal-directed answer set programming with variables, so the
RQ 4/6/7 example keeps AIDA's first-order, reified form.

The mapping from AIDA (see ../comparison.org, section s(CASP)):
- a fact is a fact;
- a delegation (presumption) is a default: `p :- not -p.`;
- a counterargument is a rule for the classical negation `-p`; s(CASP)
  adds the constraint that p and -p never hold together;
- a rule AIDA presumes is a rule with the default `not -head` in its body;
- an obligation (an owed premise) is a predicate with no rule;
- AIDA's helper with one denial per requirement becomes the helper as a
  default and one rule for its negation per requirement. Whether a
  requirement must be shown (`not submitted(D)`, negation as failure) or
  refuted (`-correct(F)`, classical negation) is the burden.

Each scenario is written twice, with the query and with its negation:
`scasp -s0 NAME_pos.pl` lists the answer sets with the target,
`NAME_neg.pl` those without it (credulous and skeptical in one view).
Run: python gen.py, then ./run.sh.
"""

from pathlib import Path

HERE = Path(__file__).resolve().parent

FRAG_G = """\
% RQ 3, fragment (g)
consent1071.
created_without.
registrable :- need.                          % meta-norm, second limb (strict)
excluded :- not -excluded.                    % the restraint, presumed
-excluded :- need.                            % its exception
-registrable :- excluded.
codet :- consent1071, not -codet.             % Oldenburg, presumed rule
need :- codet, not -need.                     % presumed rule
powerless :- not -powerless.                  % Koeln, presumed
-powerless :- constitutive.                   % its exception
-codet :- powerless.
constitutive :- consent1071, not -constitutive.        % Stuttgart, presumed rule
-constitutive :- created_without, not constitutive.    % Koeln, presumed rule
"""

BASE = """\
enter(e1, court, kp_c).
duty(C, app, E) :- metanorm, enter(E, C, _), passed(E).
passed(E) :- enter(E, _, _), not -passed(E).          % the helper, presumed
"""
META_PRES = "metanorm :- not -metanorm.                 % the gap, delegated\n"
POSITIVIST = """\
no_written.
written_only :- not -written_only.
-metanorm :- written_only, no_written.
"""
ANCHOR = "presupposed.\n-written_only :- presupposed.\n"

REQ = """\
provided_by_statute(kp_c).
required(d_appl, e1).
registrable(F) :- provided_by_statute(F).
% requirements: shown by the applicant (negation as failure) ...
-passed(E) :- enter(E, _, F), not registrable(F).
-passed(E) :- required(D, E), not submitted(D).
% ... or refuted by the court (classical negation)
-passed(E) :- enter(E, _, F), -correct(F).
correct(F) :- enter(_, _, F), not -correct(F).        % face value
"""
SUBMITTED = "submitted(d_appl).\n"
DOUBT = "conflict(kp_c).\n-correct(F) :- conflict(F), not correct_proved(F).\n"
PROOF = "proved(kp_c).\ncorrect_proved(F) :- proved(F).\ncorrect(F) :- proved(F).\n"

CYCLE = """\
claim(A, C, E) :- duty(C, A, E).
duty(C, A, E) :- claim(A, C, E).
demanded(d_contract, e1).
unrequired(d_contract, e1).
-passed(E) :- demanded(D, E), proper(D, E), not submitted(D).
proper(D, E) :- demanded(D, E), not -proper(D, E).
limited(E) :- claim(_, _, E).
-proper(D, E) :- limited(E), unrequired(D, E).
"""

SCENARIOS = {
    "frag_g": (FRAG_G, "registrable"),
    "rq4-a": (BASE + META_PRES, "duty(court, app, e1)"),
    "rq4-b": (BASE, "duty(court, app, e1)"),
    "rq4-c": (BASE + META_PRES + POSITIVIST, "duty(court, app, e1)"),
    "rq4-d": (BASE + META_PRES + POSITIVIST + ANCHOR, "duty(court, app, e1)"),
    "rq6-a": (BASE + META_PRES + REQ + SUBMITTED, "duty(court, app, e1)"),
    "rq6-b": (BASE + META_PRES + REQ, "duty(court, app, e1)"),
    "rq6-c": (BASE + META_PRES + REQ + SUBMITTED + DOUBT, "duty(court, app, e1)"),
    "rq6-d": (BASE + META_PRES + REQ + SUBMITTED + DOUBT + PROOF, "duty(court, app, e1)"),
    "rq7-a": (BASE + META_PRES + REQ + SUBMITTED + CYCLE, "claim(app, court, e1)"),
    "rq7-b": (BASE + META_PRES + REQ + SUBMITTED + CYCLE + "limited(e1).\n",
              "claim(app, court, e1)"),
    "rq7-c": (BASE + META_PRES + REQ + SUBMITTED + CYCLE + DOUBT, "claim(app, court, e1)"),
}

# RQ 6 processing: which order is lawful (cf. rq4-6-7-metanorm.org, layer 2b)
ORDERS = """\
order(o_entry). order(o_submit). order(o_prove).
lawful(o_entry) :- duty(court, app, e1).
lawful(o_submit) :- not submitted(d_appl), not duty(court, app, e1).
lawful(o_prove) :- conflict(kp_c), not duty(court, app, e1).
"""
PROCESSING = {
    "rq6p-a": BASE + META_PRES + REQ + SUBMITTED + ORDERS,
    "rq6p-b": BASE + META_PRES + REQ + ORDERS,
    "rq6p-c": BASE + META_PRES + REQ + SUBMITTED + DOUBT + ORDERS,
    "rq6p-d": BASE + META_PRES + REQ + SUBMITTED + DOUBT + PROOF + ORDERS,
}

if __name__ == "__main__":
    for name, program in PROCESSING.items():
        (HERE / f"{name}_order.pl").write_text(program + "\n?- lawful(O).\n")
    for name, (program, target) in SCENARIOS.items():
        (HERE / f"{name}_pos.pl").write_text(program + f"\n?- {target}.\n")
        (HERE / f"{name}_neg.pl").write_text(program + f"\n?- not {target}.\n")
