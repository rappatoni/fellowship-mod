enter(e1, court, kp_c).
duty(C, app, E) :- metanorm, enter(E, C, _), passed(E).
passed(E) :- enter(E, _, _), not -passed(E).          % the helper, presumed
metanorm :- not -metanorm.                 % the gap, delegated
provided_by_statute(kp_c).
required(d_appl, e1).
registrable(F) :- provided_by_statute(F).
% requirements: shown by the applicant (negation as failure) ...
-passed(E) :- enter(E, _, F), not registrable(F).
-passed(E) :- required(D, E), not submitted(D).
% ... or refuted by the court (classical negation)
-passed(E) :- enter(E, _, F), -correct(F).
correct(F) :- enter(_, _, F), not -correct(F).        % face value
submitted(d_appl).
conflict(kp_c).
-correct(F) :- conflict(F), not correct_proved(F).
order(o_entry). order(o_submit). order(o_prove).
lawful(o_entry) :- duty(court, app, e1).
lawful(o_submit) :- not submitted(d_appl), not duty(court, app, e1).
lawful(o_prove) :- conflict(kp_c), not duty(court, app, e1).

?- lawful(O).
