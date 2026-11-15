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
claim(A, C, E) :- duty(C, A, E).
duty(C, A, E) :- claim(A, C, E).
demanded(d_contract, e1).
unrequired(d_contract, e1).
-passed(E) :- demanded(D, E), proper(D, E), not submitted(D).
proper(D, E) :- demanded(D, E), not -proper(D, E).
limited(E) :- claim(_, _, E).
-proper(D, E) :- limited(E), unrequired(D, E).
conflict(kp_c).
-correct(F) :- conflict(F), not correct_proved(F).

?- not claim(app, court, e1).
