enter(e1, court, kp_c).
duty(C, app, E) :- metanorm, enter(E, C, _), passed(E).
passed(E) :- enter(E, _, _), not -passed(E).          % the helper, presumed
metanorm :- not -metanorm.                 % the gap, delegated

?- not duty(court, app, e1).
