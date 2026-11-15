enter(e1, court, kp_c).
duty(C, app, E) :- metanorm, enter(E, C, _), passed(E).
passed(E) :- enter(E, _, _), not -passed(E).          % the helper, presumed
metanorm :- not -metanorm.                 % the gap, delegated
no_written.
written_only :- not -written_only.
-metanorm :- written_only, no_written.

?- not duty(court, app, e1).
