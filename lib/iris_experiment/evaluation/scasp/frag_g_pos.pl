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

?- registrable.
