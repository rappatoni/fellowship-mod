% The IRIS comparison material in the style of PROLEG.
%
% PROLEG itself (Satoh et al., JSAI-isAI 2010) is not publicly available.
% This file re-implements its published core, NOT the original system:
% a conclusion holds if the body of one of its rules holds and none of its
% exceptions holds; an exception is itself a conclusion with rules and
% exceptions. The split into rule and exception is PROLEG's encoding of the
% burden of proof (presupposed ultimate fact theory): the party relying on
% the rule proves its body, the other party proves the exception.
%
% The mapping from AIDA (see ../comparison.org, section PROLEG):
% - a fact is fact/1;
% - a delegation (presumption) is a rule with an empty body;
% - a counterargument against it is an exception;
% - an obligation (an owed premise) is a body literal nobody proves;
% - AIDA's helper with one denial per requirement: the requirements the
%   applicant must show form the rule's body, those the court must refute
%   are exceptions.
%
% Run: ciao-shell proleg.pl SCENARIO (or no argument for all scenarios).
% Prolog's depth-first search does not terminate on a derivation cycle;
% a depth bound turns that into the answer "loop".

:- dynamic(scenario/1).
:- discontiguous(rule/2).
:- discontiguous(exception/2).
:- discontiguous(fact/1).

in(S) :- scenario(S).
in(Ss) :- Ss = [_|_], scenario(S), member(S, Ss).

% --- RQ 3, fragment (g) ----------------------------------------------------
fact(consent1071) :- in(frag_g).
fact(created_without) :- in(frag_g).
rule(registrable, [need]) :- in(frag_g).         % the applicant shows a need (restraint)
rule(need, [codet]) :- in(frag_g).
rule(codet, [consent1071]) :- in(frag_g).
exception(codet, powerless) :- in(frag_g).       % Koeln: no influence ...
rule(powerless, []) :- in(frag_g).               % ... presumed ...
exception(powerless, constitutive) :- in(frag_g). % ... unless the consent is constitutive
rule(constitutive, [consent1071]) :- in(frag_g). % Stuttgart
exception(constitutive, created_without) :- in(frag_g). % Koeln

% --- RQ 4 ----------------------------------------------------------------------
rule(duty, [metanorm, passed]) :- \+ in(frag_g).
rule(metanorm, []) :- \+ in(rq4b).               % the gap, presumed (missing in 4b)
rule(passed, []) :- in([rq4a, rq4b, rq4c, rq4d]).
exception(metanorm, positivist) :- in([rq4c, rq4d]).
rule(positivist, [written_only, no_written]) :- in([rq4c, rq4d]).
rule(written_only, []) :- in([rq4c, rq4d]).
fact(no_written) :- in([rq4c, rq4d]).
exception(written_only, presupposed) :- in(rq4d).
fact(presupposed) :- in(rq4d).

% --- RQ 6 ----------------------------------------------------------------------
layer67 :- in([rq6a, rq6b, rq6c, rq6d, rq7a, rq7b, rq7c]).
rule(passed, [registrable, submitted]) :- layer67.   % the applicant shows these
exception(passed, incorrect) :- layer67.             % the court shows this
rule(registrable, [statute]) :- layer67.
fact(statute) :- layer67.
fact(submitted) :- in([rq6a, rq6c, rq6d, rq7a, rq7b, rq7c]).
rule(incorrect, [conflict]) :- layer67.
exception(incorrect, proved) :- layer67.
fact(conflict) :- in([rq6c, rq6d, rq7c]).
fact(proved) :- in(rq6d).

% --- RQ 7 ----------------------------------------------------------------------
layer7 :- in([rq7a, rq7b, rq7c]).
rule(claim, [duty]) :- layer7.
rule(duty, [claim]) :- layer7.
exception(passed, demand_unmet) :- layer7.
rule(demand_unmet, [demanded, contract_missing]) :- layer7.
exception(demand_unmet, improper) :- layer7.
rule(improper, [limited, unrequired]) :- layer7.
rule(limited, []) :- in(rq7b).   % the scope principle; listed first, see below
rule(limited, [claim]) :- layer7.
% Clause order matters, as in Prolog: with the scope principle listed after
% the BGH rule, 7b loops as well (recorded in ../comparison.org).
fact(demanded) :- layer7.
fact(contract_missing) :- layer7.
fact(unrequired) :- layer7.

% --- the interpreter -----------------------------------------------------------
holds(G, D) :- D > 60, throw(loop(G)).
holds(G, _) :- fact(G).
holds(G, D) :- D1 is D + 1, rule(G, Body), all(Body, D1), \+ excepted(G, D1).
excepted(G, D) :- exception(G, E), holds(E, D).
all([], _).
all([G|Gs], D) :- holds(G, D), all(Gs, D).

target(frag_g, registrable).
target(S, duty) :- member(S, [rq4a, rq4b, rq4c, rq4d, rq6a, rq6b, rq6c, rq6d]).
target(S, claim) :- member(S, [rq7a, rq7b, rq7c]).

answer(S) :-
    retractall(scenario(_)), assertz(scenario(S)), target(S, T),
    catch((holds(T, 0) -> R = yes ; R = no), loop(_), R = loop),
    display(S), display(' '), display(T), display(': '), display(R), nl.

main([S]) :- !, atom_codes(A, S), answer(A).
main(_) :-
    member(S, [frag_g, rq4a, rq4b, rq4c, rq4d, rq6a, rq6b, rq6c, rq6d, rq7a, rq7b, rq7c]),
    answer(S), fail.
main(_).
