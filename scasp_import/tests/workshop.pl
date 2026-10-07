
execution(X):-proof_search(X,Y).
proof(Y):-proof_search(X,Y).
program(Y):-proof(Y).
proof(tweety_is_a_bird).
proof_search(prog, tweety_is_a_bird).
?- program(X).