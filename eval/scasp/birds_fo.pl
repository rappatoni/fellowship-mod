bird(tweety). penguin(opus).
bird(X) :- penguin(X).
ab(X) :- penguin(X).
flies(X) :- bird(X), not ab(X).
