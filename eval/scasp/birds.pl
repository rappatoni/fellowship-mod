bird(tweety). bird(sam). penguin(sam).
ab(X) :- penguin(X).
flies(X) :- bird(X), not ab(X).
