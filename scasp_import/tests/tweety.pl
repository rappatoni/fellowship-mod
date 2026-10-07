#pred bird(X) :: '@(X) ist ein Vogel.'.
fly(X):-bird(X), not abnormal(X).
bird(X):-penguin(X).
abnormal(X):-penguin(X).
bird(tweety).

?- fly(tweety).