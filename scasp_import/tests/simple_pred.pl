bird(tweety).
bird(clumsy).
bird_list([X,Y]):-bird(X),bird(Y).
-bird_list([tweety,clumsy]).

?-bird_list([X,Y]).