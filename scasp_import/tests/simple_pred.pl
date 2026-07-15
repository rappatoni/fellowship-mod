#pred bird(X) :: '@X ist ein \sn{Vogel}'.
#pred bird_list(Y) :: '@Y ist eine \sn{Liste} von \sr{Vogel}{Vögeln.}'.
bird(tweety).
bird(clumsy).
bird_list([X,Y]):-bird(X),bird(Y).
%-bird_list([tweety,clumsy]).

?-bird_list([tweety,clumsy]).