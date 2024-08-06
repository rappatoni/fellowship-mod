(* Coq script *) 
Parameter N : Set. 
Parameter O : N. 
Parameter S : (N -> N). 
Parameter Even : (N -> Prop). 
Parameter Odd : (N -> Prop). 
Axiom EO : (Even O). 
Axiom OS : (forall n:N, ((Even n) -> (Odd (S n)))). 
Axiom ES : (forall n:N, ((Odd n) -> (Even (S n)))). 
Theorem even_2 : (Even (S (S O))). 
Proof ((ES (S O)) ((OS O) EO)). 
