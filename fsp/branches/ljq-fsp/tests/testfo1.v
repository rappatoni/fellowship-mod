(* Coq script *) 
Parameter N : Set. 
Parameter O : N. 
Parameter S : (N -> N). 
Parameter Even : (N -> Prop). 
Parameter Odd : (N -> Prop). 
Goal ((Even O) -> ((forall n:N, ((Even n) -> (Odd (S n)))) -> ((forall n:N, ((Odd n) -> (Even (S n)))) -> (Even (S (S O)))))). 
Proof (fun (EO:(Even O)) => (fun (OS:(forall n:N, ((Even n) -> (Odd (S n))))) => (fun (ES:(forall n:N, ((Odd n) -> (Even (S n))))) => ((ES (S O)) ((OS O) EO))))).