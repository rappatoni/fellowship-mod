(* Coq script *) 
Parameter N : Set. 
Parameter O : N. 
Parameter S : (N -> N). 
Parameter Even : (N -> Prop). 
Parameter Odd : (N -> Prop). 
Parameter Eq : (N -> (N -> Prop)). 
Parameter Plus : (N -> (N -> N)). 
Axiom EO : (Even O). 
Axiom OS : (forall n:N, ((Even n) -> (Odd (S n)))). 
Axiom ES : (forall n:N, ((Odd n) -> (Even (S n)))). 
Axiom sym : (forall n m:N, (((Eq n) m) -> ((Eq m) n))). 
Axiom PO : (forall n:N, ((Eq ((Plus O) n)) n)). 
Axiom PS : (forall n m:N, ((Eq ((Plus (S n)) m)) (S ((Plus n) m)))). 
Axiom RewriteE : (forall n m:N, (((Eq n) m) -> ((Even n) -> (Even m)))). 
Axiom RewriteO : (forall n m:N, (((Eq n) m) -> ((Odd n) -> (Odd m)))). 
Axiom Ind : ((Even ((Plus O) O)) -> ((forall n:N, ((Even ((Plus n) n)) -> (Even ((Plus (S n)) (S n))))) -> (forall n:N, (Even ((Plus n) n))))). 
Axiom Plussym : (forall n m:N, ((Eq ((Plus n) m)) ((Plus m) n))). 
Theorem even_plus : (forall n:N, (Even ((Plus n) n))). 
Proof ((Ind (let eq:((Eq ((Plus O) O)) O) := (PO O) in ((((RewriteE O) ((Plus O) O)) (((sym ((Plus O) O)) O) eq)) EO))) (fun (n:N) => (fun (IH:(Even ((Plus n) n))) => (let eq:((Eq ((Plus (S n)) (S n))) (S ((Plus n) (S n)))) := ((PS n) (S n)) in (let eqs:((Eq (S ((Plus n) (S n)))) ((Plus (S n)) (S n))) := (((sym ((Plus (S n)) (S n))) (S ((Plus n) (S n)))) eq) in ((((RewriteE (S ((Plus n) (S n)))) ((Plus (S n)) (S n))) eqs) ((ES ((Plus n) (S n))) (let eq2:((Eq ((Plus (S n)) n)) ((Plus n) (S n))) := ((Plussym (S n)) n) in ((((RewriteO ((Plus (S n)) n)) ((Plus n) (S n))) eq2) (let H:(Odd (S ((Plus n) n))) := ((OS ((Plus n) n)) IH) in ((((RewriteO (S ((Plus n) n))) ((Plus (S n)) n)) (((sym ((Plus (S n)) n)) (S ((Plus n) n))) ((PS n) n))) H))))))))))). 
