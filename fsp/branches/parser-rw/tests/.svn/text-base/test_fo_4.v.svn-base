(* Coq script *) 
Parameter A : Set. 
Parameter R : (A -> (A -> Prop)). 
Parameter Eq : (A -> (A -> Prop)). 
Axiom Assym : (forall x y:A, (((R x) y) -> (((R y) x) -> ((Eq x) y)))). 
Axiom Trans : (forall x y z:A, (((R x) y) -> (((R y) z) -> ((R x) z)))). 
Parameter f : (A -> A). 
Axiom Incr : (forall x y:A, (((R x) y) -> ((R (f x)) (f y)))). 
Parameter M : A. 
Axiom Up : (forall x:A, (((R x) (f x)) -> ((R x) M))). 
Axiom Least : (forall x:A, ((forall y:A, (((R y) (f y)) -> ((R y) x))) -> ((R M) x))). 
Theorem tarski_fp : ((Eq M) (f M)). 
Proof (let a1:((R M) (f M)) := ((Least (f M)) (fun (y1:A) => (fun (a2:((R y1) (f y1))) => (((((Trans y1) (f y1)) (f M)) a2) (((Incr y1) M) ((Up y1) a2)))))) in ((((Assym M) (f M)) a1) ((Up (f M)) (((Incr M) (f M)) a1)))). 
