(* Coq script *) 
Parameter N : Set. 
Parameter P : (N -> (N -> Prop)). 
Goal ((exists y:N, (forall x:N, ((P x) y))) -> (forall x:N, (exists y:N, ((P x) y)))). 
Proof (fun (H:(exists y:N, (forall x:N, ((P x) y)))) => (match H with (ex_intro y x1) => let K:(forall x:N, ((P x) y)) := x1 in (fun (x:N) => (ex_intro (fun y => ((P x) y)) y (K x))) end)).