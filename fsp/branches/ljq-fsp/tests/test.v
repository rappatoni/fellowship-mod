(* Coq script *) 
Parameter A B C D : Prop. 
Goal (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))). 
Proof (fun (a:A) => (fun (ab:(A -> B)) => (fun (ac:(A -> C)) => (fun (bcd:(B -> (C -> D))) => let a2:A := a in let b:B := (ab a2) in let c:C := (ac a2) in ((bcd b) c))))).