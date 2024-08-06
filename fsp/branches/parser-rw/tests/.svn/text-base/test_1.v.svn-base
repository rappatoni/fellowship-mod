(* Coq script *) 
Parameter A B C D : Prop. 
Theorem t1 : (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))). 
Proof (fun (a:A) => (fun (ab:(A -> B)) => (fun (ac:(A -> C)) => (fun (bcd:(B -> (C -> D))) => ((bcd (ab a)) (ac a)))))). 
Theorem t2 : (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))). 
Proof (fun (a:A) => (fun (ab:(A -> B)) => (fun (ac:(A -> C)) => (fun (bcd:(B -> (C -> D))) => (let a2:A := a in (let b:B := (ab a2) in (let c:C := (ac a2) in ((bcd b) c)))))))). 
