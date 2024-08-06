(* Coq script *) 
Parameter A B : Prop. 
Goal (~(A /\ ~B) -> (~B -> ~A)). 
Proof (fun (nab:~(A /\ ~B)) => (fun (nb:~B) => (fun (toimprove2:A) => let a:A := toimprove2 in let toimprove4:~(A /\ ~B) := nab in (match (toimprove4 (conj a (fun x1 => let b:B := x1 in let toimprove5:~B := nb in (match (toimprove5 b) with end)))) with end)))).