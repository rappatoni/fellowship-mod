(* Coq script *) 
Parameter A B : Prop. 
Theorem t1 : (~(A /\ ~B) -> (~B -> ~A)). 
Proof (fun (nab:~(A /\ ~B)) => (fun (nb:~B) => (fun (H2:A) => (let a:A := H2 in (let H1:~(A /\ ~B) := nab in (match (H1 (conj a (fun x1 => (let b:B := x1 in (let H1:~B := nb in (match (H1 b) with end)))))) with end)))))). 
