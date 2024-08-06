(* Coq script *) 
Require Import Classical. 
Implicit Arguments classic [P]. 
Parameter A B : Prop. 
Theorem t1 : ((~B -> ~A) -> (A -> B)). 
Proof (fun (mt:(~B -> ~A)) => (fun (a:A) => (match classic with or_introl x1 => (let x2:B := x1 in x2) | or_intror x1 => (let b:~B := x1 in (let H1:~A := (mt (fun (H2:B) => (let x2:B := H2 in (match (b x2) with end)))) in (match (H1 a) with end))) end))). 
