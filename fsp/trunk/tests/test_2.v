(* Coq script *) 
Parameter A B C : Prop. 
Theorem t1 : ((A \/ B) -> ((B -> C) -> (A \/ C))). 
Proof (fun (ab:(A \/ B)) => (fun (bc:(B -> C)) => (match ab with or_introl x1 => (let a:A := x1 in (or_introl _ a)) | or_intror x1 => (let b:B := x1 in (let c:C := (bc b) in (or_intror _ c))) end))). 
