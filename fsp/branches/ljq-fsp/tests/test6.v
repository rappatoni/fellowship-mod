(* Coq script *) 
Parameter A B C D : Prop. 
Goal ((A \/ B) -> (((A -> C) /\  (B -> C)) -> (D -> (C /\  (D \/ A))))). 
Proof (fun (ab:(A \/ B)) => (fun (H:((A -> C) /\  (B -> C))) => (fun (d:D) => (match ab with or_introl x1 => let a:A := x1 in (match H with (conj ac bc) => let c:C := (ac a) in (conj c (or_introl _ d)) end) | or_intror x1 => let a:B := x1 in (match H with (conj ac bc) => let c:C := (bc a) in (conj c (or_introl _ d)) end) end)))).