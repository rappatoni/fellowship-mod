(* Coq script *) 
Require Import Classical. 
Implicit Arguments classic [P]. 
Parameter A : Prop. 
Theorem em : (A \/ ~A). 
Proof (match classic with or_introl x1 => (let x2:(A \/ ~A) := x1 in x2) | or_intror x1 => (let th:~(A \/ ~A) := x1 in (let x2:(A \/ ~A) := (or_intror _ (fun (H2:A) => (let a:A := H2 in (let x2:(A \/ ~A) := (or_introl _ a) in (match (th x2) with end))))) in (match (th x2) with end))) end). 
