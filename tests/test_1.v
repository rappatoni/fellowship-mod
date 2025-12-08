(* Coq script *) 
Parameter A B C D : Prop. 
Theorem t1 : (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))). 
 Proof. 
 (*elim a*)
 cut (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))). 
 firstorder. 
 (*elim ab*)
 cut (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))). 
 firstorder. 
 (*elim ac*)
 cut (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))). 
 firstorder. 
 (*elim bcd*)
 cut (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))). 
 firstorder. 
 (*cut B⇒C⇒D,d*)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (B -> (C -> D)))))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> ((B -> (C -> D)) -> D)))))). 
 firstorder. 
 (*axiom *)
 cut (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> ((B -> (C -> D)) -> D))))). 
 firstorder. 
 (*elim *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> B)))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> ((C -> D) -> D)))))). 
 firstorder. 
 (*cut A⇒B,b*)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> B))))) /\  ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> ((A -> B) -> B))))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> ((C -> D) -> D))))))). 
 firstorder. 
 (*axiom *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> ((A -> B) -> B))))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> ((C -> D) -> D)))))). 
 firstorder. 
 (*elim *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> A)))) /\  ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (B -> B))))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> ((C -> D) -> D))))))). 
 firstorder. 
 (*axiom *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (B -> B))))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> ((C -> D) -> D)))))). 
 firstorder. 
 (*axiom *)
 cut (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> ((C -> D) -> D))))). 
 firstorder. 
 (*elim *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> C)))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (D -> D)))))). 
 firstorder. 
 (*cut A⇒C,c*)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> C))))) /\  ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> ((A -> C) -> C))))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (D -> D))))))). 
 firstorder. 
 (*axiom *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> ((A -> C) -> C))))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (D -> D)))))). 
 firstorder. 
 (*elim *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> A)))) /\  ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (C -> C))))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (D -> D))))))). 
 firstorder. 
 (*axiom *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (C -> C))))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (D -> D)))))). 
 firstorder. 
 (*axiom *)
 cut (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (D -> D))))). 
 firstorder. 
 firstorder. 
Qed. 
Theorem t2 : ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D))))). 
 Proof. 
 (*elim a*)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D))))). 
 firstorder. 
 (*elim ab*)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D))))). 
 firstorder. 
 (*elim ac*)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D))))). 
 firstorder. 
 (*elim bcd*)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D))))). 
 firstorder. 
 (*cut A,d*)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> A)))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> D))))))). 
 firstorder. 
 (*axiom *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> D)))))). 
 firstorder. 
 (*cut A⇒B,a2*)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (A -> B)))))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> ((A -> B) -> D)))))))). 
 firstorder. 
 (*axiom *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> ((A -> B) -> D))))))). 
 firstorder. 
 (*elim *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> A))))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> D)))))))). 
 firstorder. 
 (*axiom *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> D))))))). 
 firstorder. 
 (*cut A⇒C,b*)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> (A -> C))))))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> ((A -> C) -> D))))))))). 
 firstorder. 
 (*axiom *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> ((A -> C) -> D)))))))). 
 firstorder. 
 (*elim *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> A)))))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> (C -> D))))))))). 
 firstorder. 
 (*axiom *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> (C -> D)))))))). 
 firstorder. 
 (*cut B⇒C⇒D,c*)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> (C -> (B -> (C -> D))))))))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> (C -> ((B -> (C -> D)) -> D)))))))))). 
 firstorder. 
 (*axiom *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> (C -> ((B -> (C -> D)) -> D))))))))). 
 firstorder. 
 (*elim *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> (C -> B))))))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> (C -> ((C -> D) -> D)))))))))). 
 firstorder. 
 (*axiom *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> (C -> ((C -> D) -> D))))))))). 
 firstorder. 
 (*elim *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> (C -> C))))))) /\  (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> (C -> (D -> D)))))))))). 
 firstorder. 
 (*axiom *)
 cut ((A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> D)))) -> (A -> ((A -> B) -> ((A -> C) -> ((B -> (C -> D)) -> (A -> (B -> (C -> (D -> D))))))))). 
 firstorder. 
 firstorder. 
Qed. 
