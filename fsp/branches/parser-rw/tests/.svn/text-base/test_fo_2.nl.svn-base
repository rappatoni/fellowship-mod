Parameter N: type
Parameter O: N
Parameter S: N→N
Parameter Even: N→bool
Parameter Odd: N→bool
Parameter Eq: N→N→bool
Parameter Plus: N→N→N
Axiom EO: Even O
Axiom OS: ∀n:N.Even n⇒Odd (S n)
Axiom ES: ∀n:N.Odd n⇒Even (S n)
Axiom sym: ∀n,m:N.Eq n m⇒Eq m n
Axiom PO: ∀n:N.Eq (Plus O n) n
Axiom PS: ∀n,m:N.Eq (Plus (S n) m) (S (Plus n m))
Axiom RewriteE: ∀n,m:N.Eq n m⇒Even n⇒Even m
Axiom RewriteO: ∀n,m:N.Eq n m⇒Odd n⇒Odd m
Axiom Ind: Even (Plus O O)⇒∀n:N.Even (Plus n n)⇒Even (Plus (S n) (S n))⇒∀n:N.Even (Plus n n)
Axiom Plussym: ∀n,m:N.Eq (Plus n m) (Plus m n)
we need to prove ∀n:N.Even (Plus n n)
  we need to prove ∀n:N.Even (Plus n n)
    by Ind
    and we need to prove Even (Plus O O)
      by PO
      we proved Eq (Plus O O) O (eq)
      by RewriteE
      and we need to prove Eq O (Plus O O)
        by sym
        and by eq
      done
      and we need to prove Even O
        by EO
      done
    done
    and consider an arbitrary but fixed n of type N
    assume Even (Plus n n) (IH)
    we need to prove Even (Plus (S n) (S n))
      by PS
      we proved Eq (Plus (S n) (S n)) (S (Plus n (S n))) (eq)
      by sym
      and by eq
      we proved Eq (S (Plus n (S n))) (Plus (S n) (S n)) (eqs)
      by RewriteE
      and by eqs
      and we need to prove Even (S (Plus n (S n)))
        by ES
        and we need to prove Odd (Plus n (S n))
          by Plussym
          we proved Eq (Plus (S n) n) (Plus n (S n)) (eq2)
          by RewriteO
          and by eq2
          and we need to prove Odd (Plus (S n) n)
            by OS
            and by IH
            we proved Odd (S (Plus n n)) (H)
            by RewriteO
            and we need to prove Eq (S (Plus n n)) (Plus (S n) n)
              by sym
              and we need to prove Eq (Plus (S n) n) (S (Plus n n))
                by PS
              done
            done
            and by H
          done
        done
      done
    done
  done
done
