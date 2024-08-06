Parameter N: type
Parameter O: N
Parameter S: N→N
Parameter Even: N→bool
Parameter Odd: N→bool
Axiom EO: Even O
Axiom OS: ∀n:N.Even n⇒Odd (S n)
Axiom ES: ∀n:N.Odd n⇒Even (S n)
we need to prove Even (S (S O))
  we need to prove Even (S (S O))
    by ES
    and we need to prove Odd (S O)
      by OS
      and we need to prove Even O
        by EO
      done
    done
  done
done
