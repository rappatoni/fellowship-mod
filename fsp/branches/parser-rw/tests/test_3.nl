Parameters A,B: bool
!!! The proof is classical; converted to the following LJ + EM proof:
μthesis:(¬B⇒¬A)⇒A⇒B.<λmt:¬B⇒¬A.λa:A.μx1:B.<classic||(μ'x2:B.<x2||x1>,μ'b:¬B.<mt||μtoimprove1:¬B.<λtoimprove2:B.μtoimprove3:⊥.<toimprove2||μ'x2:B.<b||x2*_F_>>||toimprove1>*μ'toimprove4:¬A.<toimprove4||a*_F_>>)>||thesis>
we need to prove (¬B⇒¬A)⇒A⇒B
  assume ¬B⇒¬A (mt)
  assume A (a)
  we need to prove B
    by classic
    by cases: 
      first case:
        by case hypothesis
        we proved B (x2)
        by x2
      done
      second case:
        by case hypothesis
        we proved ¬B (b)
        by mt
        and we need to prove ¬B
          assume B (toimprove2)
          we need to prove ⊥
            by toimprove2
            we proved B (x2)
            by b
            and by x2
          absurd
        done
        we proved ¬A (toimprove4)
        by toimprove4
        and by a
      absurd
done
