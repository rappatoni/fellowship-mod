Parameters A,B: bool
!!! The proof is classical; converted to the following LJ + EM proof:
μthesis:((A-B)⇒¬(¬B⇒¬A))∧(¬(¬B⇒¬A)⇒(A-B)).<(λab:A-B.μtoimprove1:¬(¬B⇒¬A).<λtoimprove2:¬B⇒¬A.μtoimprove3:⊥.<toimprove2||μtoimprove4:¬B.<λtoimprove5:B.μtoimprove6:⊥.<toimprove5||μ'b:B.<ab||λb2:B.μ'a:A.<b||μ'x1:B.<b2||x1*_F_>>>>||toimprove4>*μ'toimprove7:¬A.<toimprove7||μx1:A.<classic||(μ'x2:A.<x2||x1>,μ'a:¬A.<ab||λb:B.μ'x2:A.<a||x2*_F_>>)>*_F_>>||toimprove1>,λmt:¬(¬B⇒¬A).μ'b:B.<mt||μ'toimprove8:¬(¬B⇒¬A).<toimprove8||λnb:¬B.μna:¬A.<nb||μ'toimprove9:¬B.<toimprove9||b*_F_>>*_F_>>*μx1:A.<classic||(μ'x2:A.<x2||x1>,μ'a:¬A.<mt||μ'toimprove10:¬(¬B⇒¬A).<toimprove10||λnb:¬B.μtoimprove11:¬A.<λtoimprove12:A.μtoimprove13:⊥.<toimprove12||μ'x2:A.<a||x2*_F_>>||toimprove11>*_F_>>)>)||thesis>
we need to prove ((A-B)⇒¬(¬B⇒¬A))∧(¬(¬B⇒¬A)⇒(A-B))
  assume A-B (ab)
  we need to prove ¬(¬B⇒¬A)
    assume ¬B⇒¬A (toimprove2)
    we need to prove ⊥
      by toimprove2
      and we need to prove ¬B
        assume B (toimprove5)
        we need to prove ⊥
          by toimprove5
          we proved B (b)
          by ab
          <???B(b2)
            we proved A (a)
            by b
            we proved B (x1)
            by b2
            and by x1
          absurd
          
      done
      we proved ¬A (toimprove7)
      by toimprove7
      and we need to prove A
        by classic
        by cases: 
          first case:
            by case hypothesis
            we proved A (x2)
            by x2
          done
          second case:
            by case hypothesis
            we proved ¬A (a)
            by ab
            <???B(b)
              we proved A (x2)
              by a
              and by x2
            absurd
            
    absurd
  done
  and
  assume ¬(¬B⇒¬A) (mt)
  <???
    we proved B (b)
    by mt
    we proved ¬(¬B⇒¬A) (toimprove8)
    by toimprove8
    and assume ¬B (nb)
    we need to prove ¬A
      by nb
      we proved ¬B (toimprove9)
      by toimprove9
      and by b
    absurd
  absurd
    we need to prove A
      by classic
      by cases: 
        first case:
          by case hypothesis
          we proved A (x2)
          by x2
        done
        second case:
          by case hypothesis
          we proved ¬A (a)
          by mt
          we proved ¬(¬B⇒¬A) (toimprove10)
          by toimprove10
          and assume ¬B (nb)
          we need to prove ¬A
            assume A (toimprove12)
            we need to prove ⊥
              by toimprove12
              we proved A (x2)
              by a
              and by x2
            absurd
          done
        absurd
  ???>
done
