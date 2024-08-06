Parameters A,B: bool
!!! The proof is classical; converted to the following LJ + EM proof:
μthesis:((A-B)⇒¬(¬B⇒¬A))∧(¬(¬B⇒¬A)⇒(A-B)).<(λab:A-B.μH1:¬(¬B⇒¬A).<λH2:¬B⇒¬A.μH3:⊥.<H2||μH1:¬B.<λH2:B.μH3:⊥.<H2||μ'b:B.<ab||λb2:B.μ'a:A.<b||μ'x1:B.<b2||x1*_F_>>>>||H1>*μ'H1:¬A.<H1||μx1:A.<classic||(μ'x2:A.<x2||x1>,μ'a:¬A.<ab||λb:B.μ'x2:A.<a||x2*_F_>>)>*_F_>>||H1>,λmt:¬(¬B⇒¬A).μ'b:B.<mt||μ'H1:¬(¬B⇒¬A).<H1||λnb:¬B.μna:¬A.<nb||μ'H1:¬B.<H1||b*_F_>>*_F_>>*μx1:A.<classic||(μ'x2:A.<x2||x1>,μ'a:¬A.<mt||μ'H1:¬(¬B⇒¬A).<H1||λnb:¬B.μH1:¬A.<λH2:A.μH3:⊥.<H2||μ'x2:A.<a||x2*_F_>>||H1>*_F_>>)>)||thesis>
we need to prove ((A-B)⇒¬(¬B⇒¬A))∧(¬(¬B⇒¬A)⇒(A-B))
  assume A-B (ab)
  we need to prove ¬(¬B⇒¬A)
    assume ¬B⇒¬A (H2)
    we need to prove ⊥
      by H2
      and we need to prove ¬B
        assume B (H2)
        we need to prove ⊥
          by H2
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
      we proved ¬A (H1)
      by H1
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
    we proved ¬(¬B⇒¬A) (H1)
    by H1
    and assume ¬B (nb)
    we need to prove ¬A
      by nb
      we proved ¬B (H1)
      by H1
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
          we proved ¬(¬B⇒¬A) (H1)
          by H1
          and assume ¬B (nb)
          we need to prove ¬A
            assume A (H2)
            we need to prove ⊥
              by H2
              we proved A (x2)
              by a
              and by x2
            absurd
          done
        absurd
  ???>
done
