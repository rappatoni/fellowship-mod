Parameter A: bool
!!! The proof is classical; converted to the following LJ + EM proof:
μthesis:A∨¬A.<μx1:A∨¬A.<classic||(μ'x2:A∨¬A.<x2||x1>,μ'th:¬(A∨¬A).<R(μH1:¬A.<λH2:A.μH3:⊥.<H2||μ'a:A.<L(a)||μ'x2:A∨¬A.<th||x2*_F_>>>||H1>)||μ'x2:A∨¬A.<th||x2*_F_>>)>||thesis>
we need to prove A∨¬A
  we need to prove A∨¬A
    by classic
    by cases: 
      first case:
        by case hypothesis
        we proved A∨¬A (x2)
        by x2
      done
      second case:
        by case hypothesis
        we proved ¬(A∨¬A) (th)
        we need to prove ¬A
          assume A (H2)
          we need to prove ⊥
            by H2
            we proved A (a)
            by a
            trivial
            we proved A∨¬A (x2)
            by th
            and by x2
          absurd
        done
        trivial
        we proved A∨¬A (x2)
        by th
        and by x2
      absurd
done
