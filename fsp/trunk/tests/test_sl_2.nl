Parameters A,B: bool
we need to prove ¬(A-B)⇒¬B⇒¬A
  assume ¬(A-B) (nab)
  assume ¬B (nb)
  we need to prove ¬A
    assume A (H2)
    we need to prove ⊥
      by H2
      we proved A (a)
      by nab
      we proved ¬(A-B) (H1)
      by H1
      and <???
        we proved B (b)
        by nb
        we proved ¬B (H1)
        by H1
        and by b
      absurd
        by a
      ???>
    absurd
  done
done
