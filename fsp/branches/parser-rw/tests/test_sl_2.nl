Parameters A,B: bool
we need to prove ¬(A-B)⇒¬B⇒¬A
  assume ¬(A-B) (nab)
  assume ¬B (nb)
  we need to prove ¬A
    assume A (toimprove2)
    we need to prove ⊥
      by toimprove2
      we proved A (a)
      by nab
      we proved ¬(A-B) (toimprove4)
      by toimprove4
      and <???
        we proved B (b)
        by nb
        we proved ¬B (toimprove5)
        by toimprove5
        and by b
      absurd
        by a
      ???>
    absurd
  done
done
