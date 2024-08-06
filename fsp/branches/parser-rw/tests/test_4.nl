Parameters A,B: bool
we need to prove A⇒¬A⇒B
  assume A (a)
  assume ¬A (na)
  we need to prove B
    by na
    we proved ¬A (toimprove1)
    by toimprove1
    and by a
  absurd
done
Parameters C,D: bool
we need to prove A∨B⇒(A⇒C)∧(B⇒C)⇒D⇒C∧(D∨A)
  assume A∨B (ab)
  assume (A⇒C)∧(B⇒C) (H)
  assume D (d)
  we need to prove C∧(D∨A)
    by ab
    by cases: 
      first case:
        by case hypothesis
        we proved A (a)
        by H
        we proved A⇒C (ac) and B⇒C (bc)
        we need to prove C
          by ac
          and by a
        done
        we proved C (c)
        by c
        and
        by d
        trivial
      done
      second case:
        by case hypothesis
        we proved B (a)
        by H
        we proved A⇒C (ac) and B⇒C (bc)
        we need to prove C
          by bc
          and by a
        done
        we proved C (c)
        by c
        and
        by d
        trivial
      done
done
