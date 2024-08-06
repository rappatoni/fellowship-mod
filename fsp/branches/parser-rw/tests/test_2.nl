Parameters A,B,C: bool
we need to prove A∨B⇒(B⇒C)⇒A∨C
  assume A∨B (ab)
  assume B⇒C (bc)
  we need to prove A∨C
    by ab
    by cases: 
      first case:
        by case hypothesis
        we proved A (a)
        by a
        trivial
      done
      second case:
        by case hypothesis
        we proved B (b)
        by bc
        and by b
        we proved C (c)
        by c
        trivial
      done
done
