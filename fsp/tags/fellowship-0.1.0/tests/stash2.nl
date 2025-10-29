Parameters A,B: bool
Axiom ax: A
Axiom stashed: A⇒B
we need to prove A⇒(A⇒B)⇒A
  assume A (pop)
  assume A⇒B (stashed)
  we need to prove A
    by stashed
    and by pop
    we proved B (affine)
    by pop
  done
done
