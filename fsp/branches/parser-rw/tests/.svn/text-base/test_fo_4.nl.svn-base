Parameter A: type
Parameter R: A→A→bool
Parameter Eq: A→A→bool
Axiom Assym: ∀x,y:A.R x y⇒R y x⇒Eq x y
Axiom Trans: ∀x,y,z:A.R x y⇒R y z⇒R x z
Parameter f: A→A
Axiom Incr: ∀x,y:A.R x y⇒R (f x) (f y)
Parameter M: A
Axiom Up: ∀x:A.R x (f x)⇒R x M
Axiom Least: ∀x:A.∀y:A.R y (f y)⇒R y x⇒R M x
we need to prove Eq M (f M)
  we need to prove Eq M (f M)
    we need to prove R M (f M)
      by Least
      and consider an arbitrary but fixed y1 of type A
      assume R y1 (f y1) (a2)
      we need to prove R y1 (f M)
        by Trans
        and by a2
        and we need to prove R (f y1) (f M)
          by Incr
          and we need to prove R y1 M
            by Up
            and by a2
          done
        done
      done
    done
    we proved R M (f M) (a1)
    by Assym
    and by a1
    and we need to prove R (f M) M
      by Up
      and we need to prove R (f M) (f (f M))
        by Incr
        and by a1
      done
    done
  done
done
