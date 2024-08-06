Parameter N: type
Parameter P: N→N→bool
we need to prove ∃y:N.∀x:N.P x y⇒∀x:N.∃y:N.P x y
  assume ∃y:N.∀x:N.P x y (H)
  we need to prove ∀x:N.∃y:N.P x y
    by H
    let y be the element of type N that satisfies the property
    we proved ∀x:N.P x y (K)
    consider an arbitrary but fixed x of type N
    we need to prove P x y
      by K
    done
  done
done
