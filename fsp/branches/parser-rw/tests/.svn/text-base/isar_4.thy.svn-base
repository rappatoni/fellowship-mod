

theorem foo: "Eq M (f M)"
proof - 
  {fix y1 :: A
   assume a2: "R y1 (f y1)"
   have "R y1 M" using Up and a2 by (rule mp) 
   hence "R (f y1) (f M)" using Incr by (rule mp) 
   thus "R y1 (f M)" using Trans and a2 by (rule mp)}
  hence a1: "R M (f M)" using Least by (rule mp) 

  have "R (f M) (f (f M))" using Incr and a1 by (rule mp)
  hence "R (f M) M" using Up by (rule mp)

  thus "Eq M (f M)" using a1 and Assym by (rule mp)
qed



theorem foo: "Eq M (f M)"
proof
  have a1: "R M (f M)" 
  proof (rule Least)
    fix y1 :: A
    assume a2: "R y1 (f y1)"
    have "R y1 M" using Up and a2 by (rule mp) 
    hence "R (f y1) (f M)" using Incr by (rule mp) 
    thus "R y1 (f M)" using Trans and a2 by (rule mp)
  qed

  hence a3: "R (f M) M" 
  proof (rule Up)
    show "R (f M) (f (f M))" using Incr and a1 by (rule mp)
  qed

  thus "Eq M (f M)" using a1 and Assym by (rule mp)
qed
