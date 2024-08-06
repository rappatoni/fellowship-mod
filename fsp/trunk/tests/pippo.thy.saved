(* Isabelle script *)
theory pippo = Main :
lemma "(A --> ((A --> B) --> ((A --> C) --> ((B --> (C --> D)) --> D))))" 
proof -
(*show*) have "(A --> ((A --> B) --> ((A --> C) --> ((B --> (C --> D)) --> D))))" proof - {
assume a: "A"
assume ab: "(A --> B)"
assume ac: "(A --> C)"
assume bcd: "(B --> (C --> D))"
(*show*) have "D" proof - {
from bcd
moreover (*show*) have "B" proof - {
from ab show ?thesis using a apply - by ((drule mp)?,assumption)+
} qed

moreover (*show*) have "C" proof - {
from ac show ?thesis using a apply - by ((drule mp)?,assumption)+
} qed
 ultimately show ?thesis apply - by ((drule mp)?,assumption)+
} qed
} thus ?thesis apply - apply (rule impI)+ by ((drule mp)?,assumption)+ qed
thus ?thesis . qed
end
