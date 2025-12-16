(Ok, with that clarified let's start working on the reduction system. We have already implemented the reduction rules, but currently we have a bunch of special cases to )partially) take care of undercut, support, etc. We will replace this by global *evaluation strategy* which we shall call *call-by-onus*. The call-by-onus strategy is defined as follows:

1.) Apply lamda, admal, cons, sonc and ordinray mu/mutilde reduction rules in an arbitrary order (in practice just in the order which we have implemented).
2.) In case of a critical pair <mu alpha.t || mu'x.t'> reduce to <mu alpha.t || mu'x.n> where t' --->* n if the onus is on t' and reduce to <mu alpha.m || mu'x.t'> where t --->* m if the onus is on t.
3.) Else (i.e. if the onus is on neither t nor t') fall back on one of three strategies (default: don't simplify): call-by-name (reduce to t[mu'x.t'/alpha]) call-by-value (reduce to t'[mu alpha.t/x]) or don't simplify.)


Welcome back. Please take a look at the notes and todos. We will now start working on the reduction system. We have already implemented the reduction rules, but currently we have a bunch of special cases to (partially) take care of undercut, support, etc. We will replace this by global *evaluation strategy* which we shall call *call-by-onus*. The Call-by-onus strategy unifies the special cases of the rules and coloring and is defined as follows:

1.) For any given redex mu alpha.<t||t'>/mu'x.<t||t'>, reduce first the subterm which has the *onus*.
2.) Else (i.e. if the onus is on none or two or more subterms) fall back on one of three strategies (default: don't simplify): call-by-name (reduce to t[mu'x.t'/alpha]) call-by-value (reduce to t'[mu alpha.t/x]) or don't simplify.

Finally, we define *onus*: given a critical pair <mu alpha.t || mu'x.t'>, the onus is on t' if 

Welcome back. Please take a look at the notes and todos. We will now start working on the reduction system. We have already implemented the reduction rules, but currently we have a bunch of special cases to )partially) take care of undercut, support, etc. We will replace this by global *evaluation strategy* which we shall call *call-by-onus*. The call-by-onus strategy is defined as follows:

1.) For any given redex mu alpha.<t||t'>/mu'x.<t||t'>, reduce first the subterm which has the *onus*.
2.) Else (i.e. if the onus is on two or more subterms) fall back on one of three strategies (default: don't simplify): call-by-name (reduce to t[mu'x.t'/alpha]) call-by-value (reduce to t'[mu alpha.t/x]) or don't simplify.

Next we define *onus*. For this purpose

Welcome back. Please take a look at the notes and todos. We will now start working on the reduction system. We have already implemented the reduction rules, but currently we have a bunch of special cases to )partially) take care of undercut, support, etc. We will replace this by global *evaluation strategy* which we shall call *call-by-onus*. The call-by-onus strategy is defined as follows:

1.) For any given redex mu alpha.<t||t'>/mu'x.<t||t'>, reduce first the subterm which has the *onus*.
2.) Else (i.e. if the onus is on none or two or more subterms) fall back on one of three strategies (default: don't simplify): call-by-name (reduce to t[mu'x.t'/alpha]) call-by-value (reduce to t'[mu alpha.t/x]) or don't simplify.

Next we define *onus*. For this purpose we distinguish the following grammatical subcategories (we denote arbitrary closed terms by t and write t' for a fully simplified term t; free variables are denoted by x and alpha, respectively): 

Alternative proofs ap ::= mu _.<t'||alpha> | mu'_.<t'||alpha> where alpha is a free variable in ap. Defaults where t' is a Goal are denoted by d.

Alternative refutations ar ::= mu'_.<x||t'> | mu_.<x||t'> where x is a free variable in ar. Defaults where t' is a Laog are denoted by d.

Exceptions e ::= mu_.<t'||t> | mu'_.<t||t'> where t' is not itself an exception **or a goal/laog**.

All other mu/mu'-terms are denoted by m. Other terms including other mu/mu'-terms are denoted by o. Other non-mu/mu' terms by !m

Next we distinguish the following evaluation situations:

Right-shift ::= <t||mu'_.c> | where c can still step.

Left-shift ::= <mu_.c||t>  where c can still step

Call-by-value ::= <ap||e> | <e||mu'_.<e||t>> | <o||e> | <d||ap> | <d||o> | <d||ar> | <ar||e> | <!m||m> | <sonc||admal>

Call-by-name ::= <e||ap> | <mu_.<t||e>||e> | <e||o> | <ap||d> | <o||d> | <ar||d> | <e||ar> | <m||!m> | <lamda||cons>

Fallback ::= <m||m> | <ap||ap> | <ar||ar>

(This can be simplified by assigning evaluation priorities: mu_.<t||e> = mu'_.<e||t> > e > o >(?) ap = ar > d )

Some of these should never occur by construction and should raise a warning:

- <d||o>, <o||d>: ~~generic other terms should never be contraposed to defaults by our argument construction rules.~~ This is in fact fine and arises from unreduced exceptions (which are just of shape o).
- <ap||d>, <d||ar>: our convention is that in support operations, the default should be in the head position and supporters in the tail.
- <ap||ar>, <ar||ap> should never occur

Evaluation proceeds as follows:

Right-shift ---> continue by reducing c
Left-shift ---> continue by reducing c
Call-by-name ---> apply mu/lamda reduction rule
Call-by-value ---> apply mu'/admal reduction rule
Fallback ---> no side, has the onus, apply fallback strategy: call-by-name (reduce to t[mu'x.t'/alpha]) call-by-value (reduce to t'[mu alpha.t/x]) or don't simplify (default: don't simplify)

Finally, there is another parameter:

Skeptical/Credulous: in skeptical mode apply the system as is; in credulous mode, move <ap||e> to call-by-name and <e||ar> to call-by-value.

Before we get started on implementation, please analyse the specification I provided and compare it to the existing implementation of the reduction system. Raise any points of difference with me as well as anything that appears incorrect or missing to you.

Other notes: 

Note: the case <e||e> is interesting. This occurs when an exception occurs during the bubbling up (handling) of another exception. There are the following subcases: <e||mu'_.<e|t>>, where e encountered an exception, is in the process of bubbling up and meets a rebut; the dual <mu_.<t||e>||e>; <e||mu'_.<t1|t2>> where e is in the process of bubbling up only to encounter a separate exception from a different undercut; its dual <mu_.<t1||t2>||e>.

Lamda/Admal rules:

<lamda x.t || v*E> ---> < v ||mu'beta.<t||E>>
<lamda x.t || d*E> ---> < d ||mu'beta.<t||E>>

Default reduction rules (experimental):

mu alpha.<d||t> ---> mu alpha.<d||d_naf> (negation as failure to prove)
mu'x.<t||d> ---> mu'x.<d_asc||d> (assertion as failure to refute)

(Add these rules as experimental)

In call-by-name discipline the onus is now on d and then the computation should abort here (corresponds to negation-by-failure); in call-by-value discipline the computation can continue as the onus is on mu'beta.<t||E> and d's evaluation can be delayed.

Definition of a default: may not have the onus unless affine!


With undercut, too, we have to check alternate branches for defenders:



        
       A <--- Undercutter u
 \    /
t:A  / 
   \/ 
   
t has to be grafted on u.

Or we make sure during argument execution, that the argument is *argumentatively closed* i.e. every proposition is supported/attacked by every suitable subargument of the argument. Would this lead to cycles? No, because of capture during grafting.

Or we make sure during argument construction, that every subargument whose conclusion is a default automatically becomes a supporter. But what if the set of defaults changes? Then argumentative closure will become stale and has to be recomputed.

All this would be much easier if we had *one* node for A (graph instead of tree). Hypersequents? Future work.

The fundamentall problem is: abstractly, we have a graph structure but concrete debates have tree structure (sequence of rule applications and argument operations). If we constrain argument and debate construction too much (e.g. requiring the grafting of t on u in the above example), we make it impossible to model actual concrete debates. So ideally, one would implicitly construct the abstract structure (service) while faithfully matching the temporal structure of the current debate.

Hence: make computation of closure an optional service.
