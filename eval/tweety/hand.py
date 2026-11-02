"""The Tweety examples that are not rule files, written as theories for
tw2fspy's emitter: set attacks, attacks on attacks, bipolar supports,
claim-based frameworks, Besnard-Hunter deductive knowledge bases and
Tweety's simple PL logic. Writes hand/NAME.fspy.

Each entry: (name, source and Tweety's own result, rules). A rule is
(name, kind, body, head) as in tw2fspy.Theory; "d" defeasible (presumed),
"s" strict (declared).
"""
import os, sys
HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import tw2fspy as T

P = lambda x: ("+", x)
N = lambda x: ("-", x)
U = lambda r: ("-", ("rule", r))   # undercut: the refutation of rule r's statement

def pres(*xs):
    return [(f"arg_{x}", "defeasible", [], P(x)) for x in xs]

def att(b, a, kind="strict"):
    return (f"att_{b}_{a}", kind, [P(b)], N(a))

CASES = [
    ("setaf", """SetAfTheoryTest: arguments a, b, c, d; set attacks {b,d} -> a and
{a,c} -> c. Tweety: grounded {b,c,d}; preferred {b,c,d}.""",
     pres("a", "b", "c", "d") + [("att_bd_a", "strict", [P("b"), P("d")], N("a")),
                                 ("att_ac_c", "strict", [P("a"), P("c")], N("c"))]),
    ("extended", """ExtendedTheoryExample (Modgil's extended AF): a <-> b, c <-> d,
c attacks the attack (b,a), d attacks (a,b), e attacks (c,d). Every attack
is a presumed rule so that it can be undercut. Tweety: complete {b,d,e}.""",
     pres("a", "b", "c", "d", "e") + [att("a", "b", "defeasible"), att("b", "a", "defeasible"),
                                      att("c", "d", "defeasible"), att("d", "c", "defeasible"),
                                      ("u_c_ba", "defeasible", [P("c")], U("att_b_a")),
                                      ("u_d_ab", "defeasible", [P("d")], U("att_a_b")),
                                      ("u_e_cd", "defeasible", [P("e")], U("att_c_d"))]),
    ("extended_recursive", """RecursiveExtendedTheoryExample: as extended, and f attacks the
attack (e,(c,d)). Tweety: complete [{e,f}, {b,d,e,f}, {a,c,e,f}] (with the
accepted attacks).""",
     pres("a", "b", "c", "d", "e", "f") + [att("a", "b", "defeasible"), att("b", "a", "defeasible"),
                                           att("c", "d", "defeasible"), att("d", "c", "defeasible"),
                                           ("u_c_ba", "defeasible", [P("c")], U("att_b_a")),
                                           ("u_d_ab", "defeasible", [P("d")], U("att_a_b")),
                                           ("u_e_cd", "defeasible", [P("e")], U("att_c_d")),
                                           ("u_f_ecd", "defeasible", [P("f")], U("u_e_cd"))]),
    ("bipolar_deductive", """DeductiveBipolarArgumentation: arguments a, b, c, x; attack x -> c;
deductive supports a => x and b => c (accepting the supporter commits to
the supported). Supports as strict rules. Tweety: preferred {a,x}.""",
     pres("a", "b", "c", "x") + [("sup_a_x", "strict", [P("a")], P("x")),
                                 ("sup_b_c", "strict", [P("b")], P("c")), att("x", "c")]),
    ("bipolar_necessity", """NecessityBipolarArgumentation (Polberg & Oren 2014, modified):
attacks e -> a, b -> a, c -> d; necessity supports a => c (c needs a) and
b => b. c is argued only from a (an owed premise) by a presumed link.
Tweety: preferred {b,d,e}.""",
     pres("a", "b", "d", "e") + [("need_a_c", "defeasible", [P("a")], P("c")),
                                 att("e", "a"), att("b", "a"), att("c", "d")]),
    ("claims_collapsed", """ClaimBasedReasonersTest: arguments a, c with claim c1 and b, d with
claim c2; attacks a <-> b and c <-> d. Here an attack goes to the claim,
as AIDA's attacks do (a statement is the unit). Tweety: inherited complete
[{}, {c1}, {c2}, {c1,c2}], cl-preferred [{c1,c2}, {c1}, {c2}].""",
     pres("c1", "c2") + [att("c1", "c2"), att("c2", "c1")]),
    ("claims_premises", """ClaimBasedReasonersTest with each argument a presumed premise
(pa, pb, pc, pd) that strictly yields its claim, and the attacks between
the premises.""",
     pres("pa", "pb", "pc", "pd") + [("claim_a", "strict", [P("pa")], P("c1")), ("claim_c", "strict", [P("pc")], P("c1")),
                                     ("claim_b", "strict", [P("pb")], P("c2")), ("claim_d", "strict", [P("pd")], P("c2")),
                                     att("pa", "pb"), att("pb", "pa"), att("pc", "pd"), att("pd", "pc")]),
    ("deductive_kb", """DeductiveExample (Besnard & Hunter): KB {s, s -> h, f, f -> ~h, v, v -> ~h},
every formula a presumed premise; query h. Tweety's classical categorizer
with the simple accumulator: -2.0 (one argument for h, two undercuts).""",
     pres("s", "f", "v") + [("s_h", "defeasible", [P("s")], P("h")), ("f_nh", "defeasible", [P("f")], N("h")),
                            ("v_nh", "defeasible", [P("v")], N("h"))]),
    ("simple_pl_logic", """SimplePlLogicExample: a, t, (a,t -> b), (b -> c), (-> d), (d -> ~a),
(d -> ~c). Tweety's arguments: <{a,t,..},b>, <{a,t,..},c>, <{d,..},~a>,
<{d,..},~c>; attacks ~a -> b, ~a -> c, ~c <-> c. Grounded: ~a and ~c in,
b and c out. Facts a, t presumed; rules strict; d a strict fact.""",
     pres("a", "t") + [("r_b", "strict", [P("a"), P("t")], P("b")), ("r_c", "strict", [P("b")], P("c")),
                       ("r_d", "strict", [], P("d")), ("r_na", "strict", [P("d")], N("a")),
                       ("r_nc", "strict", [P("d")], N("c"))]),
]

def main():
    os.makedirs(os.path.join(HERE, "hand"), exist_ok=True)
    for name, doc, rules in CASES:
        th = T.Theory()
        for r in rules:
            th.add(*r)
        hdr = "\n".join("# " + l for l in doc.splitlines())
        open(os.path.join(HERE, "hand", name + ".fspy"), "w").write(T.emit(th, hdr))
        print(name)

if __name__ == "__main__":
    main()
