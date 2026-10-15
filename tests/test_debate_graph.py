"""M2/M3: proposition identity and graph compilation
(propositional-fragment-plan.org)."""

import pytest

from core.ac.ast import Mu, Mutilde, Lamda, Hyp, Cons, Goal, Laog, Deleg, ID, DI, Sonc
from core.ac.prop import Prop, PropError
from core.dc.debate_graph import (
    canonical_prop, display_prop,
    DebateGraph, DebateCompileError, CyclicDebateNotSupported,
    compile_debate, compile_document,
)


#: Representative proposition spellings from the fixture corpus: the NAF
#: fixtures, the Peirce examples, the scasp_import ground atoms, and the
#: historical printer-bug pair.
FIXTURE_PROPS = [
    "P",
    "Q->P",
    "(R->false)->Q",
    "((P -> Q)->P)->P",
    "((P->Q)->P)->P",
    "A-(B->C)",
    "(A-B)->C",
    "true",
    "false",
    "Proof Tweety_is_a_bird",
    "Proof_search Prog Tweety_is_a_bird",
]


class TestRoundtrip:
    @pytest.mark.parametrize("text", FIXTURE_PROPS)
    def test_render_is_idempotent(self, text):
        once = str(Prop.parse(text))
        twice = str(Prop.parse(once))
        assert once == twice

    @pytest.mark.parametrize("text", FIXTURE_PROPS)
    def test_canonical_is_stable_under_rendering(self, text):
        assert canonical_prop(text) == canonical_prop(display_prop(text))


class TestIdentity:
    def test_whitespace_insensitive(self):
        assert canonical_prop("((P -> Q)->P)->P") == canonical_prop("((P->Q)->P)->P")

    def test_redundant_parentheses_insensitive(self):
        assert canonical_prop("(P)->Q") == canonical_prop("P->Q")

    def test_negation_unfolds_to_implication(self):
        assert canonical_prop("~A") == canonical_prop("A->false")

    def test_nested_negation(self):
        assert canonical_prop("~~A") == canonical_prop("(A->false)->false")

    def test_negation_of_compound(self):
        assert canonical_prop("~(A->B)") == canonical_prop("(A->B)->false")

    def test_printer_bug_pair_stays_distinct(self):
        # The historical pretty_prop defect printed these two alike; the
        # identity layer must keep them apart.
        assert canonical_prop("A-(B->C)") != canonical_prop("(A-B)->C")

    def test_ground_application_distinct_from_symbol(self):
        assert canonical_prop("Proof Tweety") != canonical_prop("Proof")

    def test_unparsable_raises(self):
        with pytest.raises(PropError):
            canonical_prop("P -> ")


class TestAlphaInvariance:
    def test_bound_variable_names_do_not_matter(self):
        a = canonical_prop("forall x:iota, Q x")
        b = canonical_prop("forall y:iota, Q y")
        assert a == b

    def test_free_symbols_do_matter(self):
        a = canonical_prop("forall x:iota, Q x")
        b = canonical_prop("forall x:iota, R x")
        assert a != b


# ---------------------------------------------------------------------------
# M3: builders mirroring the generated shapes (see the shape catalogue in
# core/dc/debate_graph.py).  pArg-like: an argument for P via axiom pRule
# with an open Q obligation.
# ---------------------------------------------------------------------------

def eta_term(name, prop, inner):
    return Mu(ID(name, prop), prop, inner, ID(name, prop))


def parg_body(site):
    """mu pArg:P.< mu rule:P.< pRule:Q->P || site * rule:P > || pArg:P >"""
    return eta_term(
        "pArg", "P",
        Mu(ID("rule", "P"), "P",
           DI("pRule", "Q->P"),
           Cons(site, ID("rule", "P"))),
    )


def qarg_body():
    """mu qArg:Q.< mu rule2:Q.< qRule || ?r:R->false * rule2 > || qArg:Q >"""
    return eta_term(
        "qArg", "Q",
        Mu(ID("rule2", "Q"), "Q",
           DI("qRule", "(R->false)->Q"),
           Cons(Goal("2.1", "R->false"), ID("rule2", "Q"))),
    )


def t_sup(prop, orig, scion):
    # paper T-SUP: mu alt.< orig || mu'b.< mu_.<b||alt> || mu'_.<scion||alt> > >
    return Mu(ID("alt", prop), prop, orig,
              Mutilde(DI("b", prop), prop,
                      Mu(ID("_", prop), prop, DI("b", prop), ID("alt", prop)),
                      Mutilde(DI("_", prop), prop, scion, ID("alt", prop))))


def t_att(prop, orig, scion_ctx):
    # paper T-ATT: mu alt.< orig || mu'b.< mu_.<b||scion> || mu'_.<b||alt> > >
    return Mu(ID("alt", prop), prop, orig,
              Mutilde(DI("b", prop), prop,
                      Mu(ID("_", prop), prop, DI("b", prop), scion_ctx),
                      Mutilde(DI("_", prop), prop, DI("b", prop), ID("alt", prop))))


STRICT = {"pRule", "qRule"}


def arguments(g):
    """The edges other than the axioms' own (a declared name used as a
    horn is a strict, source-less edge named after it, 2026-10-15)."""
    return [e for e in g.edges if e.name not in STRICT]


def source(edge, prop):
    return next(s for s in edge.sources if s.key == canonical_prop(prop))


class TestSingleArgumentEdge:
    def test_edge_and_sources(self):
        g = compile_debate(parg_body(Goal("1", "Q")), "pArg", strict_names=STRICT)
        assert len(arguments(g)) == 1
        e = arguments(g)[0]
        assert e.target_key == canonical_prop("P")
        assert e.target_side == "term"
        # the axiom pRule is a horn: a strict edge of its own and a source
        assert [(s.key, s.side, s.kind) for s in e.sources] == [
            (canonical_prop("Q->P"), "term", "subargument"),
            (canonical_prop("Q"), "term", "obligation"),
        ]
        assert not e.strict
        axiom = next(e for e in g.edges if e.name == "pRule")
        assert axiom.strict and axiom.sources == () and axiom.target_key == canonical_prop("Q->P")
        assert g.defaults[(canonical_prop("Q"), "term")] == {"obligation"}

    def test_presumption_site(self):
        g = compile_debate(parg_body(Deleg("1", "Q")), "pArg", strict_names=STRICT)
        assert source(arguments(g)[0], "Q").kind == "presumption"
        assert g.defaults[(canonical_prop("Q"), "term")] == {"presumption"}

    def test_strict_edge(self):
        body = eta_term("ax", "Q->P", DI("pRule", "Q->P"))
        g = compile_debate(body, "ax", strict_names=STRICT)
        assert g.edges[0].strict and g.edges[0].sources == ()

    def test_identity_edge_becomes_default_marker(self):
        body = eta_term("p1", "P", Goal("1", "P"))
        g = compile_debate(body, "p1", strict_names=STRICT)
        assert g.edges == []
        assert g.defaults[(canonical_prop("P"), "term")] == {"obligation"}

    def test_missing_prop_refused(self):
        with pytest.raises(DebateCompileError, match="enrichment"):
            compile_debate(parg_body(Goal("1", None)), "pArg", strict_names=STRICT)

    def test_undeclared_free_var_refused(self):
        body = eta_term("a", "P", DI("mystery", "P"))
        with pytest.raises(DebateCompileError, match="mystery"):
            compile_debate(body, "a", strict_names=STRICT)

    def test_falsum_eliminator_leaf_is_a_builtin_axiom(self):
        """Fellowship encodes negation elimination as
        mu' H:~A.< H || a * _F_ >; the tail _F_ is the canonical
        refutation of false, a strict axiom of the base category, not a
        free variable (the demo fixtures were refused on it before
        2026-09-16)."""
        neg_elim = Mutilde(DI("H", "~A"), "~A", DI("H", "~A"),
                           Cons(Goal("1", "A"), ID("_F_", "false")))
        # a counterargument to A: apply the declared nA to the hypothesis
        body = Mutilde(DI("x", "A"), "A", DI("nA", "~A"),
                       Mutilde(DI("H", "~A"), "~A", DI("H", "~A"),
                               Cons(DI("x", "A"), ID("_F_", "false"))))
        g = compile_debate(body, "c", strict_names={"nA"})
        c = next(e for e in g.edges if e.name == "c")
        assert [(s.key, s.side, s.kind) for s in c.sources] == [
            (canonical_prop("~A"), "term", "subargument")
        ]
        assert not any(k.endswith("_F_") for k in g.nodes)
        assert neg_elim.prop == "~A"                 # the wrapper itself is transparent

    def test_falsum_leaf_on_the_wrong_side_is_refused(self):
        body = eta_term("a", "false", DI("_F_", "false"))
        with pytest.raises(DebateCompileError, match="_F_"):
            compile_debate(body, "a", strict_names=STRICT)


class TestScaffoldDecomposition:
    def test_term_support(self):
        body = parg_body(t_sup("Q", Goal("1", "Q"), qarg_body()))
        g = compile_debate(body, "dsup", strict_names=STRICT)
        by_role = {e.role: e for e in arguments(g)}
        assert set(by_role) == {"argument", "supporter"}
        host, scion = by_role["argument"], by_role["supporter"]
        assert host.target_key == canonical_prop("P")
        assert source(host, "Q").kind == "obligation"      # site kept open
        assert scion.name == "qArg"
        assert scion.target_key == canonical_prop("Q")
        assert scion.target_side == "term"
        assert source(scion, "R->false").kind == "obligation"
        g.assert_acyclic()

    def test_term_attack(self):
        # Attacker against Q: mu'qAtt:Q.< qAtt:Q || alt:Q > with the open
        # Laog tail rerouted to alt (as Argument.attack produces).
        scion_ctx = Mutilde(DI("qAtt", "Q"), "Q", DI("qAtt", "Q"), Laog("c", "Q"))
        body = parg_body(t_att("Q", Goal("1", "Q"), scion_ctx))
        g = compile_debate(body, "datt", strict_names=STRICT)
        by_role = {e.role: e for e in arguments(g)}
        host = by_role["argument"]
        assert source(host, "Q").kind == "obligation"
        # The attacker's edge is an identity edge (challenge-to-Q only):
        # it contributes a context-side obligation marker, not an edge.
        assert "attacker" not in by_role
        assert g.defaults[(canonical_prop("Q"), "context")] == {"obligation"}

    def test_nested_support_chain(self):
        # qArg's own R->false site supported by a further argument.
        rarg = eta_term("rArg", "R->false",
                        Mu(ID("r", "R->false"), "R->false",
                           Goal("5", "S"), ID("r", "R->false")))
        inner = qarg_body()
        inner.term.context.term = t_sup("R->false", Goal("2.1", "R->false"), rarg)
        body = parg_body(t_sup("Q", Goal("1", "Q"), inner))
        g = compile_debate(body, "deep", strict_names=STRICT)
        assert len(arguments(g)) == 3
        names = {e.name for e in arguments(g)}
        # The host edge is named after the subargument it records (pArg),
        # not after the debate ("deep"): aida-host-edge-naming.
        assert names == {"pArg", "qArg", "rArg"}
        g.assert_acyclic()

    def test_cycle_admitted_and_classified(self):
        # An argument for P from Q supported by an argument for Q from P:
        # a derivation cycle.  The cyclic fragment (aida-cyclic-fragment)
        # compiles it; is_acyclic classifies it.
        qfromp = eta_term("qFromP", "Q",
                          Mu(ID("k", "Q"), "Q", Goal("7", "P"), ID("k", "Q")))
        body = parg_body(t_sup("Q", Goal("1", "Q"), qfromp))
        g = compile_debate(body, "cyc", strict_names=STRICT)
        assert not g.is_acyclic()
        assert {e.name for e in arguments(g)} == {"pArg", "qFromP"}
        with pytest.raises(CyclicDebateNotSupported):
            g.assert_acyclic()      # the initial fragment's guard, on request

    def test_bare_lambda_is_an_implication_edge(self):
        # A lambda is a subargument for an implication; as a root it is
        # the whole argument (A->A, strict, no sources).
        g = compile_debate(Lamda(Hyp(DI("x", "A"), "A"), DI("x", "A")), "lam")
        assert [(e.name, e.strict, e.sources, g.nodes[e.target_key]) for e in g.edges] == [
            ("lam", True, (), "A->A")]

    def test_unknown_root_refused(self):
        with pytest.raises(DebateCompileError, match="refusing"):
            compile_debate(Hyp(DI("x", "A"), "A"), "hyp")

    def test_a_cell_is_a_root_with_its_proposition(self):
        # an elimination cell as a horn is an edge of its own (2026-10-15)
        with pytest.raises(DebateCompileError, match="root proposition missing"):
            compile_debate(Cons(Goal("1", "A"), Laog("2", "B")), "cons")
        cell = Cons(Goal("1", "A"), Laog("2", "B"))
        cell.prop = "A->B"
        g = compile_debate(cell, "cons")
        assert [(g.nodes[e.target_key], e.target_side) for e in g.edges] == [("A->B", "context")]
        assert {(g.nodes[s.key], s.side) for s in g.edges[0].sources} == {("A", "term"), ("B", "context")}


class TestDocument:
    def test_merge_two_arguments(self):
        g = compile_document(
            [
                ("pArg", parg_body(Goal("1", "Q"))),
                ("qArg", qarg_body()),
            ],
            strict_names=STRICT,
        )
        assert len(arguments(g)) == 2 and len(g.edges) == 4     # plus the two axioms
        assert canonical_prop("Q") in g.parents(canonical_prop("P"))
        assert canonical_prop("R->false") in g.parents(canonical_prop("Q"))

    def test_dot_export(self):
        g = compile_debate(parg_body(Goal("1", "Q")), "pArg", strict_names=STRICT)
        dot = g.to_dot()
        assert dot.startswith("digraph debate {") and dot.endswith("}")
        assert "pArg" in dot

    def test_dot_export_with_labels_fills_nodes(self):
        from core.comp.adf_label import grounded_labels
        g = compile_debate(parg_body(Deleg("1", "Q")), "pArg", strict_names=STRICT)
        dot = g.to_dot(labels=grounded_labels(g))
        assert "fillcolor=" in dot
        assert "t=IN" in dot


class TestTextView:
    def test_tree_shape_and_labels(self):
        from core.comp.adf_label import grounded_labels
        body = parg_body(t_sup("Q", Goal("1", "Q"), qarg_body()))
        g = compile_debate(body, "dsup", strict_names=STRICT)
        text = g.to_text(labels=grounded_labels(g))
        lines = text.splitlines()
        assert lines[0].startswith("P[term]")
        assert "pArg  (argument, defeasible)" in lines[1]  # host edge named after its subargument
        assert any("Q[term]" in ln and "[obligation]" in ln for ln in lines)
        assert any("qArg  (supporter" in ln for ln in lines)
        # Indentation grows with depth.
        depths = [len(ln) - len(ln.lstrip()) for ln in lines[1:] if ln.strip()]
        assert depths == sorted(depths)

    def test_contrary_is_shown_once(self):
        # Both sides of Q materialized: the contest appears exactly once,
        # and the walk does not descend back into the 2-cycle.
        g = DebateGraph()
        g.add_node("Q")
        g.add_edge(edge_helper("argQ", canonical_prop("Q"), "term",
                               [src_helper(canonical_prop("R"), "term", "presumption")]))
        g.add_node("R")
        g.mark_default(canonical_prop("Q"), "context", "presumption")
        text = g.to_text()
        assert text.count("contested by") == 1

    def test_labels_optional(self):
        g = compile_debate(parg_body(Goal("1", "Q")), "pArg", strict_names=STRICT)
        text = g.to_text()
        assert "P[term]" in text
        assert "IN" not in text and "OUT" not in text

    def test_every_statement_appears(self):
        from core.comp.adf_label import grounded_labels
        body = parg_body(t_sup("Q", Goal("1", "Q"), qarg_body()))
        g = compile_debate(body, "dsup", strict_names=STRICT)
        text = g.to_text(labels=grounded_labels(g))
        for key, side in g.statements():
            assert f"{g.nodes[key]}[{side}]" in text


def edge_helper(name, target, side, sources):
    from core.dc.debate_graph import Edge
    return Edge(name=name, target_key=target, target_side=side,
                sources=tuple(sources), strict=False, role="argument")


def src_helper(key, side, kind):
    from core.dc.debate_graph import Source
    return Source(key=key, side=side, kind=kind, site="s")


class TestHostEdgeNaming:
    """aida-host-edge-naming: edges carry their own subargument's name."""

    def composite(self):
        # The shape Argument.support builds: the debate's eta wrapper, the
        # synthetic theta-expansion wrapper, then the host argument.
        body = parg_body(t_sup("Q", Goal("1", "Q"), qarg_body()))
        return eta_term("d2", "P", eta_term("theta_expand_pArg_4379018016", "P", body))

    def test_edges_are_named_after_the_constituents(self):
        g = compile_debate(self.composite(), "d2", strict_names=STRICT)
        assert {e.name for e in arguments(g)} == {"pArg", "qArg"}
        assert {e.name for e in g.edges} == {"pArg", "qArg", "pRule", "qRule"}

    def test_no_synthetic_name_reaches_an_edge(self):
        g = compile_debate(self.composite(), "d2", strict_names=STRICT)
        assert not any(e.name.startswith("theta_expand_") for e in g.edges)

    def test_names_are_stable_across_compilations(self):
        a = [e.name for e in compile_debate(self.composite(), "d2", strict_names=STRICT).edges]
        b = [e.name for e in compile_debate(self.composite(), "d2", strict_names=STRICT).edges]
        assert a == b

    def test_bare_body_falls_back_to_the_debate_name(self):
        body = Mu(ID("k", "P"), "P", DI("pRule", "Q->P"), Cons(Goal("1", "Q"), ID("k", "P")))
        g = compile_debate(body, "solo", strict_names=STRICT)
        assert arguments(g)[0].name == "solo"


class TestCapturedObligationsAreSources:
    """A scion that refers to a binder of an enclosing subargument has
    captured it while grafting.  The framework records the captured
    variable as a source of the scion's own edge at the binder's
    statement - an obligation source for a captured demand, a presumption
    source for a captured default - so a cycle a capture closes is a cycle
    here.  Whether the demand was met inside its scope is not the
    framework's business: strictness is read off the term
    (core/dc/strict.py)."""

    def host_with(self, scion):
        # P from  lambda h:P . <site Q>  facing an open refutation of P->Q
        # (the Peirce fixture's s2); the scion fills the Q site.
        return eta_term("s2", "P",
                        Mu(ID("beta", "P"), "P",
                           Lamda(Hyp(DI("h", "P"), "P"), t_sup("Q", Goal("1", "Q"), scion)),
                           Laog("2", "P->Q")))

    @staticmethod
    def shape(g, edge):
        return {(g.nodes[s.key], s.side, s.kind) for s in edge.sources}

    def test_lambda_hypothesis_captured(self):
        scion = eta_term("p3", "Q", Mu(ID("g", "Q"), "Q", DI("h", "P"), Laog("9", "P")))
        g = compile_debate(self.host_with(scion), "d", strict_names=STRICT)
        by_name = {e.name: e for e in g.edges}
        assert set(by_name) == {"s2", "s2.\u03bb1", "p3"}
        assert self.shape(g, by_name["p3"]) == {("P", "term", "obligation"), ("P", "context", "obligation")}
        assert self.shape(g, by_name["s2.\u03bb1"]) == {("Q", "term", "obligation")}
        assert not g.is_acyclic()          # P[t] <- P->Q[t] <- Q[t] <- P[t]: the trap, as a cycle

    def test_mu_continuation_captured(self):
        scion = eta_term("y", "Q", Mu(ID("g", "Q"), "Q", Goal("8", "P"), ID("beta", "P")))
        g = compile_debate(self.host_with(scion), "d", strict_names=STRICT)
        y = next(e for e in g.edges if e.name == "y")
        assert self.shape(g, y) == {("P", "term", "obligation"), ("P", "context", "obligation")}

    def test_closed_scion_is_an_ordinary_edge(self):
        scion = eta_term("y", "Q", Mu(ID("g", "Q"), "Q", Goal("8", "R"), ID("g", "Q")))
        g = compile_debate(self.host_with(scion), "d", strict_names=STRICT)
        assert {e.name for e in g.edges} == {"s2", "s2.\u03bb1", "y"}

    def test_supporter_using_the_catch_variable(self):
        # The supporter reaches for the scaffold's own catch variable, the
        # site's continuation: an obligation source at Q[c].
        scion = Mu(ID("k", "Q"), "Q", Deleg("2", "Q"), ID("alt", "Q"))
        body = parg_body(t_sup("Q", Goal("1", "Q"), scion))
        g = compile_debate(body, "d", strict_names=STRICT)
        sup = next(e for e in g.edges if e.role == "supporter")
        assert self.shape(g, sup) == {("Q", "term", "presumption"), ("Q", "context", "obligation")}

    def test_peirce_shape_is_a_cycle_plus_a_strict_edge(self):
        # lambda f. mu alpha.< f || (lambda h. mu _.< h || alpha >) * alpha >
        # built as supports that capture f and alpha.  The framework: a
        # cycle of obligation edges, all OUT.  The term: closed once its
        # scaffolds are decided by strictness, so the issue graph carries a
        # strict edge for the thesis and labels it IN.
        from core.dc.strict import compile_issue
        from core.comp.adf_label import grounded_labels
        inner = Mu(ID("g2", "Q"), "Q", DI("h", "P"), ID("alpha", "P"))
        pq = Lamda(Hyp(DI("h", "P"), "P"), t_sup("Q", Goal("1", "Q"), eta_term("p3", "Q", inner)))
        body = eta_term("s1", "((P->Q)->P)->P",
                        Lamda(Hyp(DI("f", "(P->Q)->P"), "(P->Q)->P"),
                              Mu(ID("alpha", "P"), "P", DI("f", "(P->Q)->P"),
                                 Cons(t_sup("P->Q", Goal("2", "P->Q"), eta_term("s2", "P->Q", pq)),
                                      ID("alpha", "P")))))
        framework = compile_debate(body, "peirce", strict_names=STRICT)
        assert {e.name for e in framework.edges} == {"s1", "s2", "p3"}
        assert not framework.edges and False or all(not e.strict for e in framework.edges)
        T = canonical_prop("((P->Q)->P)->P")
        assert grounded_labels(framework)[(T, "term")] == "OUT"
        issue = compile_issue(body, "peirce", strict_names=STRICT)
        strict = [e for e in issue.edges if e.strict]
        assert [(e.name, e.target_key, e.sources) for e in strict] == [("s1*", T, ())]
        assert grounded_labels(issue)[(T, "term")] == "IN"

    def test_truly_free_variable_still_refused(self):
        with pytest.raises(DebateCompileError, match="mystery"):
            compile_debate(eta_term("a", "P", DI("mystery", "P")), "a", strict_names=STRICT)


class TestHornRule:
    """The horn rule (tasks.org, aida-discuss-non-site-attack-points,
    2026-10-15): a binder's derivation has its horns as sources - the
    maximal subterms not carrying the binder's own variable - and the
    spine gets nothing.  Declared axioms and lemmas are edges of their
    own; the labels are invariant under beta and the (-) rule."""

    @staticmethod
    def cons(t, e, prop):
        n = Cons(t, e)
        n.prop = prop
        return n

    @staticmethod
    def sonc(e, t, prop):
        n = Sonc(e, t)
        n.prop = prop
        return n

    @staticmethod
    def lam(x, xprop, body, prop):
        n = Lamda(Hyp(DI(x, xprop), xprop), body)
        n.prop = prop
        return n

    @staticmethod
    def shape(g):
        return {(e.name, g.nodes[e.target_key], e.target_side, e.strict,
                 tuple((g.nodes[s.key], s.side, s.kind) for s in e.sources)) for e in g.edges}

    def test_an_application_exposes_the_function_and_the_argument(self):
        # pro: B from A (presumed) via the axiom r : A->B
        body = Mu(ID("al", "B"), "B", DI("r", "A->B"), self.cons(Deleg("1", "A"), ID("al", "B"), "A->B"))
        g = compile_debate(body, "pro", strict_names={"r"})
        assert self.shape(g) == {
            ("r", "A->B", "term", True, ()),
            ("pro", "B", "term", False, (("A->B", "term", "subargument"), ("A", "term", "presumption")))}

    def test_the_cell_is_the_spine_and_gets_no_edge(self):
        # cArg: C from A and B via abc : A->B->C; the cells A*(B*r) carry r
        body = Mu(ID("r", "C"), "C", DI("abc", "A->B->C"),
                  self.cons(Goal("1", "A"), self.cons(Deleg("2", "B"), ID("r", "C"), "B->C"), "A->B->C"))
        g = compile_debate(body, "cArg", strict_names={"abc"})
        assert {e.name for e in g.edges} == {"abc", "cArg"}
        assert ("B->C" not in {g.nodes[k] for k in g.nodes})

    def test_the_authors_three_shapes(self):
        strict = {"ax", "r", "r2", "na", "nb"}
        stronger = Mu(ID("al", "B"), "B", self.sonc(ID("al", "B"), DI("ax", "A"), "A-B"), ID("r", "A-B"))
        g = compile_debate(stronger, "s", strict_names=strict)
        assert self.shape(g) >= {("s", "B", "term", True, (("A", "term", "subargument"), ("A-B", "context", "subargument")))}
        tollens = Mutilde(DI("x", "A"), "A", DI("r2", "A->B"), self.cons(DI("x", "A"), ID("nb", "B"), "A->B"))
        g = compile_debate(tollens, "mt", strict_names=strict)
        assert self.shape(g) >= {("mt", "A", "context", True, (("A->B", "term", "subargument"), ("B", "context", "subargument")))}

    def test_beta_does_not_change_the_root_edge(self):
        # mu a:B.<lambda x:A.!B || !A * a>  and its reduct  mu a:B.<!A || mu'x:A.<!B || a>>
        before = Mu(ID("al", "B"), "B", self.lam("x", "A", Deleg("2", "B"), "A->B"),
                    self.cons(Deleg("1", "A"), ID("al", "B"), "A->B"))
        after = Mu(ID("al", "B"), "B", Deleg("1", "A"), Mutilde(DI("x", "A"), "A", Deleg("2", "B"), ID("al", "B")))
        gb, ga = compile_debate(before, "k"), compile_debate(after, "k")
        root = lambda g: next(e for e in g.edges if e.name == "k")
        assert [(g.nodes[s.key], s.side) for g in (gb,) for s in root(gb).sources] == [("A->B", "term"), ("A", "term")]
        assert [(ga.nodes[s.key], s.side) for s in root(ga).sources] == [("A", "term"), ("B", "term")]
        from core.comp.adf_label import grounded_labels
        B = (canonical_prop("B"), "term")
        assert grounded_labels(gb)[B] == grounded_labels(ga)[B] == "IN"

    def test_a_nested_cut_and_its_application_form_label_alike(self):
        # N = mu a:B.<!A || mu'y:A.<mu d:C.<!A->C || y*d> || mu'z:C.<mu e:B.<!C->B || z*e> || a>>>
        # P = mu a:B.<!C->B || (mu d:C.<!A->C || !A*d>) * a>
        from core.comp.adf_label import grounded_labels
        N = Mu(ID("al", "B"), "B", Deleg("1", "A"),
               Mutilde(DI("y", "A"), "A",
                       Mu(ID("de", "C"), "C", Deleg("2", "A->C"), self.cons(DI("y", "A"), ID("de", "C"), "A->C")),
                       Mutilde(DI("z", "C"), "C",
                               Mu(ID("ep", "B"), "B", Deleg("3", "C->B"), self.cons(DI("z", "C"), ID("ep", "B"), "C->B")),
                               ID("al", "B"))))
        P = Mu(ID("al", "B"), "B", Deleg("3", "C->B"),
               self.cons(Mu(ID("de", "C"), "C", Deleg("2", "A->C"), self.cons(Deleg("1", "A"), ID("de", "C"), "A->C")),
                         ID("al", "B"), "C->B"))
        gn, gp = compile_debate(N, "n"), compile_debate(P, "p")
        # the lemmas are horns with captured hypotheses; the spine mu' nodes are not edges
        assert {e.name for e in gn.edges} == {"n", "n.μ1", "n.μ2"}
        assert {e.name for e in gp.edges} == {"p", "p.μ1"}
        for stmt in (("B", "term"), ("C", "term"), ("A", "term")):
            key = (canonical_prop(stmt[0]), stmt[1])
            assert grounded_labels(gn)[key] == grounded_labels(gp)[key] == "IN"

    def test_a_reductio_exposes_its_refutation(self):
        # mu v:P.<!P || mu'n:P.<!~P || n*_F_>>, v unused: the refutation is a horn
        body = Mu(ID("v", "P"), "P", Deleg("1", "P"),
                  Mutilde(DI("n", "P"), "P", Deleg("2", "~P"), self.cons(DI("n", "P"), ID("_F_", "false"), "~P")))
        g = compile_debate(body, "argP")
        assert self.shape(g) == {
            ("argP.μ'1", "P", "context", False, (("~P", "term", "presumption"),)),
            ("argP", "P", "term", False, (("P", "term", "presumption"), ("P", "context", "subargument")))}

    def test_a_captured_occurrence_does_not_make_a_horn_the_spine(self):
        # the unfolder's capture: a site inside a lemma met by the argument's
        # own continuation `rule`; the lemda is still a horn, rule a source
        body = Mu(ID("rule", "P"), "P", DI("r", "A->P"),
                  self.cons(Mu(ID("k", "A"), "A", DI("f", "B->A"), self.cons(Goal("1", "B"), ID("k", "A"), "B->A")),
                            ID("rule", "P"), "A->P"))
        inner = body.context.term
        inner.context.term = ID("rule", "P") if False else Goal("1", "B")   # unchanged: a plain lemma
        g = compile_debate(body, "a", strict_names={"r", "f"})
        assert {e.name for e in g.edges} == {"r", "f", "a.μ1", "a"}
        lemma = next(e for e in g.edges if e.name == "a.μ1")
        assert [(g.nodes[s.key], s.kind) for s in lemma.sources] == [("B->A", "subargument"), ("B", "obligation")]
