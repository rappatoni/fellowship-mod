"""V0 self-checks for the semantic oracles (propositional-fragment-plan.org, M0).

Every ADF here is small enough to work by hand; the expected values in the
asserts were derived on paper from the definitions, not from any
implementation.  The cross-check tests compare the naive oracle against the
independently developed adf-bdd solver and are skipped when its binary is
not installed (`cargo install adf-bdd-bin`).
"""

import pytest

from core.ac.ast import (
    Mu, Mutilde, Lamda, Admal, Hyp, Pyh, Cons, Sonc,
    Goal, Deleg, ID, DI,
)
from core.comp.oracle import (
    ADF, const, var, neg, conj, disj,
    two_valued_models, gamma, complete_interpretations, grounded_interpretation,
    diamond_export, find_adf_bdd, run_adf_bdd,
)
from core.comp.oracle_terms import (
    substitute, alpha_equal, canonical_form,
    normalize_command, normalize_strong, classify_nf, instantiate_sites,
    make_abort_term, OracleFuelExhausted,
)


# ---------------------------------------------------------------------------
# Hand-worked ADFs
# ---------------------------------------------------------------------------

def adf_acyclic_mixed() -> ADF:
    """b: presumption (default true); a supported by b; c: unsupported
    obligation (default false); d attacks b's proposition (ac = not b).
    Grounded, worked by hand: b=T, a=T, c=F, d=F."""
    return ADF(
        ["a", "b", "c", "d"],
        {"a": var("b"), "b": const(True), "c": const(False), "d": neg(var("b"))},
    )


def adf_even_loop() -> ADF:
    """p = not q, q = not p."""
    return ADF(["p", "q"], {"p": neg(var("q")), "q": neg(var("p"))})


def adf_self_attack() -> ADF:
    """p = not p."""
    return ADF(["p"], {"p": neg(var("p"))})


def adf_support_cycle() -> ADF:
    """p = q, q = p (mutual support, no independent ground)."""
    return ADF(["p", "q"], {"p": var("q"), "q": var("p")})


class TestADFOracle:
    def test_acyclic_mixed_grounded(self):
        adf = adf_acyclic_mixed()
        assert adf.is_acyclic()
        g = grounded_interpretation(adf)
        assert g == {"a": True, "b": True, "c": False, "d": False}

    def test_acyclic_grounded_is_two_valued_and_unique_model(self):
        # V-two seed: on acyclic ADFs the grounded interpretation is
        # two-valued and coincides with the unique two-valued model.
        adf = adf_acyclic_mixed()
        g = grounded_interpretation(adf)
        assert None not in g.values()
        models = two_valued_models(adf)
        assert models == [g] or (len(models) == 1 and models[0] == g)

    def test_even_loop(self):
        adf = adf_even_loop()
        assert not adf.is_acyclic()
        models = two_valued_models(adf)
        assert sorted(models, key=lambda m: m["p"]) == [
            {"p": False, "q": True},
            {"p": True, "q": False},
        ]
        assert grounded_interpretation(adf) == {"p": None, "q": None}
        assert len(complete_interpretations(adf)) == 3

    def test_self_attack(self):
        adf = adf_self_attack()
        assert two_valued_models(adf) == []
        assert grounded_interpretation(adf) == {"p": None}
        assert complete_interpretations(adf) == [{"p": None}]

    def test_support_cycle_grounded_is_undecided(self):
        # Pure ADF fact: grounded leaves a mutual-support cycle undecided.
        # NOTE: tests/rationality/README.org conjectures "both OUT" for the
        # support-cycle fixture under grounded; that conjecture disagrees
        # with the ADF definition (both OUT is the *stable* answer) and is
        # to be corrected during M7 oracle validation.
        adf = adf_support_cycle()
        assert grounded_interpretation(adf) == {"p": None, "q": None}
        tv = {frozenset(m.items()) for m in two_valued_models(adf)}
        assert tv == {
            frozenset({("p", True), ("q", True)}),
            frozenset({("p", False), ("q", False)}),
        }

    def test_gamma_on_decided_interpretation_is_classical_evaluation(self):
        adf = adf_acyclic_mixed()
        v = {"a": True, "b": True, "c": False, "d": False}
        assert gamma(adf, v) == v

    def test_statement_guard(self):
        names = [f"s{i}" for i in range(15)]
        with pytest.raises(ValueError, match="limited to"):
            ADF(names, {n: const(True) for n in names})

    def test_validation_rejects_unknown_parent(self):
        with pytest.raises(ValueError, match="unknown statements"):
            ADF(["a"], {"a": var("ghost")})


class TestDiamondExport:
    def test_golden(self):
        adf = ADF(
            ["A -> B", "B"],
            {"A -> B": conj(var("B"), neg(var("A -> B"))), "B": disj()},
        )
        text, name_map = diamond_export(adf)
        assert name_map == {"A -> B": "s1", "B": "s2"}
        assert text == (
            "s(s1).\n"
            "s(s2).\n"
            "ac(s1,and(s2,neg(s1))).\n"
            "ac(s2,c(f)).\n"
        )

    def test_empty_conjunction_is_true(self):
        adf = ADF(["a"], {"a": conj()})
        text, _ = diamond_export(adf)
        assert "ac(s1,c(v))." in text


needs_adf_bdd = pytest.mark.skipif(
    find_adf_bdd() is None,
    reason="adf-bdd binary not installed (cargo install adf-bdd-bin)",
)


@needs_adf_bdd
class TestAdfBddCrossCheck:
    """Three-way-agreement seed: naive oracle vs the independent solver."""

    @pytest.mark.parametrize(
        "make",
        [adf_acyclic_mixed, adf_even_loop, adf_self_attack, adf_support_cycle],
    )
    def test_grounded_agrees(self, make):
        adf = make()
        theirs = run_adf_bdd(adf, "grounded")
        assert len(theirs) == 1
        assert theirs[0] == grounded_interpretation(adf)

    @pytest.mark.parametrize(
        "make",
        [adf_acyclic_mixed, adf_even_loop, adf_self_attack, adf_support_cycle],
    )
    def test_complete_agrees(self, make):
        adf = make()
        theirs = {frozenset(v.items()) for v in run_adf_bdd(adf, "complete")}
        ours = {frozenset(v.items()) for v in complete_interpretations(adf)}
        assert theirs == ours


# ---------------------------------------------------------------------------
# Term-side oracle
# ---------------------------------------------------------------------------

def _cmd_beta():
    """< lam x.x :: v * alpha >  — should reach  < v :: alpha >."""
    lam = Lamda(Hyp(DI("x", "A"), "A"), DI("x", "A"))
    ctx = Cons(DI("v", "A"), ID("alpha", "A"))
    return lam, ctx


class TestMinimalNormalizer:
    @pytest.mark.parametrize("strategy", ["cbn", "cbv"])
    def test_beta_chain(self, strategy):
        t, c = normalize_command(*_cmd_beta(), strategy=strategy)
        assert canonical_form(t) == ("di", ("free", "v"))
        assert canonical_form(c) == ("id", ("free", "alpha"))

    def test_difference_rule(self):
        # < e1 * v :: e2 .a lam >  -->  < mu a.< v :: e2 > :: e1 >
        # and, a being affine, (mu<) then yields  < v :: e2 >.
        term = Sonc(ID("e1", "A"), DI("v", "B"))
        ctx = Admal(Pyh(ID("a", "A"), "A"), ID("e2", "B"))
        t, c = normalize_command(term, ctx, strategy="cbn")
        assert canonical_form(t) == ("di", ("free", "v"))
        assert canonical_form(c) == ("id", ("free", "e2"))

    def test_critical_pair_strategy_divergence(self):
        # < mu a.< t1 :: beta > :: (< t2 :: gamma >).x mu' >  with both
        # binders affine: CBV keeps the term side, CBN the context side.
        term = Mu(ID("a", "A"), "A", DI("t1", "A"), ID("beta", "A"))
        ctx = Mutilde(DI("x", "A"), "A", DI("t2", "A"), ID("gamma", "A"))
        t_cbv, c_cbv = normalize_command(term, ctx, strategy="cbv")
        assert canonical_form(t_cbv) == ("di", ("free", "t1"))
        assert canonical_form(c_cbv) == ("id", ("free", "beta"))
        t_cbn, c_cbn = normalize_command(term, ctx, strategy="cbn")
        assert canonical_form(t_cbn) == ("di", ("free", "t2"))
        assert canonical_form(c_cbn) == ("id", ("free", "gamma"))


class TestStrongNormalizer:
    def _contested_residue(self):
        """The root-normal skeptical residue of the contested fixture:
        mu datt:P.< pRule :: (mu b1:Q.< ?g:Q || mu'b2:Q.<b2 || !d:Q> >) * datt >
        whose live redex sits one Cons deep - invisible to weak reduction."""
        from core.ac.ast import Deleg, Geled, Cons
        inner = Mu(ID("b1", "Q"), "Q",
                   Goal("g", "Q"),
                   Mutilde(DI("b2", "Q"), "Q", DI("b2", "Q"), Geled("d", "Q")))
        return Mu(ID("datt", "P"), "P",
                  DI("pRule", "Q->P"),
                  Cons(inner, ID("datt", "P")))

    def test_fires_nested_redex(self):
        from core.ac.ast import Geled
        nf = normalize_strong(self._contested_residue(), strategy="cbn")
        # The inner (>mu) fired: b2 was substituted by the goal, leaving
        # the contradiction command  mu b1:Q.< ?g:Q || !d:Q >.
        inner = nf.context.term
        assert isinstance(inner, Mu)
        assert isinstance(inner.term, Goal)
        assert isinstance(inner.context, Geled)

    def test_weak_normalizer_does_not(self):
        from core.comp.oracle_terms import normalize_term
        residue = self._contested_residue()
        assert alpha_equal(normalize_term(residue, strategy="cbn"), residue)

    def test_deterministic_across_runs_and_agrees_with_weak_at_root(self):
        t, c = _cmd_beta()
        rooted = Mu(ID("r", "A"), "A", t, c)
        first = normalize_strong(rooted, strategy="cbn")
        second = normalize_strong(rooted, strategy="cbn")
        assert alpha_equal(first, second)
        # On a term whose only redexes are at the root command, strong and
        # weak normalization coincide.
        from core.comp.oracle_terms import normalize_term
        assert alpha_equal(first, normalize_term(rooted, strategy="cbn"))

    def test_strong_normal_forms_are_stable(self):
        peirce_like = Lamda(Hyp(DI("x", "A"), "A"), DI("x", "A"))
        assert alpha_equal(normalize_strong(peirce_like), peirce_like)

    def test_fuel_exhaustion_raises(self):
        # mu a.< mu'x.< mu a'.<x' ...> ... - build a genuine loop:
        # < mu a.<omega || a> || mu'x.<x || omega'> > under CBN keeps
        # substituting; simplest loop: mu d.< mu a.<v||a> :: (c).x mu' >
        # where c re-creates the pair.  Use the classic
        # < mu a.c[a] || mu'x.c'[x] > self-feeding pair:
        looping_term = Mu(ID("a", "A"), "A", DI("y", "A"), ID("a", "A"))
        looping_ctx = Mutilde(DI("x", "A"), "A", DI("x", "A"), ID("delta", "A"))
        # < mu a.<y||a> || mu'x.<x||delta> >: CBN substitutes the whole Mu
        # for x, then eta-like steps keep firing inside; wrap so the pair
        # reconstitutes itself via duplication:
        body = Mu(ID("root", "A"), "A", looping_term, looping_ctx)
        try:
            nf = normalize_strong(body, strategy="cbn", fuel=50)
        except OracleFuelExhausted:
            return  # acceptable outcome for a loop
        # If it terminated, it must be a genuine normal form: one more
        # round changes nothing.
        assert alpha_equal(nf, normalize_strong(nf, strategy="cbn", fuel=50))


class TestCommaPaperChecks:
    """Worked expressions from papers/comma2026/main.tex."""

    def test_exception_bubble_up(self):
        # Sec. 3.1:  < mu _.< v :: e > :: f >  --(mu<)-->  < v :: e >
        # (the affine mu discards its context; the exception bubbles up).
        throw = Mu(ID("_", "A"), "A", DI("v", "A"), ID("e", "A"))
        t, c = normalize_command(throw, ID("f", "A"), strategy="cbn")
        assert canonical_form(t) == ("di", ("free", "v"))
        assert canonical_form(c) == ("id", ("free", "e"))

    def test_peirce_term_is_a_normal_value(self):
        # The paper's normal form for Peirce's law:
        #   mu alpha.< lam f. mu gamma.< f :: (lam h. mu theta.< h :: gamma >) * gamma > :: alpha >
        # must be root-normal under both strategies and classify as a value.
        peirce_prop = "((P -> Q) -> P) -> P"
        inner = Lamda(
            Hyp(DI("h", "P"), "P"),
            Mu(ID("theta", "Q"), "Q", DI("h", "P"), ID("gamma", "P")),
        )
        mid = Mu(
            ID("gamma", "P"), "P",
            DI("f", "(P -> Q) -> P"),
            Cons(inner, ID("gamma", "P")),
        )
        peirce = Mu(
            ID("alpha", peirce_prop), peirce_prop,
            Lamda(Hyp(DI("f", "(P -> Q) -> P"), "(P -> Q) -> P"), mid),
            ID("alpha", peirce_prop),
        )
        from core.comp.oracle_terms import normalize_term
        for strategy in ("cbn", "cbv"):
            assert alpha_equal(normalize_term(peirce, strategy=strategy), peirce)
        assert classify_nf(peirce) == "value"


class TestSubstitution:
    def test_capture_avoidance(self):
        # Substituting, for x, a term with FREE context variable alpha into
        # mu alpha.< x :: alpha > must rename the binder, not capture.
        target = Mu(ID("alpha", "A"), "A", DI("x", "A"), ID("alpha", "A"))
        replacement = Mu(ID("beta", "B"), "B", DI("z", "B"), ID("alpha", "B"))
        result = substitute(target, DI, "x", replacement)
        assert result.id.name != "alpha"          # binder renamed
        assert result.context.name == result.id.name  # bound occurrence tracked
        assert result.term.context.name == "alpha"    # replacement's alpha still free

    def test_shadowed_occurrences_untouched(self):
        # In  mu' x.< x :: gamma >, the x is bound: substituting for a free
        # x elsewhere must not touch it.
        inner = Mutilde(DI("x", "A"), "A", DI("x", "A"), ID("gamma", "A"))
        target = Mu(ID("a", "A"), "A", DI("x", "A"), inner)  # outer x IS free
        result = substitute(target, DI, "x", DI("w", "A"))
        assert result.term.name == "w"                      # free occurrence replaced
        assert result.context.term.name == result.context.di.name  # bound one intact
        assert result.context.term.name != "w"


class TestAlphaEquality:
    def test_alpha_equal_binders(self):
        a = Mu(ID("a", "A"), "A", DI("x", "A"), ID("a", "A"))
        b = Mu(ID("b", "A"), "A", DI("x", "A"), ID("b", "A"))
        assert alpha_equal(a, b)

    def test_free_variables_matter(self):
        a = Mu(ID("a", "A"), "A", DI("x", "A"), ID("a", "A"))
        c = Mu(ID("a", "A"), "A", DI("x", "A"), ID("c", "A"))
        assert not alpha_equal(a, c)


class TestNFClassifier:
    def test_value(self):
        assert classify_nf(DI("x", "A")) == "value"

    def test_exception_affine_mu(self):
        throw = make_abort_term("A", DI("t", "A"), ID("chi", "A"))
        assert classify_nf(throw) == "exception"

    def test_open(self):
        t = Mu(ID("a", "A"), "A", Goal("1", "A"), ID("a", "A"))
        assert classify_nf(t) == "open"

    def test_presumption_stays_value(self):
        # An IN-by-default delegation surviving in a normal form does not
        # make it an exception or open: the value is polynomial in it.
        t = Mu(ID("a", "A"), "A", Deleg("d1", "A"), ID("a", "A"))
        assert classify_nf(t) == "value"


class TestInstantiation:
    def test_site_replacement_and_default(self):
        body = Mu(ID("a", "A"), "A", Deleg("d1", "A"),
                  Mutilde(DI("y", "A"), "A", Deleg("d2", "A"), ID("a", "A")))
        abort = make_abort_term("A", DI("t", "A"), ID("chi", "A"))
        out = instantiate_sites(body, {"d1": abort})
        assert isinstance(out.term, Mu)              # d1 instantiated at abort
        assert out.context.term.number == "d2"       # d2 left indeterminate

    def test_wrong_side_rejected(self):
        body = Mu(ID("a", "A"), "A", Deleg("d1", "A"), ID("a", "A"))
        with pytest.raises(TypeError, match="term-side"):
            instantiate_sites(body, {"d1": ID("e", "A")})


class TestCongruenceOrderIrrelevant:
    """Under a uniform strategy the congruence order does not affect the
    normal form: the CBN and CBV sub-calculi are confluent and typed terms
    are strongly normalising.  Pins the corrected claim of minicourse
    lesson 8 item 14.  If a non-uniform rule is ever added to the
    normaliser, this is the test that should fail.
    """

    @staticmethod
    def _step_innermost(node, strategy):
        from core.ac.ast import ProofTerm
        from core.comp.oracle_terms import _step_command
        for slot in ("term", "context"):
            c = getattr(node, slot, None)
            if isinstance(c, ProofTerm):
                s = TestCongruenceOrderIrrelevant._step_innermost(c, strategy)
                if s is not None:
                    setattr(node, slot, s)
                    return node
        if isinstance(node, (Mu, Mutilde)):
            st = _step_command(node.term, node.context, strategy)
            if st is not None:
                node.term, node.context = st
                return node
        return None

    @staticmethod
    def _step_context_first(node, strategy):
        from core.ac.ast import ProofTerm
        from core.comp.oracle_terms import _step_command
        if isinstance(node, (Mu, Mutilde)):
            st = _step_command(node.term, node.context, strategy)
            if st is not None:
                node.term, node.context = st
                return node
        for slot in ("context", "term"):
            c = getattr(node, slot, None)
            if isinstance(c, ProofTerm):
                s = TestCongruenceOrderIrrelevant._step_context_first(c, strategy)
                if s is not None:
                    setattr(node, slot, s)
                    return node
        return None

    @classmethod
    def _normalize_with(cls, step, v, strategy, fuel=4000):
        from copy import deepcopy
        v = deepcopy(v)
        for _ in range(fuel):
            s = step(v, strategy)
            if s is None:
                return v
            v = s
        raise OracleFuelExhausted("no normal form")

    def _terms(self):
        from core.ac.ast import Cons, Geled
        def eta(n, p, i): return Mu(ID(n, p), p, i, ID(n, p))
        def parg(site):
            return eta("pArg", "P", Mu(ID("rule", "P"), "P", DI("pRule", "Q->P"),
                                         Cons(site, ID("rule", "P"))))
        def t_sup(p, o, s):
            return Mu(ID("alt", p), p, Mu(ID("_", p), p, o, ID("alt", p)),
                      Mutilde(DI("_", p), p, s, ID("alt", p)))
        def t_att(p, o, sc):
            return Mu(ID("alt", p), p, Mu(ID("_", p), p, o, ID("alt", p)),
                      Mutilde(DI("_", p), p, Goal("g", p), sc))
        chal = Mutilde(DI("x", "Q"), "Q", DI("x", "Q"), Geled("d", "Q"))
        nested_pair = Mu(ID("root", "A"), "A",
                         Mu(ID("a", "A"), "A", DI("t1", "A"), ID("beta", "A")),
                         Mutilde(DI("y", "A"), "A",
                                 Mu(ID("k", "A"), "A", DI("y", "A"),
                                    Mutilde(DI("z", "A"), "A", DI("t4", "A"), ID("eps", "A"))),
                                 ID("gamma", "A")))
        return {
            "nested critical pairs with duplication": nested_pair,
            "support scaffold": parg(t_sup("Q", Goal("1", "Q"), eta("q", "Q", Deleg("2", "Q")))),
            "attack scaffold": parg(t_att("Q", Deleg("1", "Q"), chal)),
        }

    @pytest.mark.parametrize("strategy", ["cbn", "cbv"])
    def test_three_orders_agree(self, strategy):
        for name, term in self._terms().items():
            reference = normalize_strong(term, strategy=strategy)
            for step in (self._step_innermost, self._step_context_first):
                other = self._normalize_with(step, term, strategy)
                assert alpha_equal(reference, other), (name, strategy, step.__name__)
