"""First-order debates (tasks.org, aida-first-order-reduction; the
design decided by the author 2026-10-16).

- A statement is a proposition up to renaming of bound variables (Q1);
  a binder captures a site only for the identical statement (Q3).
- A presumed rule stands or falls as one statement: an instantiation
  ``t * c`` is a spine step, never an attack point, so no instance node
  appears (Q4).
- Only a statement can be presumed or owed: an open site under a
  first-order binder is refused (Q2), and a horn whose statement mentions
  such a variable is not exposed.  Every node is closed.
- Two strict rules (Q5): <lambda x.t || u * e> -> <t[u/x] || e> and
  <(u, v) || (y).e> -> <v || e[u/y]>.

Fixtures: tests/first_order/.  Both routes, the shared (the default) and
the unfolded, give the same verdicts.
"""

import warnings
from pathlib import Path

import pytest

from core.ac.ast import (
    Mu, Mutilde, ID, DI, Goal, LamdaFO, ConsFO, TermsPairFO, DestructTermsPairFO,
)
from core.ac.prop import Prop, TSym
from core.comp.oracle_terms import _step_command, substitute_fo
from core.dc.discipline import free_variables
from pres.gen import pres_str
from wrap.cli import setup_prover
from wrap.service import Service

TESTS = Path(__file__).parent
FIXTURES = ["tweety", "birds_fly", "generalisation", "witness", "open_source_horn",
            "hypothesis_horn"]
PIPELINES = ["shared", "unfolded"]


def session(name, pipeline="shared", typecheck=None):
    prover = setup_prover()
    svc = Service.of(prover)
    svc.set_pipeline(pipeline)
    if typecheck is not None:
        svc.set_typecheck(typecheck)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        report = svc.check((TESTS / "first_order" / f"{name}.fspy").read_text())
    return prover, svc, report


def edges(prover):
    g = prover.graph
    return {e.name: (g._show(e.target_key, e.target_side),
                     sorted((g._show(s.key, s.side), s.kind) for s in e.sources), e.strict)
            for e in g.edges}


def evaluated(svc, target, mode="skeptical"):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        result = svc.evaluate(svc.target(target), mode=mode)
    return result.nf_class, pres_str(result.normal_form)


# -- the graph ---------------------------------------------------------------------

class TestTheGraph:
    def test_an_instantiation_is_a_spine_step(self):
        prover, _svc, report = session("tweety")
        try:
            assert report.ok
            found = edges(prover)
            rule = "forall x:Animal,Bird x->Flies x"
            assert found["flies_tweety"] == ("Flies tweety[t]",
                                             [("Bird tweety[t]", "subargument"),
                                              (f"{rule}[t]", "presumption")], False)
            # the counter-instance refutes the rule itself, strictly
            assert found["penguin"][0] == f"{rule}[c]" and found["penguin"][2]
            # no instance node: Bird tweety -> Flies tweety is not a statement here
            shown = {prover.graph._show(k, side) for k, side in prover.graph.statements()}
            assert not any("Bird tweety->Flies tweety" in s for s in shown)
        finally:
            prover.close()

    def test_a_general_argument_owes_a_closed_statement(self):
        prover, _svc, report = session("birds_fly")
        try:
            assert report.ok
            assert edges(prover)["birds_fly"] == (
                "forall x:Animal,Bird x->Flies x[t]",
                [("forall x:Animal,Bird x->Wings x[t]", "obligation"),
                 ("forall x:Animal,Wings x->Flies x[t]", "subargument")], False)
        finally:
            prover.close()

    @pytest.mark.parametrize("name", FIXTURES)
    def test_every_node_is_closed(self, name):
        """No node mentions a first-order variable: its free names are all
        declared constants (the lemma mu w:Wings x is not exposed)."""
        prover, _svc, _report = session(name)
        try:
            for key in prover.graph.nodes:
                prop = prover.graph.nodes[key]
                assert set(Prop.parse(prop).free_vars()) <= set(prover.declarations), prop
        finally:
            prover.close()

    def test_an_open_site_is_refused_with_the_remedy(self):
        prover, _svc, report = session("birds_fly_open")
        try:
            dixi, = [e for e in report.entries if e.kind == "dixi"]
            message = " ".join(d.message for d in dixi.diagnostics)
            assert "not added to the document graph" in message
            assert "owes 'Wings x', which mentions x, bound by the argument" in message
            assert "presume or owe a closed one" in message
            assert prover.graph.edges == [] or "birds_fly" not in edges(prover)
        finally:
            prover.close()


def test_a_horn_using_a_bound_variable_stays_in_its_argument():
    """aida-fo-open-source-horn: the cell (x, p) * _F_ proves the closed
    exists x, P x, but from the bound x and p : P x; exposed, it owed the
    open P x and the strict argument was labelled OUT."""
    prover, svc, report = session("open_source_horn")
    try:
        assert report.ok
        assert edges(prover)["a"][1:] == ([], True)
        labels = svc.label(svc.target("a")).labellings
        assert [set(l.values()) for l in labels] == [{"IN"}]
    finally:
        prover.close()


def test_a_horn_using_a_hypothesis_about_a_bound_variable_stays_in_its_argument():
    """The cut of the closed claim under forall y uses n : Dr y -> false;
    exposed as a horn, its replay mentioned y unbound."""
    prover, _svc, report = session("hypothesis_horn")
    try:
        assert report.ok
        assert not any(name.startswith("a.") for name in edges(prover))
    finally:
        prover.close()


# -- evaluation ----------------------------------------------------------------------

EXPECTED = {
    # the rule is refuted strictly by the counter-instance pingu
    ("tweety", "flies_tweety"): "open",
    ("tweety", "penguin"): "value",
    ("birds_fly", "birds_fly"): "value",
    ("birds_fly", "wings"): "value",
    ("birds_fly", "tweety_flies"): "value",
    # gen is presumed and strictly refuted: the presumption meets a strict
    # refutation
    ("generalisation", "gen"): "exception",
    ("generalisation", "pc"): "open",
    ("generalisation", "refuted"): "value",
    ("witness", "exp"): "value",
    ("witness", "exq"): "value",
    # a horn using the bound x (or a hypothesis about it) stays in its
    # argument (aida-fo-open-source-horn): a is strict and a value
    ("open_source_horn", "a"): "value",
    # a stays evaluable; open, as its owed peirce has no argument
    ("hypothesis_horn", "a"): "open",
}


@pytest.mark.parametrize("pipeline", PIPELINES)
@pytest.mark.parametrize("name", FIXTURES)
def test_the_verdicts(name, pipeline):
    prover, svc, _report = session(name, pipeline)
    try:
        for (fixture, target), cls in EXPECTED.items():
            if fixture == name:
                for mode in ("skeptical", "credulous"):
                    assert evaluated(svc, target, mode)[0] == cls, (target, mode)
    finally:
        prover.close()


def test_the_universal_rule_substitutes_the_instance():
    """tweety_flies applies birds_fly's lambda x to tweety: the normal form
    holds birds_fly's derivation for tweety, its owed rule instantiated."""
    prover, svc, _report = session("birds_fly")
    try:
        cls, nf = evaluated(svc, "tweety_flies")
        assert cls == "value"
        assert "λx:Animal" not in nf
        assert "μb3:Wings tweety" in nf and ":Wings x" not in nf      # no node typed by the open formula
    finally:
        prover.close()


def test_the_existential_rule_substitutes_the_witness():
    prover, svc, _report = session("witness")
    try:
        cls, nf = evaluated(svc, "exq")
        assert cls == "value"
        assert nf.startswith("μexq:exists x:D,Q x.<(c,μb3:Q c.<pq:forall x:D,Pr x->Q x||c*pr_c:Pr c*b3:Q c>)")
    finally:
        prover.close()


# -- replay ----------------------------------------------------------------------------

@pytest.mark.parametrize("typecheck", ["on", "expanded"])
@pytest.mark.parametrize("name", FIXTURES)
def test_every_debate_replays_and_is_closed(name, typecheck):
    prover, svc, _report = session(name, "unfolded", typecheck)
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            for target in prover.arguments:
                found = svc.issue_term(svc.target(target), want_term=True)   # type-checked
                free = {n for _k, n in free_variables(found.term)
                        if n not in prover.declarations and n not in ("_F_", "_T_")}
                assert free == set(), target
    finally:
        prover.close()


# -- the two rules ------------------------------------------------------------------------

def test_the_forall_rule():
    # < lambda x. mu k:P x.<?:P x || k> || c * alpha >  -->  < mu k:P c.<?:P c || k> || alpha >
    body = Mu(ID("k", "P x"), "P x", Goal("1", "P x"), ID("k", "P x"))
    fired = []
    term, context = _step_command(LamdaFO("x", "D", body), ConsFO(TSym("c"), ID("alpha", "P c")),
                                  "cbn", fired)
    assert fired == ["(∀)"]
    assert term.prop == "P c" and term.term.prop == "P c"
    assert isinstance(context, ID) and context.name == "alpha"


def test_the_exists_rule():
    inner = Mutilde(DI("h", "P y"), "P y", DI("h", "P y"), ID("k", "P y"))
    fired = []
    term, context = _step_command(TermsPairFO(TSym("c"), DI("p", "P c")),
                                  DestructTermsPairFO("y", "D", inner), "cbn", fired)
    assert fired == ["(∃)"]
    assert isinstance(term, DI) and term.name == "p"
    assert context.prop == "P c" and context.context.prop == "P c"


def test_substitution_respects_shadowing_and_avoids_capture():
    shadowed = LamdaFO("x", "D", Goal("1", "P x"))
    assert substitute_fo(shadowed, "x", TSym("c")).term.prop == "P x"
    # [y/x] under lambda y: the binder is renamed, the substituted y stays free
    capturing = LamdaFO("y", "D", Goal("1", "R x y"))
    out = substitute_fo(capturing, "x", TSym("y"))
    assert out.var != "y" and out.term.prop == f"R y {out.var}"


@pytest.mark.parametrize("name, targets", [("birds_fly", ["birds_fly", "tweety_flies"]),
                                           ("witness", ["exp", "exq"]), ("tweety", ["penguin"])])
def test_every_value_replays_through_fellowship(name, targets):
    """A first-order normal form is a proof Fellowship rebuilds."""
    from core.dc.typecheck import typecheck
    prover, svc, _report = session(name)
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            for target in targets:
                result = svc.evaluate(svc.target(target))
                assert result.nf_class == "value"
                argument = prover.arguments[target]
                typecheck(prover, result.normal_form, f"nf_{target}", argument.conclusion,
                          argument.is_anti)
    finally:
        prover.close()
