"""Discourse referents (tasks.org, aida-discourse-referents).

A referent ``?x`` is a pseudo-constant local to an argument: Fellowship
holds it in the argument's goal environment (given as ``[?x:Pub]`` or
collected from the statement), never in the signature, and refuses ``qed``
on a theorem that has one - no referent is ever registered on the prover
side.  A referent the conclusion does not state is imported by an
explicit environment (a witness-only referent).
"""

import re
import warnings
from pathlib import Path

import pytest

from core.ac import referents
from pres.gen import pres_str
from wrap.cli import setup_prover
from wrap.service import Service
from wrap.syntax import parse

HEAD = """lk.
declare Pub : type.
declare c : Pub.
declare Dr : Pub -> bool.
"""


def session(text, pipeline="unfolded"):
    prover = setup_prover()
    svc = Service.of(prover)
    svc.set_pipeline(pipeline)
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        report = svc.check(text)
    return prover, svc, report


def entry(report, kind):
    return [e for e in report.entries if e.kind == kind][-1]


def fellowship(prover, command):
    """Fellowship's own verdict on a command: its notes and errors."""
    try:
        out = prover.send_command(command)
    except Exception as e:
        return str(e)
    return " ".join(str(n) for n in out.get("errors", []) + out.get("notes", []))


# -- the written form --------------------------------------------------------------

def test_a_referent_is_a_name_with_a_question_mark():
    assert referents.is_referent("?x") and not referents.is_referent("x")
    assert not referents.is_referent("?1")                  # an open goal
    assert referents.mentioned("R ?x (f ?y) c ?1.2") == {"?x", "?y"}


def test_the_parser_reads_a_referent_environment():
    a = parse("argument sup : [?x:Pub, ?y] (Dr [?x] -> false)")
    assert a["referents"] == "?x:Pub, ?y" and a["conclusion"] == "Dr [?x] -> false"
    b = parse("argument sup : (Dr [?x] -> false)")
    assert b["referents"] is None and b["conclusion"] == "Dr [?x] -> false"


def test_the_proposition_grammar_reads_a_referent_as_a_term():
    from core.ac.prop import Prop
    assert Prop.parse("Dr ?x -> false").free_vars() == {"?x"}


# -- Fellowship is the source of truth ----------------------------------------------

def test_fellowship_holds_a_referent_in_the_goal_environment_and_refuses_qed():
    prover = setup_prover()
    try:
        for line in HEAD.splitlines():
            prover.send_command(line)
        assert fellowship(prover, "theorem t : (Dr [?x] -> Dr [?x]).") == ""
        prover.send_command("elim p.")
        prover.send_command("axiom p.")
        refusal = fellowship(prover, "qed.")
        assert "discourse referent ?x" in refusal and "cannot be registered" in refusal
        prover.send_command("discard theorem.")
        assert "t" not in prover.declarations
    finally:
        prover.close()


@pytest.mark.parametrize("command, says", [
    ("declare ax : (Dr [?x]).", "Symbol ?x undefined"),
    ("declare ?x : Pub.", "Parse error"),
    ("theorem k : [?x:Pub -> Pub] (Dr [?x]).", "used with the sort"),
    ("theorem k : [?y:Pub] (Dr [?x]).", "missing from the theorem's referent environment"),
    ("theorem k : [?w] (Dr [c]).", "cannot be inferred"),
    ("theorem k : [?p:Pub -> bool] (Dr [c]).", "not a proposition or a sort"),
    ("theorem k : [?x:Nope] (Dr [c]).", "Symbol Nope undefined"),
])
def test_fellowship_refuses_what_would_register_or_mistype_a_referent(command, says):
    prover = setup_prover()
    try:
        for line in HEAD.splitlines():
            prover.send_command(line)
        assert says in fellowship(prover, command)
    finally:
        prover.close()


# -- arguments with referents -------------------------------------------------------

SUP = """argument sup : (Dr [?x] -> false).
elim p.
cut (Dr [?x]) m.
axiom p.
by default.
"""


def test_an_argument_stating_a_referent_ends_with_dixi():
    prover, _svc, report = session(HEAD + SUP + "dixi.\n")
    try:
        assert entry(report, "dixi").status == "ok", entry(report, "dixi").message
        assert "sup" in prover.arguments
        assert not prover.arguments["sup"].citable          # never a theorem
        assert "sup" not in prover.declarations
    finally:
        prover.close()


def test_an_explicit_environment_reaches_fellowship():
    text = HEAD + "argument sup : [?x:Pub] (Dr [?x] -> false).\n" + SUP.split("\n", 1)[1] + "dixi.\n"
    prover, _svc, report = session(text)
    try:
        assert entry(report, "dixi").status == "ok", entry(report, "dixi").message
        assert prover.arguments["sup"].referents == "?x:Pub"
    finally:
        prover.close()


def test_qed_on_an_argument_with_a_referent_is_refused_by_fellowship():
    text = HEAD + "argument t : (Dr [?x] -> Dr [?x]).\nelim p.\naxiom p.\nqed.\n"
    prover, _svc, report = session(text)
    try:
        closing = entry(report, "qed")
        assert closing.status in ("refused", "error")
        assert "discourse referent ?x" in closing.message
        assert "t" not in prover.arguments and "t" not in prover.declarations
    finally:
        prover.close()


def test_a_witness_only_referent_is_imported_through_the_environment():
    """Lifted 2026-10-20: a referent the conclusion does not state is
    imported by the explicit environment, and the argument's node is its
    statement in that context."""
    text = HEAD + """argument w : [?x:Pub] (exists y:Pub, Dr [y] -> Dr [y]).
elim [?x].
elim p.
axiom p.
dixi.
"""
    prover, _svc, report = session(text)
    try:
        assert entry(report, "dixi").status == "ok", entry(report, "dixi").message
        assert prover.arguments["w"].referent_env == [("?x", "Pub")]
        edge = [e for e in prover.graph.edges if e.name == "w"][0]
        assert edge.target_key.startswith("[Pub]⊢")
        assert prover.graph.shown(edge.target_key).startswith("[?x:Pub] ⊢ exists y:Pub")
    finally:
        prover.close()


# -- keys up to renaming of referents (decision (a)) -------------------------------

def key(text):
    from core.dc.debate_graph import canonical_prop
    return canonical_prop(text)


def test_a_statement_is_identified_up_to_renaming_of_its_referents():
    assert key("Dr ?x -> false") == key("Dr ?w -> false")
    assert key("P ?a ?b") == key("P ?b ?a")


def test_identity_does_not_instantiate():
    """P z z is an instance of P ?a ?b, not the same statement: the author
    writes the bridge (AIDA is not an automated prover)."""
    assert key("P ?a ?b") != key("P ?c ?c")
    assert key("Dr ?x") != key("Dr c")


def test_a_referent_is_neither_a_bound_variable_nor_a_constant():
    assert key("exists y:Pub, R ?x y") != key("exists y:Pub, R y y")
    assert key("Dr ?x") != key("Dr x")


def test_non_atomic_terms_and_function_referents():
    assert key("P (f ?x)") == key("P (f ?y)") != key("P ?x")
    assert key("Q (?g c)") == key("Q (?h c)") != key("Q (?g d)")


def test_the_referent_order_lines_two_statements_up():
    from core.ac.prop import Prop
    order = Prop.parse("R ?b (f ?a) ?b").referent_order()
    assert order == ["?b", "?a"]


# -- unfolding: substitute, then capture (step 4) ----------------------------------

DISCOURSE = Path(__file__).parent / "discourse"


def referents_in(shown):
    """The referents a displayed term mentions; AIDA displays its open
    sites as ?u1, ?u2, ..."""
    return {r for r in referents.mentioned(shown) if not re.fullmatch(r"\?u\d+", r)}


def evaluated(svc, target):
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        result = svc.evaluate(svc.target(target))
    return result.nf_class, pres_str(result.normal_form)


def test_first_order_peirce_is_a_debate():
    """a owes Dr z -> false under forall z; sup fills it with ?x := z, and
    a's g : Dr z captures sup's presumed (Dr z)⊥ - propositional Peirce at
    z, a value."""
    prover, svc, report = session((DISCOURSE / "peirce_fo.fspy").read_text())
    try:
        assert report.ok
        cls, nf = evaluated(svc, "a")
        assert cls == "value"
        assert "μsup:Dr z->false.<λp:Dr z.μm:false.<p:Dr z||g:Dr z>||sup:Dr z->false>" in nf
        assert not referents_in(nf)
    finally:
        prover.close()


@pytest.mark.parametrize("typecheck", ["on", "expanded"])
def test_the_instantiated_debate_replays(typecheck):
    prover, svc, _report = session((DISCOURSE / "peirce_fo.fspy").read_text())
    try:
        svc.set_typecheck(typecheck)
        found = svc.issue_term(svc.target("a"), want_term=True)      # type-checked
        assert not referents_in(pres_str(found.term))
    finally:
        prover.close()


def test_every_supporter_is_instantiated_by_its_own_referents():
    """A second supporter states the same referent statement with ?w: both
    are read at z where they fill a's site."""
    text = (DISCOURSE / "peirce_fo.fspy").read_text() + """argument sup2 : (Dr [?w] -> false).
elim q.
cut (Dr [?w]) n.
axiom q.
by default.
dixi.
"""
    prover, svc, report = session(text)
    try:
        assert report.ok
        svc.set_typecheck("off")
        term = pres_str(svc.issue_term(svc.target("a"), want_term=True).term)
        assert "λp:Dr z" in term and "λq:Dr z" in term and not referents_in(term)
        assert evaluated(svc, "a")[0] == "value"
    finally:
        prover.close()


def test_the_drinker_is_a_debate_that_presumes_nothing_classical():
    prover, svc, report = session((DISCOURSE / "drinker_referent.fspy").read_text())
    try:
        assert report.ok
        cls, nf = evaluated(svc, "a")
        assert cls == "value"
        # (c, λh. λy. μk.<(y, λd. μm.<d || k>) || a>), up to names
        assert re.match(r"μ\w+:exists x:Pub,Dr x->\(forall y:Pub,Dr y\)\.<\(c,λh:Dr c\.λy:Pub\.μ", nf), nf
        assert ".<(y,λ" in nf and not referents_in(nf)
    finally:
        prover.close()


def test_the_shared_route_refuses_a_referent_debate_with_a_pointer():
    prover, svc, _report = session((DISCOURSE / "peirce_fo.fspy").read_text(), pipeline="shared")
    try:
        svc.set_pipeline("shared")                  # the fixture asks for unfolded
        with pytest.raises(Exception, match="pipeline unfolded"):
            evaluated(svc, "a")
    finally:
        prover.close()


# -- an uncaught use (decision (c)), step 5 ----------------------------------------

def test_a_referent_target_owes_a_witness_and_is_open():
    """sup alone: nothing above its root catches ?x, so the witness is an
    unmet obligation; the label and the evaluation agree."""
    prover, svc, report = session((DISCOURSE / "peirce_fo.fspy").read_text())
    try:
        assert report.ok
        assert evaluated(svc, "sup")[0] == "open"
        issue = svc.issue_term(svc.target("sup"), want_term=False).issue
        assert svc.label(svc.target("sup")).labellings[0][issue] == "OUT"
        graph = svc.graph(svc.target("sup")).graph
        assert any(s.key.startswith("a witness for ?") for e in graph.edges for s in e.sources)
        # at a's use the referent is caught: a is still a value and IN
        assert evaluated(svc, "a")[0] == "value"
        issue = svc.issue_term(svc.target("a"), want_term=False).issue
        assert svc.label(svc.target("a")).labellings[0][issue] == "IN"
    finally:
        prover.close()


def test_a_referent_argument_without_sites_is_not_strict_as_a_target():
    """Strict at a caught use, but as its own target it owes the witness."""
    text = HEAD + "argument t : (Dr [?x] -> Dr [?x]).\nelim p.\naxiom p.\ndixi.\n"
    prover, svc, report = session(text)
    try:
        assert report.ok
        assert evaluated(svc, "t")[0] == "open"
        issue = svc.issue_term(svc.target("t"), want_term=False).issue
        assert svc.label(svc.target("t")).labellings[0][issue] != "IN"
    finally:
        prover.close()


def test_an_uncaught_referent_makes_a_normal_form_open():
    from core.comp.oracle_terms import uncaught_referents
    prover, svc, _report = session((DISCOURSE / "peirce_fo.fspy").read_text())
    try:
        svc.set_typecheck("off")
        term = svc.issue_term(svc.target("sup"), want_term=True).term
        assert uncaught_referents(term)
        assert not uncaught_referents(svc.issue_term(svc.target("a"), want_term=True).term)
    finally:
        prover.close()


def test_a_presumption_about_a_referent_holds_only_given_a_witness():
    """sup's presumed (Dr ?x)⊥ as its own target: a presumption about
    nobody - not IN, and open."""
    prover, svc, report = session((DISCOURSE / "peirce_fo.fspy").read_text())
    try:
        assert report.ok
        target = svc.target("issue Dr ?x:")
        assert evaluated(svc, "issue Dr ?x:")[0] == "open"
        issue = svc.issue_term(target, want_term=False).issue
        assert svc.label(target).labellings[0][issue] == "OUT"
    finally:
        prover.close()


# -- fixes to the instantiation, step 5a -------------------------------------------

def test_a_supporters_own_binder_is_renamed_where_its_name_is_taken():
    """sup proves a closed lemma under its own z; at a's use it goes under
    a's z: its z is renamed, the lemma stays a node (k contests it) and
    the whole term replays."""
    prover, svc, report = session((DISCOURSE / "peirce_fo_lemma.fspy").read_text())
    try:
        assert report.ok
        term = pres_str(svc.issue_term(svc.target("a"), want_term=True).term)   # type-checked
        assert "λz_2:Pub.λq:Dr z_2.q:Dr z_2" in term
        assert not referents_in(term)
        graph = svc.graph(svc.target("a")).graph
        assert any(e.name == "sup.λ1" for e in graph.edges)          # the lemma, a node
        assert evaluated(svc, "a")[0] == "value"
    finally:
        prover.close()


def test_a_horn_using_an_unstated_referent_is_no_horn():
    """sup's other branch uses p : Dr ?x but states the lemma's contrary,
    a closed statement: it argues about ?x, so it is no attacker of the
    closed lemma (until a node carries its context)."""
    prover, svc, report = session((DISCOURSE / "peirce_fo_lemma.fspy").read_text())
    try:
        assert report.ok
        lemma = [e for e in prover.graph.edges if e.name == "sup.λ1"][0]
        against = [e for e in prover.graph.edges
                   if e.target_key == lemma.target_key and e.target_side == "context"]
        # the lemma is stated in sup's context [?x]; k presumes it false in
        # none, which bears on it by weakening (a default marker on the
        # plain statement)
        assert [e.role for e in against] == ["weakening"]
        plain_key = against[0].sources[0].key
        assert "presumption" in prover.graph.defaults[(plain_key, "context")]
    finally:
        prover.close()


def test_a_counterargument_of_a_referent_statement_is_instantiated_at_the_use():
    text = (DISCOURSE / "peirce_fo.fspy").read_text() + """counterargument c : (Dr [?x] -> false).
elim.
by default.
dixi.
"""
    prover, svc, report = session(text)
    try:
        assert report.ok
        term = pres_str(svc.issue_term(svc.target("a"), want_term=True).term)   # type-checked
        assert "c:Dr z->false" in term and not referents_in(term)
        graph = svc.graph(svc.target("a")).graph
        assert any(e.name == "c" and e.target_side == "context" and "r#" in e.target_key
                   for e in graph.edges)
        assert evaluated(svc, "a")[0] == "value"     # c owes false's context: it fails
    finally:
        prover.close()


# -- export, import and weakening, step 5b -----------------------------------------

def test_a_witness_only_referent_is_lined_up_with_the_context():
    """a leaves exists x, Q x open under y with h : Q y; w imports ?x and
    owes Q ?x: at a's use ?x stands for y and h captures Q y - the
    intuitionistic proof, a value."""
    prover, svc, report = session((DISCOURSE / "witness_import.fspy").read_text())
    try:
        assert report.ok
        cls, nf = evaluated(svc, "a")
        assert cls == "value"
        assert re.search(r"λy:Pub\.λh:Q y\.μw:exists x:Pub,Q x\.<\(y,h:Q y\)\|\|w", nf), nf
        issue = svc.issue_term(svc.target("a"), want_term=False).issue
        assert svc.label(svc.target("a")).labellings[0][issue] == "IN"
        a, = [e for e in prover.graph.edges if e.name == "a"]
        assert [prover.graph.shown(s.key) for s in a.sources] == [
            "[?y:Pub] ⊢ exists x:Pub,Q x"]
        # w alone owes its witness
        assert evaluated(svc, "w")[0] == "open"
    finally:
        prover.close()


def test_a_supporter_is_used_once_per_lineup_innermost_on_top():
    prover, svc, report = session((DISCOURSE / "witness_two.fspy").read_text())
    try:
        assert report.ok
        term = pres_str(svc.issue_term(svc.target("a"), want_term=True).term)   # type-checked
        assert "μw:exists x:Pub,Q x.<(z,hz:Q z)" in term
        assert "μw_2:exists x:Pub,Q x.<(y,hy:Q y)" in term
        assert term.index("(z,hz") < term.index("(y,hy")      # y's use outermost: on top
        cls, nf = evaluated(svc, "a")
        assert cls == "value" and "(y,hy:Q y)" in nf
        roles = [e.role for e in prover.graph.edges]
        assert "weakening" in roles                            # [?x] to [?z, ?y]
    finally:
        prover.close()


def test_a_witness_of_another_sort_does_not_line_up():
    text = (DISCOURSE / "witness_two.fspy").read_text() + """declare Other : type.
declare c : Pub.
argument v : [?o:Other] (exists x:Pub, Q [x]).
elim [c].
dixi.
"""
    prover, svc, report = session(text)
    try:
        assert report.ok
        v, = [e for e in prover.graph.edges if e.name == "v"]
        assert v.target_key.startswith("[Other]⊢")
        assert not any(e.role == "weakening" and e.sources[0].key == v.target_key
                       for e in prover.graph.edges)
        term = pres_str(svc.issue_term(svc.target("a"), want_term=True).term)
        assert "μv" not in term
    finally:
        prover.close()


def test_closed_supporters_and_counterarguments_bear_on_a_context_by_weakening():
    prover, svc, report = session((DISCOURSE / "weakening.fspy").read_text())
    try:
        assert report.ok
        g = prover.graph
        a, = [e for e in g.edges if e.name == "a"]
        site = [s for s in a.sources if s.kind == "obligation"][0]
        assert g.shown(site.key) == "[?y:Pub] ⊢ Eff"
        weak = {(e.target_side, g.shown(e.sources[0].key)) for e in g.edges
                if e.role == "weakening" and e.target_key == site.key}
        assert weak == {("term", "Eff"), ("context", "Eff")}
        term = pres_str(svc.issue_term(svc.target("a"), want_term=True).term)   # type-checked
        assert term.index("λy:Pub") < term.index("μe:Eff") and "μ'c:Eff" in term
        assert evaluated(svc, "a")[0] == "open"        # Rain and Strike: undecided
        dot = svc.graph(svc.target("a")).graph.to_dot()
        assert 'style=dotted' in dot and 'label="weakening"' in dot
    finally:
        prover.close()


def test_the_drinker_split_supplies_its_own_omission():
    """The decision of 2026-10-20: with weakening, the claim reaches A's
    site under y, so the split is not a form AIDA offers."""
    prover, svc, report = session((DISCOURSE / "drinker_split.fspy").read_text())
    try:
        a, = [e for e in prover.graph.edges if e.name == "a"]
        shown = [prover.graph.shown(s.key) for s in a.sources]
        assert "[?y:Pub] ⊢ exists x:Pub,Dr x->(forall y:Pub,Dr y)" in shown
        assert evaluated(svc, "a")[0] != "value"
    finally:
        prover.close()
