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
    imported by the explicit environment.  Since 2026-10-22 it is a
    witness entry, the argument's own open choice, and no part of a
    statement that does not mention it: the argument's node is its closed
    conclusion (tasks.org, aida-referents-instantiation)."""
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
        assert edge.target_key == key("exists y:Pub, Dr y -> Dr y")
        assert prover.graph.shown(edge.target_key) == "exists y:Pub,Dr y->Dr y"
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
    """The referents a displayed term mentions; AIDA numbers its open
    sites ?1, ?2, ..., which no referent can be."""
    return referents.mentioned(shown)


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
            "[y?:Pub] ⊢ exists x:Pub,Q x"]
        # w alone owes its witness
        assert evaluated(svc, "w")[0] == "open"
    finally:
        prover.close()


def test_a_witness_scaffold_tries_the_variables_in_scope_innermost_on_top():
    """Since 2026-10-22 (tasks.org, aida-referents-witness-scaffold): w's
    open choice ?x is not lined up with a's variables; w is placed once,
    with a scaffold at exists x, Q x whose root is its own choice and whose
    alternatives are z and y, the innermost on top."""
    prover, svc, report = session((DISCOURSE / "witness_two.fspy").read_text())
    try:
        assert report.ok
        term = pres_str(svc.issue_term(svc.target("a"), want_term=True).term)   # type-checked
        assert term.count("μw:") == 1
        assert "(?x,?3:Q ?x)" in term                          # the root: w's own choice
        assert "(z,hz:Q z)" in term and "(y,hy:Q y)" in term
        assert term.index("(?x,") < term.index("(z,hz") < term.index("(y,hy")   # y on top
        cls, nf = evaluated(svc, "a")
        assert cls == "value" and "(y,hy:Q y)" in nf
        roles = [e.role for e in prover.graph.edges]
        assert "weakening" in roles                            # exists x, Q x to [z, y]
    finally:
        prover.close()


def test_an_open_choice_is_no_individual_for_another_argument():
    """v's ?o is its own open choice (a witness entry, 2026-10-22): no
    part of its closed conclusion and never lined up with a's variables.
    v proves the existential with c wherever the closed statement reaches,
    by weakening under a's binders too."""
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
        assert v.target_key == key("exists x:Pub, Q x")
        term = pres_str(svc.issue_term(svc.target("a"), want_term=True).term)
        assert "μv:exists x:Pub,Q x.<(c," in term and "?o" not in term
    finally:
        prover.close()


def test_closed_supporters_and_counterarguments_bear_on_a_context_by_weakening():
    prover, svc, report = session((DISCOURSE / "weakening.fspy").read_text())
    try:
        assert report.ok
        g = prover.graph
        a, = [e for e in g.edges if e.name == "a"]
        site = [s for s in a.sources if s.kind == "obligation"][0]
        assert g.shown(site.key) == "[y?:Pub] ⊢ Eff"
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
        assert "[y?:Pub] ⊢ exists x:Pub,Dr x->(forall y:Pub,Dr y)" in shown
        assert evaluated(svc, "a")[0] != "value"
    finally:
        prover.close()


# -- witness entries and instantiation (2026-10-22) --------------------------------

def statement_label(svc, target, key_text, side="term"):
    found = svc.label(svc.target(target))
    labels = found.labellings[0]
    return {found.graph.shown(k): v for (k, s), v in labels.items() if s == side}.get(key_text)


def test_a_statement_about_an_open_choice_is_its_arguments_own():
    """A witness entry is rigid and its argument's: interpretation1's
    farmer ?u is not he_does_not_bite_him's farmer ?x (tasks.org,
    aida-referents-unrelated)."""
    prover, _svc, report = session((DISCOURSE / "donkey.fspy").read_text())
    try:
        assert report.ok
        g = prover.graph
        farmers = {k for k, display in g.nodes.items() if display.startswith("farmer ?")}
        own = {k for k in farmers if "w#interpretation1:?u" in k}
        generic = {k for k in farmers if "r#" in k}        # he_does_not_bite_him's farmer ?x
        assert own and generic and not own & generic
    finally:
        prover.close()


def test_a_generic_argument_bears_on_an_instance_where_it_is_used():
    """he_does_not_bite_him is generic; interpretation1 owes bite ?u ?v,
    an instance of its contrary, so it attacks there - its presumptions
    read at ?u and ?v - and the instance is OUT.  The document graph copies
    the argument, with its parts, to the instance."""
    prover, svc, report = session((DISCOURSE / "donkey.fspy").read_text())
    try:
        g = prover.graph
        copies = [e for e in g.edges if e.name == "he_does_not_bite_him" and e.generic is not None]
        assert copies and all("w#interpretation1" in e.target_key for e in copies)
        assert any(e.name == "he_does_not_bite_him.c1" and e.generic is not None for e in g.edges)
        assert statement_label(svc, "interpretation1", "[?u:iota, ?v:iota] ⊢ bite ?u ?v") == "OUT"
        assert evaluated(svc, "interpretation1")[0] == "open"
    finally:
        prover.close()


def test_a_generic_presumption_reaches_each_instance_and_an_attack_one_only():
    prover, svc, report = session((DISCOURSE / "conrad_peter.fspy").read_text())
    try:
        assert report.ok
        assert statement_label(svc, "brawl", "beat peter conrad") == "IN"
        assert statement_label(svc, "brawl", "beat conrad peter") == "UNDEC"
        g = prover.graph
        assert (key("beat ?x ?y"), key("beat conrad peter")) in g.instantiations
        assert "presumption" in g.defaults[(key("beat peter conrad"), "term")]
    finally:
        prover.close()


def test_instances_form_a_lattice_of_nearest_generalisations():
    prover, svc, report = session((DISCOURSE / "beat_lattice.fspy").read_text())
    try:
        assert report.ok
        g = prover.graph
        G, H, K = key("beat ?x ?y"), key("beat ?x conrad"), key("beat peter ?y")
        I1, I2, I3 = key("beat peter conrad"), key("beat conrad conrad"), key("beat peter pedro")
        assert set(g.instantiations) == {(G, H), (G, K), (H, I1), (H, I2), (K, I1), (K, I3)}
        assert statement_label(svc, "fights", "beat peter pedro") == "IN"
        assert statement_label(svc, "fights", "beat peter conrad") == "UNDEC"
        assert statement_label(svc, "fights", "beat conrad conrad") == "UNDEC"
        dot = g.to_dot()                                   # the document graph
        assert 'label="instance"' in dot
    finally:
        prover.close()


def test_an_open_choice_is_never_an_alternative_for_another():
    """some_donkey under rally keeps its own choice ?w; the farmer ?u is
    no alternative; pedro is, once somebody argues donkey pedro."""
    prover, svc, report = session((DISCOURSE / "open_choice.fspy").read_text())
    try:
        assert report.ok
        term = pres_str(svc.issue_term(svc.target("rally"), want_term=True).term)   # type-checked
        assert "(?w,?3:donkey ?w)" in term and "(pedro," in term
        assert "(?u,?" not in term.split("μsome_donkey")[1]
        assert "donkey ?u" not in term
        assert evaluated(svc, "some_donkey")[0] == "value"
        assert statement_label(svc, "some_donkey", "exists y:Ind,donkey y") == "IN"
    finally:
        prover.close()


# -- the individuals of a sort, !x (2026-10-21/22) -------------------------------

SORTS = """lk.
pipeline unfolded.
declare iota : type.
declare farmer : iota -> bool.
"""


def test_a_presumed_choice_presumes_its_sort_has_an_individual():
    """!x presumes exists x:iota, true (the sort node) and is a value
    where nothing catches it; ?x owes its individual and is open."""
    prover, svc, report = session((DISCOURSE / "sort_node.fspy").read_text())
    try:
        assert report.ok
        g = prover.graph
        some, = [e for e in g.edges if e.name == "some_farmer"]
        assert (key("exists x:iota, true"), "term", "presumption") in {
            (s.key, s.side, s.kind) for s in some.sources}
        assert "presumption" in g.defaults[(key("exists x:iota, true"), "term")]
        assert evaluated(svc, "some_farmer")[0] == "value"
        assert statement_label(svc, "some_farmer", "exists x:iota,farmer x") == "IN"
    finally:
        prover.close()
    prover, svc, report = session(SORTS + """argument some_farmer_owed : [?x:iota] (exists x:iota, farmer [x]).
elim [?x].
by default.
dixi.
""")
    try:
        assert report.ok
        assert evaluated(svc, "some_farmer_owed")[0] == "open"
        assert statement_label(svc, "some_farmer_owed", "exists x:iota,farmer x") == "OUT"
        assert statement_label(svc, "some_farmer_owed", "a witness for ?x of some_farmer_owed") == "OUT"
    finally:
        prover.close()


def test_a_declared_constant_proves_its_sort_and_is_an_alternative():
    prover, svc, report = session(SORTS + """declare pedro : iota.
argument fp : farmer [pedro].
by default.
dixi.
argument some_farmer_owed : [?x:iota] (exists x:iota, farmer [x]).
elim [?x].
by default.
dixi.
argument any : [!y:iota] (exists x:iota, farmer [x]).
elim [!y].
by default.
dixi.
""")
    try:
        assert report.ok
        g = prover.graph
        sort = key("exists x:iota, true")
        assert any(e.name == "pedro" and e.target_key == sort and e.strict and not e.sources
                   for e in g.edges)
        cls, nf = evaluated(svc, "some_farmer_owed")
        assert cls == "value" and "(pedro," in nf               # the alternative pedro decides
        assert statement_label(svc, "some_farmer_owed", "exists x:iota,farmer x") == "IN"
    finally:
        prover.close()


def test_a_referent_presumed_in_a_statement_is_a_referent():
    from core.ac.prop import Prop
    assert referents.is_referent("!x") and referents.is_presumed("!x")
    assert Prop.parse("farmer !x").referent_order() == ["!x"]
    assert key("farmer !x") == key("farmer ?y")                 # matching ignores the sigil


# -- counter-instances (step G, settled 2026-10-22) --------------------------------

def test_a_counter_instance_defeats_the_instance_and_the_closed_universal_only():
    """not_c refutes P c strictly: the instance P c is OUT, the generic
    P ?z and a's site under its binder stay IN; no refutes a's closed
    universal at its own choice, which the witness scaffold settles at c,
    and a falls as one."""
    prover, svc, report = session((DISCOURSE / "counter_instance.fspy").read_text())
    try:
        assert report.ok
        assert statement_label(svc, "no", "P c") == "OUT"
        assert statement_label(svc, "no", "[?z:iota] ⊢ P ?z") == "IN"
        assert statement_label(svc, "a", "forall x:iota,P x") == "OUT"
        cls, nf = evaluated(svc, "no")
        assert cls == "value" and "c*" in nf and "?c" not in nf
        term = pres_str(svc.issue_term(svc.target("a"), want_term=True).term)   # type-checked
        assert "?c*" in term and "c*μ'alt" in term
    finally:
        prover.close()


def test_a_counterargument_of_the_sort_node_contests_a_presumed_choice():
    text = (DISCOURSE / "sort_node.fspy").read_text() + """counterargument empty : (exists x:iota, true).
by default.
dixi.
"""
    prover, svc, report = session(text)
    try:
        assert report.ok
        assert statement_label(svc, "some_farmer", "exists x:iota,true") == "UNDEC"
        assert statement_label(svc, "some_farmer", "exists x:iota,farmer x") == "UNDEC"
        assert evaluated(svc, "some_farmer")[0] == "open"
    finally:
        prover.close()


def test_a_concluded_referent_statement_is_shown_as_its_argument_states_it():
    """tasks.org, aida-referents-node-names: a's site under z registers
    Dr z -> false first; sup concludes it, and the node and its witness
    read as sup names them."""
    prover, svc, report = session((DISCOURSE / "peirce_fo.fspy").read_text())
    try:
        found = svc.graph(svc.target("sup")).graph
        shown = {found.shown(k) for k, _s in found.statements()}
        assert "[?x:Pub] ⊢ Dr ?x->false" in shown
        assert "a witness for ?x" in shown
        assert not any("?z" in text or "z?" in text for text in shown)
    finally:
        prover.close()


# -- anaphora (2026-10-22) ---------------------------------------------------------

def test_a_pronoun_resolves_to_the_latest_antecedent_alone():
    """tasks.org, aida-referents-anaphora: he_whistles tries the plumber
    and the knight, the knight on top, and keeps the knight's reading only;
    that reading is about knight_walks's own choice."""
    prover, svc, report = session((DISCOURSE / "anaphora.fspy").read_text())
    try:
        assert report.ok
        term = pres_str(svc.issue_term(svc.target("he_whistles"), want_term=True).term)
        assert "(?c," in term and "(!a,!" in term and "(!a_2,!" in term
        cls, nf = evaluated(svc, "he_whistles")
        assert cls == "value" and "(!a_2,!IN:whistles !a_2)" in nf and "whistles !a)" not in nf
        graph = svc.label(svc.target("he_whistles")).graph
        assert any("w#knight_walks:!a" in k and "whistles" in k for k, _s in graph.statements())
    finally:
        prover.close()


def test_a_resolution_supports_nothing_outside_the_pronouns_argument():
    """The readings' statements are no nodes of the document: nothing else
    can use "the knight whistles" (aida-referents-anaphora-export)."""
    prover, _svc, report = session((DISCOURSE / "anaphora.fspy").read_text())
    try:
        assert report.ok
        assert not any("whistles" in k and "w#knight_walks" in k for k in prover.graph.nodes)
    finally:
        prover.close()


def test_a_contested_reading_moves_the_resolution_to_the_next():
    prover, svc, report = session((DISCOURSE / "anaphora_silent.fspy").read_text())
    try:
        assert report.ok
        cls, nf = evaluated(svc, "he_whistles")
        assert cls == "value" and "(!a,!IN:whistles !a)" in nf
    finally:
        prover.close()


ANAPHORA = """lk.
pipeline unfolded.
declare iota : type.
declare man,walks,whistles : iota -> bool.
declare R : iota -> iota -> bool.
"""


def test_a_pronoun_without_antecedent_owes_its_individual():
    _prover, svc, report = session(ANAPHORA + """
argument alone : [?c:iota](exists x:iota, whistles [x]). elim [?c]. by default. dixi.
""")
    try:
        assert report.ok
        cls, nf = evaluated(svc, "alone")
        assert cls == "open" and "?OUT" in nf
    finally:
        _prover.close()


def test_an_indefinite_is_identified_only_where_its_statement_is_argued():
    """!w keeps its own individual unless its statement is argued of an
    earlier one: the knight is no plumber, the second man walking may be
    the first; the first never takes the second's."""
    prover, svc, report = session((DISCOURSE / "anaphora.fspy").read_text())
    try:
        assert report.ok
        assert "(!a,!IN:knight !a-~walks !a)" in evaluated(svc, "knight_walks")[1]
    finally:
        prover.close()
    prover, svc, report = session(ANAPHORA + """
argument first : [!a:iota](exists x:iota, man [x] - ~walks [x]). elim [!a]. by default. dixi.
argument second : [!b:iota](exists x:iota, man [x] - ~walks [x]). elim [!b]. by default. dixi.
""")
    try:
        assert report.ok
        assert "(!a,!IN:man !a-~walks !a)" in evaluated(svc, "second")[1]
        term = pres_str(svc.issue_term(svc.target("first"), want_term=True).term)
        assert "(!b," not in term.split("μfirst:")[1].split("||first:")[0]
    finally:
        prover.close()


def test_the_choice_of_an_argument_with_parameters_is_no_antecedent():
    """tasks.org, aida-referents-choice-under-parameter: sup's !w is one
    individual for all its instances, so it is offered to nobody."""
    prover, svc, report = session(ANAPHORA + """
argument sup : [!x:iota, !w:iota] (exists y:iota, R [!x] [y]). elim [!w]. by default. dixi.
argument he : [?c:iota](exists x:iota, whistles [x]). elim [?c]. by default. dixi.
""")
    try:
        assert report.ok
        assert "!w" not in pres_str(svc.issue_term(svc.target("he"), want_term=True).term)
        assert evaluated(svc, "he")[0] == "open"
    finally:
        prover.close()


BINDER = ANAPHORA + """
argument all_r : (forall z:iota, exists y:iota, R z y). elim z. next. dixi.
"""


def test_a_binder_captures_a_pronoun_but_not_an_indefinite():
    """Under forall z, the !w choice is offered z and keeps its own
    individual (R z z is argued by nobody); a ?w choice's by default is
    about whoever answers, so z answers: bound anaphora."""
    prover, svc, report = session(BINDER + """
argument sup : [!x:iota, !w:iota] (exists y:iota, R [!x] [y]). elim [!w]. by default. dixi.
""")
    try:
        assert report.ok
        assert "(!w,!IN:R z !w)" in evaluated(svc, "all_r")[1]
        found = svc.label(svc.target("all_r"))
        # every labelled statement is one the issue graph shows (the label
        # display failed on the losing alternative's R z z)
        assert all(k in found.graph.nodes for (k, _s) in found.labellings[0])
    finally:
        prover.close()
    prover, svc, report = session(BINDER + """
argument sup : [!x:iota, ?w:iota] (exists y:iota, R [!x] [y]). elim [?w]. by default. dixi.
""")
    try:
        assert report.ok
        assert "(z,!IN:R z z)" in evaluated(svc, "all_r")[1]
    finally:
        prover.close()


def test_two_pronouns_each_get_a_scaffold_and_owed_sites_steer_them():
    """A pronoun in an inner pair gets its witness scaffold (its statement
    is read off the outer one, core/comp/enrich.py), and the outer reading
    rests on the inner statement, not on the inner root's witness. Bare,
    both pronouns take the latest antecedent; owing farmer ?c and donkey ?d
    steers them (tasks.org, aida-referents-anaphora, 2026-10-23)."""
    prover, svc, report = session((DISCOURSE / "anaphora_two.fspy").read_text())
    try:
        assert report.ok
        cls, nf = evaluated(svc, "he_bites_him")
        assert cls == "value" and "(!b,(!b,!IN:bites !b !b))" in nf
        cls, nf = evaluated(svc, "he_bites_it")       # replays through Fellowship
        assert cls == "value"
        assert "(!a,(!b," in nf and "!IN:bites !a !b" in nf and "!IN:farmer !a" in nf
        assert "?d" not in nf and "?c" not in nf
    finally:
        prover.close()


def shown_labels(svc, target):
    """Labels by statement and side, the statement without its context."""
    with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        found = svc.label(svc.target(target))
    return {(found.graph.shown(k).split(" ⊢ ")[-1], s): v
            for (k, s), v in found.labellings[0].items()}


SELF_BITING = "(!donkey,(!farmer,!IN:bites !donkey !farmer))"


def test_a_generic_presumption_contests_a_reading():
    """no_self_biting presumes the contrary of bites !z !z; the reading
    bites !donkey !donkey is no node of the document, and takes that
    presumption from the generic statement covering it (fix (a), tasks.org,
    aida-referents-anaphora-knowledge). Two presumptions: UNDEC; the second
    pronoun falls back to the farmer. Without it, both are the donkey
    (anaphora_two.fspy)."""
    prover, svc, report = session((DISCOURSE / "self_biting.fspy").read_text())
    try:
        assert report.ok
        got = shown_labels(svc, "he_bites_him")
        assert got[("bites !donkey !donkey", "term")] == "UNDEC"
        assert got[("bites !donkey !donkey", "context")] == "UNDEC"
        assert got[("bites !farmer !farmer", "context")] == "UNDEC"
        assert got[("bites !donkey !farmer", "term")] == "IN"
        assert got[("bites !farmer !donkey", "term")] == "IN"
        assert got[("exists x:iota,exists y:iota,bites x y", "term")] == "IN"
        cls, nf = evaluated(svc, "he_bites_him")
        assert cls == "value" and SELF_BITING in nf
    finally:
        prover.close()


def test_an_argued_rule_defeats_a_reading():
    """self_biting_is_odd derives the attack from a premise it presumes;
    placed at a reading, the instance keeps that presumption (fix (b)), and
    a derivation defeats the reading's bare presumption."""
    prover, svc, report = session((DISCOURSE / "self_biting_argued.fspy").read_text())
    try:
        assert report.ok
        got = shown_labels(svc, "he_bites_him")
        assert got[("bites !donkey !donkey", "term")] == "OUT"
        assert got[("bites !donkey !donkey", "context")] == "IN"
        assert got[("~bites !donkey !donkey", "term")] == "IN"
        assert got[("bites !farmer !farmer", "term")] == "OUT"
        assert got[("bites !donkey !farmer", "term")] == "IN"
        cls, nf = evaluated(svc, "he_bites_him")
        assert cls == "value" and SELF_BITING in nf
    finally:
        prover.close()


@pytest.mark.parametrize("fixture", ["self_biting.fspy", "self_biting_argued.fspy"])
def test_a_reading_meets_a_rule_as_a_stated_instance_does(fixture):
    """The labels of the reading bites !donkey !donkey are those of a closed
    bites pedro pedro the document holds, presumed and met by the same
    rule (the document instantiates the rule there itself)."""
    text = (DISCOURSE / fixture).read_text()
    rule = text[text.index("counterargument"):]
    stated = ("lk.\npipeline unfolded.\ndeclare iota : type.\ndeclare pedro : iota.\n"
              "declare bites : iota -> iota -> bool.\n"
              "argument pedro_bites_himself : bites [pedro] [pedro].\nby default.\ndixi.\n" + rule)
    prover, svc, report = session(text)
    try:
        assert report.ok
        got = shown_labels(svc, "he_bites_him")
        reading = {side: got[("bites !donkey !donkey", side)] for side in ("term", "context")}
    finally:
        prover.close()
    prover, svc, report = session(stated)
    try:
        assert report.ok
        got = shown_labels(svc, "pedro_bites_himself")
        assert {side: got[("bites pedro pedro", side)] for side in ("term", "context")} == reading
    finally:
        prover.close()
