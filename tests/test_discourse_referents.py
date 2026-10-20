"""Discourse referents (tasks.org, aida-discourse-referents).

A referent ``?x`` is a pseudo-constant local to an argument: Fellowship
holds it in the argument's goal environment (given as ``[?x:Pub]`` or
collected from the statement), never in the signature, and refuses ``qed``
on a theorem that has one - no referent is ever registered on the prover
side.  AIDA, for now, also refuses a referent the conclusion does not
state (deferred by design).
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


def test_a_referent_the_conclusion_does_not_state_is_refused():
    """Deferred (the author, 2026-10-20): a referent outside the conclusion
    would be caught by a capture, not by the statement."""
    text = HEAD + """argument w : [?x:Pub] (exists y:Pub, Dr [y] -> Dr [y]).
elim [?x].
elim p.
axiom p.
dixi.
"""
    prover, _svc, report = session(text)
    try:
        closing = entry(report, "dixi")
        assert closing.status in ("refused", "error")
        assert "does not state it" in closing.message
        assert "w" not in prover.arguments
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
        assert nf.startswith("μalt5:exists x:Pub,Dr x->(forall y:Pub,Dr y).<(c,λ")
        assert ".<(y,λ" in nf and not referents_in(nf)
    finally:
        prover.close()


def test_the_shared_route_refuses_a_referent_debate_with_a_pointer():
    prover, svc, _report = session((DISCOURSE / "peirce_fo.fspy").read_text(), pipeline="shared")
    try:
        with pytest.raises(Exception, match="pipeline unfolded"):
            evaluated(svc, "a")
    finally:
        prover.close()
