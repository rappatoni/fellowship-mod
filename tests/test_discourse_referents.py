"""Discourse referents (tasks.org, aida-discourse-referents).

A referent ``?x`` is declared by ``referent x : SORT.`` and is the
constant ``referent_x`` below the script.  An argument that mentions one
holds for whatever the referent stands for where it is used, so it ends
with ``dixi``, never ``qed``, and it must state the referent it uses.
"""

import warnings

import pytest

from core.ac import referents
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


# -- the written form --------------------------------------------------------------

def test_a_written_referent_is_its_constant_and_shown_back():
    assert referents.expand("Dr [?x] -> false") == "Dr [referent_x] -> false"
    assert referents.expand("elim [?x]") == "elim [referent_x]"
    assert referents.show("Dr referent_x->false") == "Dr ?x->false"
    assert referents.mentioned("R referent_x (f referent_y) c") == {"referent_x", "referent_y"}


def test_the_parser_reads_a_referent_declaration_and_a_referent_statement():
    c = parse("referent x, y : Pub")
    assert c.kind == "referent" and c["names"] == ["x", "y"] and c["sort"] == "Pub"
    a = parse("argument sup : (Dr [?x] -> false)")
    assert a["conclusion"] == "Dr [referent_x] -> false"
    assert parse("cut (Dr [?x]) m").text == "cut (Dr [referent_x]) m"


# -- declaration -------------------------------------------------------------------

def test_a_referent_is_declared_as_a_constant_of_its_sort():
    prover, _svc, report = session(HEAD + "referent x : Pub.\n")
    try:
        assert report.ok
        assert prover.declarations["referent_x"] == "Pub"
        assert prover.names["referent_x"] == "referent"
    finally:
        prover.close()


def test_a_user_name_with_the_referent_prefix_is_refused():
    prover, _svc, report = session(HEAD + "declare referent_x : Pub.\n")
    try:
        assert not report.ok
    finally:
        prover.close()


def test_a_referent_is_declared_once():
    prover, _svc, report = session(HEAD + "referent x : Pub.\nreferent x : Pub.\n")
    try:
        assert entry(report, "referent").status == "refused"
        assert "already declared" in entry(report, "referent").message
    finally:
        prover.close()


# -- ending an argument --------------------------------------------------------------

SUP = """argument sup : (Dr [?x] -> false).
elim p.
cut (Dr [?x]) m.
axiom p.
by default.
"""


def test_an_argument_stating_a_referent_ends_with_dixi():
    prover, _svc, report = session(HEAD + "referent x : Pub.\n" + SUP + "dixi.\n")
    try:
        assert entry(report, "dixi").status == "ok", entry(report, "dixi").message
        assert "sup" in prover.arguments
        assert not prover.arguments["sup"].citable          # never a theorem
    finally:
        prover.close()


def test_qed_is_refused_for_an_argument_with_a_referent():
    text = HEAD + "referent x : Pub.\nargument t : (Dr [?x] -> Dr [?x]).\nelim p.\naxiom p.\nqed.\n"
    prover, _svc, report = session(text)
    try:
        closing = entry(report, "qed")
        assert closing.status in ("refused", "error")
        assert "discourse referent ?x" in closing.message and "dixi" in closing.message
        assert "t" not in prover.arguments
    finally:
        prover.close()


def test_an_undeclared_referent_is_refused():
    prover, _svc, report = session(HEAD + SUP + "dixi.\n")
    try:
        closing = entry(report, "dixi")
        assert closing.status in ("refused", "error")
        assert "undeclared referent ?x" in closing.message
        assert "referent x : SORT" in closing.message
    finally:
        prover.close()


def test_a_referent_used_but_not_stated_is_refused():
    """Deferred (the author, 2026-10-20): a witness-only referent would be
    caught by a capture, not by the statement."""
    text = HEAD + """referent x : Pub.
argument w : (exists x:Pub, Dr [x] -> Dr [x]).
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
