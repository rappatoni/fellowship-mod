"""A printed term parses back (tasks.org, aida-site-names-round-trip): sites
are named by numbers - before labelling a site is a nominal, the one
obligation or delegation of its statement - and, in a labelled term (an
evaluated normal form), by their labels IN, OUT, UNDEC."""

import warnings
from copy import deepcopy

import pytest

from core.ac.ast import Context
from core.ac.resolve import resolve
from core.ac.syntax import SyntaxErrorInSource, parse_proof_term
from core.comp.oracle_terms import alpha_equal
from core.dc.debate_graph import canonical_prop
from pres.gen import pres_str
from wrap.cli import execute_script, setup_prover
from wrap.service import AidaError, Service


def statement(prop):
    try:
        return canonical_prop(prop) if prop else prop
    except Exception:
        return prop


def parses_back(term, declarations) -> bool:
    text = pres_str(deepcopy(term))
    back = parse_proof_term(text, category="context" if isinstance(term, Context) else "term")
    return alpha_equal(term, resolve(back, declarations), prop=statement)


class TestGrammar:
    @pytest.mark.parametrize("text", ["μa:A.<?1:A||a>", "μa:A.<!2.1:A||a>",
                                      "μa:A.<!IN:A||a>", "μa:A.<?UNDEC:A||a>",
                                      "μ'x:A.<x||OUT:A?>", "μ'x:A.<x||IN:A!>"])
    def test_numbers_and_labels(self, text):
        parse_proof_term(text)

    def test_the_label_words_stay_free_as_names(self):
        parse_proof_term("μIN:A.<f||IN>")

    def test_no_other_name(self):
        with pytest.raises(SyntaxErrorInSource, match="not 'u1'"):
            parse_proof_term("μa:A.<?u1:A||a>")


@pytest.mark.parametrize("fixture", [
    "tests/peirces_law.fspy",
    "tests/counterarguments_and_undercut.fspy",
    "tests/rationality/cyclic_undercut.fspy",
    "tests/alternative_arguments.fspy",          # an evaluation that is a bare site, OUT:B?
    "tests/simple_tautology.fspy",
    "tests/first_order/tweety.fspy",
])
def test_every_printed_term_of_a_fixture_parses_back(fixture):
    """Every argument's unfolded term, every debate's term and every
    debate's evaluated normal form (308 of 310 over all fixtures,
    2026-10-30; the two others print a statement key where a proposition
    belongs, aida-open-statement-key-printed)."""
    p = setup_prover()
    try:
        with warnings.catch_warnings():
            warnings.simplefilter("ignore")
            execute_script(p, fixture, strict=False, isolate=False)
        service, terms = Service.of(p), []
        for name in p.arguments:
            try:
                terms.append(service.term(name, "unfolded").term)
            except AidaError:
                pass
        for name in p.debates:
            terms.append(service.term(name, "unfolded").term)
            terms.append(service.evaluate(name).normal_form)
        assert terms
        for term in terms:
            assert parses_back(term, p.declarations), pres_str(deepcopy(term))
    finally:
        p.close()
