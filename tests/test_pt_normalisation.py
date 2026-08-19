"""Mapping the proof-term spellings machine mode emits to the parser's.

Machine mode prints ASCII sentinels -- `;:` for mu, `;:'` for mu-tilde, `\\`
for lambda.  This maps them to the unicode the grammar is written in.

It used to rewrite `~` to `¬` and `false` to `⊥` as well.  Those were blind
str.replace calls over the whole term, so they corrupted any identifier
containing the text; and they are unnecessary, because the grammar accepts
both spellings of negation and falsum.
"""

import pytest

from core.ac.prop import PFalse, PNeg, Prop
from core.ac.syntax import parse_proof_term
from core.dc.argument import Argument


def normalise(text):
    return Argument._normalize_pt_to_unicode(text)


# ---------------------------------------------------------------------------
#  What it still does
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "ascii_form,unicode_form",
    [
        (";:t:A.<a||t>", "μt:A.<a||t>"),
        (";:'t:A.<a||t>", "μ't:A.<a||t>"),
        ("\\h:A.a", "λh:A.a"),
        (";:'k:A.<;:m:A.<a||m>||k>", "μ'k:A.<μm:A.<a||m>||k>"),
    ],
)
def test_ascii_sentinels_become_unicode(ascii_form, unicode_form):
    assert normalise(ascii_form) == unicode_form


# ---------------------------------------------------------------------------
#  What it no longer does, and why
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    "text",
    [
        "μfalsehood:A.<a||falsehood>",     # `false` inside an identifier
        "μt:A.<falsely||t>",
        "μt:A.<a||falsifier>",
    ],
)
def test_identifiers_containing_false_are_left_alone(text):
    """`s.replace("false", "⊥")` turned μfalsehood into μ⊥hood."""
    assert normalise(text) == text
    parse_proof_term(text)      # and the result is still readable


def test_negation_and_falsum_are_not_rewritten():
    assert normalise("μt:~A.<a||t>") == "μt:~A.<a||t>"
    assert normalise("μt:false.<a||t>") == "μt:false.<a||t>"


@pytest.mark.parametrize("spelling", ["~A", "¬A"])
def test_both_spellings_of_negation_parse(spelling):
    """Which is why the rewrite was unnecessary."""
    assert isinstance(Prop.parse(spelling), PNeg)


@pytest.mark.parametrize("spelling", ["false", "⊥"])
def test_both_spellings_of_falsum_parse(spelling):
    assert isinstance(Prop.parse(spelling), PFalse)


@pytest.mark.parametrize(
    "text",
    [
        ";:t:~A.<a||t>",
        ";:t:¬A.<a||t>",
        ";:t:false.<a||t>",
        ";:t:⊥.<a||t>",
    ],
)
def test_a_normalised_term_parses_whichever_spelling_it_used(text):
    parse_proof_term(normalise(text))
