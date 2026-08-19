"""The grammar must have exactly one reading of anything it accepts.

This is why the parser is a grammar rather than hand-written code. A
hand-written parser cannot report ambiguity -- it silently commits to whichever
branch it happens to try first, which is how `?n` vs `n?`, the proposition
precedence bugs, and the lambda/star reading all went unnoticed. Running Lark
with `ambiguity="explicit"` turns that class of defect into a test failure.

An `_ambig` node means the grammar admits two parses of the same input. That is
a defect whether or not the two trees differ: if they differ, the parser is
guessing; if they do not, the grammar has a redundant derivation that will mask
a real ambiguity later.
"""

import re
from pathlib import Path

import pytest

from core.ac.syntax import _parser


PROPOSITIONS = [
    "A", "true", "false", "~A", "¬A", "⊥", "⊤",
    "A->B", "A->B->C", "(A->B)->C",
    "A-B->C", "(A-B)->C", "A->B-C", "A->(B-C)", "A-B-C", "A-(B-C)",
    "Un O", "P x y", "Even (S O)", "P [x] [y]", "Father Rich Bob",
    "forall n:N,Un n", "exists y:N,forall x:N,P x y",
    "forall n,m:N,Eq n m->Eq m n",
    "~(forall x:N,Un x)",
    "B-(forall x:N,Un x)", "(forall x:N,Un x)->C",
    "forall f:termf->form,truef (f x)",
    "(exists y:N,forall x:N,P x y)->(forall x:N,exists y:N,P x y)",
    "(true-A)-B", "true-(A-B)",
]

SORTS = ["type", "bool", "N", "N->bool", "N->N->bool", "(N->N)->N", "[termf->form]->form"]

FIRST_ORDER_TERMS = ["O", "S O", "S (S O)", "Plus (S n) m", "f x y"]

PROOF_TERMS = [
    "μt:A.<a||t>",
    "μ't:A.<a||t>",
    "μt:A.<a||x*t>",
    "μt:A.<a||x*y*t>",
    "μt:A.<x*y||t>",
    "μt:A.<λh:A.a||t>",
    "μt:A.<λh:A.λk:B.a||t>",
    "μt:A.<a||λh:R.b*c>",
    "μt:A.<a||λh:R.(b*c)>",
    "μt:A.<a||(b*c)>",
    "μt:A.<?1||1?>",
    "μt:A.<!1||1!>",
    "μt:A.<?1.2.1:A||1.3:B?>",
    "μt:A.<(y,a)||t>",
    "μt:A.<a||(y:N).t>",
    "μt:A.<a||S (S O)*t>",
    "μthesis:P O.<μth:P O.<ax||O*th>||thesis>",
    "μ'thesis:D->E-B->C.<thesis||λh:B->C.(?1.1.1*1.1.2?)>",
    ";:t:A.<a||t>",                       # the ascii spellings machine mode emits
    "\\h:A.a",
]


def harvested_proof_terms():
    """Every proof-term-looking literal in the test suite.

    Guards against the grammar being unambiguous only on examples chosen to
    make it so.
    """
    found = set()
    for path in Path(__file__).parent.glob("*.py"):
        if path.name == Path(__file__).name:
            continue
        text = path.read_text()
        for match in re.finditer(r'"((?:μ|;:)[^"\n]{6,})"|\'((?:μ|;:)[^\'\n]{6,})\'', text):
            found.add(match.group(1) or match.group(2))
    return sorted(found)


def ambiguity_count(text, start):
    tree = _parser("explicit").parse(text, start=start)
    return len(list(tree.find_data("_ambig")))


@pytest.mark.parametrize("text", PROPOSITIONS)
def test_propositions_have_one_reading(text):
    assert ambiguity_count(text, "prop") == 0


@pytest.mark.parametrize("text", SORTS)
def test_sorts_have_one_reading(text):
    assert ambiguity_count(text, "sort") == 0


@pytest.mark.parametrize("text", FIRST_ORDER_TERMS)
def test_first_order_terms_have_one_reading(text):
    assert ambiguity_count(text, "foterm") == 0


@pytest.mark.parametrize("text", PROOF_TERMS)
def test_proof_terms_have_one_reading(text):
    start = "context" if text.lstrip().startswith(("μ'", ";:'")) else "term"
    assert ambiguity_count(text, start) == 0


def test_every_proof_term_in_the_suite_has_one_reading():
    from core.dc.argument import Argument

    harvested = harvested_proof_terms()
    assert len(harvested) > 10, "the harvester stopped finding proof terms"

    ambiguous = []
    for raw in harvested:
        text = Argument._normalize_pt_to_unicode(raw)
        start = "context" if text.lstrip().startswith("μ'") else "term"
        try:
            count = ambiguity_count(text, start)
        except Exception:
            continue  # a fragment of a multi-line literal, not a whole term
        if count:
            ambiguous.append((count, text))

    assert not ambiguous, f"grammar admits several readings of: {ambiguous[:3]}"
