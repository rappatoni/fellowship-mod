"""M5: audit of the experimental labellers against the M4 grounded labels.

VERDICT (2026-08-31, recorded in propositional-fragment-plan.org): the
experimental labellers in core/comp/color.py are demoted to
presentation-only.  Their green/red/yellow classifies term *shape*
("unattacked" / "attack-scaffold present" / "open"), not acceptance:

- an argument whose only ground is an unfilled obligation is green
  ("accepted") where grounded semantics says OUT;
- a *supported* argument is red ("defeated") because the support scaffold
  pattern-matches the attack heuristic - support read as defeat;
- agreements on other rows are accidental (the shape happens to coincide
  with the acceptance status).

This file pins the divergence table.  It is a tripwire, not an
endorsement: if either side changes, the recorded row fails and the audit
must be redone.  Acceptance questions are answered by
core/comp/adf_label.py; color.py must not be consulted for them.
"""

import pytest

from core.ac.ast import Mu, Mutilde, Cons, Goal, Deleg, ID, DI
from core.comp.color import AcceptanceColoringVisitor, DebateTermLabeller
from core.comp.adf_label import grounded_labels
from core.dc.debate_graph import compile_debate, canonical_prop


def eta(name, prop, inner):
    return Mu(ID(name, prop), prop, inner, ID(name, prop))


def parg(site):
    return eta("pArg", "P",
               Mu(ID("rule", "P"), "P", DI("pRule", "Q->P"),
                  Cons(site, ID("rule", "P"))))


def qarg(site):
    return eta("qArg", "Q",
               Mu(ID("rule2", "Q"), "Q", DI("qRule", "(R->false)->Q"),
                  Cons(site, ID("rule2", "Q"))))


def t_sup(prop, orig, scion):
    return Mu(ID("alt", prop), prop,
              Mu(ID("_", prop), prop, orig, ID("alt", prop)),
              Mutilde(DI("_", prop), prop, scion, ID("alt", prop)))


def t_att(prop, orig, scion_ctx):
    return Mu(ID("alt", prop), prop,
              Mu(ID("_", prop), prop, orig, ID("alt", prop)),
              Mutilde(DI("_", prop), prop, Goal("g2", prop), scion_ctx))


STRICT = {"pRule", "qRule"}

#: (case, body builder, root prop, color.py root, term-labeller root,
#:  M4 grounded label of the root statement).  The last three columns are
#: the recorded audit findings.
AUDIT_TABLE = [
    ("open-obligation",
     lambda: parg(Goal("1", "Q")), "P", "green", "green", "OUT"),
    ("open-presumption",
     lambda: parg(Deleg("1", "Q")), "P", "green", "green", "IN"),
    ("strict-axiom",
     lambda: eta("axArg", "Q->P", DI("pRule", "Q->P")), "Q->P",
     "green", "green", "IN"),
    ("supported-by-presumption",
     lambda: parg(t_sup("Q", Goal("1", "Q"), qarg(Deleg("2", "R->false")))),
     "P", "red", "red", "IN"),
    ("supported-by-obligation",
     lambda: parg(t_sup("Q", Goal("1", "Q"), qarg(Goal("2", "R->false")))),
     "P", "red", "red", "OUT"),
    ("attacked-site",
     lambda: parg(t_att("Q", Goal("1", "Q"),
                        Mutilde(DI("qAtt", "Q"), "Q",
                                DI("qAtt", "Q"), ID("alt", "Q")))),
     "P", "red", "red", "OUT"),
]

IDS = [row[0] for row in AUDIT_TABLE]


@pytest.mark.parametrize(
    "case,make,root_prop,color_expected,termlab_expected,grounded_expected",
    AUDIT_TABLE, ids=IDS)
def test_audit_row(case, make, root_prop, color_expected, termlab_expected,
                   grounded_expected):
    body = make()
    assert AcceptanceColoringVisitor().classify(body) == color_expected
    assert DebateTermLabeller().label_of(body).status == termlab_expected
    graph = compile_debate(body, case, strict_names=STRICT)
    labels = grounded_labels(graph)
    assert labels[(canonical_prop(root_prop), "term")] == grounded_expected


def test_divergence_is_real():
    """At least one row disagrees under green=IN / red=OUT / yellow=UNDEC -
    the audit's reason for the demotion verdict.  If this ever fails, the
    experimental labellers have converged and the verdict deserves review."""
    mapping = {"green": "IN", "red": "OUT", "yellow": "UNDEC"}
    diverging = [
        case for case, _, _, color, _, grounded in AUDIT_TABLE
        if mapping[color] != grounded
    ]
    assert diverging == ["open-obligation", "supported-by-presumption"]
