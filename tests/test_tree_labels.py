"""The acceptance-tree export colours by the grounded ADF labels
(tasks.org, aida-renderers-grounded-labels).

The corpus is the six-case audit table that retired the shape colouring:
on it the old classifier read a supported argument as defeated and an
unfilled obligation as accepted.  Here every box on the spine must carry
the fill of its statement's grounded label, and no other colour source
exists.
"""

import re

import pytest

from core.ac.ast import Mu, Mutilde, Cons, Goal, Deleg, ID, DI
from core.comp.adf_label import grounded_labels
from core.dc.debate_graph import compile_debate, canonical_prop
from pres.tree import render_acceptance_tree_dot, AcceptanceTreeRenderer, LABEL_FILL


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

CORPUS = [
    ("open-obligation", lambda: parg(Goal("1", "Q")), "P", "OUT"),
    ("open-presumption", lambda: parg(Deleg("1", "Q")), "P", "IN"),
    ("strict-axiom", lambda: eta("axArg", "Q->P", DI("pRule", "Q->P")), "Q->P", "IN"),
    ("supported-by-presumption",
     lambda: parg(t_sup("Q", Goal("1", "Q"), qarg(Deleg("2", "R->false")))), "P", "IN"),
    ("supported-by-obligation",
     lambda: parg(t_sup("Q", Goal("1", "Q"), qarg(Goal("2", "R->false")))), "P", "OUT"),
    ("attacked-site",
     lambda: parg(t_att("Q", Goal("1", "Q"),
                        Mutilde(DI("qAtt", "Q"), "Q", DI("qAtt", "Q"), ID("alt", "Q")))),
     "P", "OUT"),
]

FILL_RE = re.compile(r'fillcolor="([a-z0-9]+)"')


@pytest.mark.parametrize("case,make,root_prop,expected", CORPUS, ids=[c[0] for c in CORPUS])
def test_root_box_carries_the_grounded_label(case, make, root_prop, expected):
    body = make()
    graph = compile_debate(body, case, strict_names=STRICT)
    labels = grounded_labels(graph)
    assert labels[(canonical_prop(root_prop), "term")] == expected
    dot = render_acceptance_tree_dot(body, labels=labels)
    first_fill = FILL_RE.search(dot)
    assert first_fill and first_fill.group(1) == LABEL_FILL[expected]


@pytest.mark.parametrize("case,make,root_prop,expected", CORPUS, ids=[c[0] for c in CORPUS])
def test_every_box_matches_its_statement(case, make, root_prop, expected):
    body = make()
    graph = compile_debate(body, case, strict_names=STRICT)
    labels = grounded_labels(graph)
    renderer = AcceptanceTreeRenderer(labels=labels)
    # Walk the spine the way _emit does and compare fills to labels.
    node, inherited = body, None
    while node is not None:
        label = renderer.label_of(node)
        colour = label if label is not None else inherited
        if isinstance(node, (Mu, Mutilde)):
            side = "term" if isinstance(node, Mu) else "context"
            assert colour == labels.get((canonical_prop(node.prop), side), inherited)
        _, node = renderer._label_and_child(node)
        inherited = colour


def test_no_labels_means_no_fill():
    dot = render_acceptance_tree_dot(parg(Goal("1", "Q")))
    assert "fillcolor" not in dot
