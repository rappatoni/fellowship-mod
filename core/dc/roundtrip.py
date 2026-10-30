"""The round trip of a registered debate (tasks.org,
aida-debate-round-trip-warning): its term, as given, against the term its
extracted arguments unfold to (core/dc/extract.py, then the debate in its
session, core/dc/debate.py ``unfold_debate``).

The given term comes from another compiler - an importer's - and AIDA
takes it as the record; recompiling it from the arguments is a check of
both compilers.  The comparison is literal, up to alpha (site numbers
counted as names, propositions as statements); a mismatch is reported
with the first place the two terms part - the first pair of subterms, in
order, whose own constructor, proposition or binding differs - and the
debate stays usable (the author, 2026-10-29).  The graphs and their
grounded labels are compared too, as data for the study of which
equivalence the round trip needs (aida-round-trip-equivalence-study).
"""

from copy import deepcopy
from dataclasses import dataclass
from typing import Optional

from core.ac.ast import ProofTerm
from core.comp.oracle_terms import canonical_form
from core.dc.debate_graph import canonical_prop

#: For each canonical-form head, the positions of its children and the AST
#: slots they come from (core/comp/oracle_terms.py, canonical_form).
_CHILDREN = {
    "mu": ((2, "term"), (3, "context")),
    "mutilde": ((2, "term"), (3, "context")),
    "lamda": ((2, "term"),),
    "admal": ((2, "context"),),
    "cons": ((1, "term"), (2, "context")),
    "sonc": ((1, "context"), (2, "term")),
    "lamdafo": ((3, "term"),),
    "consfo": ((2, "context"),),
    "pairfo": ((2, "term"),),
    "destructfo": ((3, "context"),),
}


@dataclass
class RoundTrip:
    #: equal up to alpha, sites as names, propositions as statements
    literal: bool
    #: where they first part: the slots from the root, and the two subterms
    path: Optional[str] = None
    given: Optional[str] = None
    recompiled: Optional[str] = None
    #: the graphs equal up to edge names; the grounded labels equal on
    #: every statement both graphs have
    graphs_equal: Optional[bool] = None
    labels_equal: Optional[bool] = None

    def describe(self) -> str:
        if self.literal:
            return "its arguments unfold to the term it was given"
        graphs = {True: "the same graph", False: "a different graph", None: "graphs not compared"}
        labels = {True: "the same labels", False: "different labels", None: "labels not compared"}
        return (f"its arguments unfold to a different term ({graphs[self.graphs_equal]}, "
                f"{labels[self.labels_equal]}); they first part at {self.path or 'the root'}: "
                f"given {self.given} - recompiled {self.recompiled}")


def compare(given: ProofTerm, recompiled: ProofTerm, *, given_graph=None, recompiled_graph=None,
            given_labels=None, recompiled_labels=None, width: int = 160) -> RoundTrip:
    options = dict(sites="order", prop=_statement)
    left, right = canonical_form(given, **options), canonical_form(recompiled, **options)
    result = RoundTrip(left == right)
    if not result.literal:
        path, a, b = _first_difference(left, right, given, recompiled)
        result.path, result.given, result.recompiled = path, _short(a, width), _short(b, width)
    if given_graph is not None and recompiled_graph is not None:
        result.graphs_equal = _signature(given_graph) == _signature(recompiled_graph)
    if given_labels is not None and recompiled_labels is not None:
        shared = set(given_labels) & set(recompiled_labels)
        result.labels_equal = all(given_labels[s] == recompiled_labels[s] for s in shared)
    return result


def _first_difference(left, right, a, b, path=()):
    """The first pair of subterms, in order, whose heads differ."""
    if not (isinstance(left, tuple) and isinstance(right, tuple) and left and right
            and left[0] == right[0] and left[0] in _CHILDREN and len(left) == len(right)):
        return ".".join(path), a, b
    children = dict(_CHILDREN[left[0]])
    heads_equal = all(left[i] == right[i] for i in range(1, len(left)) if i not in children)
    if not heads_equal:
        return ".".join(path), a, b
    for index, slot in _CHILDREN[left[0]]:
        if left[index] != right[index]:
            return _first_difference(left[index], right[index], getattr(a, slot), getattr(b, slot),
                                     path + (slot,))
    return ".".join(path), a, b


def _statement(prop):
    try:
        return canonical_prop(prop) if prop else prop
    except Exception:
        return prop


def _signature(graph):
    return (sorted((e.target_key, e.target_side, e.strict,
                    tuple(sorted((s.key, s.side, s.kind) for s in e.sources))) for e in graph.edges),
            {k: frozenset(v) for k, v in graph.defaults.items()})


def _short(term, width: int) -> str:
    from pres.gen import ProofTermGenerationVisitor
    if not isinstance(term, ProofTerm):
        return repr(term)
    text = ProofTermGenerationVisitor(verbose=-1).visit(deepcopy(term)).pres
    return text if len(text) <= width else text[:width - 1] + "…"
