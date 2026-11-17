"""A `*` term as the left operand of `*` survives printing and reading.

`*` nests to the right in AIDA's term grammar (core/ac/syntax.py): in a
term slot ``X*Y`` is a subtraction introduction (Sonc) whose left operand
must be a single context, in a context slot a stack (Cons) whose left
operand must be a single term.  A left operand that is itself built with
`*` therefore needs parentheses, from Fellowship's printer (core.ml) and
from AIDA's own (pres/gen.py).  Without them:

- the stack ``(th*1!)*r`` printed as ``th*1!*r`` and read back as
  ``th*(1!*r)``, with ``th`` a context variable: no error, but the
  argument silently missed the document graph;
- the subtraction introduction ``(1!*!2)*r`` at the head of a stack
  printed as ``1!*!2*r`` and did not parse (tasks.org,
  aida-parse-subtraction-intro-in-stack).
"""

import io
import warnings
from contextlib import redirect_stdout
from pathlib import Path

import pytest

from core.ac.syntax import parse_proof_term
from core.dc.debate_graph import canonical_prop
from core.dc.typecheck import shape
from pres.gen import pres_scaffolds, pres_str
from wrap.cli import execute_script, setup_prover

FIXTURE = Path(__file__).parent / "star_operand_parens.fspy"


@pytest.mark.parametrize("category, text", [
    ("term", "(th:P*1:S!)*r:R"),              # a stack left of a subtraction
    ("term", "(f a*1:S!)*r:R"),               # a first-order stack there
    ("term", "((x:s).1:S!)*r:R"),             # a destructor there
    ("context", "(1:S!*!2:A)*r:C"),           # a subtraction heading a stack
    ("term", "(((th:P*1:S!)*r:R)*2:T!)*q:Q"),  # nested on the left twice
    ("term", "th:P*1:S!*r:R"),                # the right-nested reading
])
def test_printing_reads_back_as_the_same_term(category, text):
    term = parse_proof_term(text, category=category)
    printed = pres_str(term)
    assert shape(parse_proof_term(printed, category=category)) == shape(term), printed


def test_the_two_readings_print_differently():
    left = parse_proof_term("(th:P*1:S!)*r:R", category="term")
    right = parse_proof_term("th:P*1:S!*r:R", category="term")
    assert shape(left) != shape(right)
    assert pres_str(left) != pres_str(right)
    assert pres_scaffolds(left) != pres_scaffolds(right)   # display only, not read back


@pytest.fixture(scope="module")
def loaded():
    """A session of its own: the fixture declares into it (isolate=False,
    so that the document graph can be inspected afterwards)."""
    prover = setup_prover()
    with warnings.catch_warnings(), redirect_stdout(io.StringIO()):
        warnings.simplefilter("ignore")
        execute_script(prover, str(FIXTURE), strict=True, isolate=False,
                       render_files=False, stop_marker=False)
    yield prover
    prover.close()


def test_both_arguments_reach_the_document_graph(loaded):
    edges = {e.name: e for e in loaded.graph.edges}
    for name in ("stack_in_subtraction", "subtraction_in_stack"):
        assert name in loaded.arguments, name
        assert name in edges, f"'{name}' is registered but not in the document graph"


def test_the_stack_keeps_its_hypothesis_as_a_term(loaded):
    """The counterargument against Passed e0 rests on the guard, the
    presumed refutation of Submitted d0 and the denial, not on a free
    context variable."""
    edge = next(e for e in loaded.graph.edges if e.name == "stack_in_subtraction")
    assert (edge.target_key, edge.target_side) == (canonical_prop("Passed e0"), "context")
    sources = {(s.key, s.side, s.kind) for s in edge.sources}
    assert (canonical_prop("Required d0 e0"), "term", "subargument") in sources
    assert (canonical_prop("Submitted d0"), "context", "presumption") in sources
    assert len(sources) == 3, sources
