import threading
"""Machine-payload plumbing that first-order support depends on.

Two pieces of the payload used to be dropped on the floor:

* the ``kind`` of each declaration, which is the only thing distinguishing a
  sort from a proposition -- Fellowship prints both annotations identically, so
  ``λx:A.t`` and ``λx:N.t`` cannot be told apart without it;
* the per-goal ``env``, the first-order variable context.

These tests pin both.  They use synthetic payloads so they neither need nor
wait on the prover; ``test_fo_fixtures.py`` covers the real end-to-end replay.
"""

from core.ac.signature import Declaration
from core.dc.argument import Argument
from wrap.document import Document
from wrap.prover import ProverWrapper
from wrap.sexp_parser import SexpParser


def _decls_state(entries):
    return {"decls": entries}


def _blank_wrapper():
    """A ProverWrapper with no subprocess -- only the decl merge is exercised."""
    pw = ProverWrapper.__new__(ProverWrapper)
    pw.doc = Document()
    pw.lock = threading.RLock()
    return pw


def test_declaration_kinds_survive_the_payload():
    pw = _blank_wrapper()
    pw._update_declarations_from_state(
        _decls_state(
            [
                {"name": '"N"', "kind": "sort", "sort": '"type"'},
                {"name": '"O"', "kind": "sort", "sort": '"N"'},
                {"name": '"P"', "kind": "sort", "sort": '"N->bool"'},
                {"name": '"ax"', "kind": "prop", "prop": '"forall n:N,P n"'},
                {"name": '"mA"', "kind": "moxia", "prop": '"A"'},
            ]
        )
    )

    assert pw.declarations["N"].kind == "sort"
    assert pw.declarations["O"].kind == "sort"
    assert pw.declarations["ax"].kind == "prop"
    assert pw.declarations["mA"].kind == "moxia"


def test_declarations_still_read_as_plain_strings():
    """Existing readers interpolate declarations directly; that must not break."""
    pw = _blank_wrapper()
    pw._update_declarations_from_state(
        _decls_state([{"name": '"P"', "kind": "sort", "sort": '"N->bool"'}])
    )

    value = pw.declarations["P"]
    assert isinstance(value, str)
    assert value == "N->bool"
    assert f"{value}" == "N->bool"


def test_missing_type_field_is_not_stringified():
    """A payload without a sort/prop kept None; it must not become "None"."""
    pw = _blank_wrapper()
    pw._update_declarations_from_state(
        _decls_state([{"name": '"broken"', "kind": "sort"}])
    )

    assert pw.declarations["broken"] is None


def test_declaration_repr_shows_the_kind():
    assert repr(Declaration("bool", "sort")) == "Declaration('bool', kind='sort')"


# ---------------------------------------------------------------------------
#  Per-goal first-order variable context
# ---------------------------------------------------------------------------

# Captured verbatim from fsp while replaying tests/fo_exists_forall.fspy, at the
# point where both the exists-elimination variable y and the forall-introduction
# variable x are in scope.
_PAYLOAD = """
(state (mode subgoals)(current-goal-index 1)
 (goals (goal (meta "1.1.1.2.1")(kind goal)(side lhs)
              (active-prop "forall x:N,P x y")
              (hyps ((name "H")(prop "exists y:N,forall x:N,P x y")(visible t)))
              (ccls ((name "th")(prop "exists y:N,P x y")(visible t)))
              (env ((name "y")(sort "N")) ((name "x")(sort "N")))))
 (proof-term "?1")
 (messages (errors) (warnings) (notes)))
"""


def _state_from(payload):
    pw = ProverWrapper.__new__(ProverWrapper)
    pw._sexp = SexpParser()
    # SexpParser tokenizes bytes, matching how the payload arrives off the PTY.
    return pw._from_machine_payload(pw._sexp.parse(payload.encode()))


def _parse_into_argument(payload):
    argument = Argument.__new__(Argument)
    argument.goal_envs = {}
    argument._parse_proof_state(_state_from(payload))
    return argument


def test_goal_env_is_captured():
    argument = _parse_into_argument(_PAYLOAD)

    assert argument.goal_envs == {"1.1.1.2.1": {"y": "N", "x": "N"}}


def test_goal_env_preserves_innermost_first_order():
    argument = _parse_into_argument(_PAYLOAD)

    # Fellowship lists the innermost binder first; downstream resolution relies
    # on that order when a name is shadowed.
    assert list(argument.goal_envs["1.1.1.2.1"]) == ["y", "x"]


def test_goal_env_capture_leaves_assumptions_alone():
    argument = _parse_into_argument(_PAYLOAD)

    assert argument.assumptions == {
        "1.1.1.2.1": {"prop": "forall x:N,P x y", "label": None}
    }


def test_propositional_goal_has_an_empty_env():
    payload = """
    (state (mode subgoals)(current-goal-index 1)
     (goals (goal (meta "1")(kind goal)(side rhs)(active-prop "A")
                  (hyps )(ccls )(env )))
     (proof-term "?1")
     (messages (errors) (warnings) (notes)))
    """
    argument = _parse_into_argument(payload)

    assert argument.goal_envs == {"1": {}}
