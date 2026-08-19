from dataclasses import dataclass

from core.ac.ast import Admal, Cons, Context, Deleg, DI, Geled, Goal, Hyp, ID, Laog, Lamda, Mu, Mutilde, ProofTerm, Pyh, Sonc, Term, FirstOrderNotSupported, first_order_node


@dataclass(frozen=True)
class AltStructure:
    binder_name: str
    prop: str | None
    elements: tuple[Term, ...]


@dataclass(frozen=True)
class AlternativeCounterexampleStructure:
    binder_name: str
    prop: str | None
    elements: tuple[Context, ...]


@dataclass(frozen=True)
class DefeasibleWarrantStructure:
    binder_name: str
    prop: str | None
    support: Term
    exception: Context


@dataclass(frozen=True)
class ApplicationStructure:
    binder_name: str
    prop: str | None
    function: Term
    argument: Term
    argument_prop: str | None


@dataclass(frozen=True)
class DualApplicationStructure:
    binder_name: str
    prop: str | None
    condition: Context
    warrant: Context
    condition_prop: str | None


@dataclass(frozen=True)
class DualDefeasibleWarrantStructure:
    binder_name: str
    prop: str | None
    support: Term
    requirement: Context


def match_alt_structure(node: ProofTerm) -> AltStructure | None:
    """Recognize and flatten right-associated alternative structures.

    The base shape is::

        mu SomeVar:P.<mu_:P.<t:P||SomeVar:P>||mu'_:P.<t':P||SomeVar:P>>

    Only the literal ``_`` is treated as the discarded affine binder.  The
    matcher intentionally does not enforce proposition equality; type errors
    are expected to be caught by the type checker.
    """
    pair = _match_alt_pair(node)
    if pair is None:
        return None

    binder_name, prop, left_element, right_element = pair
    nested = match_alt_structure(right_element)
    if nested is not None:
        elements = (left_element, *nested.elements)
    else:
        elements = (left_element, right_element)

    return AltStructure(binder_name=binder_name, prop=prop, elements=elements)


def match_alternative_counterexample_structure(node: ProofTerm) -> AlternativeCounterexampleStructure | None:
    """Recognize and flatten left-associated alternative counterexamples.

    The base shape is the dual alternative form::

        mu' SomeVar:P.<mu_:P.<SomeVar:P||E:P>||mu'_:P.<SomeVar:P||E':P>>

    The recursive tail is on the left/term side: if ``E`` is itself an
    alternative counterexample structure, its elements are emitted before
    ``E'``.
    """
    pair = _match_alternative_counterexample_pair(node)
    if pair is None:
        return None

    binder_name, prop, left_element, right_element = pair
    nested = match_alternative_counterexample_structure(left_element)
    if nested is not None:
        elements = (*nested.elements, right_element)
    else:
        elements = (left_element, right_element)

    return AlternativeCounterexampleStructure(binder_name=binder_name, prop=prop, elements=elements)


def match_application_structure(node: ProofTerm) -> ApplicationStructure | None:
    """Recognize application structures.

    The shape is::

        mu SomeVar:A.<f||v:B*SomeVar:A>
    """
    if not isinstance(node, Mu):
        return None
    if not isinstance(node.id, ID):
        return None
    if not isinstance(node.context, Cons):
        return None
    if not isinstance(node.context.context, ID):
        return None
    if node.context.context.name != node.id.name:
        return None

    return ApplicationStructure(
        binder_name=node.id.name,
        prop=node.prop,
        function=node.term,
        argument=node.context.term,
        argument_prop=node.context.term.prop,
    )


def match_dual_application_structure(node: ProofTerm) -> DualApplicationStructure | None:
    """Recognize dual application structures.

    The AST-valid dual shape is::

        mu' SomeVar:A.<SomeVar:A*E:B||F>
    """
    if not isinstance(node, Mutilde):
        return None
    if not isinstance(node.di, DI):
        return None
    if not isinstance(node.term, Sonc):
        return None
    if not isinstance(node.term.term, DI):
         return None
    if node.term.term.name != node.di.name:
        return None

    return DualApplicationStructure(
        binder_name=node.di.name,
        prop=node.prop,
        condition=node.term.context,
        warrant=node.context,
        condition_prop=node.term.context.prop,
    )


def match_defeasible_warrant_structure(node: ProofTerm) -> DefeasibleWarrantStructure | None:
    """Recognize a defeasible warrant shape.

    Accepted shapes are::

        mu SomeVar:P.<mu_:P.<t:P||SomeVar:P>||mu'_:P.<t:P||E:P>>
        mu SomeVar:P.<mu_:P.<t:P||E:P>||mu'_:P.<t:P||SomeVar:P>>

    The two occurrences of ``t`` must be structurally equal.
    """
    if not isinstance(node, Mu):
        return None
    if not isinstance(node.id, ID):
        return None

    outer_name = node.id.name
    if not isinstance(node.term, Mu):
        return None
    if not isinstance(node.term.id, ID) or node.term.id.name != "_":
        return None

    if not isinstance(node.context, Mutilde):
        return None
    if not isinstance(node.context.di, DI) or node.context.di.name != "_":
        return None
    if not _same_tree(node.term.term, node.context.term):
        return None

    left_returns_to_binder = isinstance(node.term.context, ID) and node.term.context.name == outer_name
    right_returns_to_binder = isinstance(node.context.context, ID) and node.context.context.name == outer_name

    if left_returns_to_binder and not right_returns_to_binder:
        exception = node.context.context
    elif right_returns_to_binder and not left_returns_to_binder:
        exception = node.term.context
    else:
        return None

    return DefeasibleWarrantStructure(
        binder_name=outer_name,
        prop=node.prop,
        support=node.term.term,
        exception=exception,
    )


def match_dual_defeasible_warrant_structure(node: ProofTerm) -> DualDefeasibleWarrantStructure | None:
    """Recognize a dual defeasible warrant shape.

    Accepted shapes are::

        mu' SomeVar:P.<mu_:P.<t:P||E:P>||mu'_:P.<SomeVar:P||E:P>>
        mu' SomeVar:P.<mu_:P.<SomeVar:P||E:P>||mu'_:P.<t:P||E:P>>

    The two occurrences of ``E`` must be structurally equal.
    """
    if not isinstance(node, Mutilde):
        return None
    if not isinstance(node.di, DI):
        return None

    outer_name = node.di.name
    if not isinstance(node.term, Mu):
        return None
    if not isinstance(node.term.id, ID) or node.term.id.name != "_":
        return None

    if not isinstance(node.context, Mutilde):
        return None
    if not isinstance(node.context.di, DI) or node.context.di.name != "_":
        return None
    if not _same_tree(node.term.context, node.context.context):
        return None

    left_is_binder = isinstance(node.term.term, DI) and node.term.term.name == outer_name
    right_is_binder = isinstance(node.context.term, DI) and node.context.term.name == outer_name

    if right_is_binder and not left_is_binder:
        support = node.term.term
    elif left_is_binder and not right_is_binder:
        support = node.context.term
    else:
        return None

    return DualDefeasibleWarrantStructure(
        binder_name=outer_name,
        prop=node.prop,
        support=support,
        requirement=node.term.context,
    )


def _match_alt_pair(node: ProofTerm) -> tuple[str, str | None, Term, Term] | None:
    if not isinstance(node, Mu):
        return None

    if not isinstance(node.id, ID):
        return None

    outer_name = node.id.name

    if not isinstance(node.term, Mu):
        return None
    if not isinstance(node.term.id, ID):
        return None
    if node.term.id.name != "_":
        return None
    if not isinstance(node.term.context, ID):
        return None
    if node.term.context.name != outer_name:
        return None

    if not isinstance(node.context, Mutilde):
        return None
    if not isinstance(node.context.di, DI):
        return None
    if node.context.di.name != "_":
        return None
    if not isinstance(node.context.context, ID):
        return None
    if node.context.context.name != outer_name:
        return None

    return outer_name, node.prop, node.term.term, node.context.term


def _match_alternative_counterexample_pair(node: ProofTerm) -> tuple[str, str | None, Context, Context] | None:
    if not isinstance(node, Mutilde):
        return None
    if not isinstance(node.di, DI):
        return None

    outer_name = node.di.name

    if not isinstance(node.term, Mu):
        return None
    if not isinstance(node.term.id, ID) or node.term.id.name != "_":
        return None
    if not isinstance(node.term.term, DI) or node.term.term.name != outer_name:
        return None

    if not isinstance(node.context, Mutilde):
        return None
    if not isinstance(node.context.di, DI) or node.context.di.name != "_":
        return None
    if not isinstance(node.context.term, DI) or node.context.term.name != outer_name:
        return None

    return outer_name, node.prop, node.term.context, node.context.context


def _same_tree(left: ProofTerm, right: ProofTerm) -> bool:
    if type(left) is not type(right):
        return False

    if isinstance(left, Mu):
        return (
            _same_tree(left.id, right.id)
            and left.prop == right.prop
            and _same_tree(left.term, right.term)
            and _same_tree(left.context, right.context)
        )
    if isinstance(left, Mutilde):
        return (
            _same_tree(left.di, right.di)
            and left.prop == right.prop
            and _same_tree(left.term, right.term)
            and _same_tree(left.context, right.context)
        )
    if isinstance(left, Lamda):
        return _same_tree(left.di, right.di) and _same_tree(left.term, right.term)
    if isinstance(left, Admal):
        return _same_tree(left.id, right.id) and _same_tree(left.context, right.context)
    if isinstance(left, Cons):
        return _same_tree(left.term, right.term) and _same_tree(left.context, right.context)
    if isinstance(left, Sonc):
        return _same_tree(left.context, right.context) and _same_tree(left.term, right.term)
    if isinstance(left, Hyp):
        return _same_tree(left.di, right.di) and left.prop == right.prop
    if isinstance(left, Pyh):
        return _same_tree(left.id, right.id) and left.prop == right.prop
    if isinstance(left, ID):
        return left.name == right.name and left.prop == right.prop
    if isinstance(left, DI):
        return left.name == right.name and left.prop == right.prop
    if isinstance(left, (Deleg, Geled)):
        return left.prop == right.prop
    if isinstance(left, (Goal, Laog)):
        return left.number == right.number and left.prop == right.prop

    found = first_order_node(left)
    if found is not None:
        raise FirstOrderNotSupported("Structural comparison", found)
    return False
