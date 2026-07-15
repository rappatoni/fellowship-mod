from dataclasses import dataclass

from core.ac.ast import ID, DI, Mu, Mutilde, ProofTerm, Term


@dataclass(frozen=True)
class AltStructure:
    binder_name: str
    prop: str | None
    elements: tuple[Term, ...]


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