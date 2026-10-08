class ProofTerm:
    pass

class Term(ProofTerm):
    pass

class Context(ProofTerm):
    pass

class Mu(Term):
    def __init__(self, id_: "ID", prop: str, term: "Term", context: "Context"):
        self.id = id_
        self.prop = prop
        self.term = term
        self.context = context
        self.contr = None
        self.pres = None
        self.flag = None
        if not isinstance(id_, ID):
            raise TypeError(f"Mu expects ID binder, got {type(id_).__name__}")
        if not isinstance(term, Term):
            raise TypeError(f"Mu.term expects a Term node, got {type(term).__name__}")
        if not isinstance(context, Context):
            raise TypeError(f"Mu.context expects a Context node, got {type(context).__name__}")
        # self.contr = term.prop +"vs"+ context.prop if term.prop and context.prop else None

class Mutilde(Context):
    def __init__(self, di_: "DI", prop: str, term: "Term", context: "Context"):
        self.di = di_
        self.prop = prop
        self.term = term
        self.context = context
        self.contr = None
        self.pres = None
        self.flag = None
        if not isinstance(di_, DI):
            raise TypeError(f"Mutilde expects DI binder, got {type(di_).__name__}")
        if not isinstance(term, Term):
            raise TypeError(f"Mutilde.term expects a Term node, got {type(term).__name__}")
        if not isinstance(context, Context):
            raise TypeError(f"Mutilde.context expects a Context node, got {type(context).__name__}")
        # self.contr = term.prop +"vs"+ context.prop if term.prop and context.prop else None

class Lamda(Term):
    def __init__(self, hyp: "Hyp",  term: "Term"):
        self.di = hyp
        self.term = term
        self.prop = None
        self.pres = None
        self.flag = None
        if not isinstance(hyp, Hyp):
            raise TypeError(f"Lamda expects Hyp binder, got {type(hyp).__name__}")
        if not isinstance(term, Term):
            raise TypeError(f"Lamda.term expects a Term node, got {type(term).__name__}")
        # self.prop = di.prop+"->"+term.prop if di_.prop and term.prop else None

# class Hyp(ProofTerm):
#     def __init__(self, id_):
#         self.id = id_

class Pyh(ProofTerm):
    def __init__(self, id: "ID", prop: str):
        self.id = id
        self.prop = prop
        self.pres = None
        self.flag = None
        if not isinstance(id, ID):
            raise TypeError(f"Pyh expects ID in binder position, got {type(id).__name__}")

class Admal(Context):
    def __init__(self, hyp: "Pyh", context: "Context"):
        self.id = hyp
        self.context = context
        self.prop = None
        self.pres = None
        self.flag = None
        if not isinstance(hyp, Pyh):
            raise TypeError(f"Admal expects Pyh binder, got {type(hyp).__name__}")
        if not isinstance(context, Context):
            raise TypeError(f"Admal.context expects a Context node, got {type(context).__name__}")

class Cons(Context):
    def __init__(self, term: "Term", context: "Context"):
        self.term = term
        self.context = context
        self.prop = None
        self.pres = None
        self.flag = None
        if not isinstance(term, Term):
            raise TypeError(f"Cons.term expects a Term node, got {type(term).__name__}")
        if not isinstance(context, Context):
            raise TypeError(f"Cons.context expects a Context node, got {type(context).__name__}")
        # self.prop = term.prop + "->"+context.prop if term.prop and context.prop else None

class Sonc(Term):
    def __init__(self, context: "Context", term: "Term"):
        self.context = context
        self.term = term
        self.prop = None
        self.pres = None
        self.flag = None
        if not isinstance(context, Context):
            raise TypeError(f"Sonc.context expects a Context node, got {type(context).__name__}")
        if not isinstance(term, Term):
            raise TypeError(f"Sonc.term expects a Term node, got {type(term).__name__}")

class Goal(Term):
    def __init__(self, number, prop = None):
        self.number = number
        self.prop = prop
        self.pres = None
        self.flag = None

class Laog(Context):
    def __init__(self, number, prop = None):
        self.number = number
        self.prop = prop
        self.pres = None
        self.flag = None

class Deleg(Term):
    def __init__(self, number, prop = None):
        self.number = number
        self.prop = prop
        self.pres = None
        self.flag = None

class Geled(Context):
    def __init__(self, number, prop = None):
        self.number = number
        self.prop = prop
        self.pres = None
        self.flag = None

class ID(Context):
    def __init__(self, name, prop = None):
        self.name = name
        self.prop = prop
        self.pres = None
        self.flag = None

class DI(Term):
    def __init__(self, name, prop = None):
        self.name = name
        self.prop = prop
        self.pres = None
        self.flag = None

class Hyp(ProofTerm):
    def __init__(self, di: "DI", prop: str):
        self.di = di
        self.prop = prop
        self.pres = None
        self.flag = None
        if not isinstance(di, DI):
            raise TypeError(f"Hyp expects DI in binder position, got {type(di).__name__}")


# ---------------------------------------------------------------------------
#  First-order constructs
# ---------------------------------------------------------------------------
#
# Fellowship's four first-order proof-term constructors (core.ml:400-436).
# Each keeps its body in `.term` or `.context` so that the many traversals
# which recurse blindly on those two attributes descend into them for free.
#
# Three of the four are printed ambiguously with a propositional counterpart:
# LambdaFO like Lambda, ConsFO like Cons, TermsPairFO like TermsPair.  The
# parser cannot tell them apart on syntax alone, so it produces the
# propositional node and `core.ac.resolve` reclassifies using the prover's
# declaration table.  DestructTermsPairFO is the exception: `(x:S).c` has no
# propositional twin in AIDA's fragment, so the parser emits it directly.


class LamdaFO(Term):
    """Universal introduction: ``λx:S.t``, binding a first-order variable.

    Spelled exactly like :class:`Lamda`; the annotation is a sort rather than
    a proposition, which is the only difference and is not visible in the
    printed form.
    """

    def __init__(self, var: str, sort, term: "Term"):
        self.var = var
        self.sort = sort
        self.term = term
        self.prop = None
        self.pres = None
        self.flag = None
        if not isinstance(term, Term):
            raise TypeError(f"LamdaFO.term expects a Term node, got {type(term).__name__}")


class ConsFO(Context):
    """Universal instantiation: ``t*c``, where ``t`` is a first-order term.

    Spelled exactly like :class:`Cons`; the head is a first-order term rather
    than a proof term.
    """

    def __init__(self, fo_term, context: "Context"):
        self.fo_term = fo_term
        self.context = context
        self.prop = None
        self.pres = None
        self.flag = None
        if not isinstance(context, Context):
            raise TypeError(
                f"ConsFO.context expects a Context node, got {type(context).__name__}"
            )


class TermsPairFO(Term):
    """Existential introduction: ``(t,u)`` with witness ``t``.

    Fellowship's printer discards this node's binder and body
    (``core.ml:449``), so the proposition cannot be reconstructed from the
    printed term; it has to come from the enclosing goal.  The witness, which
    is what ``elim [t]`` replay needs, is printed.
    """

    def __init__(self, witness, term: "Term"):
        self.witness = witness
        self.term = term
        self.prop = None
        self.pres = None
        self.flag = None
        if not isinstance(term, Term):
            raise TypeError(
                f"TermsPairFO.term expects a Term node, got {type(term).__name__}"
            )


class DestructTermsPairFO(Context):
    """Existential elimination: ``(x:S).c``, binding a first-order variable."""

    def __init__(self, var: str, sort, context: "Context"):
        self.var = var
        self.sort = sort
        self.context = context
        self.prop = None
        self.pres = None
        self.flag = None
        if not isinstance(context, Context):
            raise TypeError(
                f"DestructTermsPairFO.context expects a Context node, got "
                f"{type(context).__name__}"
            )


#: The four first-order constructors, as a tuple for isinstance tests.
FIRST_ORDER_NODES = (LamdaFO, ConsFO, TermsPairFO, DestructTermsPairFO)


def first_order_node(node: "ProofTerm"):
    """The first first-order construct in ``node``, or None if there is none.

    Reduction and the debate operations do not yet handle first-order terms:
    the reduction rules for first-order AC/DC are not settled, and grafting
    into a first-order binder needs design work.  They use this to refuse such
    a term outright rather than walk into it and quietly do the wrong thing --
    several of them fall through to "return the node unchanged" or "not equal"
    on a node they do not recognise, which would be silent and wrong.
    """
    if isinstance(node, FIRST_ORDER_NODES):
        return node
    for slot in ("term", "context"):
        child = getattr(node, slot, None)
        if isinstance(child, ProofTerm):
            found = first_order_node(child)
            if found is not None:
                return found
    return None


class FirstOrderNotSupported(NotImplementedError):
    """Raised where a first-order construct has no defined behaviour yet."""

    def __init__(self, operation: str, node: "ProofTerm"):
        super().__init__(
            f"{operation} does not support first-order terms yet, and this one "
            f"contains a {type(node).__name__}. The first-order reduction "
            f"rules are not settled (tasks.org, aida-first-order-reduction)."
        )
        self.node = node
