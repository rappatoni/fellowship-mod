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
