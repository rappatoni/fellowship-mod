from lark import Lark, Transformer
import re
import warnings
import collections
from copy import deepcopy
import logging
from typing import Optional, Any, List, Tuple, Dict

logger = logging.getLogger('fsp.parser')

class Grammar():
    def __init__(self):
        self. proof_term_grammar = r'''
            ?start: proof_term

            proof_term: term | context | mu | mutilde | lamda | cons | goal

            term: di | lamda | mu | goal

            context: id | cons | mutilde | laog

            mu: "μ" id ":" prop "." "<" term "||" context ">"
            mutilde: "μ'" di ":" prop "." "<" term "||" context ">"
            lamda: "λ" hyp "." term
            cons: term "*" context
            goal: "?" number
            laog: "?" number

            prop: /[^.<>()]+/
            hyp: di ":" prop
            id: /[a-zA-Z_][a-zA-Z0-9_]*/
            di: /[a-zA-Z_][a-zA-Z0-9_]*/
            number: NUMBER ("." NUMBER)*

            %import common.NUMBER
            %import common.WS
            %ignore WS
        '''

        self.parser = Lark(self.proof_term_grammar, start='start')

easy_term = "μthesis:B.<μx1:B.<r1ttAimpliesB||?1.2.1*x1>||thesis>"
easy_undercut_term = " μthesis:¬A.<μH1:¬A.<λH2:A.μH3:⊥.<H2||μ'issue:A.<μstash:A.<λH:A.μFresh:B.<r1||H*Fresh>||issue*μ'affine:B.<issue||stash>>||μ'adapter:A.<μFresh2:¬A.<r2||?1.1.2.1.2.1*Fresh2>||μ'H1:¬A.<H1||adapter*_F_>>>>||H1>||thesis>"
Example_proof_term = "μthesis:¬A.<μH1:¬A.<λH2:A.μH3:⊥.<H2||μ'issue:A.<μstash:A.<λH:A.μFresh:B.<r1||H*Fresh>||issue*μ'affine:B.<issue||stash>>||μ'adapter:A.<μFresh2:¬A.<r2||μsupport:C.<μalt1:C.<?1.1.2.1.2.1.1.1||support>||μ'alt2:C.<μFresh5:C.<r3||?1.1.2.1.2.1.2.1.2.1*Fresh5>||support>>*Fresh2>||μ'H1:¬A.<H1||adapter*_F_>>>>||H1>||thesis>"

grammar=Grammar()
easy_test = grammar.parser.parse(easy_term)
easy_undercut_test = grammar.parser.parse(easy_undercut_term)
test = grammar.parser.parse(Example_proof_term)
#print("PARSER_TEST", test)

class ProofTermTransformer(Transformer):

    def proof_term(self, items):
        return items[0]

    def term(self, items):
        return items[0]

    def context(self, items):
        return items[0]

    def mu(self, items) -> "Mu":
        id_ = items[0]
        prop = items[1]
        term = items[2]
        context = items[3]
        return Mu(id_, prop, term, context)

    def mutilde(self, items) -> "Mutilde":
        di_ = items[0]
        prop = items[1]
        term = items[2]
        context = items[3]
        return Mutilde(di_, prop, term, context)

    def lamda(self, items) -> "Lamda":
        hyp = items[0]
        term = items[1]
        return Lamda(hyp, term)

    # def hyp(self, items):
    #     id_ = items[0]
    #     return Hyp(id_)

    def cons(self, items) -> "Cons":
        term = items[0]
        context = items[1]
        return Cons(term, context)

    def goal(self, items) -> "Goal":
        number = items[0]
        return Goal(number)

    def laog(self, items) -> "Laog":
        number = items[0]
        return Laog(number)

    def number(self, items):
        return '.'.join(items)

    def prop(self, items):
        return str(items[0])

    def id(self, token) -> "ID":
        name = str(token[0])
        # if re.fullmatch(re.compile('.*tt.*'),name):
        #     propstring = name.split("tt")[1]
        #     propstring = propstring.replace("implies", "->")
        #     propstring = propstring.replace ("ll", "(")
        #     propstring = propstring.replace ("rr", ")")
        #     propstring = propstring.replace("after","*")
        #     propstring = propstring.replace("not","¬")
        #     prop = propstring
        # else:
        # print(token)
        prop = None
        return ID(name, prop)

    def di(self, token) -> "DI":
        name = str(token[0])
        # if re.fullmatch(re.compile('.*tt.*'),name):
        #     # print("Matched")
        #     propstring = name.split("tt")[1]
        #     propstring = propstring.replace("implies", "->")
        #     propstring = propstring.replace ("ll", "(")
        #     propstring = propstring.replace ("rr", ")")
        #     propstring = propstring.replace("after","*")
        #     propstring = propstring.replace("not","¬")
        #     prop = propstring
        #     # print("PROP")
        #     # print(prop)
        # else:
        prop = None
        return DI(name, prop)

    def hyp(self, token) -> "Hyp":
        di = token[0]
        prop = str(token[1])
        return Hyp(di, prop)

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
        
transformer = ProofTermTransformer()
proof_term_ast = transformer.transform(test)
easy_ast = transformer.transform(easy_test)
easy_undercut_ast = transformer.transform(easy_undercut_test)


def pretty_natural(proof_term: "ProofTerm", semantic: "Rendering_Semantics") -> str:
    lines = []
    traverse_proof_term(semantic, proof_term, lines, indent=0)
    return '\n'.join(lines)

class ProofTermVisitor:
    def visit(self, node):
        if node is None:
            return None
        if isinstance(node, Mu):
            return self.visit_Mu(node)
        elif isinstance(node, Mutilde):
            return self.visit_Mutilde(node)
        elif isinstance(node, Lamda):
            return self.visit_Lamda(node)
        elif isinstance(node, Cons):
            return self.visit_Cons(node)
        elif isinstance(node, Goal):
            return self.visit_Goal(node)
        elif isinstance(node,Laog):
            return self.visit_Laog(node)
        elif isinstance(node, ID):
            return self.visit_ID(node)
        elif isinstance(node, DI):
            return self.visit_DI(node)
        else:
            return self.visit_unhandled(node)

    def visit_Mu(self, node: Mu):
        #contexts are visited before terms to make recursive construction of objects easier which are read from left to right.
        node.context = self.visit(node.context)
        node.term = self.visit(node.term)

        return node

    def visit_Mutilde(self, node: Mutilde):
        node.context = self.visit(node.context)
        node.term = self.visit(node.term)
        return node

    def visit_Lamda(self, node: Lamda):
        # node.di = self.visit(node.di)
        node.term = self.visit(node.term)
        return node

    def visit_Cons(self, node: Cons):
        node.context = self.visit(node.context)
        node.term = self.visit(node.term)
        return node

    def visit_Goal(self, node: Goal):
        return node

    def visit_Laog(self, node: Laog):
        return node

    def visit_ID(self, node: ID):
        return node

    def visit_DI(self, node: DI):
        return node

    def visit_unhandled(self, node):
        return node

class PropEnrichmentVisitor(ProofTermVisitor):
    """
    Replaces the old hack of parsing 'tt' from ID/DI name. 
    Instead, we rely on global knowledge:
      - assumption_mapping: {goal_number: proposition}
      - axiom_props: { 'r1': 'A->B', ...}
    which we can pass in the constructor.
    For example, for each Goal node, we set .prop = assumption_mapping[goal_number].
    For each ID or DI named 'r1', we set .prop = axiom_props['r1'], etc.
    Optionally, for Lamda, Cons, Mu, etc., we can set node.prop if we have enough info.
    """
    def __init__(self, axiom_props=None, assumptions=None, bound_vars=None, verbose: bool = False):
        self.assumptions = assumptions if assumptions else {}
        self.axiom_props = axiom_props if axiom_props else {}
        self.bound_vars = bound_vars if bound_vars else {}
        self.verbose = verbose
 
    def visit_Goal(self, node: Goal):
        node = super().visit_Goal(node)
        goal_num = node.number.strip()
        if node.prop:
            return node
        else:
            if self.assumptions.get(goal_num):
                logger.info("Enriching Goal with type %s", self.assumptions[goal_num]["prop"])
                node.prop = self.assumptions[goal_num]["prop"]
            else:
                if getattr(self, "verbose", False):
                    warnings.warn(f'Enrichment of node {node} not possible: {goal_num} not a key in {self.assumptions}')
                else:
                    logger.info("Enrichment skipped for Goal %s: key %s not in assumptions", node, goal_num)
            return node

    def visit_Laog(self, node: Laog):
        node = super().visit_Laog(node)
        laog_num = node.number.strip()
        if self.assumptions.get(laog_num):
            logger.info("Enriching Laog with type %s", self.assumptions[laog_num]["prop"][:-4])
            node.prop = self.assumptions[laog_num]["prop"][:-4]
        return node

    def visit_ID(self, node: ID):
        node = super().visit_ID(node)
        if node.name in self.axiom_props and node.name not in self.bound_vars:
            if node.prop is None:
                logger.info("Enriching axiom %s with type %s",
                            node.name, self.axiom_props[node.name].strip("()"))
                node.prop = self.axiom_props[node.name].strip("()")
            else:
                logger.debug("Axiom %s already enriched with type %s", node.name, node.prop)
        elif node.name in self.bound_vars and node.prop is None:
            logger.info("Enriching bound variable %s with type %s",
                        node.name, self.bound_vars[node.name])
            node.prop = self.bound_vars[node.name]
            if node.prop.startswith("~") or node.prop.startswith("¬"):
                node.flag = "bound_negation"
                if self.verbose: pass
                logger.debug('Bound negation flagged for instruction generation.')
        elif node.prop:
            self.bound_vars[node.name] = node.prop
        elif node.name == "_F_":
            node.flag = "Falsum"
            logger.debug("Falsum flagged for instruction generation.")
        else:
            warnings.warn(f'Enrichment of node {node} not possible.')
        return node

    def visit_DI(self, node: DI):
        node = super().visit_DI(node)
        if node.name in self.axiom_props and node.name not in self.bound_vars:
            if node.prop is None:
                logger.info("Enriching axiom %s with type %s based on declared axiom",
                            node.name, self.axiom_props[node.name].strip("()"))
                node.prop = self.axiom_props[node.name].strip("()")
            else:
                logger.debug("Axiom %s already enriched with type %s", node.name, node.prop)
        elif node.name in self.bound_vars and node.prop is None:
            logger.info("Enriching bound variable %s with type %s based on bound variable",
                        node.name, self.bound_vars[node.name])
            node.prop = self.bound_vars[node.name]
            if node.prop.startswith("~") or node.prop.startswith("¬"):
                node.flag = "bound_negation"
                if self.verbose: pass
                logger.debug('Bound negation flagged for instruction generation.')
        elif node.name == "_F_":
            node.flag = "Falsum"
            logger.debug("Falsum flagged for instruction generation.")
        else:
            warnings.warn(f'Enrichment of node {node.name} not possible.')
        return node

    def visit_Lamda(self, node: Lamda):
        self.bound_vars[node.di.di.name] = node.di.prop
        node = super().visit_Lamda(node)
        # Optionally compute node.prop from node.di.prop + "->" + node.term.prop
        # if node.di.prop and node.term.prop exist.
        if node.prop is None and node.di.prop and node.term.prop:
            node.prop = f"{node.di.prop}->{node.term.prop}"
        return node

    def visit_Cons(self, node: Cons):
        node = super().visit_Cons(node)
        # Similarly, if node.term.prop and node.context.prop => node.prop = ...
        if node.prop is None and node.term.prop and node.context.prop:
            node.prop = f"{node.term.prop}->{node.context.prop}"
        return node

    def visit_Mu(self, node: Mu):
        self.bound_vars[node.id.name]=node.prop
        node = super().visit_Mu(node)
        node.contr = node.term.prop if node.term.prop else node.context.prop if node.context.prop else None
        return node

    def visit_Mutilde(self, node: Mutilde):
       
        self.bound_vars[node.di.name]=node.prop
        node = super().visit_Mutilde(node)
        #print("node.term, node.context", node.term.prop, node.context.prop)
        node.contr = node.term.prop if node.term.prop else node.context.prop if node.context.prop else None
        return node


#Argument Reducer Helpers

def _var_occurs(name: str, node: ProofTerm) -> bool:
    """Return True iff *name* appears free inside *node*."""
    if isinstance(node, (ID, DI)):
        return node.name == name
    if isinstance(node, Lamda):
        # variable shadowing inside lamda binder (node.di.di.name)
        if node.di.di.name == name:
            return False  # bound, not free
    if isinstance(node, Mu):
        if node.id.name == name:
            return False
    if isinstance(node, Mutilde):
        if node.di.name == name:
            return False
    # recurse
    for child in getattr(node, 'term', None), getattr(node, 'context', None):
        if child and _var_occurs(name, child):
            return True
    return False

def _is_affine(varname: str, command: ProofTerm) -> bool:
    """A binder is *affine* if its bound variable occurs nowhere in the
    command body (term + context)."""
    if isinstance(command, (Mu, Mutilde)):
        return not _var_occurs(varname, command.term) and not _var_occurs(varname, command.context)
    return False

def _subst(node, name: str, replacement):
    """Return a **deep‑copied** version of *node* where every **free**
    occurrence of the variable *name* (i.e. an ``ID``/``DI`` whose *name*
    equals *name*) is replaced by a deep‑copy of *replacement*.

    The substitution is *capture‑avoiding*: whenever we descend beneath a
    binder that *re‑binds* the same name – namely λ, µ, or μ̃ – we stop
    the traversal along that branch.
    """
    # Atomic cases -----------------------------------------------------------
    if isinstance(node, (ID, DI)):
        return deepcopy(replacement) if node.name == name else deepcopy(node)

    # Binder blocks substitution under shadowing -----------------------------
    if isinstance(node, Lamda) and node.di.di.name == name:
        return deepcopy(node)                  # λx. …  — shadowed
    if isinstance(node, Mu) and node.id.name == name:
        return deepcopy(node)                  # μx. …  — shadowed
    if isinstance(node, Mutilde) and node.di.name == name:
        return deepcopy(node)                  # μ' x. … — shadowed

    # Recursive descent ------------------------------------------------------
    new_node = deepcopy(node)
    if hasattr(new_node, 'term') and new_node.term is not None:
        new_node.term = _subst(new_node.term, name, replacement)
    if hasattr(new_node, 'context') and new_node.context is not None:
        new_node.context = _subst(new_node.context, name, replacement)
    return new_node

    
class ArgumentTermReducer(ProofTermVisitor):
    """
    Implements the following reduction rules (each left and right):
       - lambda
       - undercut (defeat and defence)
       - TODO: rebut
       - TODO: support
       - mu
       - TODO: eta
    """

    def __init__(self, *, verbose: bool = True, assumptions=None, axiom_props=None):
        super().__init__()
        self.verbose      = verbose
        self.assumptions  = assumptions  or {}
        self.axiom_props  = axiom_props or {}
        self._root        = None     # set by reduce()
        self._step        = 0        # counter for pretty printing
        
    def reduce(self, root: "ProofTerm") -> "ProofTerm":
        """Return the *normal* form obtained by exhaustively applying the
        implemented reduction rules.  For the moment this is equivalent to a
        single traversal because only one local rule is implemented."""
        self._root = root
        return self.visit(root)

    # helper – pretty‑print current state ------------------------------
    def _print_current(self) -> None:
        if not self.verbose:
            return
        show = deepcopy(self._root)
        show = PropEnrichmentVisitor(assumptions=self.assumptions,
                                     axiom_props=self.axiom_props).visit(show)
        show = ProofTermGenerationVisitor().visit(show)
        self._step += 1
        logger.info("[reduction step %d]\n%s\n", self._step, show.pres)

    def visit_Mu(self, node: Mu):
        # First, normalise the sub‑components so that the rule also fires in
        # inner positions.
        

        # λ‑rule ---------------------------------------------------------
        if isinstance(node.term, Lamda) and isinstance(node.context, Cons):
            lam: Lamda = node.term
            cons: Cons = node.context
            if self.verbose: logger.info("Applying λ‑rule")
            v      = cons.term      # the argument v
            E      = cons.context  # the continuation E (might itself be None)
            # Build the new µ'‑binder that will become the *context* part.
            new_context = Mutilde(
                di_=lam.di.di,          # the variable x
                prop=lam.di.prop,     # its declared type
                term=deepcopy(lam.term),
                context=deepcopy(E)
            )
            node.term    = deepcopy(v)
            node.context = new_context
            self._print_current()
            self.visit_Mu(node)
            
            return node

        # affine helper --------------------------------------------------
        def _guards(inner_mu: Mu, ctx_mt: Mutilde):
            # both binders affine in their own commands
            if not (_is_affine(inner_mu.id.name, inner_mu) and
                    _is_affine(ctx_mt.di.name,   ctx_mt)):
                if self.verbose: logger.debug("Guard A failed")
                return False
            # both protect goals with equal prop (we ignore number for now)
            if not isinstance(inner_mu.term, Goal) and (isinstance(ctx_mt.context, Goal) or isinstance(ctx_mt.context.context.term, Goal)):
                if self.verbose: logger.debug("Guard B failed")
                return False
            if not isinstance(ctx_mt.context, Mutilde):
                if self.verbose: logger.debug("Guard C failed")
                return False
            if not node.id.name == inner_mu.context.name:
                if self.verbose: logger.debug("Guard D failed")
                return False
            #if isinstance(inner_mu.term, Goal) and isinstance(ctx_mt.context.context.term, Goal):
                #What is the point of this guard again?
             #   print(f"termprop {inner_mu.term.prop} vs Contextprop {ctx_mt.term.prop}")
              #  return inner_mu.term.prop == ctx_mt.context.context.term.prop
            if self.verbose: logger.debug("termprop %s vs contextprop %s", inner_mu.term.prop, ctx_mt.term.prop)
            return inner_mu.term.prop == ctx_mt.term.prop

        
        # affine μ‑rule --------------------------------------------
        if isinstance(node.term, Mu) and isinstance(node.context, Mutilde):
            inner_mu: Mu = node.term
            ctx_mt:   Mutilde = node.context
            if self.verbose: logger.info("Alternative Arguments %s and %s detected.", inner_mu.id.name, ctx_mt.di.name)
            if self.verbose: logger.info("Guards %s", _guards(inner_mu, ctx_mt))
            if _guards(inner_mu, ctx_mt)==True:

                # Case 1: ctx_mt.context is µ̃ with affine binder → throw‑away
                if self.verbose: logger.info("Case 1? %s", _is_affine(ctx_mt.context.di.name, ctx_mt.context))
                if _is_affine(ctx_mt.context.di.name, ctx_mt.context):
                    if self.verbose: logger.info("Applying defence rule")
                    # Apply rewrite: μ α .⟨ G || α ⟩
                    node.term    = deepcopy(inner_mu.term)
                    node.context = deepcopy(inner_mu.context)  # should be ID α
                    self._print_current()
                    self.visit_Mu(node)
                    return node

                # Case 2: ctx_mt.context already in normal form (no more redexes we know)
                # Rough heuristic: after recursive call, if no λ‑redex or affine‑µ pattern
                # matches in that sub‑tree, regard it as normal‑form.
                if self.verbose: logger.info("Case 2? %s",  not self._has_next_redex(ctx_mt.context))
                if not self._has_next_redex(ctx_mt.context):
                    if self.verbose: logger.info("Applying defeat rule")
                    node.term    = deepcopy(ctx_mt.term)
                    node.context = deepcopy(ctx_mt.context)  # t*
                    self._print_current()
                    self.visit_Mu(node)
                    return node
                else:
                    if self.verbose: logger.info("Simplifying attacker argument")
                    ctx_mt.context = self.visit_Mutilde(ctx_mt.context)
                    self.visit_Mu(node)
                    return node


        # general μ‑rule --------------------------------------------
    # ────────────────────────────────────────────────────────────────────
        # GENERAL µ‑REDUCTION  ⟨ µ x.c  ∥  E ⟩  →  [E/x]c
        # Pattern in the AST:  node.term  is a *Mu*  (µ x.c)
        #                      node.context           is  E
        # After substitution we replace *node* by the command *c* with the
        # variable occurrences rewritten.  To stay within the current AST
        # we *mutate* *node* in‑place so that it *becomes* that command
        # (its *id/prop* remain untouched — this is adequate for our use
        # cases because outer binders are unaffected by the rewrite).
        # ────────────────────────────────────────────────────────────────────
        if isinstance(node.term, Mu):
            if self.verbose: logger.info("Applying general Mu reduction rule")
            inner = node.term          # µ x.c
            x     = inner.id.name
            E     = node.context       # replacement

            # Perform capture‑avoiding substitution inside the *command* c.
            new_term    = _subst(inner.term,    x, deepcopy(E))
            new_context = _subst(inner.context, x, deepcopy(E))

            # Splice the substituted command into *node* (drop µ x.).
            node.term    = new_term
            node.context = new_context
            self._print_current()
            self.visit_Mu(node)
            return node
        if isinstance(node.context, Mutilde):
            if self.verbose: logger.info("Applying general Mutilde rule")
            inner = node.context       # µ x.c
            alpha     = inner.di.name
            v     = node.term       # replacement

            # Perform capture‑avoiding substitution inside the *command* c.
            new_term    = _subst(inner.term,    alpha, deepcopy(v))
            new_context = _subst(inner.context, alpha, deepcopy(v))

            # Splice the substituted command into *node* (drop µ x.).
            node.term    = new_term
            node.context = new_context
            self._print_current()
            self.visit_Mu(node)
            return node
        #node = super().visit_Mu(node)

        node = super().visit_Mu(node)
        return node

    def visit_Mutilde(self, node: Mutilde):
        # First, normalise the sub‑components so that the rule also fires in
        # inner positions.
        # node = super().visit_Mutilde(node)

        # λ‑rule ---------------------------------------------------------
        if isinstance(node.term, Lamda) and isinstance(node.context, Cons):
            lam: Lamda = node.term
            cons: Cons = node.context

            v      = cons.term      # the argument v
            E      = cons.context  # the continuation E (might itself be None)

            # Build the new µ'‑binder that will become the *context* part.
            new_context = Mutilde(
                di_=lam.di.di,          # the variable x
                prop=lam.di.prop,     # its declared type
                term=deepcopy(lam.term),
                context=deepcopy(E)
            )
            node.term    = deepcopy(v)
            node.context = new_context
            self._print_current()
            self.visit_Mutilde(node)
            return node

        # affine helper --------------------------------------------------
        def _guards(inner_mu: Mu, ctx_mt: Mutilde):
            # both binders affine in their own commands
            if not (_is_affine(inner_mu.id.name, inner_mu) and
                    _is_affine(ctx_mt.di.name,   ctx_mt)):
                if self.verbose: logger.debug("Guard Atilde failed")
                return False
            # both protect goals with equal prop (we ignore number for now)
            if not (isinstance(inner_mu.context, Laog) and isinstance(ctx_mt.context, Laog)):
                if self.verbose: logger.debug("Guard Btilde failed")
                return False
            if not isinstance(inner_mu.term, Mu):
                return False
            if not node.di.name == ctx_mt.term.name:
                if self.verbose: logger.debug("Guard Ctilde failed")
                return False
            if self.verbose: logger.debug("termprop %s vs contextprop %s", inner_mu.term.prop, ctx_mt.term.prop)
            return inner_mu.context.prop == ctx_mt.context.prop
        
        # affine μ'‑rule --------------------------------------------
        if isinstance(node.term, Mu) and isinstance(node.context, Mutilde):
            inner_mu: Mu = node.term
            ctx_mt:   Mutilde = node.context
            if self.verbose: logger.info("Alternative Counterarguments %s and %s detected.", inner_mu.id.name, ctx_mt.di.name)
            if _guards(inner_mu, ctx_mt)==True:

                # Case 1: inner_mu.term is µ̃ with affine binder → throw‑away
                if self.verbose: logger.info("Case 1? %s", _is_affine(inner_mu.term.id.name, inner_mu.term))
                if _is_affine(inner_mu.term.id.name, inner_mu.term):
                    if self.verbose: logger.info("Applying Mutilde defence rule")
                    # Apply rewrite: μ α .⟨ G || α ⟩
                    node.term    = deepcopy(ctx_mt.term)
                    node.context = deepcopy(ctx_mt.context)  # should be ID α
                    self._print_current()
                    self.visit_Mutilde(node)
                    return node

                # Case 2: ctx_mt.context already in normal form (no more redexes we know)
                # Rough heuristic: after recursive call, if no λ‑redex or affine‑µ pattern
                # matches in that sub‑tree, regard it as normal‑form.
                if self.verbose: logger.info("Case 2? %s",  not self._has_next_redex(inner_mu.term))
                if not self._has_next_redex(inner_mu.term):
                    if self.verbose: logger.info("Applying Mutilde defeat rule")
                    node.term    = deepcopy(inner_mu.term)
                    node.context = deepcopy(inner_mu.context)  # t*
                    self._print_current()
                    self.visit_Mutilde(node)
                    return node
                else:
                    if self.verbose: logger.info("Simplifying attacker argument")
                    inner_mu.term  = self.visit_Mu(inner_mu.term)
                    self.visit_Mu(node)
                    return node

                    # self.visit_Mu(inner_mu.term)

        if isinstance(node.term, Mu):
            if self.verbose: logger.info("Applying general Mu reduction rule")
            inner = node.term          # µ x.c
            x     = inner.id.name
            E     = node.context       # replacement

            # Perform capture‑avoiding substitution inside the *command* c.
            new_term    = _subst(inner.term,    x, deepcopy(E))
            new_context = _subst(inner.context, x, deepcopy(E))

            # Splice the substituted command into *node* (drop µ x.).
            node.term    = new_term
            node.context = new_context
            self._print_current()
            self.visit_Mutilde(node)
            return node

        if isinstance(node.context, Mutilde):
            if self.verbose: logger.info("Applying general Mutilde rule")
            inner = node.context       # µ x.c
            alpha     = inner.di.name
            v     = node.term       # replacement
            # Perform capture‑avoiding substitution inside the *command* c.
            new_term    = _subst(inner.term,    alpha, deepcopy(v))
            new_context = _subst(inner.context, alpha, deepcopy(v))

            # Splice the substituted command into *node* (drop µ x.).
            node.term    = new_term
            node.context = new_context
            self._print_current()
            self.visit_Mutilde(node)
            return node
        node = super().visit_Mutilde(node)
        
        return node

    def _has_next_redex(self, n: ProofTerm) -> bool:
        """Very lightweight check: look for patterns we *currently* reduce."""
        if isinstance(n, Mu):
            # λ‑redex?
            if isinstance(n.term, Lamda) and isinstance(n.context, Cons):
                return True
            # any mu or mutilde pattern?
            if isinstance(n.term, Mu) or isinstance(n.context, Mutilde):
                return True
        if isinstance(n, Mutilde):
            if isinstance(n.term, Lamda) and isinstance(n.context, Cons):
                return True
            # any mu or mutilde pattern?
            if isinstance(n.term, Mu) or isinstance(n.context, Mutilde):
                return True
        # recurse quickly
        for child in getattr(n, 'term', None), getattr(n, 'context', None):
            if child and self._has_next_redex(child):
                return True
        return False



    
class Rendering_Semantics:
    def __init__(self, indentation, Mu, Mutilde, Lamda, Cons, Goal, ID, DI):
        self.indentation = indentation
        self.Mu = Mu
        self.Mutilde = Mutilde
        self.Lamda = Lamda
        #self.Hyp = Hyp
        self.Cons = Cons
        self.Goal = Goal
        #self.Done = Done
        self.ID = ID
        self.DI = DI

natural_language_rendering = Rendering_Semantics('   ', "we need to prove ", f"we proved ", f"assume ", f"and", f"? ", f"done ", f"by ")
natural_language_dialectical_rendering = Rendering_Semantics('   ', "Assume a refutation of ", f"Assume a proof of  ", f"assume ", f"and", f"? ", f"but then we have a contradiction, done ", f"by ")
natural_language_argumentative_rendering = Rendering_Semantics('   ', ["We will argue for ", "undercutting ", "supported by alternative ", "undercut by "], [f"We will argue against ", "using ", "by adapter"], f"assume ", f"and", f"by default ", f"done ", f"by ")

def traverse_proof_term(semantic, term, lines, indent): # TODO: change to instantiation of ProofTermVisitor
    indent_str = semantic.indentation * indent
    if isinstance(term, Mu):
        if term.id.name == "stash":
            lines.append(f"{indent_str}"+semantic.Mu[1] + f"{term.prop}"+ " in")
            traverse_proof_term(semantic, term.term, lines, indent + 1)
        elif term.id.name == "support":
            lines.append(f"{indent_str}"+semantic.Mu[0] + f"{term.prop}")
            traverse_proof_term(semantic, term.term, lines, indent + 1)
            lines.append(f"{indent_str}"+semantic.Mu[2])
            traverse_proof_term(semantic, term.context, lines, indent + 1)
        elif re.compile(r'alt\d').match(term.id.name):
            traverse_proof_term(semantic, term.term, lines, indent)
            traverse_proof_term(semantic, term.context, lines, indent)
        elif term.id.name == "undercut":
             # Note that the result is somewhat ugly due to the need for an adapter to deal with Fellowship's treatment of negation.
             lines.append(f"{indent_str}"+semantic.Mu[0] + f"{term.prop}" + f"({term.id.name})")
             traverse_proof_term(semantic, term.context, lines, indent + 1)
             lines.append(f"{indent_str}"+semantic.Mu[3])
             traverse_proof_term(semantic, term.term, lines, indent + 1)
        else:
            lines.append(f"{indent_str}"+semantic.Mu[0] + f"{term.prop}" + f"({term.id.name})")
            traverse_proof_term(semantic, term.term, lines, indent + 1)
            traverse_proof_term(semantic, term.context, lines, indent + 1)
        # lines.append(f"{indent_str}{semantic.Done}")
    elif isinstance(term, Mutilde):
        if term.di.name == "issue":
            lines.append(f"{indent_str}"+semantic.Mutilde[0] + f"{term.prop}")
            traverse_proof_term(semantic, term.term, lines, indent)
            lines.append(f"{indent_str}"+semantic.Mutilde[1])
            traverse_proof_term(semantic, term.context, lines, indent)
        elif term.di.name == "adapter":
            traverse_proof_term(semantic, term.term, lines, indent)
            lines.append(f"{indent_str}".removesuffix(semantic.indentation)+semantic.Mutilde[2])
        # For now an abandoned attempt to pretty print Fellowship's peculiar treatment of negation.
        # elif term.prop == "¬A":
        #     traverse_proof_term(semantic, term.context, lines, indent + 1)
        elif re.compile(r'alt\d').match(term.di.name):
            traverse_proof_term(semantic, term.term, lines, indent)
            traverse_proof_term(semantic, term.context, lines, indent)
        else:
            lines.append(f"{indent_str}"+semantic.Mutilde[0] + f"{term.prop} " + f"({term.di.name})")
            traverse_proof_term(semantic, term.term, lines, indent + 1)
            traverse_proof_term(semantic, term.context, lines, indent + 1)
        
            
    elif isinstance(term, Lamda):
        lines.append(f"{indent_str}"+semantic.Lamda + f"{term.di.prop}" + f"({term.di.di.name})")
        traverse_proof_term(semantic, term.term, lines, indent)
    # elif isinstance(term, Hyp):
    #     lines.append(f"{indent_str}"+semantic.Hyp + f"{term.id}")
    elif isinstance(term, Cons):
        lines.append(f"{indent_str}"+semantic.Cons)
        traverse_proof_term(semantic, term.term, lines, indent)
        traverse_proof_term(semantic, term.context, lines, indent)
    elif isinstance(term, Goal):
        lines.append(f"{indent_str}"+semantic.Goal + f"{term.number}")
    elif isinstance(term, DI):
        lines.append(f"{indent_str}" + semantic.DI + f"{term.name}")
    elif isinstance(term, ID):
        lines.append(f"{indent_str}".removesuffix(semantic.indentation) + semantic.ID)
    else:
        lines.append(f"{indent_str}Unhandled term type: {type(term)}")

# nat = pretty_natural(proof_term_ast, natural_language_argumentative_rendering )
# dia = pretty_natural(proof_term_ast, natural_language_dialectical_rendering )
# arg = pretty_natural(easy_undercut_ast, natural_language_argumentative_rendering)
# easy = pretty_natural(easy_ast, natural_language_argumentative_rendering)
# print(nat)
# print(dia)
# print(arg)
# print(easy)

def match_trees(nodeA: "ProofTerm", nodeB: "ProofTerm", mapping: Dict[str, str]) -> bool:
    # If node types are different, cannot match
    if type(nodeA) != type(nodeB):
        logger.debug("Type mismatch: %s vs %s", type(nodeA), type(nodeB))
        return False
    logger.debug("Match")
    # Handle different node types
    if isinstance(nodeA, Mu):
        # Match id_, prop, term, context
        idB = nodeB.id.name
        idA = nodeA.id.name
        if idB in mapping:
            if mapping[idB] != idA:
                return False
        else:
            mapping = mapping.copy()
            mapping[idB] = idA
        if nodeB.prop != nodeA.prop:
            return False
        if not match_trees(nodeA.term, nodeB.term, mapping):
            return False
        if not match_trees(nodeA.context, nodeB.context, mapping):
            return False
        return True

    elif isinstance(nodeA, Mutilde):
        # Match di_, prop, term, context
        diB = nodeB.di.name
        diA = nodeA.di.name
        if diB in mapping:
            if mapping[diB] != diA:
                return False
        else:
            mapping = mapping.copy()
            mapping[diB] = diA
        if nodeB.prop != nodeA.prop:
            return False
        if not match_trees(nodeA.term, nodeB.term, mapping):
            return False
        if not match_trees(nodeA.context, nodeB.context, mapping):
            return False
        return True

    elif isinstance(nodeA, Lamda):
        # Match di_, prop, term
        diB = nodeB.di.di.name
        diA = nodeA.di.di.name
        if diB in mapping:
            if mapping[diB] != diA:
                return False
        else:
            mapping = mapping.copy()
            mapping[diB] = diA
        if nodeB.prop != nodeA.prop:
            return False
        if not match_trees(nodeA.term, nodeB.term, mapping):
            return False
        return True

    elif isinstance(nodeA, Cons):
        # Match term and context
        if not match_trees(nodeA.term, nodeB.term, mapping):
            return False
        if not match_trees(nodeA.context, nodeB.context, mapping):
            return False
        return True

    elif isinstance(nodeA, Goal):
        # Goal numbers don't matter.
        # if nodeA.number != nodeB.number:
        #     return False
        return True

    elif isinstance(nodeA, ID) or isinstance(nodeA, DI):
        # Match names with mapping
        nameA = nodeA.name
        nameB = nodeB.name
        if nameB in mapping:
            if mapping[nameB] != nameA:
                return False
        else:
            mapping = mapping.copy()
            mapping[nameB] = nameA
        return True

    else:
        # Unhandled node type
        return False


def get_child_nodes(node: "ProofTerm") -> List["ProofTerm"]:
    child_nodes = []
    if isinstance(node, Mu):
        child_nodes.append(node.term)
        child_nodes.append(node.context)
    elif isinstance(node, Mutilde):
        child_nodes.append(node.term)
        child_nodes.append(node.context)
    elif isinstance(node, Lamda):
        child_nodes.append(node.term)
    elif isinstance(node, Cons):
        child_nodes.append(node.term)
        child_nodes.append(node.context)
    # For ID, DI, and Goal, there are no child nodes
    return child_nodes

def is_subargument(A: "ProofTerm", B: "ProofTerm") -> bool:
    # Returns True if B is a subargument of A up to variable renaming
    # We need to traverse A and attempt to match B starting from each node
    # Might be worth to also create a convenience method to generate all subarguments.
    nodes_to_visit = [A]
    # First we need to strip the thesis root.
    B = B.term
    while nodes_to_visit:
        current_node = nodes_to_visit.pop()
        if match_trees(current_node, B, {}):
            return True
        # Add children to the nodes_to_visit list
        child_nodes = get_child_nodes(current_node)
        nodes_to_visit.extend(child_nodes)
    return False

def contrary(A: str) -> str:
    # Returns the contrary of assumption A
    if A.endswith('_bar'):
        return A[:-4]
    else:
        return A + '_bar'

def neg(A: str) -> str:
    return '¬'+A

def label_assumption(node: "ProofTerm", A: str, assumptions: Dict[str, Dict[str, Any]]) -> Tuple[str, "ProofTerm"]: #TODO: Implement as an instantiation of ProofTermVisitor
    #TODO: Have to first test all own assumptions that are not parts of stashed or supporting subarguments.
    # If any are OUT or UNDEC, any (intermediate) conclusion of the argument becomes UNDEC.
    
    if node is None:
        return ('UNDEC',node)

    if isinstance(node, Goal):
        # Use the goal number to retrieve the corresponding proposition
        logger.debug("ISGOAL")
        goal_number = node.number.strip()
        goal_prop = assumptions[goal_number]["prop"]
        if goal_prop is None:
            logger.debug("%s", goal_prop)
            return ('UNDEC', node)

        elif goal_prop == A:
        # A matches the goal's proposition
            return ('IN', node)
        elif goal_prop == contrary(A):
            return ('OUT',node)
        else:
            # A does not match the goal's proposition
            return ('UNDEC', node)


    elif isinstance(node, ID):
        logger.debug("ISID")
        # Rule: An assumption A is OUT if the root is an ID object with prop A
        if node.prop == A:
            return ('OUT',node)
        else:
            return ('UNDEC', node)

    elif isinstance(node, DI):
        logger.debug("ISDI")
        # Rule: An assumption A_bar is OUT if the root is a DI object with prop A
        if node.prop == contrary(A):
            return ('OUT',node)
        else:
            return ('UNDEC', node)

    elif isinstance(node, Lamda):
        logger.debug("ISLAMDA")
        label_di = label_assumption(node.di.di, A, assumptions)[0]
        label_term = label_assumption(node.term, A, assumptions)[0]

        if label_di == 'UNDEC' and label_term == 'IN' and node.prop != contrary(A):
            return ('IN', node)
        elif label_di == 'OUT' or label_term == 'OUT' or node.prop == contrary(A):
            return ('OUT',node)
        else:
            return ('UNDEC', node)

    elif isinstance(node, Cons):
        logger.debug("ISCONS")
        label_term = label_assumption(node.term, A, assumptions)[0]
        label_context = label_assumption(node.context, A, assumptions)[0]

        if (label_term == 'IN' or label_context == 'IN') and not (label_term == 'OUT' or label_context == 'OUT'):
            return ('IN', node)
        elif label_term == 'OUT' or label_context == 'OUT' or node.prop == A:
            return ('OUT',node)
        else:
            return ('UNDEC', node)

    elif isinstance(node, Mu):
        logger.debug("ISMU")
        if node.id.name == 'stash':
            logger.debug("ISSTASH")
            if node.prop == A:
            # An assumption A is IN if the root is a Mu-object with id 'stash' and prop A
                return ('IN', node)
            elif node.prop == contrary(A) or node.prop == neg(A):
                return ('OUT',node)
            else:
                return ('UNDEC', node)
        elif node.id.name == 'support':
            logger.debug("ISSUPPORT")
            if node.prop == A:
            # An assumption that gets shadowed by an alternative mu-bound continutation is UNDEC. This is because the bound continuation provides an alternative way to derive the assumption so it is no longer needed.. 
                return ('UNDEC', node)
            elif node.prop == contrary(A) or node.prop==neg(A):
                return ('OUT',node)
            else:
                label_term = label_assumption(node.term, A, assumptions)[0]
                label_context = label_assumption(node.context, A, assumptions)[0]
            
                if (label_term == 'IN' or label_context == 'IN') and not (label_term == 'OUT' or label_context == 'OUT'):
                    return ('IN', node)

                elif label_term == 'OUT' or label_context == 'OUT':
                    return ('OUT',node)

                else:
                    return ('UNDEC', node)
                
        elif node.prop != contrary(A) and node.prop != neg(A) :
            logger.debug("ISNOTCONTR")
            label_term = label_assumption(node.term, A, assumptions)[0]
            label_context = label_assumption(node.context, A, assumptions)[0]
            
            if (label_term == 'IN' or label_context == 'IN') and not (label_term == 'OUT' or label_context == 'OUT'):
                return ('IN', node)

            elif label_term == 'OUT' or label_context == 'OUT':
                return ('OUT',node)

            else:
                return ('UNDEC', node)

        else:
            
            return ('OUT',node)


    elif isinstance(node, Mutilde):
        logger.debug("ISMUTILDE")
        if node.di.name == 'stash':
            if node.prop == contrary(A) or node.prop == neg(A):
                    # An assumption A_bar is IN if the root is a Mutilde-object with di 'stash' and prop A
                    return ('IN', node)
            elif node.prop == A:
                    return ('OUT',node)
            else:
                return ('UNDEC', node)
        #TODO elif support, stash
        elif node.prop != A:
            label_term = label_assumption(node.term, A, assumptions)[0]
            label_context = label_assumption(node.context, A, assumptions)[0]

            if (label_term == 'IN' or label_context == 'IN') and not (label_term == 'OUT' or label_context == 'OUT'):
                return ('IN', node)
            
            elif label_term == 'OUT' or label_context == 'OUT':
                return ('OUT',node)

            else:
                return ('UNDEC', node)
            
        else:
            return ('OUT',node)

    else:
        return ('UNDEC', node)


class ProofTermGenerationVisitor(ProofTermVisitor):
    """Generate a proof term from an (enriched or rewritten) argument body."""

    def __init__(self, verbose: bool = False):
        self.verbose = verbose
        pass
    
    def visit_Mu(self, node : Mu):
        node = super().visit_Mu(node)
        if self.verbose == True:
            node.pres = f'μ{node.id.name}:{node.prop}.<{node.term.pres}|{node.contr}|{node.context.pres}>'
        else:
            node.pres = f'μ{node.id.name}:{node.prop}.<{node.term.pres}||{node.context.pres}>'
        return node

    def visit_Mutilde(self, node : Mutilde):
        node = super().visit_Mutilde(node)
        if self.verbose == True:
            node.pres = f"μ'{node.di.name}:{node.prop}.<{node.term.pres}|{node.contr}|{node.context.pres}>"
        else:
            node.pres = f"μ'{node.di.name}:{node.prop}.<{node.term.pres}||{node.context.pres}>"
        return node

    def visit_Lamda(self, node : Lamda):
        node = super().visit_Lamda(node)
        node.pres = f'λ{node.di.di.name}:{node.di.prop}.{node.term.pres}'
        return node

    def visit_Cons(self, node : Cons):
        node = super().visit_Cons(node)
        node.pres = f'{node.term.pres}*{node.context.pres}'
        return node

    def visit_Goal(self, node : Goal):
        node = super().visit_Goal(node)
        node.pres = f'{node.number}:{node.prop}'
        return node
    def visit_Laog(self, node : Laog):
        node = super().visit_Laog(node)
        node.pres = f'{node.number}:{node.prop}'
        return node

    def visit_ID(self, node : ID):
        node = super().visit_ID(node)
        if node.prop:
            node.pres = f'{node.name}:{node.prop}'
        else:
            node.pres= f'{node.name}'
        return node

    def visit_DI(self, node : DI):
        node = super().visit_DI(node)
        if node.prop:
            node.pres = f'{node.name}:{node.prop}'
        else:
            node.pres= f'{node.name}'
        return node

    def visit_unhandled(self, node):
        return node

def fn(negated_prop : str):
    return negated_prop.replace("¬","~")


class InstructionsGenerationVisitor(ProofTermVisitor): #TODO: make this class purely functional instead of using a side effect.
    """Generate instructions from an (enriched) argument body."""

    def __init__(self):
        self.instructions = collections.deque('')
        pass

    def return_instructions(self, proofterm):
        self.instructions.clear()
        self.visit(proofterm)
        return self.instructions
    
    def visit_Mu(self, node : Mu):
        node = super().visit_Mu(node)
        if node.id.name == 'thesis':
            pass
        else:
            if node.contr:
                if (node.prop.startswith("¬") or node.prop.startswith("¬")) and node.prop == node.contr :
                    for _ in range(3):
                        self.instructions.popleft()
                    self.instructions.appendleft(f"elim {node.id.name}")
                else:
                    self.instructions.appendleft(f"cut ({fn(node.contr)}) {node.id.name}.")
            else:
                raise Exception("Could not identify cut proposition.")
        return node

    def visit_Mutilde(self, node : Mutilde):
        node = super().visit_Mutilde(node)
        if node.contr:
            if (node.prop.startswith("¬") or node.prop.startswith("¬")) and node.prop == node.contr:
                    self.instructions.appendleft(f"elim {node.di.name}")
            else:
                self.instructions.appendleft(f"cut ({fn(node.contr)}) {node.di.name}.")
        else:
            raise Exception("Could not identify cut proposition.")

        
        return node

    def visit_Lamda(self, node : Lamda):
        node = super().visit_Lamda(node)
        if node.di.di.name:
            self.instructions.appendleft(f'elim {node.di.di.name}.')
        else:
            raise Exception("Missing hypothesis name.")
        return node

    def visit_Cons(self, node : Cons):
        node = super().visit_Cons(node)
        self.instructions.appendleft(f'elim.')
        return node

    def visit_Goal(self, node : Goal):
        node = super().visit_Goal(node)
        self.instructions.appendleft(f'next.')
        return node
    
    def visit_Laog(self, node:Laog):
        node = super().visit_Laog(node)
        self.instructions.appendleft(f'next.')
        return node
    

    def visit_ID(self, node : ID):
        node = super().visit_ID(node)
        if node.name:
            if node.name == "thesis":
                return node
            if node.flag == "bound negation":
                return node
            if node.flag == "Falsum":
                return node
            else:
                self.instructions.appendleft(f'axiom {node.name}.')
        else:
            raise Exception("Axiom name missing.")
        return node

    def visit_DI(self, node : DI):
        node = super().visit_DI(node)
        if node.name:
            if node.flag == "bound negation":
                return node
            if node.flag == "Falsum":
                return node
            else:
                self.instructions.appendleft(f'axiom {node.name}.')
        else:
            raise Exception("Axiom name missing.")
        return node

    def visit_unhandled(self, node):
        raise Exception("Unhandled node type: {type(node)}")
        return node

# ────────────────────────────────────────────────────────────────────────────────
#  Graft operator
#     • *graft_single*   — replace one specific open goal ?k / !k having prop P
#     • *graft_uniform*  — replace **all** open goals / laogs whose prop is P
#
#  Capturing semantics
#  -------------------
#  When we splice an argument *A* (its proof‑term AST) in place of a Goal or
#  Laog that sits underneath binders introducing variables
#
#        x₁:P₁  …  xₘ:Pₘ     (μ‑IDs capture **Laogs** )
#        y₁:Q₁  …  yₙ:Qₙ     (λ / μ̃‑DIs capture **Goals** )
#
#  then **inside the clone of A** any *open* Goal whose proposition is some Qⱼ is
#  replaced by the DI‑variable yⱼ, and any *open* Laog whose proposition equals
#  Pᵢ **or Pᵢ_bar** (wrapper convention) is replaced by the ID‑variable xᵢ.
# ────────────────────────────────────────────────────────────────────────────────

#__all__ = ["graft_single", "graft_uniform"]

# ---------------------------------------------------------------------------
#  Helper – capture‑aware substitution of Goals / Laogs by variables
# ---------------------------------------------------------------------------

class _CapturingSubst(ProofTermVisitor):
    """Replace captured Goals / Laogs by the corresponding variable."""

    def __init__(self, *, goal_map: dict[str, str], laog_map: dict[str, str]):
        # prop → DI‑var  /  prop → ID‑var
        self.goal_map = goal_map
        self.laog_map = laog_map

    # generic traversal -------------------------------------------------
    def visit(self, node):
        if node is None:
            return None
        if isinstance(node, Goal):
            return self._rewrite_goal(node)
        if isinstance(node, Laog):
            return self._rewrite_laog(node)
        new_node = deepcopy(node)
        if hasattr(node, 'term') and node.term is not None:
            new_node.term = self.visit(node.term)
        if hasattr(node, 'context') and node.context is not None:
            new_node.context = self.visit(node.context)
        return new_node

    # leaves ------------------------------------------------------------
    def _rewrite_goal(self, node: Goal):
        #print("Goal Map", self.goal_map)
        var = self.goal_map.get(node.prop)
        return DI(var, node.prop) if var is not None else deepcopy(node)

    def _rewrite_laog(self, node: Laog):
        #print("Laog Map", self.laog_map)
        var = self.laog_map.get(node.prop)
        return ID(var, node.prop) if var is not None else deepcopy(node)

# ---------------------------------------------------------------------------
#  Helper – traversal that performs the grafting itself
# ---------------------------------------------------------------------------

class _GraftVisitor(ProofTermVisitor):
    """Walk *body_B* and replace matching Goal/Laog by *replacement_ast*."""

    def __init__(self, *, replacement_ast, target_prop: str,
                 target_number: str | None, target_is_goal: bool):
        self.replacement_proto = deepcopy(replacement_ast)
        self.target_prop   = target_prop
        self.target_number = target_number       # None ⇒ uniform graft
        self.target_is_goal = target_is_goal
        # Binder stacks --------------------------------------------------
        self.id_stack: list[tuple[str, str]] = []   # μ‑binders (Laog)
        self.di_stack: list[tuple[str, str]] = []   # λ / μ̃ binders (Goal)

    # binder helpers ----------------------------------------------------
    def _push_id(self, name: str, prop: str):
        self.id_stack.append((name, prop))
    def _pop_id(self):
        self.id_stack.pop()
    def _push_di(self, name: str, prop: str):
        self.di_stack.append((name, prop))
    def _pop_di(self):
        self.di_stack.pop()

    # traversal ---------------------------------------------------------
    def visit(self, node):
        if node is None:
            return None
        if isinstance(node, Goal):
            return self._maybe_graft(node, is_goal=True)
        if isinstance(node, Laog):
            return self._maybe_graft(node, is_goal=False)

        if isinstance(node, Mu):               # ID‑binder (captures Laog)
            self._push_id(node.id.name, node.prop)
            new = deepcopy(node)
            new.term    = self.visit(node.term)
            new.context = self.visit(node.context)
            self._pop_id()
            return new

        if isinstance(node, (Mutilde, Lamda)):
            varname = node.di.name if isinstance(node, Mutilde) else node.di.di.name
            varprop = node.prop  if isinstance(node, Mutilde) else node.di.prop
            self._push_di(varname, varprop)
            new = deepcopy(node)
            new.term = self.visit(node.term)
            if hasattr(node, 'context'):
                new.context = self.visit(node.context)
            self._pop_di()
            return new

        if isinstance(node, Cons):
            new = deepcopy(node)
            new.term    = self.visit(node.term)
            new.context = self.visit(node.context)
            return new

        return deepcopy(node)

    # ------------------------------------------------------------------
    def _make_maps(self):
        goal_map = {prop: name for name, prop in reversed(self.di_stack)}
        laog_map = {}
        for name, prop in reversed(self.id_stack):
            laog_map[prop] = name
            laog_map[f"{prop}_bar"] = name
        return goal_map, laog_map

    def _maybe_graft(self, node, *, is_goal: bool):
        # wrong kind? -> leave untouched
        if is_goal != self.target_is_goal:
            return deepcopy(node)
        # proposition must match
        if node.prop != self.target_prop:
            return deepcopy(node)
        # single‑site mode: number must line up
        if self.target_number is not None and node.number.strip() != self.target_number:
            return deepcopy(node)

        goal_map, laog_map = self._make_maps()
        subst   = _CapturingSubst(goal_map=goal_map, laog_map=laog_map)
        grafted = subst.visit(deepcopy(self.replacement_proto))
        return grafted

# ---------------------------------------------------------------------------
#  Utilities
# ---------------------------------------------------------------------------

def _find_target_info(ast: "ProofTerm", number: str) -> Tuple[Optional[str], Optional[bool]]:
    """Locate ?number / !number in *ast* – return (prop, is_goal)."""
    res = {'prop': None, 'kind': None}
    class _Find(ProofTermVisitor):
        def visit_Goal(self, n):
            if n.number.strip() == number:
                res['prop'], res['kind'] = n.prop, 'goal'
            return n
        def visit_Laog(self, n):
            if n.number.strip() == number:
                res['prop'], res['kind'] = n.prop, 'laog'
            return n
    _Find().visit(ast)
    if res['prop'] is None:
        return None, None
    return res['prop'], (res['kind'] == 'goal')


def _check_conclusion_matches(repl_ast: "ProofTerm", *, target_is_goal: bool, target_prop: str) -> None:
    """Ensure *repl_ast*’s root binder fits the Goal/Laog it replaces."""
    if isinstance(repl_ast, Mu):
        root_kind, root_prop = 'mu', repl_ast.prop
    elif isinstance(repl_ast, Mutilde):
        root_kind, root_prop = 'mutilde', repl_ast.prop
    else:
        raise TypeError("replacement AST must start with Mu or Mutilde binder")

    if target_is_goal:
        if root_kind != 'mu' or root_prop != target_prop:
            raise TypeError("Goal expects Mu‑binder with matching prop")
    else:
        plain_target = target_prop.rstrip("_bar")
        if root_kind != 'mutilde' or root_prop.rstrip("_bar") != plain_target:
            raise TypeError("Laog expects Mutilde‑binder with matching prop")

# ---------------------------------------------------------------------------
#  Public API functions
# ---------------------------------------------------------------------------

def graft_single(body_B: "ProofTerm", goal_number: str, body_A: "ProofTerm") -> "ProofTerm":
    """Replace the unique Goal/Laog *goal_number* in *body_B* by *body_A*."""
    tgt_prop, is_goal = _find_target_info(body_B, goal_number)
    if tgt_prop is None:
        raise ValueError(f"no Goal/Laog numbered {goal_number} found")

    # type‑check replacement
    _check_conclusion_matches(body_A, target_is_goal=is_goal, target_prop=tgt_prop)

    # 2) freshness workaround: rename scion binders if they clash
    body_A = _alpha_rename_if_needed(body_A, body_B)

    visitor = _GraftVisitor(replacement_ast=body_A,
                            target_prop=tgt_prop,
                            target_number=goal_number,
                            target_is_goal=is_goal)
    return visitor.visit(body_B)


def graft_uniform(body_B: "ProofTerm", body_A: "ProofTerm") -> "ProofTerm":
    """Uniform graft: replace **all** Goals/Laogs with the conclusion of *A*."""
    logger.info("Uniformly grafting %r on %r", body_A, body_B)
    # determine conclusion of A from its *root* binder
    if isinstance(body_A, Mu):
        concl_prop      = body_A.prop
        target_is_goal  = True      # replaces Goals
    elif isinstance(body_A, Mutilde):
        concl_prop      = body_A.prop
        target_is_goal  = False     # replaces Laogs
        logger.debug("Target %s is LAOG", concl_prop)
    else:
        raise TypeError("argument A must start with Mu or Mutilde binder")

    body_A = _alpha_rename_if_needed(body_A, body_B)
    # No extra type‑check needed – we built criteria from A itself.
    visitor = _GraftVisitor(replacement_ast=body_A,
                            target_prop=concl_prop,
                            target_number=None,
                            target_is_goal=target_is_goal)
    return visitor.visit(body_B)


class NegIntroRewriter(ProofTermVisitor):
    """
    Collapse the pattern
    
        μ thesis:¬A . ⟨ μ H1:¬A . ⟨ λ H2:A . μ H3:⊥ . ⟨ H2 ∥ ?k ⟩ ∥ H1 ⟩ ∥ thesis ⟩
    
    to the command that sat in the Laog/Goal “?k”.

    *If the pattern is not encountered*, we emit a **warnings.warn** and leave the
    AST untouched; the caller can inspect the ``changed`` attribute afterwards.
    """
    def __init__(self):
        self.changed = False          # set to True when a rewrite fires

    # generic traversal -------------------------------------------------
    def visit(self, node):
        logger.debug("Rewriting negation %r to counterargument", node)
        if node is None:
            return None
        if isinstance(node, Mu) and node.id.name == "thesis" and node.prop.startswith("¬"):
            body = self._match_neg_intro(node)
            if body is not None:
                self.changed = True
                return deepcopy(body)         # splice B in
        # default: deep-copy & recurse
        new = deepcopy(node)
        for attr in ("term", "context"):
            if hasattr(node, attr) and getattr(node, attr) is not None:
                setattr(new, attr, self.visit(getattr(node, attr)))
        if isinstance(node, Lamda):            # Lamda only has .term
            new.term = self.visit(node.term)
        return new

    # try to recognise the concrete pattern -----------------------------
    def _match_neg_intro(self, thesis_mu: Mu):
        """
        Return the sub-command *B* if the pattern matches, else *None*.
        Only syntactic checks – props & variable names – are done.
        """
        # print(f"Trying to match negation introduction pattern to node of type {thesis_mu} with term {thesis_mu.term}, term prop {thesis_mu.term.prop}, term term {thesis_mu.term.term} and term lambda term {thesis_mu.term.term.term} with prop thesis_mu.term.term.term.prop")
        inner_mu = thesis_mu.term
        if not (isinstance(inner_mu, Mu) and inner_mu.prop == thesis_mu.prop):
            logger.debug("Failed: %r is not Mu node or its prop %s does not equal the thesis prop %s",
                         inner_mu, inner_mu.prop, thesis_mu.prop)
            return None
        lam = inner_mu.term
        if not isinstance(lam, Lamda):
            logger.debug("Failed: %r is not the expected Lamda term", inner_mu.term)
            return None
        mu_bot = lam.term
        if not (isinstance(mu_bot, Mu) and mu_bot.prop == "⊥"):
            logger.debug("Failed: %r is not Mu node or its prop %s does not equal ⊥", mu_bot, inner_mu.prop)
            return None
        # shape looks good: body sits in mu_bot.context, which is ?k or !k
        logger.debug("Returning negation normalized body")
        return mu_bot.context

    # -------------------------------------------------------------------
    def rewrite(self, ast: "ProofTerm") -> "ProofTerm":
        out = self.visit(ast)
        if not self.changed:
            warnings.warn("NegIntroRewriter: no outer negation-introduction pattern found")
        return out

def _collect_binder_names(node: ProofTerm, names=None):
    """Return the **set** of all µ / µ̃ / λ *binder* names occurring in *node*."""
    if names is None:
        names = set()
    if isinstance(node, Mu):
        names.add(node.id.name)
    elif isinstance(node, Mutilde):
        names.add(node.di.name)
    elif isinstance(node, Lamda):
        names.add(node.di.di.name)  # di holds a Hyp → DI → name
    # recurse
    for child in (getattr(node, 'term', None), getattr(node, 'context', None)):
        if child is not None:
            _collect_binder_names(child, names)
    return names

def _fresh(prefix: str, taken: set[str]) -> str:
    """Generate a fresh variable starting with *prefix* not in *taken*."""
    if prefix not in taken:
        return prefix
    i = 2
    while f"{prefix}{i}" in taken:
        i += 1
    return f"{prefix}{i}"

def _alpha_rename_if_needed(scion: "ProofTerm", root: "ProofTerm") -> "ProofTerm":
    """Return *scion* with all **conflicting binders** α‑renamed fresh w.r.t. *root*."""
    root_names = _collect_binder_names(root)
    scion_names = _collect_binder_names(scion)
    mapping = {}
    for nm in scion_names:
        if nm in root_names:
            fresh_nm = _fresh(nm, root_names | scion_names)
            mapping[nm] = fresh_nm
    if mapping:
        scion = deepcopy(scion)
        _AlphaRename(mapping).visit(scion)
    return scion

class _AlphaRename(ProofTermVisitor):
    """Capture‑avoiding α‑renamer.  *mapping* is old‑name → new‑name."""
    def __init__(self, mapping: dict[str, str]):
        self.mapping = mapping

    # binders – rename & recurse -------------------------------------
    def visit_Mu(self, node: Mu):
        if node.id.name == "thesis":
            return super().visit_Mu(node)
        if node.id.name in self.mapping:
            node.id.name = self.mapping[node.id.name]
        return super().visit_Mu(node)

    def visit_Mutilde(self, node: Mutilde):
        if node.di.name in self.mapping:
            node.di.name = self.mapping[node.di.name]
        return super().visit_Mutilde(node)

    def visit_Lamda(self, node: Lamda):
        if node.di.di.name in self.mapping:
            node.di.di.name = self.mapping[node.di.di.name]
        return super().visit_Lamda(node)

    # leaves – replace occurrences -----------------------------------
    def visit_ID(self, node: ID):
        if node.name in self.mapping:
            node.name = self.mapping[node.name]
        return node

    def visit_DI(self, node: DI):
        if node.name in self.mapping:
            node.name = self.mapping[node.name]
        return node
