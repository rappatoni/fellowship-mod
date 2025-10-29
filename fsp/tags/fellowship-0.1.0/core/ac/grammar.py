from lark import Lark, Transformer
from core.ac.ast import Mu, Mutilde, Lamda, Cons, Goal, Laog, ID, DI, Hyp

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
