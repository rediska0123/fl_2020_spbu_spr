class Grammar:
    def __init__(self, gram, terms, nonterms):
        self.rules = gram
        self.terms = terms
        self.nonterms = nonterms
        self.start = 'S'
        self.add_nonterm(self.start)

    def __repr__(self):
        return str(self)

    def __str__(self):
        res = ''
        for key, val in self.rules.items():
            res += key + ' -> ' + ' | '.join([' '.join(rhs) for rhs in val]) + '\n'
        return res

    def add_rule(self, a, b):
        self.rules[a].append(b)

    def add_term(self, t):
        if t not in self.terms:
            self.terms.append(t)

    def add_nonterm(self, nt):
        if nt not in self.nonterms:
            self.nonterms.append(nt)

    def is_term(self, symb):
        return symb in self.terms

    def is_nonterm(self, symb):
        return symb in self.nonterms
