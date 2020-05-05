from ply import lex, yacc
from collections import defaultdict
import string


err = ''
rules = defaultdict(list)
terms = []
nonterms = []

tokens = ('TERM', 'NONTERM', 'EPS', 'RARROW', 'COMMA', 'OR', 'NEWLINE')

t_TERM = r'[a-z]+'
t_NONTERM = r'[A-Z]+'
t_EPS = r'ϵ'
t_RARROW = r'-\>'
t_COMMA = r'\,'
t_OR = r'\|'
t_NEWLINE = r'\n'
t_ignore = ' \t'


def t_error(t):
    global err
    err = 'Illegal character {}'.format(t.value[0])
    t.lexer.skip(1)


lex.lex()
precedence = (
    ('left', 'OR'),
    ('left', 'COMMA'),
)


def p_statement_nonterm_seq(p):
    """statement : NONTERM RARROW seq NEWLINE statement
                 | NONTERM RARROW seq NEWLINE"""
    rules[p[1]] += p[3]
    process_symb(p[1])


def p_seq_multiple(p):
    """seq : seq OR rhs"""
    p[0] = p[1].copy()
    p[0].append(p[3])


def p_seq_single(p):
    """seq : rhs"""
    p[0] = [p[1]]


def p_rhs_multiple(p):
    """rhs : rhs COMMA TERM
           | rhs COMMA NONTERM"""
    p[0] = p[1].copy()
    p[0].append(p[3])
    process_symb(p[3])


def p_rhs_single(p):
    """rhs : TERM
           | NONTERM"""
    p[0] = [p[1]]
    process_symb(p[1])


def p_rhs_single_eps(p):
    """rhs : EPS"""
    p[0] = ['']


def p_error(p):
    global err
    err = 'Syntax error at {}'.format(p)


def process_symb(symb):
    if symb in string.ascii_lowercase:
        if symb not in terms:
            terms.append(symb)
    else:
        if symb not in nonterms:
            nonterms.append(symb)


yacc.yacc()


def parse(text):
    global rules, terms, nonterms, err
    rules = defaultdict(list)
    terms = []
    nonterms = []
    err = ''
    yacc.parse(text)
    return rules, terms, nonterms, err


if __name__ == '__main__':
    while True:
        try:
            s = input('rule > ') + '\n'
        except EOFError:
            break
        print(parse(s))

