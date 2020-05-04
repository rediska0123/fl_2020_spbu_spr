import argparse
from src.parser_utils.parser import parse
from collections import defaultdict
from typing import List


class Grammar:
    def __init__(self, gram: defaultdict, terms: List[str], nonterms: List[str]):
        self.rules = gram
        self.terms = terms
        self.nonterms = nonterms
        self.start = 'S'

    def __repr__(self):
        return str(self)

    def __str__(self):
        res = ''
        for key, val in self.rules.items():
            res += key + ' -> ' + ' | '.join([' '.join(rhs) for rhs in val]) + '\n'
        return res


def grammar_from_string(s: str):
    g, t, nt, err = parse(s)
    return (None if err != '' else Grammar(g, t, nt)), err


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Parse context free grammars')
    parser.add_argument('--inp', metavar='inp', type=str, help='input file path', default='examples/grammar_example')
    args = parser.parse_args()
    input_file = args.inp
    with open(input_file, 'r') as f:
        text = ''.join(f.readlines())
    gr, err = grammar_from_string(text)
    if err == '':
        print('Parsed successfully!')
        print(gr)
    else:
        print('Failed to parse: {}'.format(err))
