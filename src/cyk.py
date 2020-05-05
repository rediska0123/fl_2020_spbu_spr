from cocke_younger_kasami_algorithm import cyk
from chomsky_normal_form import get_chomsky_normal_form
import argparse
from grammar_parser import grammar_from_string

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='CYK algorithm implementation')
    parser.add_argument('--g', metavar='grammar_file', type=str, help='file with grammar definition', default='../examples/grammar_example')
    parser.add_argument('--s', metavar='string_file', type=str, help='file with input string', default='../examples/grammar_str')
    args = parser.parse_args()
    with open(args.g, 'r') as f:
        text = ''.join(f.readlines())
    g, err = grammar_from_string(text)
    if err != '':
        raise Exception(err)

    with open(args.s, 'r') as f:
        s = ''.join(f.readlines()).split(',')

    g, _ = get_chomsky_normal_form(g)
    res, table = cyk(g, s)
    print('{} is {}in the grammar\n'.format(s, '' if res else 'not '))
    print('Chomsky normal form of the input grammar:\n{}'.format(g))
    print('CYK table:\n{}'.format(table))