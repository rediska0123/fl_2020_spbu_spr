from grammar_parser import grammar_from_string


def test_grammar_parser():
    tests = [
        (
            """A -> B
               C -> d,E | GH,R|ϵ|ϵ
            """, True,
            ['A', 'B', 'C', 'E', 'GH', 'R', 'S'], ['d'],
            {'A': [['B']], 'C': [['d', 'E'], ['GH', 'R'], [''], ['']]}
        ),
        (
            """ S->R,S | R
                R->a,S,b | c,R,d
                R->a,b | c,d | ϵ
            """, True,
            ['S', 'R'], ['a', 'b', 'c', 'd'],
            {'S': [['R', 'S'], ['R']], 'R': [['a', 'S', 'b'], ['c', 'R', 'd'], ['a', 'b'], ['c', 'd'], ['']]}
        ),
        (
            """ S->Ra,S | R
            """, False, [], [], {}
        ),
        (
            """ 
            S->R,S | R
            """, False, [], [], {}
        ),
        (
            """ S->R,S | R""",
            False, [], [], {}
        ),
        (
            """ S->ϵS,S | R
            """, False, [], [], {}
        ),
    ]
    for grammar_text, is_correct, nonterms, terms, rules in tests:
        gr, err = grammar_from_string(grammar_text)
        if err != '':
            assert not is_correct, 'Grammar {} failed to parse, expected success'.format(grammar_text)
            continue
        assert is_correct, 'Grammar {} successfully parsed, expected error'.format(grammar_text)
        assert sorted(gr.terms) == sorted(terms), 'Wrong terms'
        assert sorted(gr.nonterms) == sorted(nonterms), 'Wrong nonterms'
        assert sorted(gr.rules) == sorted(rules), 'Wrong rules'
