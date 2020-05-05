from cocke_younger_kasami_algorithm import cyk
from grammar_parser import grammar_from_string
from chomsky_normal_form import get_chomsky_normal_form


def test_cyk():
    tests = [
        ("""S -> ϵ | a,S,b,S\n""", 'a,b,a,a,b,b', True),
        ("""S -> ϵ | a,S,b,S\n""", 'a,b', True),
        ("""S -> ϵ | a,S,b,S\n""", '', True),
        ("""S -> ϵ | a,S,b,S\n""", 'a', False),
        ("""S -> ϵ | a,S,b,S\n""", 'a,b,a', False),
        (
            """S -> a,S | A
               A -> A,b | ϵ  
            """, 'a,b,a', False
        ),
        (
            """S -> a,S | A
               A -> A,b | ϵ  
            """, '', True
        ),
        (
            """S -> a,S | A
               A -> A,b | ϵ  
            """, 'a,a,a,b,b,b,b,b', True
        ),
        (
            """S -> a,S | A
               A -> A,b | ϵ  
            """, 'a,b', True
        )
    ]

    for grammar_text, s, accepts in tests:
        gr, err = grammar_from_string(grammar_text)
        assert err == '', 'Grammar {} should be parsed correctly'.format(grammar_text)
        gr, _ = get_chomsky_normal_form(gr)
        res, table = cyk(gr, s.split(','))
        assert res == accepts, 'Error in CYK at grammar {}, string {}'.format(gr, s)
