from src.grammar.grammar_definition import Grammar
from collections import defaultdict
from src.grammar_parser import grammar_from_string
from copy import deepcopy


class NewNamesGetter:
    def __init__(self):
        self.last = ['A']

    def get_new_name(self):
        res = ''.join(self.last)
        i = len(self.last) - 1
        while i >= 0 and self.last[i] == 'Z':
            i -= 1
        if i == -1:
            self.last = ['A' for _ in range(len(self.last) + 1)]
        else:
            self.last[i] = chr(ord(self.last[i]) + 1)
        return res


ng = NewNamesGetter()


def get_chomsky_normal_form(inp):
    new_start = add_new_start(inp)
    delete_long = delete_long_rules(new_start)
    delete_eps = delete_eps_rules(delete_long)
    delete_chain, pairs = delete_chain_rules(delete_eps)
    delete_unreachable = delete_unreachable_symbols(delete_chain)
    delete_multiple_term = delete_multiple_terminals_rules(delete_unreachable)

    log = """Input grammar:\n{}\n
0. Add new start state S and rename old start state to S\':\n{}\n
1. Delete rules longer than 2:\n{}\n
2. Delete eps-rules:\n{}\n
3. Delete chain rules:\nChain pairs: {}\n{}\n
4. Delete unreachable rules:\n{}\n
5. Delete multiple term rules:\n{}
""".format(inp, new_start, delete_long, delete_eps, pairs, delete_chain, delete_unreachable, delete_multiple_term)
    return delete_multiple_term, log


def add_new_start(g):
    g = deepcopy(g)
    new_g = Grammar(defaultdict(list), g.terms, g.nonterms)
    new_g.add_nonterm('S\'')
    for l, rhs in g.rules.items():
        l_ = l if l != 'S' else 'S\''
        for r in rhs:
            r_ = deepcopy(r)
            for i in range(len(r)):
                if r_[i] == 'S':
                    r_[i] = 'S\''
            new_g.add_rule(l_, r_)
    new_g.add_rule('S', ['S\''])

    return new_g

def delete_long_rules(g):
    global ng
    g = deepcopy(g)
    new_g = Grammar(defaultdict(list), g.terms, g.nonterms)
    for l, rhs in g.rules.items():
        for r in rhs:
            # l -> r
            length = len(r)
            if length <= 2:
                new_g.add_rule(l, r)
                continue
            symb = ng.get_new_name()
            for i in range(0, length - 1):
                new_g.add_nonterm(symb + str(i + 1))
            for i in range(length - 1):
                x = l if i == 0 else symb + str(i)
                y = r[-1] if i == length - 2 else symb + str(i + 1)
                # x -> r[i] y
                new_g.add_rule(x, [r[i], y])
    return new_g


def delete_eps_rules(g):
    g = deepcopy(g)
    rules = [[k, deepcopy(val)] for k, val in g.rules.items()]
    eps_reachable = []
    for l, rhs in rules:
        for r in rhs:
            if r == ['']:
                eps_reachable.append(l)
    all_eps_reachable = []
    while len(eps_reachable) != 0:
        all_eps_reachable += deepcopy(eps_reachable)
        new_eps_reachable = []
        for k in range(len(rules)):
            l, rhs = rules[k]
            if l == '':
                continue
            if l in eps_reachable:
                rules[k][0] = ''
                continue
            flag_any_eps = False
            for i in range(len(rhs)):
                flag_all_eps = True
                for j in range(len(rhs[i])):
                    if rhs[i][j] in eps_reachable:
                        rules[k][1][i][j] = ''
                    elif rhs[i][j] != '':
                        flag_all_eps = False
                if flag_all_eps:
                    flag_any_eps = True
            if flag_any_eps:
                new_eps_reachable.append(l)
        eps_reachable = new_eps_reachable

    new_g = Grammar(defaultdict(list), g.terms, g.nonterms)

    for l, rhs in g.rules.items():
        for r in list(filter((['']).__ne__, rhs)):
            new_g.add_rule(l, r)
            if len(r) == 2:
                if r[0] in all_eps_reachable:
                    new_g.add_rule(l, [r[1]])
                if r[1] in all_eps_reachable:
                    new_g.add_rule(l, [r[0]])
    if g.start in all_eps_reachable:
        new_g.add_rule(g.start, [''])

    return new_g


def delete_chain_rules(g):
    g = deepcopy(g)
    n = len(g.nonterms)
    pairs = [(i, i) for i in range(n)]
    used = [[i == j for j in range(n)] for i in range(n)]
    out = [[] for _ in range(n)]
    non_chain_rules = [[] for _ in range(n)]
    for l, rhs in g.rules.items():
        i = g.nonterms.index(l)
        for r in rhs:
            if len(r) == 1 and g.is_nonterm(r[0]):
                j = g.nonterms.index(r[0])
                out[i].append(j)
            else:
                non_chain_rules[i].append(r)
    last = 0
    while last < len(pairs):
        x, y = pairs[last]
        last += 1
        for z in out[y]:
            if not used[x][z]:
                pairs.append((x, z))
                used[x][z] = True

    new_g = Grammar(defaultdict(list), g.terms, g.nonterms)
    for l, rhs in g.rules.items():
        for r in rhs:
            if len(r) == 1 and g.is_nonterm(r[0]):
                continue
            new_g.add_rule(l, r)

    pairs = []
    for i in range(n):
        for j in range(n):
            if i != j:
                if used[i][j]:
                    pairs.append((g.nonterms[i], g.nonterms[j]))
                    for rule in non_chain_rules[j]:
                        new_g.add_rule(g.nonterms[i], rule)

    return new_g, pairs


def delete_unreachable_symbols(g):
    g = deepcopy(g)
    used = defaultdict(bool)

    used['S'] = True
    q = ['S']
    last = 0
    while last < len(q):
        rhs = g.rules[q[last]]
        last += 1
        for r in rhs:
            for x in r:
                if not used[x]:
                    used[x] = True
                    q.append(x)

    new_g = Grammar(defaultdict(list), [], [])
    for t in g.terms:
        if used[t]:
            new_g.add_term(t)
    for t in g.nonterms:
        if used[t]:
            new_g.add_nonterm(t)
    for l, rhs in g.rules.items():
        if not used[l]:
            continue
        for r in rhs:
            flag = True
            for x in r:
                if used[x]:
                    continue
                flag = False
                break
            if flag:
                new_g.add_rule(l, r)

    return new_g


def delete_multiple_terminals_rules(g):
    def to_nonterminal(s: str):
        return s.upper() + '\'\''

    g = deepcopy(g)
    to_add = []
    new_g = Grammar(defaultdict(list), g.terms, g.nonterms)
    for l, rhs in g.rules.items():
        for r in rhs:
            r_ = deepcopy(r)
            if len(r) == 2:
                for i in range(len(r_)):
                    if g.is_term(r_[i]):
                        to_add.append(r_[i])
                        r_[i] = to_nonterminal(r_[i])
            new_g.add_rule(l, r_)
    to_add = list(set(to_add))

    for x in to_add:
        y = to_nonterminal(x)
        new_g.add_nonterm(y)
        new_g.add_rule(y, [x])

    return new_g


if __name__ == '__main__':
    gr, err = grammar_from_string("""S -> R,S | R
R -> a,S,b | c,R,d
R -> a,b | c,d | ϵ
""")
    if err != '':
        raise Exception(err)
    gr, log = get_chomsky_normal_form(gr)
    print(log)
