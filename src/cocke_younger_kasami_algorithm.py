from prettytable import PrettyTable


def cyk(g, s):
    """
    Takes a grammar in chomsky normal form and returns True if s is in this grammar language.
    """
    n = len(s)
    m = len(g.nonterms)
    dp = [[[False for _ in range(m)] for _ in range(n + 1)] for _ in range(n + 1)]

    if s == '':
        return [''] in g.rules[g.start]

    for len_ in range(1, n + 1):
        for i in range(n - len_ + 1):
            j = i + len_
            # dp[i][j]: [i, j)
            for k in range(m):
                # dp[i][j][k]
                for r in g.rules[g.nonterms[k]]:
                    if len(r) == 0:
                        continue
                    if len(r) == 1:
                        dp[i][j][k] |= (len_ == 1 and s[i] == r[0])
                        continue
                    # len(r) == 2
                    x = g.nonterms.index(r[0])
                    y = g.nonterms.index(r[1])
                    for mid in range(i + 1, j):
                        dp[i][j][k] |= (dp[i][mid][x] and dp[mid][j][y])

    t = PrettyTable([''] + [str(i) for i in range(1, n + 1)])
    for j in range(1, n + 1):
        row = [str(j)]
        for i in range(n):
            row.append(','.join([g.nonterms[k] for k in range(m) if dp[i][j][k]]))
        t.add_row(row)

    return dp[0][n][g.nonterms.index(g.start)], t

