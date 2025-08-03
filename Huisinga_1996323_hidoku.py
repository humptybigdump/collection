from pysat.solvers import Glucose4
import sys
from itertools import chain

join = lambda xs: list(chain.from_iterable(xs))

vars = {}
fresh_var = 1
def enc_var(s):
    global fresh_var
    if s in vars:
        return vars[s]
    vars[s] = fresh_var
    fresh_var += 1
    return vars[s]

x = lambda i, j, n: enc_var("x_%d_%d_%d" % (i, j, n))
s = lambda i, j: enc_var("s_%d_%d" % (i, j))

to = lambda a, b: [-a, b]
iff = lambda a, b: [to(a, b), to(b, a)]
neither = lambda a, b: [-a, -b]

def at_most_one(xs, si):
    f = [to(xs[i], s(xs[i], si)) for i in range(0, len(xs)-1)]
    f += [to(s(xs[i-1], si), s(xs[i], si)) for i in range(1, len(xs)-1)]
    f += [neither(xs[i], s(xs[i-1], si)) for i in range(1, len(xs))]
    return f

W = -1
H = -1
max = -1

ns = lambda i, j: [x(i, j, n) for n in range(1, max)]
ps = lambda n: [x(i, j, n) for i in range(W) for j in range(H)]
surroundings = lambda i, j: [(m, n) for m in range(i-1, i+2) for n in range(j-1, j+2)
                             if 0 <= m < W and 0 <= n < H and (m, n) != (i, j)]
snake = lambda i, j, n: [-x(i, j, n)] + [x(k, l, n+1) for k, l in surroundings(i, j)]

def encode(w, h, field):
    global W
    global H
    global max
    W = w
    H = h
    max = w*h + 1
    f = []
    f += [ns(i, j) for i in range(w) for j in range(h)]
    f += join(at_most_one(ns(i, j), h*i + j) for i in range(w) for j in range(h))
    f += [snake(i, j, n) for i in range(w) for j in range(h) for n in range(1, max-1)]
    f += [[x(i, j, field[i][j])] for i in range(w) for j in range(h) if field[i][j] != 0]
    f += at_most_one(ps(max-1), max)
    return f

def parse(s):
    s = s.split(":")
    w, h = map(int, s[0].split(","))
    unparsed_rows = [r for r in s[1].split(";") if r != ""]
    field = list(map(list, zip(*[map(int, r.split(",")) for r in unparsed_rows])))
    return w, h, field

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("usage: python hidoku.py <hidoku input>")
        sys.exit(1)
    w, h, field = parse(sys.argv[1])
    f = encode(w, h, field)
    g = Glucose4()
    for c in f:
        g.add_clause(c)
    sat = g.solve()
    if not sat:
        print("sol:UNSAT")
        sys.exit(0)
    m = set(g.get_model())
    rows = []
    for j in range(h):
        row = []
        for i in range(w):
            vals = [n for n in range(1, max) if x(i, j, n) in m]
            if len(vals) != 1:
                print("got invalid model %s" % vals)
                sys.exit(1)
            row += [str(vals[0])]
        rows += [row]
    print("sol:%s;" % ";".join(",".join(r) for r in rows))