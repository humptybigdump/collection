#!/usr/bin/python3

import argparse
import math
import itertools

def var(pig, hol, n):
    return hol * n + pig + 1

def phole(n):
    npig = n
    nhol = n-1
    print("c Do {} fit into {} holes?".format(npig, nhol))
    nvars = npig * nhol
    nclauses = npig + nhol * int(npig * (npig - 1) / 2)
    for i in range(1, nvars+1):
        hole = math.ceil(i / npig)
        pigeon = (i-1) % npig + 1
        print("c Variable {}: Pigeon {} in Hole {}".format(i, pigeon, hole))
    print("p cnf", nvars, nclauses)
    # at-least-one pigeon per hole
    for pigeon in range(npig):
        vars = [ str(var(pigeon, h, n)) for h in range(nhol) ]
        print(" ".join(vars) + " 0")
    for hole in range(nhol):
        vars = [ str(-var(p, hole, n)) for p in range(npig) ]
        # at-most-one pigeon per hole
        for a, b in itertools.combinations(vars, 2):
            print(a, b, 0)
            #print("{} {} 0".format(-a, -b))


def phole2(n):
    npig = n
    nhol = n-1
    print("c Do {} fit into {} holes?".format(npig, nhol))
    nvars = npig * nhol
    nclauses = npig + nhol * (3 * npig - 3)
    print("p cnf", 2*nvars, nclauses)
    # at-least-one pigeon per hole
    for pigeon in range(npig):
        vars = [ str(var(pigeon, h, n)) for h in range(nhol) ]
        print(" ".join(vars) + " 0")
    for hole in range(nhol):
        vars = [ var(p, hole, n) for p in range(npig) ]
        for a,b in zip(vars, vars[1:]):
            print(-(a+nvars), b+nvars, 0)
            print(-(a+nvars), -b, 0)
            print(-a, a+nvars, 0)
        


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("n", type=int)
    parser.add_argument("--seq", action="store_true")
    args = parser.parse_args()
    if args.seq:
        phole2(args.n)
    else:
        phole(args.n)


if __name__ == "__main__":
    main()