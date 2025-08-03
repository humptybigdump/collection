%sat.pl

%pos(a) in A und neg(a) in A
contradict([A1|A]) :- A1 = pos(X), member(neg(X), A),!.
contradict([A1|A]) :- A1 = neg(X), member(pos(X), A),!.
contradict([_|A]) :- contradict(A).

contradict2([pos(X)|A]) :- member(neg(X), A),!.
contradict2([neg(X)|A]) :- member(pos(X), A),!.
contradict2([_|A]) :- contradict(A).

contradict3(A) :- member(pos(X), A), member(neg(X), A).

sat(P, X) :- solve(P,[], X).

solve([], A, A).

solve([K|P], A, X) :- 
    member(L, K),
    Aneu = [L|A],
    not(contradict(Aneu)),
    solve(P, Aneu, X).