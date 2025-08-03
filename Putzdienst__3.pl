%putzdienst.pl

%L : Liste an Insassen z.B. L = [a,b,c,d,e].
%N : Wie viele Putzdienste vergeben werden z.B. N = 4.
%K : Der Kte insasse wird jeweils gewählt z.B. K = 2.
%X : Die Insassen die nicht für den Putzdienst gewählt werden
keinPutzdienstFuer(L,0,_,X) :- X=L.
keinPutzdienstFuer([P|L], N, K, X) :- 
    Nneu is N-1,
    rotate([P|L], Lneu),
    keinPutzdienstFuer(Lneu, Nneu, K, Xneu),!,
    delete(Xneu, P, X).

keinPutzdienstFuer2(L,N,K,X) :- keinPutzdienstHelper(L, N, K, K, X).

keinPutzdienstHelper([],_,_,_,[]).
keinPutzdienstHelper(L,0,_,_,L).
keinPutzdienstHelper([_|LS], N, 1, K, X) :- N > 0, NNext is N - 1,
    keinPutzdienstHelper(LS, NNext, K, K, X).
keinPutzdienstHelper([P|PS], N, I, K, X) :- N > 0, I > 1, INext is I - 1,
    append(PS, [P], PSNext), keinPutzdienstHelper(PSNext, N, INext, K, X).

generate(N, N, [N]).
generate(I, N, [I|L]) :- I < N, INext is I + 1, generate(INext, N, L).

rotate([X|[Y|Xs]],Lneu) :- append(Xs, [X|[Y]],Lneu).

