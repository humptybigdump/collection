%mathematiker_wg.pl
nummerierung(A,B,C) :- range(A), range(B), range(C), even(A),
     12 is A+B+C, not(A=B), not(B=C), not(A=C).
even(A) :- 0 is mod(A,2).
range(X) :- between(1,7,X).



%detektivraetsel.pl
aussage(alice, freund(bob)).
aussage(alice, feind(carl)).
aussage(bob, nichtDaheim(bob)).
aussage(bob, kenntNicht(bob)).
aussage(carl, unschuldig(carl)).
aussage(carl, daheim(bob)).
aussage(carl, daheim(alice)).
aussage(carl, daheim(carl)).
%...
%Füge weitere aussagen entsprechend der geg. Aussagen ein.

%Widersprüche

widerspruch(freund(X), feind(X)).
widerspruch(daheim(X), nichtDaheim(X)).
widerspruch(freund(X), kenntNicht(X)).
widerspruch(feind(X), kenntNicht(X)).
%...
%Füge weitere Widerspruchserkennungsmuster entsprechend der hinzugefügten aussagen hinzu.


taeter(T) :-
    select(T, [alice, bob, carl], Rest),
    not(inkonsistent(Rest)).


%Implementiere inkonstinten. Ist erfüllt gdw. es ein paar von Zeugen gibt die sich widersprechen.
inkonsistent(Zeugen) :- 
    member(X,Zeugen),member(Y,Zeugen),X\==Y,
    aussage(X,A1),aussage(Y,A2),widerspruch(A1,A2).

inkonsistent2(Zeugen) :- select(Z1, Zeugen, Rest1), select(Z2, Rest1, _), 
    aussage(Z1, A), aussage(Z2, B), widerspruch(A, B).


%schlafplaetze.pl
aaron(A) :- member(A, [1,2,3,4]).
bob(B,C) :- member(B, [2,3,4,5]), not(adjacent(B,C)).
connor(C) :- member(C, [2,3,4]).
edison(E,C) :- not(adjacent(E,C)).

bett(X) :- member(X, [1,2,3,4,5]).

adjacent(X,Y) :- abs(X - Y) = 1.

distinct1([X|XS]) :- not(member(X,XS)), distinct1(XS).
distinct1([]).

schlafplaetze(A,B,C,D,E) :-
    bett(A), bett(B), bett(C), bett(D), bett(E),
    distinct1([A,B,C,D,E]),
    aaron(A), bob(B,C), connor(C), edison(E,C).