%detektivraetsel.pl

aussage(alice, freund(bob)).
aussage(alice, feind(carl)).
%...
%Füge weitere aussagen entsprechend der geg. Aussagen ein.

%Widersprüche

widerspruch(freund(X), feind(X)).
%...
%Füge weitere Widerspruchserkennungsmuster entsprechend der hinzugefügten aussagen hinzu.


taeter(T) :-
    select(T, [alice, bob, carl], Rest),
    not(inkonsistent(Rest)).


%Implementiere inkonstinten. Ist erfüllt gdw. es ein paar von Zeugen gibt die sich widersprechen.
inkonsistent(Zeugen) :- _.
