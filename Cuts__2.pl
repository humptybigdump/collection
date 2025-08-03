%cuts.pl

if_then_else(B,T,F) :- B, !, T.
if_then_else(B,T,F) :- F.


