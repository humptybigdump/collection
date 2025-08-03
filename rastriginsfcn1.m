function scores = rastriginsfcn1(pop)
%function scores = rastriginsfcn1(pop_binary)
%Um eins verschobene Rastrigin-Funktion
%Material zur Vorlesung Computational Intelligence
%
%Copyright 2003-2004 The MathWorks, Inc
%Modifiziert: [Ralf Mikut, KIT, 2020]

scores = 1+rastriginsfcn(pop-1);



