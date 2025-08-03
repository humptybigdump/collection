function scores = quadrfcn(pop)
%function scores = quadrfcn(pop)
%Einfache quadratische Zielfunktion
%Material zur Vorlesung Computational Intelligence
%
%Copyright 2003-2004 The MathWorks, Inc
%Modifiziert: [Ralf Mikut, KIT, 2020]


scores =  sum( (pop -  2 * ones(size(pop))).^2,2);

