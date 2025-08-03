%% Matlab script zur PLU Zerlegung, Geometrische Modelle Übungsblatt 1 Matlab-Aufgabe

%Matrix A und Vektor b mit zufallswerten befüllen
A = randi([1 10], 20, 20);
b = randi([1 10], 20, 1);

%(P)LU Zerlegung an der Matrix A ausführen
[L,U] = lu(A);

%Ergebnisse für L und U einsetzten
C = L;
D = U;

%Nach Y auflösen (Ay = b)
X = linsolve(C,b);
Y = linsolve(D,X);
