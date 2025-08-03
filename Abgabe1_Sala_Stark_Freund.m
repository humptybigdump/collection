%% MATLAB AUFGABE GEO MODELLE BLATT 1
% Erarbeitet von Bogdan Sala, Johannes Stark und Marie Freund
% Lösen eines LGS Ax=b mit LU-Zerlegung

A=randi([1 10], 20,20);
% 20x20 Matrixeinträge mit zufälligen Zahlen von 1-10

b=randi([1 10], 20,1);
% erstellt einen Spaltenvektor mit 20 zufälligen Einträgen

[L,U]=lu(A);
% factorizes the full or sparse matrix A into 
% an upper triangular matrix U and a permuted lower triangular matrix L 
% such that A = L*U

Y=linsolve(L,b);
X=linsolve(U,Y);
