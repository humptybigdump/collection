%% Matlab script zu Geometrische Modelle Übungsblatt 3 Matlab-Aufgabe

clear variables;
close all;
clc;
%% Matrizen und Vektoren erstellen
% Matrix A
A = randi([1 10], 20, 10);

% Matrix B
B = A'*A;

% Vektor b
b = randi([1 10],20,1);

%% Anforderungen erfüllt?
% Rangs(A)?
if(rank(A) == 10)
    disp('Rang(A) = 10');
else
    disp('Matrix A hat nicht Rang 10');
end

% Symmetrie von B
if(B == B')
    disp('Matrix B symmetrisch');
else
    disp('Matrix B nicht symmetrisch');
end

%% Graph
figure
scatter(1:10,eig(B), 'r');
title('Eigenwerte von B');
xlabel('Eigenwert i');
ylabel('Wert');

%% Choleskyzerlegung
R = chol(B);

%R^T R = R'*R;
G = R';
GGtransp = G*G';

%% LGS lösen mit Choleskyzerlegung
y_chol = linsolve(G,A'*b);
x_chol = linsolve(G',y_chol)

% Plausibilität des Ergebnisses testen
x = linsolve(A,b);
x_test = linsolve(A'*A, A'*b);
