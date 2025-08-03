%% MATLAB AUFGABE GEO MODELLE BLATT 4
% Erarbeitet von Bogdan Sala, Johannes Stark und Marie Freund
% Lösen eines LGS Ax=b mit QR-Zerlegung


clc;
clear;

A=randi([1 10], 20,20);
% 20x20 Matrixeinträge mit zufälligen Zahlen von 1-10

b=randi([1 10], 20,1);
% erstellt einen Spaltenvektor mit 20 zufälligen Einträgen

[Q,R]=qr(A);
y = Q'*b;
x = R\y;

right = linsolve(A,b);

%compares the 2 vectors x and right to check if identical
tf = isequal(x,right);