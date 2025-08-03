%% Geometrische Modelle der Geodäsie
% Blatt 1 Aufgabe 5
% Ramon Butzer, Kangje Zhao, Chuang Ju

clear variables;
close all;
clc;

% Erstelle Matrix A mit 20 Zeilen und Spalten und zufälligen Einträgen
A = randi([1 10], 20, 20);

% Erstelle Spaltenvektor b mit 20 Spalten und zufälligen Einträgen
b = randi([1 10],20,1);

% Bestimme die L,U Zerlegung von A
[L, U] = lu(A);

% Löse das LGS Ly = b
y = linsolve(L,b);

% Löse das LGS Ux = y
x = linsolve(U,y)
