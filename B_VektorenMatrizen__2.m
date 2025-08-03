%% Vektoren
clear all
close all
clc

%%
% Vektoren
v = [1;2;3]
v1 = [1 2 3]

% Matrizen
A = [1, 2, 3; 4, 5, 6; 7, 8, 9]
B = zeros(3,4)
C = ones(3)

I = eye(3,3)

matrix = zeros(3,3,3)

% Zugriff auf Elemente
a_21 = A(2,1)

% Zugriff auf vektoren
vA = A(:,3)
vA1 = A(1,:)