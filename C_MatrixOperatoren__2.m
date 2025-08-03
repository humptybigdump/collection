%% Transponieren
clear all
close all
clc 

v = [1 2 3];
A = [1 2 2; 4 3 6; 7 2 1];

%%
% Transponieren
v_t = v'

% Invertieren einer Matrix
A_inv = inv(A)

% Lösen eines LGS A*x = v_t
x = inv(A) * v_t
x1 = A\v_t

% Punkt Operator
A1 = A./2
b = [1;2;3]
c = b.*b