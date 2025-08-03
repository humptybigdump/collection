close all;
clear all;

A = [1, 2, 3; 2, 3, 4; 1, 0, 1;1, 1, 1];
[U,S,V] = svd(A);
b = [1; 1; 2; 3];
sigma_plus = (S'*S)^(-1)*S';

%x =V*pinv(S)*U'*b;
x =V*sigma_plus*U'*b;

% overdetermined matrix!!
r = A*x -b