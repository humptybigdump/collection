%%Numerische Mathemarik matlab2
%Lili Gao-2130950
%Qilin Zhang-2389459
%Bo Wang-2388376

% L-infinity approximation
clear all;
close all;
% sampled values
x_s = (0:1/19:1)';
y_s = exp(x_s);

% y = a0+a1*x+a2*x^2+a3*x^3
% linear equations: A-matrix
A = ones(20,20);
for i = 1:20
    A(:,i)=x_s.^(i-1);
end

% solve the linear equation using the singular value decomposition
[U,S,V] = svd(A);
% pseudo inverse
S_r_plus = S;

% Sigma_plus matrix
for i=1:length(x_s)
    if S_r_plus(i,i)<0.0001
        S_r_plus(i,i) = 0; % truncate
    else
        S_r_plus(i,i) = 1/S_r_plus(i,i);
    end
    
end

% the coefficient of linear combination of B solved
coef =V*S_r_plus*U'*y_s;
syms x
for i = 1:20
    X(i)=x^(i-1);
end
% approximated function
phi = sum(coef'.*X);
% original function
f = exp(x);

% Plots
fplot(f,[0,1],'k','LineWidth',1.4);
hold on;
fplot(phi,[0,1],'--','LineWidth',2);
grid on;
legend('f','L-infinity Approximation');
