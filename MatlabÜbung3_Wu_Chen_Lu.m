%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         NM Uebung2          %
%  Xingyu Wu,    2338934      %
%  Xiang Chen,   1985804      %
%  Wentao Lu     2272180      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


clc;
clear;
close all;
% fx = exp(x)
% x[0,1] --> t[-1,1]
% x = 1/2 + 1/2*t
% fx = exp(x) = exp(1/2 + 1/2*t) = ft 
% tk = cos((2k-1)*pi/(2n)) ,   Tn(tk) = 0
% x = 1/2(1+t) = 1/2(1+cos((2k-1)*pi/(40)),k=1,2,3,4...20
%fx = exp(x) = exp(1/2(1+cos((2k-1)*pi/(40))) , k=1,2,3,4...20

%% Element
n = 20;
for i = 1:n
    X(i) = [0.5*(1+cos((2*i-1)*pi/(2*n)))];
end
Y = exp(X);

%% Lagrane Polynomial

m = length(X);
L = ones(m,m);
for k = 1 : m
    V = 1;
    for i = 1 : m
        if k ~= i
            V = conv(V,poly(X(i))) / (X(k) - X(i));
        end
    end
    %L1(k, :) = V; 
    l(k, :) = poly2sym(V);
end
%C = Y * L1;
L = Y * l;
%% Plot
figure
box on;
f = exp(X);
plot(X,f,'r-*');
hold on
ezplot(L,[0,1]);
axis([0 1 -inf inf])
title('Approximation')
xlabel('X')
ylabel('Y')
legend('Original Funktion','Approximation')
L