%% Numerische Mathematik
%  Matlab Uebung 3
%  Kate Clintworth, Maike Zoller, Niklas Pfeffer
%
%%%%%%%%%%%%%%%%%

% clc
% clear all
% close
%% Aufgabe 1 
% % a) Determine best L infinity approximation 

% Sampling at 20 places
s=20;

syms x;

f=exp(x);

% Basis
B(1:s)=x.^((1:s)-1);

% Sampling in U at 20 places in C[0,1]
U=rand(s,1);

% insert x values from U into f
fU=exp(U); 

% insert x values from U into B
BU=double(subs(B,x,U));

% Best L infinity approximation phi
phi=sum(B'.*(BU\fU)); 


% % Plot f and phi in one figure

figure;
hold on;
fplot(f,[0,1],'r');
fplot(phi,[0,1],'b*');
legend('f(x)','L-infinity-Approximation \phi','Location','northwest');
hold off;

