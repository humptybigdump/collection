%%%%%%%%%%%%%%%%%%%%%%%%%
% Numerische Mathematik %
% WS 20/21              %
% Matlab-Blatt 3        %
% Lea Hansen            %
% Alexandra Schiefer    %
%%%%%%%%%%%%%%%%%%%%%%%%%

clc;
clear;
close all;

%% Daten produzieren
syms x
f_x = exp(x);
C = (0:0.05:0.95)';
for i = 1:1:20
    B(i) = x^(i-1);
end
f_C = exp(C);

%% a) L_unendlich-Approximation an f in U an 20 Stellen 
B_C = double(subs(B,x,C));
LGS = B_C\f_C;
phi = B'.*LGS;
phi_dach = sum(phi);

%% b) Plot der Daten und geschaetzten Funktionen
figure;
fplot(f_x,[0,1],'b');
hold on;
fplot(phi_dach,[0,1],'rx');
legend('exp(x)','L_{unendlich}-Approximation','Location','southeast');
title(['Schaubild der Funktion und der Approximation']);