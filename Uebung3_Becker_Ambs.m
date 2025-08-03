%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Numerische Mathematik      %
%  Matlab Uebung 3            %
%  Steffen Becker, Felix Ambs %
%  WS20/21                    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all
close all

%% Aufgabe a)
n=20;                           % Abtastung an 20 Stellen
syms x;
f_x = exp(x);
B(1:n) = x.^( (1:n) -1);        % Bildung von B
U=rand(n,1);                    % Abtastung an 20 zufälligen Stellen in [0,1]    
B_U = double(subs(B,x,U));      % Werte aus U für x in B einsetzen
f_U = exp(U);                   % Werte aus U in f_x einsetzen
phi_dach = sum(B'.*(B_U\f_U));  % L_unendlich-Approximation phi_dach berechnen

%% b) Plot
figure;
hold on;
fplot(f_x,[0,1],'r');
fplot(phi_dach,[0,1],'bo');
legend('f(x)','L\infty-Approximation \phi','Location','northwest');
hold off;