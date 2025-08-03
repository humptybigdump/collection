%% Matlab-Blatt 3, Numerische Mathematik WS20/21
%Simon Martin, 2000593
%Miriam Jäger, 2063370

clear all;
close all;


syms x

f = exp(x);             %Funktion

C = (0.05:0.05:1)';     %Intervall

for i = 0:1:19
    Basis(i+1) = x^i;            %1,x, x^2, x^3, x^4,....
end

%Funktionswerte berechnen:
f_werte = exp(C);

%Werte für Basis berechnen:
Basis_Werte = subs(Basis,x,C);
Basis_Werte = double(Basis_Werte);

%Gleichungssystem lösen
Loesung = Basis_Werte\f_werte;

x_Werte = Basis'.*Loesung;        %Linearkombination aus Lösung

%approximierte Funktion:
x_summe = sum(x_Werte);

syms x

figure
fplot(f,[0,1],'r--')
hold on
fplot(x_summe, [0,1],'b*')
legend('exp(x)','Approximation')
title('Funktion und Approximation')



