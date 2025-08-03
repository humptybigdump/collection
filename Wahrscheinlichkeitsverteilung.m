clear all
close all
clc
%% Wahrscheinlichkeitsverteilungsbeispiel
% n = 10 Würfe
% k Anzahl gewürfelter Sechsen

k = 0:1:10;
n = 10;
p = 1/6;
q = 5/6;

% Erwartungswert

E = n*p;

% Wahrscheinlichkeitsdichtefunktion 
for i = k
    f_k(i+1) = nchoosek(n,i)*p^i*q^(n-i);
end

figure;
bar(k,f_k,0.5);
xlabel('Anzahl gew\"urfelter Sechsen $k$ bei $n = 10$ W\"urfen', 'Interpreter', 'Latex')
ylabel('Wahrscheinlichkeitsfunktion $f(k)$', 'Interpreter','Latex')
hold on;
xline(E,'--r',{'Erwartungswert'});

% Verteilungsfunktion
F_k = zeros(1,11);
for i=k
    F_k(i+1) = sum(f_k(1:i+1));
end

figure;
stairs(k,F_k,'-.or');
xlabel('Anzahl gew\"urfelter Sechsen $k$ bei $n = 10$ W\"urfen', 'Interpreter', 'Latex')
ylabel('Verteilungsfunktion $F(k)$', 'Interpreter','Latex')
hold on;
xline(3,'--b',{'k=3'});
yline(F_k(4),'--b',{'F(3)'});
%% Chips-Aufgabe

f_9 = (nchoosek(150,9)*nchoosek(35,1))/nchoosek(185,10)
f_10 = (nchoosek(150,10)*nchoosek(35,0))/nchoosek(185,10)
f_ge9 = f_9+f_10

%% Call-Center

f_p0 = 6^0*exp(-6)/factorial(0);
f_p1 = 6^1*exp(-6)/factorial(1);
f_p2 = 6^2*exp(-6)/factorial(2);
f_p3 = 6^3*exp(-6)/factorial(3);
f_p4 = 6^4*exp(-6)/factorial(4);

P_xle4 = f_p0 + f_p1 + f_p2 + f_p3 + f_p4;
