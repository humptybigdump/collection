% Zentraler Grenzwertsatz
clear variables
close all
clc

% 100 mal Ziehen aus Gefaess mit 4 schwarzen und einer weissen Kugel
% gesucht: Wie gross ist Wahrscheinlichkeit fuer zwischen 21 und 24 mal
% weiss?

n = 100;
p = 0.2;
q = 0.8;

%% Binomialverteilung

% Wahrscheinlichkeitsdichte nach Binomialverteilung
for i = 0:100
    f_B(i+1) = nchoosek(100, i)*p^i*q^(100-i);
end

% Wahrscheinlichkeit fuer zwischen 21 und 24 weisse Kugeln
a = 21;
b = 24;

P_21bis24_B = sum(f_B(a+1:b+1));

% hier sollte der Binomialkoeffizient in der Berechnung eine Warnung
% ausgeben

%% Naeherung: Normalverteilung
% nach zentralem Grenzwertsatz

mu  = n*p;
var = n*p*q;
sig = sqrt(var);

alpha = (a - mu - 0.5)/sig;
beta  = (b - mu + 0.5)/sig;
% Hier koennte man auch die vertafelte Verteilung nutzen und als z alpha
% und beta einsetzen.

P_21bis24_N = normcdf(beta) - normcdf(alpha);
% In MATLAB ist Phi mit cumulative density function -> cdf auszurechnen

%% Plots
x = 0:100;

figure
stem(x, f_B, 'r')
hold on
plot(x, normpdf(x, mu, sig),'k')
grid on
legend('Binomialverteilung', 'Naeherungsnormalverteilung')
xlabel('Anzahl gezogener weisser Kugeln')
ylabel('Wahrscheinlichkeitsdichte')


