%% Prueflinie, GMM-Beispiel 1
% Lisa Dalheimer
% WiSe 2122

clear variables
close all
clc

%% Beobachtungen

% Zahlenwerte eintragen
l1 = 239.996;
l2 = 359.943;
l3 = 599.946;
l4 = 119.949;
l5 = 359.953;
l6 = 240.007;

% 6 x 1 - Vektor der Beobachtungen erzeugen
l = [l1 l2 l3 l4 l5 l6]';

% Anzahl der Beobachtungen
n = length(l);
%% Funktionales Modell bzw. Design-Matrix aufstellen

% Funktionales Modell siehe Übungsfolien
% Unbekannte: xd = [s_AB, s_BC, s_CD, a]'

% Designmatrix ist einfach ablesbar, da linear:

A = ...
[1 0 0 -1;
 1 1 0 -1;
 1 1 1 -1;
 0 1 0 -1;
 0 1 1 -1;
 0 0 1 -1];

%% Stochastisches Modell

% Std. Abw. entspricht je 2mm + 2ppm
sigma_l = zeros(n,1);

for i= 1:n
    sigma_l(i) = 0.002 + 2*10^(-6)*l(i);
end

% VKM der Beobachtungen (keine Korrelation -> Diagonalmatrix)
C_ll = diag(sigma_l.^2);

% A-Priori-Varianzfaktor
sigma_0 = 10^(-3);

% Kofaktormatrix
Q_ll = C_ll / sigma_0^2;

% Gewichtsmatrix
P = inv(Q_ll);

%% Ausgleichung (quasi ALLES, was man ausrechnen kann:)

% Schätzwerte für die Unbekannten
xd = inv(A'*P*A)*A'*P*l;

% VKM der Unbekannten
Q_xdxd = inv(A'*P*A);
C_xdxd = sigma_0^2*Q_xdxd;

% ausgeglichene Beobachtungen
ld = A*xd;            
Q_ldld = A*Q_xdxd*A'; % aus Varianzfortpflanzungsgesetz leicht einsehbar

% Verbesserungen
vd = ld -l;

% a-posteriori-Varianzfaktor
r = length(l) - length(xd); % Redundanz
sigma_0d =sqrt(vd'*P*vd/r);



