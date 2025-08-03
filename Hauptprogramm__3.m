% Übung 4 - Übungsplatt
% Parameterschätzung und Messdaten-Fit mit DGL
% Reaktionskonstanten einer chemischen Reaktion aus Messdaten ermitteln
% Hauptprogramm.m

% Oberfläche vorbereiten
clc; clear; close all

% Messdaten
t_exp   = [0 2 4 6 8 10 15 20 25 30 35 40 45 55 70 103 143 410 ...
        505 595]; % h
c_A_exp = [5.60 5.20 4.91 4.76 4.45 4.03 4.17 3.81 3.65 3.60 3.52 3.27...
        3.27 3.07 2.93 2.58 2.43 2.22 2.24 2.24]; % kmol/m³

% Anfangswerte des ode-Solvers
%     c_A0  c_B0  c_C0  c_D0
Z0 = [5.6;  5.6;  0;    0.6]; % kmol/m³
tspan = [0 600]; % h

% Startwerte für k1 und k2 setzen
k0 = [0.001; 0.001];

% Optionen setzen
options = optimoptions('lsqnonlin', 'Display', 'iter');
k = lsqnonlin(@(k)FSum(k, t_exp, c_A_exp, Z0, tspan), k0, [0;0], [],options);



