
% Übung 4 - Übungsaufgabe
% Startzustand herstellen
clc; clear; close all;

% Variablen und Messwerte deklarieren
R = 8.314; % [J/(mol*K)]
T_mess   = [0    20   40  60  80  100]; % Temperatur in °C
Eta_mess = [1792 1001 653 467 355 283]; % dynamische Viskosität in mPa*s

% Startparameter vorgeben
Eta_0 = 100; % [mPa*s]
E_0   = 1000;  % [J/mol]
P0 = [Eta_0; E_0];

% Solver aufrufen
options = optimoptions('lsqnonlin', 'Display', 'iter');
P = lsqnonlin(@(P)FSum(P,T_mess, Eta_mess, R),P0, [], [], options)


