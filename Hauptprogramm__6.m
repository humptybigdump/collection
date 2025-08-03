%% Hauptprogramm

% Startzustand
clc, clear all, close all

% Reaktionsparameter
T=273.15+650;   % Kelvin
p=10;           % bar
R=8.314;        % J/mol/K

% Kinetikparameter: 
%  cCO , cH2O   ,cH2,  cCO2,k_wgs (mol/kg/s),E_A (J/mol)
k=[0.13, 0.49, -0.45, -0.12, 1e5,            83500];

% Zusammensetzung Synthesegas
y_syngas     = [0.05, 0.6, 0.3, 0.05];        % Molanteil
M_syngas     = sum(y_syngas.*[18 28 2 44]);   % Molare Masse g/mol
n_syngas_ges = 3000/60/M_syngas;              % Molstrom Syngas mol/s
n_syngas = n_syngas_ges*y_syngas;             % Einzelne Molenströme mol/s

% Katalysatorparameter
m_kat= 5;                                % kg

% Startwert Wasserdampfstoffstrom in mol/s
n_H2O_Start = 1;
% Berechnung des Parameters n_H2O mit fsolve
tic
n_H2O_Opt = fsolve(@(n_H2O)Fun(n_H2O, T, p, R, k, m_kat, n_syngas), n_H2O_Start);
toc
% Berechneten Wasserstrom in kg/min umrechnen 
m_H2O_Opt = n_H2O_Opt*18*60/1000