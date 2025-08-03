%BEGINN: Skript.m
%ausführbares Skript

clear all
clc

%Vorgabe M1W [kg/h]
%Modellgleichung 8, VORG_M1W
M1W = 70;
%Vorgabe M1S [kg/h]
%Modellgleichung 9, VORG_M1S
M1S = 30;

%Vorgabe M2S [kg/h]
%Modellgleichung 10, VORG_M2S
M2S = 0;

%Vorgabe x3S, [-]
%Modellgleichung 11, VORG_x3S
x3S = 0.20;

%Vorgabe alpha, [-]
%Modellgleichung 12, VORG_alpha
alpha = 0.15;

%Zusammenfassen zu X_PARA
X_PARA = [M1W, M1S, M2S, x3S, alpha];

%Festlegung der Reihenfolge der Variablen von X_VAR
%X_VAR = [M2W, M3W, M3S, M4W, M5W, M4S, M5S]
%Schätzwerte für X_VAR
X_VAR0 = [1, 1, 1, 1, 1, 1, 1];

%Setzen der Optionen für 'fsolve'
%'Display': 'iter' displays output at each iteration, and gives the default
%exit message.
%'TolFun': Termination tolerance on the function value, a positive scalar.
%The default is 1e-6. See Tolerances and Stopping Criteria.
options = optimset('display', 'iter', 'TolFun', 1e-10);

%Lösen des Gleichungssystems
[X_VAR_LSG, GUETE] = fsolve(@(X_VAR) Ue2_Modellgleichungen(X_VAR, X_PARA), X_VAR0, options)
