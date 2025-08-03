close all
clear all
clc

%% a)
% Parameter initialisieren, Verwendung eines "struct" mit der Bezeichnung "Parameter"

Parameter.A_Zelle = 200; % Elektrodenfl�che in cm�

Parameter.d_separator = 20e-6; % Dicke des Separators in m
Parameter.sigma_electrolyte = 1; % Leitf�higkeit des Elektrolyten in S/m, Einfluss des Separators wird im Folgenden vernachl�ssigt

Parameter.tau = 5; % Tortuosit�t der Kathodenschicht
Parameter.epsilon = 0.2; % Porosit�t der Kathodenschicht

Parameter.R_CT = 1; % Ladungstransferwiderstand in Ohm
Parameter.Q_CT = 11e-3; % Doppelschichtkapazit�t in F
Parameter.n_CT = 0.8; % Exponent

Parameter.R_Diff = 1.5; % Diffusionswiderstand in Ohm
Parameter.tau_Diff = 800; % Zeitkonstante Diffusion in sec
Parameter.n_Diff = 0.45; % Exponent
Parameter.C_diff = 100; % differentielle Kapazit�t in F

Parameter.R_CC = 0.25; % Kontaktwiderstand in Ohm
Parameter.Q_CC = 1e-5; % Doppelschichtkapazit�t in F
Parameter.n_CC = 0.95; % Exponent

Parameter.d_electrode = 50e-6; % Dicke der Elektrodenschicht in m
Parameter.sigma_electrode = 100; % elektronische Leitf�higkeit der Elektrode in S/m
Parameter.d_collector = 15e-6; % Dicke des Stromableiters in m
Parameter.sigma_collector = 37e6; % elektronische Leitf�higkeit des Stromableiters in S/m
Parameter.L_Al = 1e-7; % Induktivit�t Stromableiter in H

Parameter.f = logspace(-4,6,1000); % Frequenzvektor, muss sp�ter mit 2*pi() multipliziert werden

% Nutzen Sie f�r die weiteren Aufgabenteile Ihren Code aus �bung 6 als
% Basis

% Berechnung der Einzelimpedanzen

% Gesamtimpedanz berechnen

% Impedanzen plotten


%% Nutzen Sie f�r die Bearbeitung der Aufgaben b)-d) ebenfalls ihre Skriptstruktur aus �bung 6



%% e)

fhandle = @(L_opt)optimize(Parameter, L_opt);
result = fminsearch(fhandle,10);

Text = ['Das Minimum liegt bei L = ', num2str(result), ' d_electrode'];
disp(Text);
