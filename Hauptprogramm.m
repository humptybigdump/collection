% Gewöhnliche Differenzialgeichungen und Gleichgewichtszustände
% Beispielaufgabe: Das Lösen eines expliziten DGL-Systems mit Matlab

% Hauptprogramm

%Startzustand vorgeben:
clc
clear all
close all

% Konstanten bzw. Variablen deklarieren (Einheiten angeben!):
k1 = 2.0e-3;    % [m^6/(s*mol²)
k2 = 1.0e-3;    % [m^3/(s*mol²)
k3 = 0.5e-3;    % [m^6/(s*mol²)

% Laufvariable bzw. Integrationsspanne vorgeben (Zeitliche Änderung 
% der Konzentration):
tSpan=[0  10000]; %s

% Anfangswerte festlegen:
CA0=1.0;    % [mol/m³]
CB0=0.01;   % [mol/m³]
CC0=0;      % [mol/m³]
CD0=0;      % [mol/m³]
CE0=0;      % [mol/m³]

% Startwerte als Zustandsvektor vorgeben:
% Wichtig: Z0 muss ein Spaltenvektor sein!
Z0=[CA0; CB0; CC0; CD0; CE0];  

% Funktion erstellen bzw. aufrufen (DGL-System lösen):
[tWerte, ZWerte] = ode45(@(t,Z)DGL(t,Z,k1,k2,k3),tSpan,Z0);

% Ergebnisse Visualisieren:
yLabelTexte=['CA';'CB';'CC';'CD';'CE'];

for n=1:5
    subplot(5,1,n)
    plot(tWerte,ZWerte(:,n),'b-')
    ylabel(['Konz. ' yLabelTexte(n,:)])
    xlabel('Zeit t')
end

% -----------------------------------------------------