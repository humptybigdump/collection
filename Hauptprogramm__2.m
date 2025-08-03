% Simulation einer Differenzialthermoanalyse
% Lösen eines DGL-Systems
clc, clear all, close all

% Daten:
k01    =    3e9;    % m³/s
k02    =    3e8;    % m³/s
Ea1    =    100e3;   % J/mol
Ea2    =    120e3;   % J/mol
dH1    =    -70e3;  % J/mol
dH2    =    -50e3;  % J/mol
R      =    8.314;  % J/(mol*K)
U      =    500;    % W/(m²*K)
A      =    100e-4; % m²
CK     =    8.8e2;  % J/K
cp     =    130;    % J(kg*K)
mP     =    12e-3;  % kg
VP     =    1.5e-6; % m³

To0    =    300;    % K
alpha  =    0.05;   % K/s

% Anfangswerte
CA0    =    5500;   % mol/m³
CB0    =    0;      % mol/m³
CC0    =    0;      % mol/m³

TR0 = To0;
TP0 = To0;

% Startwerte für den Zustandsvektor vorgeben
Z0=[CA0; CB0; CC0; TR0; TP0];

% Zeitspanne vorgeben:
tspan=[0 10000];    %s

% DGL System lösen lassen:
% Achtung: Dieses DGl-System ist "steif, d.h., mit den gegebenen Daten 
% erhalten die DGLn einen schnellen und einen lagsamen ablaufenden
% Vorgang, deren Zeitkonstanten sich um sehr viele Zehnerpotenzen 
% unterscheiden: --> "steif"

% DGL-Solver für steife Gleichungen
% a) rechnen implizit, Folge "unbedingt" stabil bei großen Schrittweiten
% b) verfügen über eine effektive Strategie zum Schrittweitenwechsel

[tListe, ZListe]=ode15s(@(t,Z)DGL(t,Z, k01, k02, Ea1, Ea2, ...
    dH1, dH2, R, U, A, CK, cp, mP, VP, To0, alpha), tspan, Z0);

% Resultate plotten;
yTexte={'CA [mol/m³]';'CB [mol/m³]';'CC [mol/m³]';'TR [K]';'TP [K]'};
           
for n=1:5
    subplot(5,2,2*(n-1)+1)
    plot(tListe,ZListe(:,n),'k-')
    xlabel('t [s]')
    ylabel(yTexte(n,:))
end

subplot(2,2,2)
TP=ZListe(:,5);
TR=ZListe(:,4);
plot(tListe,TP-TR,'k*')
ylabel('T_P-T_R [K]')
xlabel('t [s]')

To=To0+alpha*tListe;

subplot(2,2,4)
plot(tListe,To,'k-')
xlabel('t [s]')
ylabel('T_o [K]')