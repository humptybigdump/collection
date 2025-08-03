close all;
clear all;
clc;
%% Aufgabe a)

% Parameter initialisieren

A_electrode = 200; % Elektrodenfl�che in cm� -> ggf. in m� umrechnen

d_separator = 20e-6; % Dicke des Separators in m
sigma_electrolyte = 1;% Leitf�higkeit des Elektrolyten im Separator in S/m

R_CT = 1; % Ladungstransferwiderstand in Ohm
Q_CT = 11e-3; % Doppelschichtkapazit�t in F
n_CT = 0.8; % Exponent

R_Diff = 1.5; % Diffusionswiderstand in Ohm
tau_Diff = 800; % Zeitkonstante Diffusion in sec
n_Diff = 0.45; % Exponent
C_diff = 100; % differentielle Kapazit�t in F

R_CC = 0.25; % Kontaktwiderstand in Ohm
Q_CC = 1e-5; % Doppelschichtkapazit�t in F
n_CC = 0.95; % Exponent

d_electrode = 50e-6; % Dicke der Elektrodenschicht in m
sigma_electrode = 100; % elektronische Leitf�higkeit der Elektrode in S/m
d_collector = 15e-6; % Dicke des Stromableiters in m
sigma_collector = 37e6; % elektronische Leitf�higkeit des Stromableiters in S/m
L_Al = 1e-7; % Induktivit�t Stromableiter in H

f = logspace(-4,6,1000); % Frequenzvektor, muss sp�ter mit 2*pi() multipliziert werden

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Berechnung der Einzelimpedanzen

R_electrolyte = 1/sigma_electrolyte*d_separator/(A_electrode/1e4); % Elektrolytwiderstand in Ohm, Umrechnung cm�->m�

Z_CT = []; % Ladungstransferimpedanz

Z_W = []; % Warburg Impedanz (Diffusion)
Z_dC = []; % differentielle Kapazit�t

Z_CC = []; % Impedanz des Kontaktwiderstandes 

RC = []; % ohmscher Widerstand
% Ber�cksichtigt nur Stromfluss senkrecht zur Elektrode, Bahnwiderstand des Ableiters ist nicht ber�cksichtigt

Z_L = []; % Induktivit�t

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Gesamtimpedanz berechnen

Z_Gesamt = []; % Elektrodenimpedanz 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Impedanz plotten
% Benutzen Sie hier den Code aus vergangenen �bungen
figure
%....

%% b)

% Zeilenvektor Schichtdickenvariation
L_var = [];


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Berechnung der Einzelimpedanzen, die von der Schichtdicke abh�ngig sind
% da die Variablen ZCT, ZDiff und RC schon zuvor als double-Vektor definiert wurden, 
% k�nnen diese nicht gleichbenamt nun als cell-Format verwendet werden.
% Daher m�ssen die erw�hnten Variablen unter neuem Namen angelegt werden.
for i=1:length([])
       
RCT(i) = R_CT/L_var(i); % RCT sinkt mit steigender Schichtdicke
QCT(i) = [];
Z_CT_var{i} = [];

RDiff(i) = []; 
Z_Diff_var{i} = [];


RC_var{i} = []; 
% R_electrode steigt mit steigender Schichtdicke
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Berechnung Gesamtimpedanz in Abh�ngigkeit der Schichtdicke

for i=1:length([])
    Z{i} = [];
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot Gesamtimpedanzen f�r die verschiedenen Schichtdicken
figure;
% ...

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hinzuf�gen der Legende
liste = cell(length([]),1);
for i=1:length([])
    liste{i} = ['Schichtdicke * ' int2str(L_var(i))];
end
legend(liste);


%% c) 

% Bestimmen des gr��ten Realteil der Impedanzen 
for i=1:length([])
    Re_end(i) = [];
end
% Plot gr��ter Realteil der Impedanzen in Abh�ngigkeit der Schichtdicke
figure;
% ...


%% d)

% Nlinfit-Regression
% modelfun = @(beta0, X) beta0(1)+beta0(2)./X mit X=Schichtdickenvariation
% beta0=Vektor mit Startwerten der zu bestimmenden Variablen, hier also quasi beta0=[a0 b0]
var = [1 1]; % definieren der Startwerte
funktion = @([]) []; % anlegen der Modellfuntion als handle
parameter = nlinfit([]); % Bestimmen der gefitteten Funktionsparameter druch nichtlinearen Fit

%...  %Berechnen und Plotten der Funtion und 



%% Bonus
% neue x-Werte definieren. Hierzu Zeilenvektor von 0.1 bis 20 mit Schrittweie 0.2 erstellen
% neue y-Werte mittels Funktion berechnen lassen
% neue x- und y-Wrte gestrichelt in schwarz in bestehendes Figure plotten
% durch Auswahl einer beliebigen Schichtdicke, ergibt sich nun direkt die zugeh�rige Elektrodenimpedanz
neu_x = [];
neu_y = funktion([]);
%... % plotten der Ergebnisse


