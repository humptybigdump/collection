close all;
clear all;
clc;
keyboard % hält das Skript an (nur für die Übung)

%% Parameter initialisieren
A_electrode = 200; % Elektrodenfläche in cm² -> ggf. in m² umrechnen

d_separator = 20e-6; % Dicke des Separators in m
sigma_electrolyte = 1;% Leitfähigkeit des Elektrolyten im Separator in S/m

R_CT = 1; % Ladungstransferwiderstand in Ohm
Q_CT = 11e-3; % Doppelschichtkapazität in F
n_CT = 0.8; % Exponent

R_Diff = 1.5; % Diffusionswiderstand in Ohm
tau_Diff = 800; % Zeitkonstante Diffusion in sec
n_Diff = 0.45; % Exponent
C_diff = 100; % differentielle Kapazität in F

R_CC = 0.25; % Kontaktwiderstand in Ohm
Q_CC = 1e-5; % Doppelschichtkapazität in F
n_CC = 0.95; % Exponent

d_electrode = 50e-6; % Dicke der Elektrodenschicht in m
sigma_electrode = 100; % elektronische Leitfähigkeit der Elektrode in S/m
d_collector = 15e-6; % Dicke des Stromableiters in m
sigma_collector = 37e6; % elektronische Leitfähigkeit des Stromableiters in S/m
L_Al = 1e-7; % Induktivität Stromableiter in H

f = logspace(-4,6,1000); % Frequenzvektor, muss später mit 2*pi() multipliziert werden
keyboard

%% Berechnung der Einzelimpedanzen

R_electrolyte = 1/sigma_electrolyte*d_separator/(A_electrode/1e4); % Elektrolytwiderstand in Ohm, Umrechnung cm²->m²

Z_CT = R_CT./(1+(1i*2*pi*f*R_CT*Q_CT).^n_CT); % Ladungstransferimpedanz

Z_W = R_Diff * tanh((1i*2*pi()*f*tau_Diff).^n_Diff)./((1i*2*pi()*f*tau_Diff).^n_Diff); % Warburg Impedanz (Diffusion)
Z_dC = 1./(1i*2*pi()*f*C_diff); % differentielle Kapazität

Z_CC = R_CC./(1+(1i*2*pi()*f*R_CC*Q_CC).^n_CC); % Impedanz des Kontaktwiderstandes 

RC = 1/sigma_collector*d_collector/(A_electrode/1e4)+1/sigma_electrode*d_electrode/(A_electrode/1e4); % ohmscher Widerstand
% Berücksichtigt nur Stromfluss senkrecht zur Elektrode, Bahnwiderstand des Ableiters ist nicht berücksichtigt

Z_L = 1i*2*pi().*f*L_Al; % Induktivität

%% Gesamtimpedanz berechnen

Z_Gesamt = R_electrolyte + Z_CT + Z_W + Z_dC + Z_CC + RC + Z_L; % Elektrodenimpedanz 
keyboard

% Impedanz plotten

figure
plot(Z_Gesamt)
axis equal;
set(gca, 'YDir', 'reverse');
grid on;
xlabel('Re(Z)');
ylabel('Im(Z)');
title('Gesamtimpedanzmodell')
keyboard

%% Zeilenvektor Schichtdickenvariation

L_var = [1 2 3 4 5 10];

%% Berechnung der Einzelimpedanzen, die von der Schichtdicke abhängig sind
% da die Variablen ZCT, ZDiff und RC schon zuvor als double-Vektor definiert wurden, 
% können diese nicht gleichbenamt nun als cell-Format verwendet werden.
% Daher müssen die erwähnten Variablen unter neuem Namen angelegt werden.
for i=1:length(L_var)
       
RCT(i) = R_CT/L_var(i); % RCT sinkt mit steigender Schichtdicke
QCT(i) = Q_CT*L_var(i); % QCT steigt mit steigendender Schichtdicke
Z_CT_var{i} = RCT(i)./(1+(1i*2*pi()*f*RCT(i)*QCT(i)).^n_CT);

RDiff(i) = R_Diff/L_var(i); % RDiff sinkt mit steigender Schichtdicke
Z_Diff_var{i} = RDiff(i)*tanh((1i*2*pi()*f*tau_Diff).^n_Diff)./((1i*2*pi()*f*tau_Diff).^n_Diff)+1./(1i*2*pi()*f*C_diff*L_var(i));
% Warburg-Impedanz und differentielle Kapazität hier in Z_Diff zusammengefasst
% tau_Diffändert sich nicht, C_diff steigt mit steigendender Schichtdicke

RC_var{i} = 1/sigma_collector*d_collector/(A_electrode/1e4)+1/sigma_electrode*d_electrode*L_var(i)/(A_electrode/1e4); 
% R_electrode steigt mit steigender Schichtdicke

end

%% Berechnung Gesamtimpedanz in Abhängigkeit der Schichtdicke

for i=1:length(L_var)
    Z{i} = R_electrolyte + Z_CT_var{i} + Z_Diff_var{i} + Z_CC + RC_var{i}+Z_L;
    Re_end(i) = real(Z{i}(1));
end
keyboard

%% Plot Gesamtimpedanzen für die verschiedenen Schichtdicken
liste = cell(length(L_var),1);
figure;
for i=1:length(L_var)
    plot(Z{i});
    liste{i} = ['Schichtdicke * ' int2str(L_var(i))];
    hold all;
end
axis equal;
set(gca, 'YDir', 'reverse');
grid on;
xlabel('Re(Z)');
ylabel('Im(Z)');
title('Gesamtimpedanzmodell Schichtdickenvariation')
legend(liste);
keyboard

%% Plot größter Realteil der Impedanzen in Abhängigkeit der Schichtdicke
figure;
plot(L_var,Re_end, 'o');
grid on;
xlabel('Schichtdicke * L1 [µm]');
ylabel('Re(Z) [\Omega]');
title('Gesamtimpedanzmodell Abhängigkeit von der Schichtdicke')
keyboard

%% Nlinfit-Regression
% als Startparameter für den Fit für a und b = 1 gewählt, somit ergibt sich var = [1,1]
% in Zeile 130 wirde die Funktion definiert, ihr werden die Startparameter und x-Werte übergeben
% die Parameter der Funktion werden in 118 zurückgegeben
% in 119 werden die zur Funktion zugehörigen y-Werte berechnet
var = [1 1];
funktion = @(var, L_var) var(1)+var(2)./L_var;
parameter = nlinfit(L_var, Re_end, funktion, var);
neu_y = funktion(parameter, L_var);
hold on;
plot(L_var, neu_y, 'r');
keyboard

%% Bonus
% neue x-Werte definieren. Hierzu Zeilenvektor von 0.1 bis 20 mit Schrittweie 0.2 erstellen
% neue y-Werte mittels Funktion berechnen lassen
% neue x- und y-Wrte gestrichelt in schwarz in bestehendes Figure plotten
% durch Auswahl einer beliebigen Schichtdicke, ergibt sich nun direkt die zugehörige Elektrodenimpedanz
neu_x = [0.1:0.2:20];
neu_y2 = funktion(parameter, neu_x);
plot(neu_x, neu_y2, '--k');
legend('Interpolationspunkte' , 'Interpolation' , 'Extrapolation')


