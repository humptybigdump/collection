% Hauptprogramm zu Aufgabe 2, Übung 4

% Workspace, Figures und Kommandozeile vorbereiten
clear, clc, close all;

% Konstanten und Stoffdaten einlesen
load('Poly_Koeff.mat');
R = 8.314*1e-3; %kJ/(molK)
p_ref = 1; %bar

% Zielvariablen anlegen
p = [5, 10, 20]; % bar
T = linspace(500+273.15,1100+273.15,20); % K


% Eingansvektor
       %H2    CH4     H2O    CO     CO2
n_0 = [0      1       2.5    0      0]'; %mol/s
% Elementbilanz
      %H2     CH4     H2O    CO    CO2
Aeq = [2       4       2     0      0 ;  %H
       0       1       0     1      1 ;  %C
       0       0       1     1      2];  %O   
% Elementeingangsvektor
beq = Aeq*n_0; 

% Startvektor für Sovler
n_start = [1;1;1;1;1];


% Stoffmengenströme am thermodynamischen Gleichgewicht berechnen
Result = zeros(length(T), length(n_0), length(p));
parfor i=1:length(p)
    for j=1:length(T)
        Result(j,:,i) = fmincon(@(n)G(n, T(j), p(i), Poly_Koeff, ...
            R, p_ref), n_start, [],[], Aeq, beq);
    end
end

% Methanumsatz am Gleichgewicht berechnen
for i=1:length(p)
    for j=1:length(T)
        X_CH4(j,i) = (n_0(2)-Result(j,2,i) )/n_0(2);
    end
end


% Plotten des Methanumsatzes für die jeweiligen Drücke
plot(T-273.15,X_CH4*100)
legend('5 bar', '10 bar', '20 bar', 'Location', 'southeast')
xlabel('Temperatur [°C]')
ylabel('Methanumsatz [%]')
xlim([500,1100])
ylim([0,100])

