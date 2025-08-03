%% Skript zur Veranschaulichung der Wellenpropagation
% Zum Ausführen des gesamten Skriptes im Menü "Run" klicken
% Zum Ausführen der einzelnen Sections im Menü "Run Section" klicken

% Initialisierung
clear                                   % Lösche Workspace
clc                                     % Lösche Command Window Benachrichtigungen
close all                               % Schließe alle offenen Plots

%% Nur räumlicher Cosinus

k = 0.1;                                % Wellenzahl k in 1/meter, hier ein Vektor (für Länge 1m bis 1000m)
z = 0:1000;                             % Distanz z in meter
welle = cos(-k*z);                      % Welle/Schwingung in abh. des Raumes. k ist also einfach nur ein Skalierungsfaktor

% Erstelle Plot
figure;
set(gcf,'Position',[250 150 1500 800])
plot(z,welle)                           % x-Achse=z, y-Achse=welle
title('Welle mit nur räumlicher Abhängigkeit - keine Ausbreitung mit der Zeit')
ylabel('Normierte Amplitude [a.u.]'), xlabel('z [m]')
grid on


%% Räumlicher und zeitlicher Cosinus - Welle propagiert in +z Richtung
% Wir betrachten die räumliche Ausbreitung mit -k*z
% Aufgrund des Minuszeichen sehen wir in der Animation, dass die Welle sich
% in positive z-Richtung ausbreitet


k = 0.1;                                % Wellenzahl k in 1/meter
z = 0:1000;                             % Distanz z in meter, hier ein Vektor (für Länge 1m bis 1000m)

f = 10;                                 % Frequenz f in 1/sekunden
t = 1:0.01:10;                          % Zeit t in sekunden, hier ein Vektor, welchen wir in einer for-loop zeitlich durchlaufen

% Erstelle Plot
figure;
set(gcf,'Position',[250 150 1500 800])
for n=1:length(t)
    welle = cos(2*pi*f*t(n)-k*z);       % Berechne Welle in der loop für jeden Zeitpunkt t(n) neu
    plot(z,welle)
    grid on
    pause(0.1); 
end


%% Räumlicher und zeitlicher Cosinus - Welle propagiert in -z Richtung
% Wir betrachten die räumliche Ausbreitung mit +k*z
% Aufgrund des Minuszeichen sehen wir in der Animation, dass die Welle sich
% in negative z-Richtung ausbreitet

k = 0.1;                                % Wellenzahl k in 1/meter
z = 0:1000;                             % Distanz z in meter, hier ein Vektor (für Länge 1m bis 1000m)

f = 10;                                 % Frequenz f in 1/sekunden
t = 1:0.01:10;                          % Zeit t in sekunden, hier ein Vektor, welchen wir in einer for-loop zeitlich durchlaufen

% Erstelle Plot
figure;
set(gcf,'Position',[250 150 1500 800])
for n=1:length(t)
    welle = cos(2*pi*f*t(n)+k*z);       % Berechne Welle in der loop für jeden Zeitpunkt t(n) neu
    plot(z,welle)
    grid on
    pause(0.1); 
end
