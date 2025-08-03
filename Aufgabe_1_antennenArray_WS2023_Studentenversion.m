close all;
%% ANTENNENARRAYS
% In dieser Aufgabe wird der Gruppenfaktor einer Antennengruppe in
% Abhängigkeit vom Elementabstand und der Elementanzahl bestimmt, sowie die
% Gesamtcharakteristik berechnet.
% Zunächst wird von einer homogen belegten Antennengruppe ausgegangen, 
% später werden bestimmte Amplitudenbelegungen ausprobiert und deren
% Auswirkungen auf die Nebenkeulen gezeigt. 
% Zum Schluss wird noch die Phasenbelegung mit einem Phased Array demonstriert. 

% AUFGABE 1: Das Hertz'sche Dipol
% AUFGABE 2: Array ohne Amplitudenbelegung
% AUFGABE 3: Array mit Amplitudenbelegung
% AUFGABE 4: Phased Array

% Anmerkung: Die Richtcharakteristiken werden aus der Strombelegung
% berechnet, d.h. Umrechung in dB erfolgt nach x_dB = 20*log(x_lin) 
% = mag2db();

%% Winkelbereich und Hilfsgrößen

% Hilfsgröße zur Umrechung von Grad in Bogenmaß 
rad = pi/180;

% Vektor mit allen Winkelschritten in Grad
psi_deg = -180:0.1:180;
theta_deg = ???
% Winkelschritte von Grad in Bogenmaß umrechnen 
psi = psi_deg.*rad;
theta = ???

%% AUFGABE 1: 
% Bestimme die Richtcharakteristik eines Dipols mit 
% homogener und mit sinusförmiger Strombelegung bei verschiedenen Längen des
% Dipols und vergleiche diese Richtcharakteristik mit der des 
% Hertz‘schen Dipols. Die Richtcharakteristik eines Dipols ist
% rotationssymmetrisch, es genügt daher, das Vertikaldiagramm zu bestimmen.

zeta = pi/2.*cos(theta);  % für 2l = lambda/2
% zeta = pi/10.*cos(theta);  % für 2l = lambda/10
% zeta = pi.*cos(theta);  % für 2l = lambda

% Richtcharakteristik eines Elements (VERTIKALDIAGRAMME!!!)
elementfaktor_1 = sin(theta);                                   % Hertz'sches Dipol (4.65)
elementfaktor_2 = ???       % Sinusförmige Stromverteilung für l = lambda/2 (5.27)    
elementfaktor_3 = ???  % Sinusförmige Stromverteilung für l = lambda (5.35)
elementfaktor_4 = ???               % Homogene Stromverteilung = infinitisimales Dipol

% Plotten: polar - linear//////////////////////////////////////////////////
figure(1); 
polar(theta, abs(elementfaktor_1),'r');
hold on;    % Im gleichen Diagramm darstellen
polar(theta, abs(elementfaktor_2),'b');
polar(theta, abs(elementfaktor_3),'--k');
polar(theta, abs(elementfaktor_4),'--g');
view([90 -90]); %theta_deg = 0:0.1:360; %ODER -180:0.1:180 mit view([90 -90]); !!!
hold off;
legend('Hertzsches Dipol','Sinus lambda/2','Sinus lambda', 'Homogen lambda/2','Location','NorthEastOutside');

% Plotten: kartesisch - logarithmisch//////////////////////////////////////
figure(2);
hold on;
plot(theta_deg, mag2db(abs(elementfaktor_1)));
plot(theta_deg, mag2db(abs(elementfaktor_2)));
plot(theta_deg, mag2db(abs(elementfaktor_3)));
plot(theta_deg, mag2db(abs(elementfaktor_4)));
hold off;
grid on;
legend('Hertzsches Dipol','Sinus lambda/2','Sinus lambda', 'Homogen lambda/2','Location','NorthEastOutside');
title('Richtcharakteristiken des Hertzschen Dipols');
xlabel('\theta in Grad');
ylabel('in C_{ges}(\theta) in dB');

% Y-Achse auf sinnvollen Wertbereich begrenzen
ylim([ max(mag2db(abs(elementfaktor_1))-40), max(mag2db(abs(elementfaktor_1)))]);
ylim([ max(mag2db(abs(elementfaktor_2))-40), max(mag2db(abs(elementfaktor_2)))]);
ylim([ max(mag2db(abs(elementfaktor_3))-40), max(mag2db(abs(elementfaktor_3)))]);
ylim([ max(mag2db(abs(elementfaktor_4))-40), max(mag2db(abs(elementfaktor_4)))]);
