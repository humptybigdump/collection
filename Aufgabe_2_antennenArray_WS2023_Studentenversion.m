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

% Hilfsgröße zur Umrechung von Grad in Bogenmaß. 
rad = pi/180;

%% AUFGABE 2: 

% AUFGABE 2a: Bestimmen sie den Gruppenfaktor und Gruppencharaktistik eines 
% Arrays aus Hertz‘schen Dipolen (Array in x-Richtung, Dipol parallel zur z-Achse)
    % Die Amplitudenbelegung sei hierbei konstant 
    % Phasenverschiebung zwischen benachbarten Elementen = 0
    % Equidistanter Abstand 
% Berücksichtigen sie hierbei verschiedene Ausrichtungen der Hertz‘schen 
% Dipole und des Arrays zueinander
% In dem Programm sollten sie folgende Parameter variieren können
    % Elementanzahl
    % Elementabstand 
    % Phaseninkrement

% Für N = 2 und verschiedene Abstände entstehen gerade die 
% Richtdiagramme, die im Skript in Bild 6.5 für den Phasenwinkel von 0° 
% dargestellt sind.
% Wie kann man das Zustandekommen dieser Richtdiagramme anschaulich
% erklären? In welche Richtungen treten Maxima und Nullstellen auf und
% warum? -> Richtdiagramme abspeichern und erklären.

% AUFGABE 2b: Betrachte das Horizontaldiagramm von zwei Hertz'schen Dipolen, 
% die in z-Richtung ausgerichtet und in x-Richtung angeordnet sind. In diesem 
% Fall ist die Form des Horizontaldiagramms des Gruppenfaktors und der 
% Gesamtcharakteristik identisch. Wieso? -> Begründung!

% AUFGABE 2c: Unter welchen Bedingungen treten Grating Lobes auf? Was ist der
% Unterschied zwischen Grating Lobes und Nebenkeulen/Sidelobes? Wie ändern sich 
% Halbwertsbreite und Arraygewinn in Abhängigkeit von Elementanzahl und 
% Elementabstand?

N = 4 ;                 % Anzahl der Elemente
d_lambda = 0.5;         % Abstand zwischen den Elementen  d/lambda
phi = 0;              % Phaseninkrement deltaPHI

%% VERTIKALDIAGRAMM (Lineares Array in x-Richtung, Abb. 6.2.a)

% Vektor mit allen Winkelschritten in Grad.
theta_deg = 0:0.1:360; %ODER -180:0.1:180 mit view([90 -90]); !!!
% Winkelschritte von Grad in Bogenmaß umrechnen. 
theta = theta_deg.*rad;
psi = 0; % Vertikaldiagramm

% Richtcharakteristik eines Elements (Vertikaldiagramm)
elementfaktor = sin(theta);  % Hertz'sches Dipol (4.65)
                                                         
% Gruppencharakteristik ohne Phaseninkrement phi_z = 0 (in X-RICHTUNG)
f_gr_x = sin((N/2)*(2*pi*d_lambda*cos(psi)*sin(theta) - phi.*rad))./sin(0.5*(2*pi*d_lambda*cos(psi)*sin(theta)-phi.*rad));

% Gesamtcharakteristik bestimmen
c_ges =  ???;

% Plotten: polar - linear
figure(1);
% Elementfaktor, Gruppenfaktor und Gesamtcharakteristik im gleichen Diagramm darstellen.  

polar(theta, abs(f_gr_x),'-k');
hold on;
polar(theta, abs(elementfaktor),'b');
polar(theta, abs(c_ges),'--r');
hold off; 
legend('Gruppenfaktor','Element','Gesamt','Location','NorthEastOutside');

% Plotten: kartesisch - logarithmisch
figure(2);
plot(theta_deg, mag2db(abs(c_ges)));
Str = [N d_lambda phi];% Elementanzahl und Abstand in die Titelzeile mit aufnehmen
title(sprintf('N = %d, d = %2.2f \\lambda, \\phi = %d°', Str));
xlabel('\theta in Grad');
ylabel('Gesamtcharakteristik in C_{ges}(\theta) in dB');

% Y-Achse auf sinnvollen Wertbereich begrenzen
ylim([ max(mag2db(abs(c_ges))-40), max(mag2db(abs(c_ges)))]);

%% HORIZONTALDIAGRAMM (Lineares Array in x-Richtung, Abb. 6.2.a)

% Vektor mit allen Winkelschritten in Grad.
psi_deg = 0:0.1:360;
% Winkelschritte von Grad in Bogenmaß umrechnen. 
psi = psi_deg.*rad;
theta = pi/2; % Horizontaldiagramm

% Richtcharakteristik eines Elements (Horizontaldiagramm)
elementfaktor = ones(1, length(psi));  % Hertz'sches Dipol (4.65)
                                                         
% Gruppencharakteristik  ohne Phaseninkrement phi_z = 0 (6.13)
f_gr_x = ???;

% Gesamtcharakteristik bestimmen
c_ges =  f_gr_x.*elementfaktor;

% Plotten: polar - linear
figure(3);
% Elementfaktor, Gruppenfaktor und Gesamtcharakteristik im gleichen Diagramm darstellen.  
polar(psi, abs(f_gr_x),'-k');
hold on;
polar(psi, abs(elementfaktor),'b');
polar(psi, abs(c_ges),'--r');
hold off;
legend('Gruppenfaktor','Element','Gesamt','Location','NorthEastOutside');

% Plotten: kartesisch - logarithmisch
figure(4);
plot(theta_deg, mag2db(abs(c_ges)));
Str = [N d_lambda phi];% Elementanzahl und Abstand in die Titelzeile mit aufnehmen
title(sprintf('N = %d, d = %2.2f \\lambda, \\phi = %d°', Str));
xlabel('\psi in Grad');
ylabel('Gesamtcharakteristik in C_{ges}(\psi) in dB');

% Y-Achse auf sinnvollen Wertbereich begrenzen
ylim([ max(mag2db(abs(c_ges))-40), max(mag2db(abs(c_ges)))]);



