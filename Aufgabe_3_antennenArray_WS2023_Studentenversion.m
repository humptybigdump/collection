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

% Vektor mit allen Winkelschritten in Grad.
psi_deg = -180:0.1:180;
theta_deg = -180:0.1:180;

% Winkelschritte von Grad in Bogenmaß umrechnen. 
psi = psi_deg.*rad;
theta = theta_deg.*rad;

%% AUFGABE 3: Antennenarray mit Amplitudenbelegung
% In diesem Aufgabenteil werden Gruppenfaktoren von Arrays berechnet, deren
% Elemente mit einer unterschiedlichen Amplitude abstrahlen.

% Aufgabe 3: Analysiere die Nebenkeulen der verschiedenen
% Amplitudenbelegungen (Ablesen der Nebenkeulenunterdrückung). Welche
% Amplitudenbelegungen können verwendet werden, wenn eine
% Nebenkeulenunterdrückung von -25 dB gefordert wird?

% Konvention für die weiteren Berechnungen in diesem Abschnitt:
% Das Element in der Mitte bekommt den Index 0 und die weiteren Elemente
% sind symmetrisch um dieses Element angeordnet.

m = 3;          % Anzahl der Antennen Links, (bzw. rechts) von mittleren Element.
d_lambda = 0.5; % Abstand zwischen den Elementen  d/lambda
phi = 0;        % Phaseninkrement

N = 2*m + 1;    % Gesamtzahl der Antennen (immer ungerade)
n = -m:1:m;     % Vektor mit den Elementindex. 



% Berechnung der Strombelegungen
% Hinweis: Die Belegung wird so gewählt, dass der Strom der äußeren
% Elemente nicht Null wird! Aus diesem Grund steht bei Dreieck und Cosinus im
% Nenner die Zahl m+1 und nicht m. Wäre der Strom in dem Element gleich
% Null, würde aus auch nicht abstrahlen und man könnte es weglassen.

Ia_n = ones(size(n));           % Konstante Belegung
Ib_n = 1 - abs(n/(m+1));        % Dreieckbelegung. 
Ic_n = ???;      % Cosinus-Belegung
Id_n = ???; % Cosinus^2-Belegung
Ie_n = binomial((N-1),(n+m));   % Binomialbelegung (benötigt Datei binomial.m)
SLL_max = -25.*ones(1,length(theta)); %sidelobe level max

% Strombelegungen plotten:
figure(1);
stem(n,Ia_n);
xlabel('Element');
ylabel('Amplitude');
title('Konstante Belegung');
% iwantaprettyplot;

figure(2);
stem(n,Ib_n);
xlabel('Element');
ylabel('Amplitude');
title('Dreickförmige Belegung');
% iwantaprettyplot;

figure(3);
stem(n,Ic_n);
xlabel('Element');
ylabel('Amplitude');
title('Cosinus Belegung');
% iwantaprettyplot;

figure(4);
stem(n,Id_n);
xlabel('Element');
ylabel('Amplitude');
title('Cosinus^2 Belegung');
% iwantaprettyplot;

figure(5);
stem(n,Ie_n/max(Ie_n));
xlabel('Element');
ylabel('Amplitude');
title('Binomialbelegung');
% iwantaprettyplot;

% Ergebnis-Vektoren schon vorbelegen, hilft gegen MATLAB-Warnungen
% wegen sich vergrößernder Variablen in der folgenden for-Schleifen: 
f_gr_0 = size(theta);
f_gr_a = size(theta);
f_gr_b = size(theta);
f_gr_c = size(theta);
f_gr_d = size(theta);
f_gr_e = size(theta);

for k = 1:length(theta)    
array_coeff = exp(-1i*2*pi*d_lambda*cos(theta(k)).*n + n.*phi.*rad);
af  = sum(array_coeff);
f_gr_0(k) = af;
af_a = sum(array_coeff.*Ia_n);
af_b = sum(array_coeff.*Ib_n);
af_c = sum(array_coeff.*Ic_n);
af_d = sum(array_coeff.*Id_n);
af_e = sum(array_coeff.*Ie_n);
f_gr_a(k) = af_a;
f_gr_b(k) = af_b;
f_gr_c(k) = af_c;
f_gr_d(k) = af_d;
f_gr_e(k) = af_e;
end

% Darstellung des Gruppenfaktors in dB
f_gr_a_log = mag2db(abs(f_gr_a)/max(abs(f_gr_a)));
f_gr_b_log = mag2db(abs(f_gr_b)/max(abs(f_gr_b)));
f_gr_c_log = mag2db(abs(f_gr_c)/max(abs(f_gr_c)));
f_gr_d_log = mag2db(abs(f_gr_d)/max(abs(f_gr_d)));
f_gr_e_log = mag2db(abs(f_gr_e)/max(abs(f_gr_e)));

% Diagramm auf festen Wertebereich einschränken.
limits = [0,180,-40,0];

figure(6);
plot(theta_deg,f_gr_a_log);
xlabel('Elevation \theta in Grad');
ylabel('Gruppenfaktor in dB');
title('Konstante Belegung');
axis(limits);
% iwantaprettyplot;

figure(7);
plot(theta_deg,f_gr_b_log);
xlabel('Elevation \theta in Grad');
ylabel('Gruppenfaktor in dB');
title('Dreieckförmige Belegung');
axis(limits);
% iwantaprettyplot;

figure(8);
plot(theta_deg,f_gr_c_log);
xlabel('Elevation \theta in Grad');
ylabel('Gruppenfaktor in dB');
title('Cosinus-Belegung');
axis(limits);
% iwantaprettyplot;

figure(9);
plot(theta_deg,f_gr_d_log);
xlabel('Elevation \theta in Grad');
ylabel('Gruppenfaktor in dB');
title('Cosinus^2-Belegung');
axis(limits);
% iwantaprettyplot;

figure(10);
plot(theta_deg,f_gr_e_log);
xlabel('Elevation \theta in Grad');
ylabel('Gruppenfaktor in dB');
title('Binomialbelegung');
axis(limits);
% iwantaprettyplot;

figure(11);
plot(theta_deg,f_gr_a_log,theta_deg,f_gr_b_log,theta_deg,f_gr_c_log,...
    theta_deg,f_gr_d_log,theta_deg,f_gr_e_log,theta_deg,SLL_max);
axis(limits);
title('Richtdiagramme mit Amplitudenbelegungen');
legend('Konstant','Dreieck','cos','cos^2','Binomial','Location','EastOutside','SLLmax');
% iwantaprettyplot;


