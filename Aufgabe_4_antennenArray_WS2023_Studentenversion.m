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

%% AUFGABE 4: Phased Array

% AUFGABE 4a: Für was lässt sich eine Phasenbelegung technisch nutzen? Wie 
% wirken sich dabei Grating-Lobes auf den Schwenkbereich aus?

% AUFGABE 4b: Wiederhole Aufgabe 2b mit den Phaseninkrementen 90° und 180° 
% und vergleiche sie mit den Darstellung im Skript (Bild 6.6 und 6.7).
% Wie kann man das Zustandekommen dieser Richtdiagramme anschaulich
% erklären? In welche Richtungen treten Maxima und Nullstellen auf und
% warum? -> Richtdiagramme abspeichern und erklären

%% ZUSATZAUFGABE: Dreidimensionale Richtcharakteristik
% WIRD NICHT IM WORKSHOP BEHANDELT 

rad = pi/180;

N = 2; % Number of Elements
d_lambda = 0.5; % Spacing between elements  d/lambda

% Achtung: In MATLAB sind phi und theta vertauscht 
% und um pi/4 verschoben. 
% Daher: sin(theta) im Skript wird in MATLAB zu cos(phi)!

[x,y,z] = sphere(50);
[thetaa,phia,ra] = cart2sph(x,y,z);
 
[I, K] = size(phia);
f_gr = zeros(I,K);
for i = 1:I
    for k = 1:K
        if phia(i,k) == 0
            f_gr(i,k) = N;
        else
            f_gr(i,k) = ...
                sin(N*pi*d_lambda*sin(phia(i,k)))./sin(pi*d_lambda.*sin(phia(i,k)));
        end
    end
end

%f_gr = sin(N*pi*d_lambda*sin(phia))./sin(pi*d_lambda.*sin(phia));


dipole = cos(phia);
stab_ges = abs(sinc(0.5*cos(thetaa).*cos(phia)));

e_theta = abs(cos(thetaa).*sin(phia));
e_phi = abs(sin(thetaa));
e_ges = sqrt(e_theta.^2 + e_phi.^2);

c_ges = e_ges.*f_gr;
% %e_theta = cos(phia);
% e_theta = sinc(1/2.*sin(phia)).*cos(phia);
% e_theta2 = max(10*log10(1000*abs(e_theta)),zeros(size(phia)));
% 
[xn,yn,zn] = sph2cart(thetaa,phia,e_phi);
figure(1);
surf(xn,yn,zn);
xlabel('X');
ylabel('Y');
zlabel('Z');
title('E_\phi');
axis equal;

[xn,yn,zn] = sph2cart(thetaa,phia,e_theta);
figure(2);
surf(xn,yn,zn);
xlabel('X');
ylabel('Y');
zlabel('Z');
title('E_\theta');
axis equal;

[xn,yn,zn] = sph2cart(thetaa,phia,e_ges);
figure(3);
surf(xn,yn,zn);
xlabel('X');
ylabel('Y');
zlabel('Z');
title('E_{ges}');
axis equal;

[xn,yn,zn] =  sph2cart(thetaa,phia,stab_ges);
figure(4);
surf(xn,yn,zn);
xlabel('X');
ylabel('Y');
zlabel('Z');
title('Stab in x-Richtung');
axis equal;

[xn,yn,zn] = sph2cart(thetaa,phia,f_gr);
figure(5);
surf(xn,yn,zn);
xlabel('X');
ylabel('Y');
zlabel('Z');
title('Gruppenfaktor');
axis equal;

[xn,yn,zn] = sph2cart(thetaa,phia,c_ges);
figure(6);
surf(xn,yn,zn);
xlabel('X');
ylabel('Y');
zlabel('Z');
title('Richtcharakteristik des Arrays');
axis equal;