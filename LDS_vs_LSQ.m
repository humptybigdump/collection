% MATLAB Übung FFTvsLSQ
%        
% SPEKTRAL-ANALYSE MIT FFT UND LSQ
% ================================
% SoSe22
% A Heck & A Ciesielski

clear all;
close all;

%% Aufgabe 1
 
% Einlesen der Gezeitendaten (künstliche Zeitreihe mit den 61 wichtigsten
% Gezeitenwellen nach WaveFrequencies.dat + Rauschen
load BFOsignal.dat -ascii
N1 = length(BFOsignal); % Anzahl der Datenpunkte
tag_mes = BFOsignal(1:N1,1); % "gemessenes" Tag
zeit_mes = BFOsignal(1:N1,2); % "gemessenes" Zeit
g_mes = BFOsignal(1:N1,3); % synthetisch Schweresignal in nm/s2

% Extrahieren der Zeitachse (Spalte 1 --> t) und 
%                 Schweredaten (Spalte 2 --> data_t)

data_t=grav(:,2);
t= ?
dt= ?

%% Aufgabenteil 2&3neu

nover = 0;
f_sampl = ?

[G,f] = pwelch(data_t,hann(2^13),20,[],f_sampl);

figure;
plot(f,10*log10(G))
xlabel('Frequenz in 1/days')
ylabel('LDS in dB relativ zu 1 nm^2/s^4 * days ')
xlim([0 3.5]);

figure;
plot(f,G)
xlabel('Frequenz in 1/days')
ylabel('LDS in nm^2/s^4 * days ')
xlim([0 3.5]);
  
%% Aufgabe 4 
% Bestimmung der unbekannten Fourierkoeffizienten mit LSQ-Methode

% Einlesen der G-Matrix ("Forward Operator")
A=load('BFOregression.dat', 'ascii');
[N,M] = size(A); 

tag_mat = A(1:N,1); % "gemessenes" Tag
zeit_mat = A(1:N,2); % "gemessenes" Zeit

% Überprüfen Sie die Länge der Matrix mit Zeitreihen
% Bestätigen Sie Tage und Stunden


%
%
%
%
%

% Extrahieren Sie nur die Regressoren in die Matrix
G= BFOsignal(  ?? ,  ??  )




% Bestimmung des Vektors der unbekannten Fourierkoeffizienten
x=inv(G'*G)*G'*data_t;

% Paramter a und b aus Vektor der unbekannten Fourierkoeffizienten extrahieren
  a = x(2:2: ?? ); % 2*61
  b = x(3:2: ?? ); % 2*61 +1
  
%% Aufgabe 5 
% Bestimmung von Amplitude und Phase mit LSQ-Methode  

% Amplituden (Formel 3 auf Beiblatt)
  Z = [x(1); sqrt(a.^2+b.^2)];
  
% Phasen 
  phi=[0; atan2(b,a)*180/pi];
  % atan2 liefert auch positive Phasen. Physikalisch nicht möglich => -360°
  phi(phi>0) = phi(phi>0)-360;

%% Aufgabe 6
% Plot der Residuen

% Berechnung der Residuen
e = data_t - G*x;
  
  figure
  plot(t,e);
  xlabel('Zeit in Tage');
  ylabel('Schwere-Residuum in nm/s^2')
  title('Schwere Residuum nach LSQ');

% Fehlerrechnung
    
  Q = inv(G'*G);
  Q_ii = diag(Q);
  
  % STD der geschaetzten Parameter (Formel 1 auf Beiblatt)
  m0 = sqrt(e'*e/(N-M)); 
  mx_i = m0*sqrt(Q_ii);
  
  % STDs für Parameter a unf b aus mx_i extrahieren
  a_err = mx_i(2:2:34);
  b_err = mx_i(3:2:35);
  
  % STD der Amplituden (Formel 7 und 6 auf Beiblatt)
  sigma_abs = sqrt((a./(sqrt(a.^2+b.^2))).^2.*(a_err.^2)+(b./(sqrt(a.^2+b.^2))).^2.*(b_err.^2));
  
  % STD der Phasen (Formel 8 auf Beiblatt)
  sigma_phi = sqrt((-b./(a.^2+b.^2)).^2.*(a_err.^2)+(a./(a.^2+b.^2)).^2.*(b_err.^2))*180/pi;

%%
disp('Z =');
disp(Z);

disp('phi =');
disp(phi);

disp('sigma_abs =');
disp(sigma_abs);

disp('sigma_phi =');
disp(sigma_phi);



