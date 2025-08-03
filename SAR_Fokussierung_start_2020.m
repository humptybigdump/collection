%% Konstanten und Parameter
  c = 3*10^8;                 % Lichtgeschwindigkeit in m/s
  v = 7098.0194;              % Satellitengeschwindigkeit in m/s

% Sensorparameter, entnommen aus dem File-Header des ERS-Bildes
% für Range Chirp
  lambda=0.05656;             % Wellenlänge der Trägerwelle
  fs=18.962468*10^6;          % Sampling Frequency
  dtau = 1/fs;                % Sampling period
  k=(4.18989015*10^(11));     % FM Rate Range Chirp Hz/s = 1/(s^2)
  tau_p=37.12*10^(-6);        % Chirp Länge [s]

% für Azimut Chirp
  r0=852358.15;               % Range zur Szenenmitte [m]
                              % Vereinfachung: r0 ändert sich eigentlich von near- to far range
  ta=0.627;                   % Aperturzeit (theor.: theta_a*r0/v=lambda/D_a*r0/v = 0.627s )
  prf=1679.902;               % Pulse Repitition Frequency (theor. Azimut Bandbreite W_a=2*v/L=1420Hz)

%% Daten einladen und darstellen
  raw = load('test_image.mat'); raw_image = raw.img_az_rg_conv;


%% Range Chirp erstellen


% Schritte R1-R7 sind für die Darstellung des Chirp_Signales
% für die eigentliche Filterung (siehe Abschnitt "Range Fokussierung"
% weiter unten) werden sie nicht gebraucht.

% R1: Darstellung des Chirp-Signals
  figure;
  subplot(3,1,1); plot(tau.*10^6, real(range_chirp));  title('Real Range Chirp'); xlabel('\tau /µs'); ylabel('Amplitude');
  subplot(3,1,2); plot(tau.*10^6, imag(range_chirp),'-r');  title('Imag Range Chirp'); xlabel('\tau /µs'); ylabel('Amplitude');
  subplot(3,1,3); plot(tau.*10^6,(angle((range_chirp)))); title('Wrapped Phase'); xlabel('\tau /µs'); ylabel('Phase /rad');
  
% R2: Berechnung des Fourierspektrums des Chirps

% R3: Berechung des Amplitudenspektrums

% R4: Erstellung der Frequenzachse

% R5: Für den Plot: Umsortierung des Amplitudenspektrums

% R6: Phasen- und Frequenzgang des Chirps

% R7: Darstellung analog zu Seite 15 im Skript SAR-Grundlagen und Normierung

%% Azimut Chirp erstellen

%% Range Fokussierung (~Komprimierung, ~Dekonvolution)

%% Azimut Fokussierung
% Hinweis: die Azimuth-Fokussierung findet für jede Spalte der Bildmatrix
% statt (in Richtung der Dimension 1). Dazu müssen u.U. Zeilenvektoren,
% z.b. die Referenzfunktion, transponiert werden. Achtung: beim transponieren
% mit dem Hochkomma ' ändert sich das Vorzeichen des Imaginärteiles! Hier
% daher zur Sicherheit mit .' oder mit dem Befehl transpose transponieren!

%% Multi-Looking