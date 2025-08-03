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
  clear raw
  figure;imagesc(abs(raw_image));colormap('gray')
  title('Raw Data Image','FontSize',12)
  xlabel('Range','FontSize',12);ylabel('Azimuth','FontSize',12);

  [N_az,N_rg] = size(raw_image);  % Anzahl range- und azimuth Zeilen

%% Range Chirp erstellen
  tau=-tau_p/2:dtau:tau_p/2;      %zeitliche Abtastung
  range_chirp = exp(i*pi*k*tau.^2);   %=Impulsantwortfunktion des Filters


% Schritte R1-R7 sind für die Darstellung des Chirp_Signales
% für die eigentliche Filterung (siehe Abschnitt "Range Fokussierung"
% weiter unten) werden sie nicht gebraucht.

% R1: Darstellung des Chirp-Signals
  figure;
  subplot(3,1,1); plot(tau.*10^6, real(range_chirp));  title('Real Range Chirp'); xlabel('\tau /µs'); ylabel('Amplitude');
  subplot(3,1,2); plot(tau.*10^6, imag(range_chirp),'-r');  title('Imag Range Chirp'); xlabel('\tau /µs'); ylabel('Amplitude');
  subplot(3,1,3); plot(tau.*10^6,(angle((range_chirp)))); title('Wrapped Phase'); xlabel('\tau /µs'); ylabel('Phase /rad');
  
% R2: Berechnung des Fourierspektrums des Chirps
  range_chirp_fft=fft(range_chirp);

% R3: Berechung des Amplitudenspektrums
  range_chirp_fft_abs=abs(range_chirp_fft);

% R4: Erstellung der Frequenzachse
  N_rgchrp=numel(range_chirp);
  fny=1/(2*dtau);
  df=1/(N_rgchrp*dtau);
  f_rg=-fny:df:fny-df;

% R5: Für den Plot: Umsortierung des Amplitudenspektrums
  range_chirp_fft_abs_shift=fftshift(range_chirp_fft_abs);

% R6: Phasen- und Frequenzgang des Chirps
  figure;
  subplot(1,3,1); plot(tau.*10^6, unwrap(angle((range_chirp)))-min(unwrap(angle((range_chirp)))));  title('Unwrapped Phase'); xlabel('\tau /µs'); ylabel('Phase /rad'); grid on;
  subplot(1,3,2); plot(tau.*10^6, k.*tau.*10^(-6));  title('Frequenz'); xlabel('\tau /µs'); ylabel('Frequenz /MHz'); grid on;
  subplot(1,3,3); plot(f_rg.*10^-6,10*log10(range_chirp_fft_abs_shift));  title('Amplitudenspektrum'); xlabel('f /MHz'); ylabel('Amplitude /dB'); grid on;

% R7: Darstellung analog zu Seite 15 im Skript SAR-Grundlagen und Normierung
  figure
  subplot(1,2,1); plot((range_chirp_fft_abs_shift/max(range_chirp_fft_abs_shift(N_rgchrp/2))),f_rg.*10^-6);  title('Amplitudenspektrum'); xlabel('Modulus'); ylabel('f /MHz'); grid on;
  subplot(1,2,2); plot(tau(1:end-1).*10^6, (diff(unwrap(angle((range_chirp))))./dtau./(2*pi)).*10^(-6));  title('Frequenz'); xlabel('\tau /µs'); ylabel('f /MHz'); grid on;
  axis([-20,20,-10,10]);


%% Azimut Chirp erstellen

%% Range Fokussierung (~Komprimierung, ~Dekonvolution)

% Auf gleich Länge bringen
  N_rgpad=(N_rg-N_rgchrp)/2;
  range_chirp_ref=padarray(range_chirp,[0 N_rgpad]); 
  
% Fouriertransformation
  f_range_chirp_ref=fft(ifftshift(range_chirp_ref));
  
% Konjugation
  fcon_range_chirp_ref=conj(f_range_chirp_ref);
  
% Vervielfältigung
  fcon_range_chirp_ref_v=repmat(fcon_range_chirp_ref,[N_az,1]);
  
% Dekonvolution / matched filter
  fft_raw_image = fft(raw_image,[],2); % FFT der Rohdaten
  img_rg_focussed = ifft(fft_raw_image.*fcon_range_chirp_ref_v,[],2);
  
% Plot des range-fokussierten Bildes
  figure;imagesc(abs(img_rg_focussed));colormap('gray')
  title('Range Compressed Image','FontSize',12)
  xlabel('Range','FontSize',12);ylabel('Azimuth','FontSize',12);  


%% Azimut Fokussierung
% Hinweis: die Azimuth-Fokussierung findet für jede Spalte der Bildmatrix
% statt (in Richtung der Dimension 1). Dazu müssen u.U. Zeilenvektoren,
% z.b. die Referenzfunktion, transponiert werden. Achtung: beim transponieren
% mit dem Hochkomma ' ändert sich das Vorzeichen des Imaginärteiles! Hier
% daher zur Sicherheit mit .' oder mit dem Befehl transpose transponieren!

%% Multi-Looking