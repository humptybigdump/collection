% Übungsprogramm zur Fouriertransformation
%  - Veranschaulichung von kohärentem und nicht-kohärentem Sampling 
%  - Veranschaulichung von spectral leakage
%
% überarbeitete und erweiterte Version Aug. 2011
%  - noch ohne Berechnung für Phase!
%  - Weg dahin: Acos(2pif0t+phi) auf teilen in Acos(phi)cos - Asin(phi)sin
%    aufteilen; ergibt Überlagerung von 4 sinc Schwingungen mit Amplituden 
%    Acos(phi), Asin(phi); Achtung: prüfen auf Vorzeichen, speziell imag(FFT)

   clear
%% Zeitreihe

%  Beispiel: reine Cosinuus-Schwingung; Einheiten: Meter und Sekunde 

%  Allgemeine Angaben  
    fc=0.02;         % Beispiel "kohärente" Frequenz; Schwingung passt vollständig ins Intervall T
    C=1;             % Schwingungsamplitude in Einheit m
    P=0*pi/180;      % Phase in radiant
    N=1000;          % Länge der Zeitreihe  

%  Festlegung der Perioden, für die Cosiuns gerechnet werden soll
   ButtonName = questdlg('Lage bzgl. df-Schrittweite/Intervall', ...
                         'Auswahl der Frequenz', ...
                         'genau', 'in der Mitte', 'irgendwo','genau');
   switch ButtonName
      case 'genau',
       f0=0.02;     % Schwingungspeak liegt genau auf dem Frequenzraster
      case 'in der Mitte',
       f0=0.0205;   % Schwingungspeak in der Mitte eines Frequenzintervalls
      case 'irgendwo',
       f0=0.0203;   % Schwingungspeak liegt beliebig  
   end % switch    

%  Grundlegende Informationen (i.A. aus der Messdatenreihe zu entnehmen)
    dt=1;            % Abtastintervall, hier 1 sec
    T=N*dt;          % Gesamtlänge der Zeitreihe

%  Aus Schritt 2 folgende grundlegende Angaben zur Frequenzachse
    df=1/(N*dt);     % Frequenzintervall = Schrittweite auf der Frequenzachse
    fny=1/(2*dt);    % Maximalfrequenz ('Nyquistfrequenz')    
    
%  Konstruktion der Zeitachse 
    t=-T/2:dt:T/2-dt;  

%  Konstruktion der Frequenzachse für zweiseitiges MATLAB-Spektrum 
%  für den Fall N=gerade!!; sonst -fny+df/2:df:fny-df/2  
    f=-fny:df:fny-df;

%  Berechnung der Cos-Schwingungen
    y=C*cos(2*pi*f0*t+P);

%  Plotten der Zeitreihe
    plot(t,y);
    xlabel('Zeit t / sec ');
    ylabel('Messwerte / m ');
    title('Zeitreihe');
    pause
    
%  Hanning-Fenster und gewichtete Cos-Schwingung
    wh=hanning(N);
    yh=2*y.*wh';      %Faktor 2, um der Abnahme der Amplitude entgegenzuwirken

%  Für die weitere Verarbeitung werden die Zeitreihen periodisch versetzt
%  Der Ansatz nach Buttkus (siehe Skript, Gl.5.11) geht von einer Zeitreihe
%  von -T/2 bis T/2-1 aus. Die DFT setzt voraus, dass diese Zeitreihe
%  in der Form j*dt mit j=0,1,...N-1 vorliegt. Deshalb muss die linke
%  Hälfte der Zeitreihe mit ifftshift rechts angesetzt werden. Das geht,
%  weil die DFT voraussetzt, dass auch die Zeitreihe periodisch ist;
%  nachzuweisen über die Symmetrieeigenschaften von cos und sin.
%  Weitere Bemerkung: ohne diesen Shift funktioniert das u.g. Beispiel
%  für die Amplitude (also alles quadriert), aber beim Realteil teilw.
%  unterschiedliche Vorzeichen zwischen FFT-Resultat und sinc-Modell
    y=ifftshift(y);
    yh=ifftshift(yh);
    
%% Fouriertransformation
   
%  Berechnung des gewichteten und ungewichteten Spektrums
%  und Verschiebung der 2. Hälfte um -N, damnit das Resultat
%  konform ist mit der theoretischen Berechnung
    Z = fftshift(dt*fft(y));
    ZH = fftshift(dt*fft(yh));

%  Zum Vergleich 1: Konstruktion der Cosinus-Peaks, gefaltet mit der 
%  FFT des Rechteckfensters nach Skript, Gl. (5.11)
%  Diese Funktion wird 10fach höher gesampelt, damit sie in den u.g.
%  Beispielen als kontinuierliche Funktion erkennbar wird.
%  Die Frequenzachse muss von -fny nach fny-df laufen, damit die
%  Verschiebung in der sinc-Funktion passt
    fsinix=(-1)*fny:df/10:fny-df/10;
    sinixp = T/2*sin(pi*T*(fsinix-f0))./(pi*T*(fsinix-f0));
    sinixn = T/2*sin(pi*T*(fsinix+f0))./(pi*T*(fsinix+f0));
    sinix=sinixp+sinixn;
    Nsinix = N*10;
     
%  Zum Vergleich 2: Faltung mit FFT des Hanning-Fensters
%  Diese Funktion wird 10fach höher gesampelt, damit sie in den u.g.
%  Beispielen als kontinuierliche Funktion erkennbar wird.
    WHann = T/2*sin(pi*T*(fsinix-f0))./(pi*T*(fsinix-f0))...
          + T/2*sin(pi*T*(fsinix+f0))./(pi*T*(fsinix+f0))...
          + 0.5*T/2*sin(pi*T*(fsinix-f0+1/T))./(pi*T*(fsinix-f0+1/T))...
          + 0.5*T/2*sin(pi*T*(fsinix+f0+1/T))./(pi*T*(fsinix+f0+1/T))...
          + 0.5*T/2*sin(pi*T*(fsinix-f0-1/T))./(pi*T*(fsinix-f0-1/T))...
          + 0.5*T/2*sin(pi*T*(fsinix+f0-1/T))./(pi*T*(fsinix+f0-1/T));
    
%  Plotten des komplexen Spektrums
    figure
    plot(f,real(Z))
    xlabel('Frequenz f / Hz ');
    ylabel('Spektraldichte / m/Hz ');
    title('Dichtespektrum Realteil');
    pause
   
    plot(f,imag(Z))
    xlabel('Frequenz f / Hz ');
    ylabel('Spektraldichte / m/Hz ');
    title('Dichtespektrum Imaginärteil');
    pause

%  Umrechnung der komplexen Spektren Z und ZH in Amplituden- und Phasenspektrum
    Amp=abs(Z);       %ohne Normierung, also Amplitudendichte !!
    Phas=angle(Z);    %in Radiant !!
    RZ=real(Z); %.*sin(pi*T*(f-f0)+pi);
    AmpH=abs(ZH);
    
%  Plotten der für uns relevanten 1. Hälfte des (Amplituden-)spektrums
%  von Frequenz 0 bis zur Nyquistfrequenz, multipliziert mit 2
    plot(f(N/2:N),2*Amp(N/2:N))
    axis([0 fny 0 C*N+N/10])                          %Achsenskalierung
    xlabel('Frequenz f / Hz ');
    ylabel('Amplitudendichte / mHz ');
    title('Amplitudendichtespektrum');
    pause
  
%  Plotten der unmittelbaren Umgebung des Frequenzpeaks bei fc
    stem(f0,C*N,':k')              %Lage des "eigentlichen" Frequenz-Peaks
    axis([fc-5*df fc+5*df 0 C*N+C*N/10])   %Achsenskalierung 
    xlabel('Frequenz f / Hz ');
    ylabel('Amplitudendichte / m/Hz ');
    title('Amplitudendichtespektrum');
    hold on
    pause
    stem(f(N/2:N),2*Amp(N/2:N))
    pause
    plot(fsinix(Nsinix/2:Nsinix),2*abs(sinix(Nsinix/2:Nsinix)))
    pause
    hold off
    
%  Plotten des zweiseitigen Realteiles zwischen +/- 0.025 Hz, um
%  Spectral Leakage zwischen den beiden Schwingungen bei positiver
%  und negativer Frequenz zu demonstrieren
    minscale=min(sinix);
    maxscale=max(sinix);
    stem(f(1:N),real(Z(1:N)))
    axis([-fc-5*df fc+5*df minscale+minscale/8 maxscale+maxscale/10])           %Achsenskalierung
    xlabel('Frequenz f / Hz ');
    ylabel('Spektraldichte / m/Hz ');
    title('Realteil zur Verdeutlichung von Spectral leakage');
    hold on
    pause
    plot(fsinix(1:Nsinix),sinixp(1:Nsinix),'g')
    plot(fsinix(1:Nsinix),sinixn(1:Nsinix),'r')
    pause
    plot(fsinix(1:Nsinix),(sinix(1:Nsinix)),'--k')
    pause
    hold off
    
%  Plotten der Amplitudendichte des Hanning-gewichteten Spektrums
    stem(f0,C*N,':k')              %Lage des "eigentlichen" Frequenz-Peaks
    axis([fc-5*df fc+5*df 0 C*N+C*N/10])   %Achsenskalierung 
    xlabel('Frequenz f / Hz ');
    ylabel('Amplitudendichte / m/Hz ');
    title('Amplitudendichtespektrum Hanning-gewichtet');
    hold on
    pause
    stem(f(N/2:N),2*AmpH(N/2:N))
    pause
    plot(fsinix(Nsinix/2:Nsinix),2*abs(WHann(Nsinix/2:Nsinix)))
    pause
    hold off
   
%  Ausgabe auf dem Bildschirm: lfd. Index, Frequenz
%  und Amplitudendichte
%    disp (' Index  Frequenz / Hz    Amplitudendichte / ms ')
%    for i=1:100
%        str=sprintf('%5d%14.4f%18.4f',i,f(i),Amp(i));
%        disp(str)
%    end
   
%% Nutzung / inverse Fouriertransformation
% wird nur durchgeführt, wenn Schwingung vollständig in das Zeitintervall
% Ndt passt

    rest=fix(1/f0)-1/f0;
    if rest==0     
        peakf=f0/df+T/2+1;  
    
%   Frequenz, Amplitude und Phase des größten Peaks im Spektrum
        freq=f(peakf)           % Frequenz
        A=2/(N*dt)*Amp(peakf)   % Amplitude (jetzt normiert!)
        Phi=Phas(peakf)         % Phase
  
%   Konstruktion einer Cos-Schwingung mit den o.g. Parametern
%   z.B. als einfache Korrekturfunktion
        c=A*cos(2*pi*freq*t + Phi);
        plot(t(1:100),y(1:100),t(1:100),c(1:100),'r');
        xlabel('Zeit t / sec ');
        ylabel('Messwerte / m ');
       title('Hauptschwingung der Zeitreihe y');
       pause
    end
    
%%  inverse Fouriertransformation
    ZI=ifft(ifftshift(Z));
    yr=y;                        %Variante 1
    
%  Variante 2
    yr=fftshift(y);              
    ZI=fftshift(ZI);
    
%  Plotten der rücktransformierten Zeitreihe; es sollte keinen Unterschied 
%  zu y geben; 
%  Variante 1 ohne fftshift (s.o.); damit kann darauf hingewiesen werden, dass
%  Cos-Schwingung vollständig (kohärenter Fall) oder nicht vollständig
%  in die zur Verfügung stehende Länge T passt 
    plot(t+T/2,ZI,t+T/2,yr);
    xlabel('Zeit t / sec ');
    ylabel('Messwerte / m ');
%    title('rücktransformierte Zeitreihe; ohne fftshift');    %Variante 1
    title('rücktransformierte Zeitreihe');
