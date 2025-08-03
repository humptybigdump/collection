% MATLAB exercise FILTGRAV, status 20100611
%        
% Verschiedene digitale Tiefpassfilter
% ====================================
%
% Definition des Filters im Zeitbereich
% Impulsantwort symmetrisch bzgl t=0, MATLAB-Symmetrie
% Maximalwert auf h(1), gehört zu t=0
% 
% hier: Reihenfolge der Plots vertauscht für Verwendung
% in der Vorlesung

% schwarzer Hintergrund in Matlab6,7
%  colordef black   
  clear

% Einlesen der Daten
  load bfet1907.tid -ascii
  n=length(bfet1907);
  grec=bfet1907(1:n,3);  %beobachtete Schwere
  x=bfet1907(1:n,4);     %Luftdruck
  gsynt=bfet1907(1:n,5); %Synthetische Gezeiten
  y=grec-gsynt;          %Gez.Korr. Schwere
  ldr=detrend(x);
  grv=detrend(y);

% Definition Zeit- Frequenzachsen:  
  n2=ceil(n/2);
  tt=-n2:1:n2-1;        
  t=0:1:n-1;
  df=1/(n);
  f=0:df:1.0-df;
  fshft=-0.5:df:0.5-df;
  
  disp ' '
  disp 'Welches Tiefpassfilter darfs denn sein ?'
  disp '========================================'
  disp 'Folgende Impulsantwortfunktionen im Angebot:'
  disp 'idealer Tiefpass: 'i''
  disp 'Gaussfilter:      'g''
  disp 'Hanningfilter:    'h''
  disp 'Rechteckfilter:   'r''
  value_type=input('Eingabe ==>  ','s');

  switch(value_type)
      case {'i'}
        % Ideales Tiefpassfilter
        % Start- und Stoppfrequenz des Filters
        % fa=0.025;
        % fe=0.055;
        % Wn=[2*fa 2*fe];   %dies ist ein Bandpass
          name='ideales Tiefpassfilter';
          fe=0.003;
          Wn=[2*fe];
          hsym=fir1(n-1,Wn,hanning(n));

      case {'g'}
        % GAUSS-Filter
          name='Gauss-Filter';
          kn=70;
        % hsym=gauss(t,kn);
          hsym = exp(-tt.^2/(2*kn^2)) / (kn*sqrt(2*pi));
        % hsym=hsym/(2*kn*sqrt(pi));       % Normierung für 2D-Fall!! =>
        % Filtergain (freq) = 1  

      case {'h'}
        % Hanningfenster als Impulsantwort
          name='Hanning-Filter';
          hsym=0*t;
          kn=160;                     % halbe Länge des Filters
          w=hanning(2*kn+1);
          for i=(n2-kn+1):(n2+kn+1)
              hsym(i)=w((i-(n2-kn)));
          end
          hsym=hsym/(kn+1);     %Normierung => H(f=0)=1;

      case {'r'}
        % gleitendes Mittel mit konstanten Gewichten =>
        % Rechteck-Impulsantwort
          name='Rechteck-Filter';
          hsym=0*t;
          kn=120;                     % halbe Länge des Filters
          for i=(n2-kn+1):(n2+kn+1)
              hsym(i)=1;
          end
          hsym=hsym/(2*kn+1);  %Normierung => H(f=0)=1;
        
      otherwise
          error('unbekannter Filtertyp')
  end
  
% ---------------------------------------------------------------

% Umsortierung auf MATLAB-Symmetrie 
  h=ifftshift(hsym);

% Fouriertransformation der Impulsantwort  
  H=fft(h);
  H=H';
  A=abs(H);
  LA=log(A);
  smax=max(LA);
  smin=min(LA);

% Plot mit linearer Y-Achse und Symmetrie bzgl f=0
  figure
  plot(fshft,fftshift(A));
  xlabel('Frequenz / 1/Stunden')
  ylabel('Amplitudencharakteristik')
  title('Filter-Übertragungsfunktion, symmetrisch bzgl. f=0')
  axis([-0.02,0.02,0,1.1]);
  pause
 
% Plotten der Impulsantwort
  figure
  plot(tt,hsym)
  pmax=max(hsym);
  pmin=min(hsym);
  xlabel('Stunden')
  ylabel('Impulsantwort h(t)')
  title('Impulsantwort symmetrisches Tiefpassfilter')
  axis([-1024,1024,(pmin-abs(pmin/10)),(pmax+pmax/10)]);
  pause
  
% Plotten der auf MATLAB-Symmetrie umsortierten Impulsantwort 
  figure
  plot(t,h)
  xlabel('Stunden')
  ylabel('Impulsantwort h(t)')
  title('Impulsantwort Tiefpassfilter, "MATLAB-Symmetrie"')
  axis([0,n-1,(pmin-abs(pmin/10)),(pmax+pmax/10)]);
  pause  
  
% Anwendung des Filters auf Luftdruckzeitreihe
  X=fft(ldr);
  Y=H.*X;
  lf=ifft(Y);
  
% Daten gefiltert - gegen ungefiltert
  plot(t(500:5500),lf(500:5500),'r','LineWidth',2)
  hold on
  plot(t(500:5500),ldr(500:5500),'b','LineWidth',0.5)
  xlabel('Stunden')
  ylabel('hPa')
  title('Luftdruck: Originaldaten (blau) vs. Tiefpass-gefilterte Daten (rot) ')
  text(3700,15,[name])
  hold off
  
%  save(value_type,'lf');
  
% end of MATLAB exercise DIGFILT
