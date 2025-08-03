% MATLAB-Übung AUTOREG
% Berechnung von Autokorrelation und Leistungsdichtespektrum eines
% autoregressiven Prozesses 2. Ordnung
%
% Letzte Änderung: 19.06.07

  clear
  close all
  nall=8192;      % Länge der Zeitreihe
  nseg=8192;      % Länge eines Segmentes für Schätzung Leitsungsdichte
  nseg2=256;
  nover=0;        % Anzahl der überlappenden Werte
  nu1=2*nall/nseg; % Anzahl der Freiheitsgrade; jedes Spektrum bringt 2 Freiheitsgrade
  nu2=2*nall/nseg2;
  dt=1;           % Abtastintervall
  Fs=1/dt;        % Sampling Frequency

% Definition der Zeit- und Frequenzachsen
  t=0:dt:nall-1;  % Zeitachse
  nlag=-40:dt:40; % Retardierung; x-Achse für Autokorrelation
  f01=1/nseg;      % Spektrale Auflösung
  f02=1/nseg2;
  fny=1/(2*dt);   % Nyquist-Frequenz
  f1=0:f01:fny;     % Frequenzachse
  f2=0:f02:fny;
  
% Realisation eines AR(2)-Prozesses
  a1=1;
  a2=-0.6; 
  x(1)=randn(1,1);
  x(2)=randn(1,1);
  z=randn(nall,1);
  for i=3:nall
    x(i)=a1*x(i-1)+a2*x(i-2)+z(i);
  end
  
% Plot der Datenreihe
  figure
  pos = get(gcf,'pos');
  set(gcf,'pos',[100 400 800  400])
%  plot(t(1:1000),x(1:1000))
  plot(t(1:500),x(1:500))
  xlabel('Zeit')
  ylabel('x(t)')
  title('AR(2)-Prozess; a1=1.0, a2=-0.6')
  pause

% Berechnung der Autokorrelation des AR(2)-Prozesses nach Buttkus(eng) S. 248
% zunächst nur einseitig
  r(1)=1;
  r(2)=a1/(1-a2);
  for i=3:41,
    r(i)=a1*r(i-1)+a2*r(i-2);
  end
% Umordnen zum Plotten der zweiseitigen Autokorrelation
  for i=41:81
    akf(i)=r(i-40);
  end
  for i=1:40
    akf(i)=r(42-i);
  end

% Schätzung der Autokorrelationsfunktion:
  cxx=xcov(x,'unbiased');
  varx=cxx(nall);
  rxx=cxx/varx;

% Plot der Autokorrelation, Vergleich wahre und geschätzte Autokorrelation
  figure
  pos = get(gcf,'pos');
  set(gcf,'pos',[100 400 800  400])
  plot(nlag,akf,'b')
  xlabel('Retardierung')
  ylabel('Autokorrelation von x(t)');
  title('AR(2)-Prozess, wahre (blau) und geschätzte (rot) Autokorrelation');
  pause
  hold on
  plot(nlag,rxx((nall-40):(nall+40)),'r') 
  pause
  hold off

% Berechnung der zweiseitigen Spektraldichtefunktion nach Buttkus
  Gxx=1./(2.36+1.2*cos(4*pi*f1)-3.2*cos(2*pi*f1));     
  
% Multiplikation mit 2 für Vergleich mit dem geschätzten Power-
% spektrum, wenn PWELCH ohne Option 'twosided' aufgerufen wird;
% in diesem Fall liefert PWELCH die einseitige Power Spektral Density!
  Gxx=2*Gxx;    

% Plotten des Leistungsdichtespektrums
  figure
  pos = get(gcf,'pos');
  set(gcf,'pos',[100 400 800  400])
  plot(f1,Gxx,'r');
  xlabel('Frequenz');
  ylabel('G(f)');
  title('AR(2)-Prozess, Leistungsdichtespektrum');
  pause

% Schätzung der Leistungsdichte mit Hanningfenster 
%  w=hanning(seg);
%  P=pwelch(x,w,[],[],Fs);

% Schätzung der Leistungsdichte mit Rechteckfenster
  P1=pwelch(x,nseg,nover,[],Fs);
  P2=pwelch(x,nseg2,nover,[],Fs);
  
% Plot der geschätzten Leistungsdichte; Vergleich mit wahrem Spektrum 
% LINEARE y-Skala; N=length(P);
%  figure
%  plot(f1,P1);
%  xlabel('Frequenz');
%  ylabel('G(f)');
%  title('Vergleich Leistungsspektren');
%  pause
%  hold on
%  plot(f2,P2,'y','LineWidth',3); 
%  pause
%  plot(f1,Gxx,'r','LineWidth',2);
%  hold off
%  pause
  
% Plot der geschätzten Leistungsdichte; Vergleich mit wahrem Spektrum
%  N=length(P);
  figure
  pos = get(gcf,'pos');
  set(gcf,'pos',[100 400 800  400])
  semilogy(f1,P1);
  axis([0 0.5, 0.001,1000])
  xlabel('Frequenz');
  ylabel('G(f)');
  title('Vergleich Leistungsdichtespektren');
  pause
  hold on
  semilogy(f2,P2,'y','LineWidth',3); 
  pause
  semilogy(f1,Gxx,'r','LineWidth',2);
  text (0.3,200,['blau: 1 Segment, T=8192 s']);
  text (0.3,90,['gelb: ',num2str(nall/nseg2),' Segmente, L= ',num2str(nseg2),' s']);
  text (0.3,40,['rot: wahre Leistungsdichte']);
  hold off
  pause

% Berechnung und Plotten des 95%-Vertrauensintervalls mit
% ChiQuadrat-Verteilung
% nu: Anzahl der Freiheitsgrade
% 0.975: 97,5 Prozent der Daten liegen über dieser Grenze
% 0.025:  2,5 Prozent der Daten liegen über dieser Grenze
%  figure
%  plot(f2,P2,f1,Gxx,'LineWidth',2); 
%  chihigh2=chi2inv(0.025,nu2);
%  chilow2=chi2inv(0.975,nu2);
%  clow2=nu2*Gxx/chilow2;              %P2 -> Gxx
%  chigh2=nu2*Gxx/chihigh2;

%  chihigh1=chi2inv(0.025,nu1);
%  chilow1=chi2inv(0.975,nu1);
%  clow1=nu1*Gxx/chilow1;            %P1 ->  Gxx
%  chigh1=nu1*Gxx/chihigh1;

%  hold on
%  plot(f1,clow2,'m',f1,chigh2,'c');
%  pause
%  plot(f1,clow1,'--m',f1,chigh1,'--c');
%  hold off
%  pause

% Variante mit logarithmischer Y-Skala
  chihigh2=chi2inv(0.025,nu2);
  chilow2=chi2inv(0.975,nu2);
  clow2=nu2*P2/chilow2;              %P2 -> Gxx
  chigh2=nu2*P2/chihigh2;

  chihigh1=chi2inv(0.025,nu1);
  chilow1=chi2inv(0.975,nu1);
  clow1=nu1*P1/chilow1;            %P1 ->  Gxx
  chigh1=nu1*P1/chihigh1;
  
  figure
  pos = get(gcf,'pos');
  set(gcf,'pos',[100 400 800  400])
  semilogy(f1,P1,f1,Gxx,'LineWidth',2); 
  xlabel('Frequenz');
  ylabel('G(f)');
  title('Leistungsdichtespektren incl. Konfidenzintervall ohne Mittelung');
  hold on
  semilogy(f1,clow1,'--m',f1,chigh1,'--c');
  hold off
  pause

  figure
  pos = get(gcf,'pos');
  set(gcf,'pos',[100 400 800  400])
  semilogy(f2,P2,f1,Gxx,'LineWidth',2); 
  xlabel('Frequenz');
  ylabel('G(f)');
  title('Leistungsdichtespektren incl. Konfidenzintervall mit Mittelung');
  hold on
  semilogy(f2,clow2,'m',f2,chigh2,'c');
  hold off

  
  