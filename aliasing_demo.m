% MATLAB exercise ALIASER, status 20130501
%        
% DEMONSTRATION OF THE ALIAS ERROR
% ================================
%
% Lecture SPECTRAL ANALYSIS AND DIGITAL FILTERING
%            
% by Hans-Georg Wenzel
% Geodaetisches Institut,
% Universitaet Karlsruhe,
% Englerstr. 7,
% D-76128 KARLSRUHE
% 
% Updated May 2013 - Malte Westerhaus
% Neufassung August 2017 - Malte Westerhaus
%

  clear;
  close all

% Anzahl der Messwerte Originalzeitreihe
  N = 16384;
  dt = 1;
  
% Zeit Vektor t1 mit einer Länge von N Stunden und 1h Abtastintervall:
  t1=0:dt:N-1;
% Berechung einer Sinuswelle mit Amplitude 100 und Periode Sekunden:
  per=200;
  forg=1/per;
  y1=100*sin(2*pi*t1/per);
  plot(t1,y1)
  axis([0 4096 -130 130]);
  xlabel('Zeit in Sekunden')
  ylabel('Amplitude')
  title('Beispielprogramm zum Aliasing - Originalzeitreihe; T=200 s')
  disp 'enter any key to continue'
  pause

% Zeit Vektor für ein beliebiges Abtastintervall zwischen 1s und 100s
% d.h. korrektes Abtastintervall für o.g. Sinuswelle!
  dtc=input('Eingabe Abtastinterval zwischen 1s und 100s ==>  ');
  tc=0:dtc:N-1;
% Berechung der Sinuswelle wie oben, aber mit neuem Abtastintervall:
  yc=100*sin(2*pi*tc/per);
  
% Plotten der beiden Zeitreihen in einer Grafik:
  figure
  pos = get(gcf,'pos');
  set(gcf,'pos',[100 400 1000  400])

  plot(t1,y1,tc,yc,'r',tc,yc,'ko')
  axis([0 4096 -130 130]);
  x0=[0 4096 0];
  y0=[0 0 0];
  hold on
  plot(x0,y0,'k--');
  xlabel('Zeit in Sekunden')
  ylabel('Amplitude')
  title(['Abtastintervall dt=',num2str(dtc),' s => KEIN ALIASING'])
  disp 'enter any key to continue'
  hold off
  pause

% Interpolation nach Whittaker-Shannon 
% siehe auch Buttkuss, Chap. 5.2, p.69, eq.(5.18) mit Interpolationsformel
% für maximales Abtastintevall (i.e. minimale Anzahl Abtastwerte) 
  for i=1:N
     xxx=(i-tc)/dtc;  
     sinx=sinc(xxx);
     yint(i)=sum(yc.*sinx);
  end
  
% Plotten der Original- und der interpolierten Zeitreihe in einer Grafik:  
  figure
  pos = get(gcf,'pos');
  set(gcf,'pos',[100 400 1000  400])

  plot(t1,y1,t1,yint,'r',tc,yc,'ko')
  axis([0 4096 -130 130]);
  hold on
  plot(x0,y0,'k--');
  xlabel('Zeit in Sekunden')
  ylabel('Amplitude')
  title(['Orig. Zeitreihe vs rekonst. Zeitreihe (dt=',num2str(dtc),' s)'])
  disp 'enter any key to continue'
  hold off
  clear yint;
  clear sinix;
  clear xxx;
  pause
  
% Zeit Vektor für ein beliebiges Abtastintervall > 100h
% d.h. falsches Abtastinterval!!
% Einlesen des Abtastintervalls dtw (z.B. 78h, schön ungleichmässig):
  dtw=input('Eingabe beliebiges (falsches) Abtastinterval > 100s ==>  ');
  tw=0:dtw:N-1;
% Nyquist-Frequenz des falschen Abtastintervalls
  fny=1/(2*dtw);
% Wie weit ist die Frequenz des Originalsignals von der Nyquist-Frequenz des
% falschen Abtastintervalls eintfernt?
  wobinich=ceil((forg-fny)/fny);
% Alias-Frequenz, (mehrfach) gefaltet an der Nyquist-frequency
  if mod(wobinich,2)==0
      fali=forg-wobinich*fny;
      alias_string=['Alias-Frequenz = f_a = f_o - ',num2str(wobinich),'*f_n_y = ']; 
  else
      fali=(wobinich+1)*fny-forg;
      alias_string=['Alias-Frequenz = f_a = ',num2str(wobinich),'*f_n_y - f_o = '];
  end
  
% Berechung der Sinuswelle wie oben, aber mit falschem Abtastintervall:  
  yw=100*sin(2*pi*tw/per);
% Originalzeitreihe und Alias-Zeitreihe in einer Grafik:
  figure
  pos = get(gcf,'pos');
  set(gcf,'pos',[100 400 1000  400])

  plot(t1,y1,tw,yw,tw,yw,'o')
  axis([0 4096 -130 130]);
  xlabel('Zeit in Sekunden')
  ylabel('Amplitude')
  title(['Abtastintervall dt=',num2str(dtw),' s => ALIASING !!'])
  disp 'enter any key to continue'
  pause
  
% als Test: auch die Interpolation der Alias-zeitreihe nach Shannon 
% (see Buttkuss, Chap. 5.2, p.69, eq.(5.18)) kann die Sache nicht retten!
  for i=1:N
     xxx=(i-tw)/dtw;
     sinx=sinc(xxx);
     ywint(i)=sum(yw.*sinx);
  end
  plot(t1,y1,t1,ywint,'r',tw,yw,'ro')
  axis([0 4096 -130 130]);
  xlabel('Zeit in Sekunden')
  ylabel('Amplitude')
  title(['Orig. Zeitreihe vs rekonst. Zeitreihe (dt=',num2str(dtw),' h)'])
  disp 'enter any key to continue'
  pause
  hold off
 
% Berechnung und Plot des Fourier-Spektrums der Original-zeitreihe
% zunächst Festlegung der Bildgröße
  figure
  pos = get(gcf,'pos');
  set(gcf,'pos',[100 400 1000  400])
% FFT
  z1=fft(y1);
% Berechnung des einseitigen Amplitudenspektrums
  az1=abs(z1)*2/length(y1);
  df=1/(N*dt);
  fny1=1/(2*dt);           
  freq1=0:df:fny1-df;
  plot(freq1,az1(1:length(y1)/2));
  axis([0 0.006 0 110]);
  xlabel('Frequenz in 1/s')
  ylabel('Spektrale Amplitude')
  title('ALIASING: Faltung an Nyquist-Frequenz(en) des falschen Abtastintervalls')
  hold on
  pause
 
% FFT der interpolierten Alias-Zeitreihe
% Hinweis: die Verwendung der interpolierten Zeitreihe für die folgenden
% Darstellungen hat Vorteile beim Plotten, z.B. ist die Länge der Zeitreihen 
% und damit die spektrale Auflösung df gleich. Das Spektrum kann also mit
% der gleichen Parametern (Frequenz-Achse, Anzahl etc.) wie die Original-
% Zeitreihe geplottet werden. Das ändert aber nichts an der Aliasfrequenz 
% und der Nyquist-Frequenz fnyw=1/(2*dtw), die weiterhin dem falschen 
% Abtastinterval dtw entspricht.
  zw=fft(ywint);
  azw=abs(zw)*2/length(ywint);
  plot(freq1,azw(1:length(ywint)/2),'m');
  pause
  
% die falsche, zu dtw gehörende Nyquist-Frequenz 
  fnyw=1/(2*dtw);
  
% Markierung der sich periodisch wiederholenden Nyquist-Frequenz fnyw
  for i=1:wobinich
      xx(i,1:3)=[i*fnyw i*fnyw i*fnyw];
      yy(i,1:3)=[0 110 0];
  end
  for i=1:wobinich
      plot(xx(i,1:3),yy(i,1:3),'k--');
  end
  pause
  
% Markierung der Faltungen an der sich periodisch wiederholenden (falschen)
% Nyquist-Frequenz
  x(wobinich)=2*wobinich*fnyw-forg;
  y(wobinich)=0;
    for i=(wobinich-1):-1:1
      x(i)=2*i*fnyw-x(i+1);
  end
  for i=wobinich:-1:1
      plot(x(i),0,'or');
      plot([x(i) x(i) x(i)],[0 10 0],'r--');
      pause
  end
  
% Beschriftung zur Information
  text(0.0006,100,['Frequenz Original Zeitreihe: f_o = 1/200 s = ' num2str((forg*1000)) ' mHz'])
  text(0.0006,92,['Abtastintervall: dt = ' num2str(dtw) ' s'])
  text(0.0006,84,['Nyquist-Frequenz: f_n_y = 1/(2dt) = ' num2str(fny*1000) ' mHz'])
  text(0.0006,76,[alias_string num2str(fali*1000) ' mHz'])
  hold off
  
% end of MATLAB exercise ALIASER