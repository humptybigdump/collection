% MATLB-Übung TKREIS, Version 2010
% Messungen WS2007/2008 mit Sokkisha
% Achtung: keine Normierung des Amplitudenspektrums !!
% Normierung wird angebracht bei Berechnung der Korrekturfunktionen 
% Achsenbeschriftung umgestellt auf DIN

% schwarzer Hintergrund in Matlab6,7
%  colordef black        

  clear
  close all
% Einlesen des Datensatzes  
  load tkreis08.dat  -ascii
  t=tkreis08(:,1)*0.61856716;  %Schrittweite in mgon, durch Versuchsaufbau vorgegeben
  N=length(t);       %Länge des Vektors = Anzahl der Datenpunkte

% Umordnen des mehrspaltigen Datensatzes auf 1-spaltige Arrays 
  yh1=tkreis08(:,2);
  yr1=tkreis08(:,3);
  yh2=tkreis08(:,4);
  yr2=tkreis08(:,5);

% Mittel aus allen 4 Messungen
  ym=(yh1+yr1+yh2+yr2)/4;  
% Mittelwert abziehen
  ym=ym-mean(ym);

% plotte das Mittel der Winkelablesungen  Weg1 / Weg2:
  plot(t,ym)
  xlabel('Winkel / mGon')
  ylabel('Differenz Theodolit-Referenz / mgon')
  title('MATLAB Übung TKREIS: Interpolationsfehler Theodolit')
  disp ' enter any key to continue'
  pause
% print

% Abtastinterval dt = 0.61856716 mGon 
% Winkelbereich insgesamt T = N*dt mGon
% Grund"frequenz" df = 1/T = 1/(N*dt)
% Nyquist"frequenz" fny = 1/(2*dt)
% freq = Frequenzachse für das Spektrum
% !! Hinweis: wenn es sich bei der x-Koordinate nicht um die
% !! Zeit, sondern um eine andere Größe (Winkel, Länge etc. )
% !! wird die inverse Größe 1/x im Allgemeinen nicht als 
% !! Frequenz, sondern als Ortsfrequenz bezeichnet. Die Ortsfrequenz
% !! bezeichnet hier also ein räumliches Schwingungsmaß mit der 
% !! Einheit 1/mgon)
% !!
% !! Hinweis 2: Genaugenommen muss zwischen den Fällen Anzahl N der Messwerte gerade
% !! oder ungerade unterschieden werden. Die unten berechneten Frequenzachsen
% !! gelten für N=gerade. Im ungeraden Fall müsste es heissen:
% !! freq2=0:df:(2*fny-df/2) bzw. freq=0:df:(fny-df/2) [siehe Übung])
%
  dt=0.61856716;   % mgon
  df=1/(N*dt);     % 1/mgon
  fny=1/(2*dt);    % 1/mgon
  freq2=0:df:(2*fny-df);   % 1/mgon   Plottet beide Hälften des Spektrums
  freq=0:df:(fny-df);   % 1/mgon      Plottet nur die erste Hälft des Spektrums

% berechne Fourier Spektrum (Buttkus, 5.23)
  Z=dt*fft(ym);

% entferne Summe der Messwerte (erster Wert des Spektrums)
% bringt Vorteile beim Plotten (alternativ: Mittelwert abziehen)
%  Z(1)=0;

  figure
% 1) MATLAB berechnet Spektrum Z(n*df) für 0 <= n <= N-1, d.h.
% zwischen 0 und (2*fny-f0). Plotte Realteil des Spektrums als 
% Funktion der Frequenz (verallgemeinerte Frequenz)
  stem(freq2,real(Z));
  axis([0, 1.6, -6 6]);
  title('MATLAB Übung TKREIS, Realteil von 0 bis 2*fny') 
  xlabel('Ortsfrequenz / 1/mgon')
  ylabel('Realteil / mgon*mgon')
  pause
  % Plotten einer senkrechten Linie an der Nyquist-Frequenz
    hold on
    xx=[fny fny fny];
    yy=[-8 1 6];
    plot(xx,yy,'g--');
    hold off
    pause

  figure  
  stem(freq2,imag(Z));
  title('MATLAB Übung TKREIS, Imaginärteil von 0 bis 2*fny') 
  xlabel('Ortsfrequenz / 1/mgon')
  ylabel('Imaginärteil / mgon*mgon')
  axis([0, 1.6, -6 6]);
  pause
  % Plotten einer senkrechten Linie an der Nyquist-Frequenz
    hold on
    xx=[fny fny fny];
    yy=[-8 1 6];
    plot(xx,yy,'g--');
    hold off
    pause

% 2) In vielen Fällen findet man eine Darstellung des Spektrums
% als Funktion der Frequenz zwischen -fny bis +fny (siehe Gl. (4.4)
% im Skript). Da das diskrete Spektrum mit N periodisch ist, sind
% beide Darstellungen äquivalent. Zur Umsortierung des Spektrums
% zur "zweiseitigen" Darstellung kann man den MATLAB-Befehl
% fftshift verwenden. 
%--
% Auskommentiert Mai 2009
%--
%  freq1=(fny*(-1)+df):df:fny;
%  stem(freq1,fftshift(real(Z)));
%  title('MATLAB Übung TKREIS, Realteil von -fny bis +fny') 
%  xlabel('Ortsfrequenz / 1/mgon')
%  ylabel('Realteil')
%  pause
%  stem(freq1,fftshift(imag(Z)));
%  xlabel('Ortsfrequenz / 1/mgon')
%  ylabel('Imaginärteil')
%  title('MATLAB Übung TKREIS, Imaginärteil von -fny bis +fny') 
%  pause

% Berechnung des Betragsspektrums.
  AZ=abs(Z);

% Berechnung des Phasenspektrums
  PZ=atan2(imag(Z),real(Z))*180/pi;
  
% ===================================================================
% Bemerkung: bei reellen Eingangsdaten und Verwendung des halbseitigen 
% Spektrums kann das Betragsspektrum durch Multiplikation mit 2/(N*dt)
% in das Amplitudenspektrum umgerechnet werden; aus dem Amplitudenspektrum 
% lassen sich die tatsächlichen Amplituden der cos-Schwingungen ablesen.
% ====================================================================
  AMP=abs(Z(1:(N/2)))*2/(N*dt);
  
  figure
% plotte das Amplituden Spektrum von 0 bis fny als Linienspektrum
  stem(freq,AMP,'k-o');
  axis([0 fny 0 0.35])
  xlabel('Ortsfrequenz / 1/mgon')
  ylabel('Amplitude / mGon')
  title('MATLAB Übung TKREIS, Amplituden-Spektrum') 
  disp ' enter any key to continue'
  pause
% zoomen, um Frequenzen der größten Peaks besser ablesen zu können
  axis([0, 0.14,0,0.35])
  pause
% wieder rauszoomen
  axis([0 fny 0 0.35])
  
  figure
% alternativ: plotte das Amplituden Spektrum von 0 bis fny
  plot(freq,AMP,'k-o');
  axis([0 fny 0 0.35])
  xlabel('Ortsfrequenz / 1/mgon')
  ylabel('Amplituden / mGon')
  title('MATLAB Übung TKREIS, Amplituden-Spektrum') 
  disp ' enter any key to continue'
  pause
  
  close 2
  close 3
  
%  figure
% plotte das Phasen Spektrum
  stem(freq,PZ(1:(N/2)),'k-o');
  axis([0 fny -200 200])
  xlabel('Ortsfrequenz / 1/mgon')
  ylabel('Phase / Grad')
  title('MATLAB Übung TKREIS, Phasen-Spektrum')
  disp ' enter any key to continue'
  pause 

% Verwendung des Matlab-Befehls "find", um den Index k 
% des Maximum des Amplituden Spektrums zu finden. Wegen
% der Zweiseitigkeit des Spektrums taucht das Maximum
% zweimal auf; hier wird nur der erste Index verwendet.
  l=find(AMP == max(AMP));
  k=l(1);
  cycle=1/freq(k);
  cy=['tkreis Max-Differenz bei ',num2str(cycle),' mGon'];
  c1=['Amplitude and Phase (Gruppe 1) bei max: ',num2str(AMP(k)),' mgon, ', num2str(PZ(k)), ' deg'];
  disp(cy)
  disp(c1)
  disp('  ')

% Alternativ: Ausgabe der ersten 12 Array-Elemente des Amplitudenspektrums 
% (normiert!) auf dem Bildschirm mit dazupassendem Index und der Frequenz:
  disp (' Index  Ortsfrequenz / 1/mgon   Amplitude / mgon         Phase / Grad')
  for i=1:12
      str=sprintf('%5d%16.4f%26.4f%26.2f',i,freq(i),AMP(i),PZ(i));
      disp(str)
  end
  pause
  
% Im vorliegenden Beispiel: 
% größter Peak des Amplitudenspektrums bei Index 4,
% zweitgrößter Peak bei Index 6,

% Konstruktion der Korrektur-Funktion aus dem Maximal-Peak des Spektrums
% bei Index 4. Dieser Index wurde auch schon von "find" gefunden und der
% Variablen k zugeordnet. Nomierung nicht vergessen (s.o.)
  kfn1=AMP(4)*cos(2*pi*freq(4)*t + PZ(4)*pi/180);
  
% Hinweis: alternativ wäre die Nutzung von Real- und Imaginärteil möglich:
%  kfn1ri=2/(N*dt)*(real(Z(4))*cos(2*pi*freq(4)*t)-imag(Z(4))*sin(2*pi*freq(4)*t));
%  plot(t,kfn1,t,kfn1ri);
%  pause
  
% Korrektur der Messwerte
  y1c=ym-kfn1;

  figure
% plotte Daten und Korrekturfunktion KF1 (aus Maximalpeak)
  plot(t,ym,'b',t,kfn1,'r');
  xlabel('Winkel / mgon')
  ylabel('Winkeldifferenz / mgon')
  title('Beobachtungen (blau) versus Korrektur-Funktion KF1 (rot)')
  pause
  plot(t,ym,'b--','LineWidth',0.5);
  xlabel('Winkel / mgon')
  ylabel('Winkelifferenz / mgon')
  title('Rohdaten (blau) und korrigierte Beobachtungen (rot)')
  hold on
  plot(t,y1c,'m','LineWidth',1.5)
  hold off
  pause
  
% Hinzunahme des zweitgrößten Peaks (liegt bei Index 6) 
  kfn2=kfn1 + AMP(6)*cos(2*pi*freq(6)*t + PZ(6)*pi/180);
 
% Korrektur der Messwerte 
  y2c=ym-kfn2;

% plotte Daten und Korrekturfunktion KF2 (aus den zwei grössten Peaks)
  plot(t,ym,'b',t,kfn2,'r');
  xlabel('Winkel / mgon')
  ylabel('Winkeldifferenz / mgon')
  title('Mittel der Beobachtungen (blau) versus Korrektur-Funktion KF3 (rot)')
  pause
  plot(t,ym,'b--','LineWidth',0.5);
  xlabel('Winkel / mgon')
  ylabel('Winkeldifferenz / mgon')
  title('Rohdaten (blau) und korrigierte Beobachtungen (rot)')
  hold on
  plot(t,y2c,'m','LineWidth',1.5)
  hold off
  pause

%---
% Auskommentiert Mai 2012
%---
% plotte die korrigierten Original-Zeitserien in eine Graphik
%  plot(t,(yh1-mean(yh1)-kfn2),t,(yr1-mean(yr1)-kfn2),'w');
%  axis([0 40 -2.5 2.0])
%  xlabel('Winkel / mGon')
%  ylabel('Winkeldifferenz / mGon')
%  title('MATLAB Übung TKREIS, korrigierte Original-Daten Weg 1 (Hin: gelb)') 
%  pause
%  plot(t,(yh2-mean(yh2)-kfn2),t,(yr2-mean(yr2)-kfn2),'w');
%  axis([0 40 -2.5 2.0])
%  xlabel('Winkel / mGon')
%  ylabel('Winkeldifferenz / mGon')
%  title('MATLAB Übung TKREIS, korrigierte Original-Daten Weg 2 (Hin: gelb)') 
%  pause

% berechne Fourier Spektrum der korrigierten Beobachtungen
  ZC=dt*fft(y2c);

% entferne Mittelwert (erster Wert des Spektrums)
  ZC(1)=0;

% berechne das Amplituden-Spektrum
  AMPC=abs(ZC(1:(N/2)))*2/(N*dt);

% berechne das Phasen Spektrum
  PZC=atan2(imag(ZC),real(ZC))*180/pi;

  figure
% plotte das Amplituden Spektrum
  stem(freq,AMPC,'k');
  axis([0 0.9 0.0 0.35])
  xlabel('Ortsfrequenz / 1/mgon')
  ylabel('Amplitude / mGon')
  title('MATLAB Übung TKREIS, Amplituden-Spektrum korr. Daten') 
  text(0.3,7.0,[' Spektrum der korrigierten Beobachtungen'])
  disp ' enter any key to continue'
  pause
  
% end of MATLAB exercise TKREIS  