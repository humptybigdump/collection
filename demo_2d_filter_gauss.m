% MATLAB exercise MYIMAGE, status 20020703
%        
% IMAGE PROCESSING USING 2D FFT
% =============================
%
% Lecture SPECTRAL ANALYSIS AND DIGITAL FILTERING           
% 
% modified by Malte Westerhaus, 03.07.2002/30.06.06 
%
% Definition akausales Tiefpassfilter (gleitendes Mittel) im ZEITBEREICH
% !!! hier: gleitendes Mittel im Zeitbereich !!!!
% !!! Umstellung: Plotten der Spektren mit mesh anstatt mit image !!!!
%
% Filterung mit pasentreuem (kausalem) Filter und korrekter Normierung.
% Alle Spektren werden zweiseitig (-fny -> fny bzw. 0 -> 2fny) geplottet
% ===================================================================
%

% schwarzer Hintergrund in Matlab6,7
  clear
  close all
  
% Default Farbskalierung der Spektren
  cmin=-20;
  cmax=10;
  
% Einlesen des Bildes:
%  Z=double(imread('Aulabau_Lageplan_von_Kit_webseite.jpg','jpg'));
  Z=double(imread('trump1068.jpg','jpg'));

% Ausschnitt wählen 1:256,1:256:
  n=1024;
  n2=ceil(n/2);
  xx=0:1:n-1;
  yy=0:1:n-1;
  xv=-n/2:1:n/2-1;
  yv=-n/2:1:n/2-1;
  X=Z(45:1068,45:1068);   % Ausschnitt wählen
  X(1:10,1:10)
  
% Parameter, die für die gleichartige Skalierung der Bilder bebraucht werden
  xmean=mean(mean(X));  
  xmax=max(max(X));
  scal=65;           %empirischer Parameter für Bildskalierung

% Plotten des Bildes:
  figure
  pos = get(gcf,'pos');
  set(gcf,'pos',[20 20 1200  900])
  image(scal*X/xmax);
  colormap(gray)
%  axis equal
  axis off
  title('Der Präsident')
  pause

% Impulsantwort symmetrisches Tiefpassfilter ("Matlab"-Symmetrie).
% Dabei wird Periodizität der diskreten Zeitreihen/Spektren
% mit N ausgenutzt.
% Normierung der Filterverstärkung (Gain) auf 1
  kn=10;
%  kn=mean(mean(std(X)));     % Std.abweichung Gaussfilter <=> Länge des Filters ??
  hsym=d2gauss(n,kn,n,kn,0); 
  h=ifftshift(hsym);         % ==> "Matlab-Symmetrie"
  h=h/(2*kn*sqrt(pi));       % Normierung => Filtergain (freq) = 1

% plotten der Impuls-(Gewichts-)funktion des noch verschobenen Filters
  figure
%  mesh(xx(1:n),yy(1:n),hsym)
  mesh(xv(1:n),yv(1:n),hsym)
  colormap(cool)
  view(37.5,30)
  axis([-n/2 n/2-1 -n/2 n/2-1 0 max(max(hsym))])
  title('Impulsantwort des Tiefpassfilters');
  xlabel('x-Koordinate')
  ylabel('y-Koordinate')
  zlabel('Amplitude');
  pause
  
% plotten der Impuls-(Gewichts-)funktion des Filters
  figure
  z=h(1:n,1:n);
  mesh(xx(1:n),yy(1:n),z)
  colormap(cool)
  view(37.5,30)
  axis([1 n 1 n 0 max(max(z))])
  title('Impulsantwort des Tiefpassfilters');
  xlabel('x-Koordinate')
  ylabel('y-Koordinate')
  zlabel('Amplitude');
  pause

% Berechnung der Frequenz-Antwort-funktion des Filters
% mittels 2-D FFT
  H=fft2(h);

% Amplitude (Gain) der Frequenz-Antwort-funktion H
  Z2=(abs(H(1:n,1:n)));

% Berechnung der Frequenz- (besser: Ortsfrequenz)-achsen
  dx=1.0;
  dy=1.0;
  fx0=1/(n*dx);
  fy0=1/(n*dy);

% korrekte Achse für Matlab-Symmetrie
  fx2=0:fx0:(1/dx-fx0);
  fy2=0:fy0:(1/dy-fy0);

% nur für Darstellungszwecke: Frequenzachse von -fny bis fny
  fx2d=-1/(2*dx):fx0:(1/(2*dx)-fx0);
  fy2d=-1/(2*dy):fy0:(1/(2*dy)-fy0);

% um die Symmetrie des Filters zu zeigen, zunächst
% ein Plot von -fny bis fny
  figure
  Z2d=fftshift(Z2);
  axis([-0.5,0.5, -0.5, 0.5, 0, 1])
  subplot(1,2,1), mesh(fx2d,fy2d,Z2d);
  view(37.5,30)
  title('Übertragungsfunktion des symmetrischen Tiefpassfilters');
  xlabel('Ortsfrequenz fx')
  ylabel('Ortsfrequenz fy')
  zlabel('Amplitude (gain)');
%  pause

% jetzt plotten in Matlab Symmetrie
  subplot(1,2,2), mesh(fx2,fy2,Z2);
  view(37.5,30)
  axis([0.0,1.0, 0.0, 1.0, 0, 1])
  pos = get(gcf,'pos');
  set(gcf,'pos',[2 550 1400  420])
  title('Übertragungsfunktion des "Matlab"-symmetrischen Tp-Filters');
  xlabel('Ortsfrequenz fx')
  ylabel('Ortsfrequenz fy')
  zlabel('Amplitude (gain)');
  pause

% das komplementäre Hochpassfilter kann einfach als 1 - Tiefpass
% definiert werden
  figure
  ZZ2=1-Z2;
  mesh(fx2,fy2,ZZ2);
%  colormap(jet)
  view(37.5,30)
  title('Übertragungsfunktion eines Hochpass-Filters');
  xlabel('Ortsfrequenz fx')
  ylabel('Ortsfrequenz fy')
  zlabel('Amplitude (gain)');
  pause

% 2-D Darstellung des Tiefpassfilters (Amplitude)
%  image(128*Z2)  dies ist Variante image ohne Frequenz-Achsen
%  axis off
%  axis equal
%  figure
%  mesh(fx2,fy2,Z2);
%  view(0,-90)
%  axis square
%  shading interp
%  grid off
%  title('2D-Darstellung Tiefpassfilter')
%  xlabel('Ortsfrequenz fx')
%  ylabel('Ortsfrequenz fy')
%  colorbar
%  pause

% Berechnung der 2D-fft des Bildes
  Y=fft2(X);

% 3D-Darstellung Amplitudenspektrum des Bildes:
%  colormap(gray)
%  A=log10(abs(Y(1:n,1:n)));
%  mesh(fx2(1:n2),fy2(1:n2),A(1:n2,1:n2));
%  view(37.5,30)
%  axis([0,0.5,0,0.5,0,max(max(A))]);
% ,'position',[20,20,1000,300])
  figure
  A=log10(abs(Y(1:n,1:n)));
  subplot(1,2,1),  mesh(fx2d,fy2d,fftshift(A));
  view(37.5,30)
  axis([-0.5,0.5,-0.5,0.5,-20,10]);
  caxis([cmin,cmax]);

 % colormap(parula)
  xlabel('Ortsfrequenz fx')
  ylabel('Ortsfrequenz fy')
  zlabel('log. Amplitude');
  title('Der Präsident im Spektralbereich (2D log Amplitudenspektrum)')
  
  subplot(1,2,2), mesh(fx2,fy2,A);
  pos = get(gcf,'pos');
  set(gcf,'pos',[2 300 1400  420])
  view(37.5,30)
  axis([0,1.0,0,1.0,-20,10]);
  caxis([cmin,cmax]);
  xlabel('Ortsfrequenz fx')
  ylabel('Ortsfrequenz fy')
  zlabel('log. Amplitude');
  title('MATLAB-Symmtrie')
  pause
  
% Tiefpassfilterung des Bildes:
  LPF=H.*Y;
  
% gefiltertes 2D Amplitudenspektrum des Bildes
%  colormap(gray)
  figure
  TP=log10(abs(LPF(1:n,1:n)));
  subplot(1,2,1), mesh(fx2d,fy2d,fftshift(TP));
  view(37.5,30)
  axis([-0.5,0.5,-0.5,0.5,-20,10]);
  caxis([cmin,cmax]);
  xlabel('Ortsfrequenz fx')
  ylabel('Ortsfrequenz fy')
  zlabel('log. Amplitude');
  title('2D Tiefpass-gefiltertes Amplitudenspektrum des Originalbildes')

  subplot(1,2,2), mesh(fx2,fy2,TP);
  pos = get(gcf,'pos');
  set(gcf,'pos',[2 50 1400  420])
  view(37.5,30)
  axis([0,1.0,0,1.0,-20,10]);
  caxis([cmin,cmax]);
  xlabel('Ortsfrequenz fx')
  ylabel('Ortsfrequenz fy')
  zlabel('log. Amplitude');
  title('Tiefpass-gefiltertes Spektrum in MATLAB-Symmtrie')
  pause 

% Hochpass-Filterung des Bildes und Plot
  figure
  HPF=(1-H).*Y;  
%  colormap(gray)
  HP=log10(abs(HPF(1:n,1:n)));
  subplot(1,2,1), mesh(fx2d,fy2d,fftshift(HP));
  view(37.5,30)
  axis([-0.5,0.5,-0.5,0.5,-20,10]);
  caxis([cmin,cmax]);
  xlabel('Ortsfrequenz fx')
  ylabel('Ortsfrequenz fy')
  zlabel('log. Amplitude');
  title('2D Hochpass-gefiltertes Amplitudenspektrum des Originalbildes')
  
  subplot(1,2,2), mesh(fx2,fy2,HP);
  pos = get(gcf,'pos');
  set(gcf,'pos',[2 450 1400  420])
  view(37.5,30)
  axis([0,1.0,0,1.0,-20,10]);
  caxis([cmin,cmax]);
  xlabel('Ortsfrequenz fx')
  ylabel('Ortsfrequenz fy')
  zlabel('log. Amplitude');
  title('Hochpass-gefiltertes Spektrum in MATLAB-Symmtrie')
  pause 
 
  
% inverse Fouriertransformation
  lpf=ifft2(LPF);

% wg. numerischer Effekte (u.a. Fenstereffekt) enthält das 
% tiefpassgefilterte Bild auch Imaginäranteile, die im
% folgenden vernachlässigt werden.
  lpfr=real(lpf);    %=tiefpassgefiltertes Originalbild

% Darstellung des Tp-gefilterten Bildes
  figure
  pos = get(gcf,'pos');
  set(gcf,'pos',[20 20 1200  900])
  image(scal*lpfr/xmax);
  colormap(gray)
  axis off
%  axis equal
  title('Tiefpass-gefilterter Präsident')
  pause

% Rücktransformation des hochpassgefilterten Bildes    
  hpf=ifft2(HPF);
  hpfr=real(hpf);

  figure
  pos = get(gcf,'pos');
  set(gcf,'pos',[20 20 1200  900])
  image((scal+60)*(hpfr+xmean)/xmax)  %+xmean, da der Mittelwert des Bildes durch Hochpass rausgefiltert
  colormap(gray)
  axis off
%  axis equal
  title('Hochpass-gefilterter Präsident')
  disp 'enter any key to continue'
  pause

%  close all
% end of MATLAB exercise MYIMAGE