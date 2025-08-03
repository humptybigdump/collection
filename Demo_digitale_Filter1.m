% MATLAB exercise DIGFILT, status 20040630
%        
% DESIGN AND APPLICATION OF A DIGITAL FILTER
% ==========================================
%
% 
% hier impulsantwort z(t) gleich über Zeitvektor tt (1025 Werte) berechnet; 
% Peak auf z(513); das gibt bei fftshift aber immer noch Phasenverschiebung (1h)
% hier Umsortierung mit ifftshift - dann stimmt es; schnöpft nicht mit Matlab4
% Vergleich mit Konvolution rausgenommen

% schwarzer Hintergrund in Matlab6,7
%  colordef black        
  clear
  close all
  
% Definition Zeit- Frequenzachsen:  
  n=1024;
  n2=n/2;
  tt=-n2:1:n2;        
  t=0:1:n;
  df=1/(n+1);
  f=0:df:1.0-df;
  fshft=-0.5:df:0.5-df;
  
% Start- und Stoppfrequenz des Filters
  fa=0.025;
  fe=0.055;
%  fa=0.01;
%  fe=0.3;

% Impulsantwort eines symmetrischen, phasentreuen Bandpass (Buttkus, (14.7))
  z=2*sin(2*pi*(fe-fa)*0.5.*tt).*cos(2*pi*0.5*(fe+fa).*tt)./(pi*tt);
  z(513)=0.06;
%  z(513)=0.58;

% Mit Matlab-Befehl FIR1
  Wn=[2*fa 2*fe];
  B=fir1(n,Wn,hanning(n+1));

% Zunächst Plot über die Zeitachse 
  plot(tt,z);
  xlabel('Zeit / Stunden')
  ylabel('Impulsantwort h(t)')
  title('DIGFILT1: Impulsantwort idealer Bandpass, FIR1')
  axis([-512,512,-0.06,0.06]);
  pause
  figure
  plot(t,z);
  xlabel('aufgetragen über Index; entspricht dt = 1 Stunde')
  ylabel('Impulsantwort h(t)')
  title('DIGFILT1: Impulsantwort idealer Bandpass, FIR1')
  axis([0,1025,-0.06,0.06]);
  pause

% Test des Effektes einer nicht umsortierter Impulsantwort  
%  ibs=B;
%  ihs=z;

  figure
  plot(t,ifftshift(z),'b')
  xlabel('Zeit / Stunden')
  ylabel('Impulsantwort h(t)')
  title('DIGFILT1: Impulsantwort idealer Bandpass, umsortiert für FFT')
  axis([0,1025,-0.06,0.06]);
  text(250,0.04,['Umsortieren mit Matlab-Befehl "ifftshift"'])
  pause
  
% Hanning-Window und FFT
  w=hanning(n+1);
  z=z.*w';

% Umsortierung für FFT-Methode
  ibs=ifftshift(B);
  ihs=ifftshift(z);
  
  HH=fft(ihs);
  BB=fft(ibs);
  hhamp=abs(HH);
  bbamp=abs(BB);
  bphas=unwrap(atan2(imag(BB),real(BB)));

%  semilogy(f,hhamp,'k',f,bbamp,'b');
  plot(f,hhamp,'k',f,bbamp,'b');
  xlabel('Frequenz / 1/Stunden')
  ylabel('Übertragungsfunktion H(f)')
  title('Übertragungsfunktion idealer Bandpass, "MATLAB-Symmetrie"')
  pause

% Zum Vergleich mal ein Plot mit linearer Y-Achse und Symmetrie bzgl f=0
  plot(fshft,fftshift(bbamp),'b');
  xlabel('Frequenz / 1/Stunden')
  ylabel('Übertragungsfunktion H(f)')
  title('Übertragungsfunktion idealer Bandpass, symmetrisch bzgl. f=0')
  text(-0.25,1.2,['Umsortieren mit Matlab-Befehl "fftshift"'])
  pause
  
  figure
  [AX,H1,H2]=plotyy(f(15:70),bphas(15:70),f(15:70),bbamp(15:70));
  xlabel('Frequenz / 1/Stunden')
  ylabel(AX(1),'Phase')
  ylabel(AX(2),'Gain')
  title('Übertragungsfunktion (Phase) idealer Bandpass, "MATLAB-Symmetrie"')
  pause

% Anwendung des Filters:
% Berechnung eines verrauschten deterministischen Signales durch
% Addition einer pseudo-random Signales mit Standardabweichung 1.0
% auf eine Sinuswelle mit Amplitude 1 und einer Periode von 25 Stunden

  xr=randn(n+1,1);
  per=25;
  xd=1.0*sin(2*pi*t/per);
  x=xr'+xd;

% Darstellung der ersten np Stunden:
  np=100;
  plot(t(1:np),x(1:np),t(1:np),xd(1:np))
  xlabel('Zeit in Stunden')
  ylabel('meter')
  title('verrauschte Original Zeitreihe')
  disp 'enter any key to continue'
  pause

% Filterung per Fourier-Transformation
  X=fft(x);
  Y=HH.*X;
  yf=ifft(Y);
  plot(t(n2:n2+100),x(n2:n2+100),t(n2:n2+100),xd(n2:n2+100),t(n2:n2+100),yf(n2:n2+100))
%  plot(t(1:100),x(1:100),t(1:100),yf(1:100),t(1:100),xd(1:100))
  xlabel('Zeit in Stunden')
  ylabel('meter')
  title('verrauschte, gefilterte (rot) und Original-Zeitreihe (schwarz)')
  pause

% end of MATLAB exercise DIGFILT
