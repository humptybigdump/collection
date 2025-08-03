% MATLAB exercise DEMO2FFT
%        
% EXAMPLE FOR 2D FFT
% ==================
%           
% by Malte Westerhaus
% Geodaetisches Institut,
% Universitaet Karlsruhe,
% Englerstr. 7,
% D-76128 KARLSRUHE
% 

% schwarzer Hintergrund in Matlab6,7
%  colordef black    
  clear
  close all

% select area 1:256,1:256:
  n=256;
  
% Achsen Zeitbereich  
  xx=0:1:n-1;
  yy=0:1:n-1;
  xxs=-128:1:127;
  
% Achsen Frequnezbereich
  dx=1.0;
  dy=1.0;
  fx0=1/(n*dx);
  fy0=1/(n*dy);
  fx=-1/(2*dx):fx0:(1/(2*dx)-fx0);
  fy=-1/(2*dy):fy0:(1/(2*dy)-fy0);

% define cube (=lowpass filter) in space domain
% this definition gives a non-causal filter without phase shift !
  h0=zeros(n);
  kn=8;
  km=8;
  knkm=1./((kn+1)*(km+1));

  for a=(n/2-floor(kn/2)):(n/2+floor(kn/2))
      for b=(n/2-floor(kn/2)):(n/2+floor(kn/2))
          h0(a,b)=knkm;
      end                                  
  end
  h=ifftshift(h0);

% plot cube in space domain 
% commands have to be changed if filter 2 is plotted
  z=h0(90:167,90:167);
%  surf((xx(90:167)-n/2),(yy(90:167)-n/2),z)
%  shading faceted
%  shading flat
  mesh((xx(90:167)-n/2),(yy(90:167)-n/2),z)
%  colormap(parula)
  colormap(jet)
  view(37.5,30)
  title('2D-Rechteck im Ortsbereich');
  xlabel('x in m')
  ylabel('y in m')
  zlabel('Amplitude');
  pause

% step-wise 2D-Fourier-Transformation of cube
% (as implemented in M-File fft2)
  t1=['z(x,y)'];
  t2=['Z(fx,y)'];
  t3=['Z(fx,fy)'];

  disp ' '
  disp(t1)
  fprintf('%9.4f%9.4f%9.4f%9.4f%9.4f%9.4f%9.4f%9.4f%9.4f%9.4f%9.4f%9.4f\n',h0(123:134,123:134));
  pause
  disp ' '
  disp ' '
  disp(t2)
  H1=fftshift(fft(h,[],1));                %1-D fft of each column of h
  for i=123:134
      for j=123:134
         fprintf('%6.3f%6.3f%s',real(H1(i,j)),imag(H1(i,j)),'i')
      end
      fprintf('\n')
  end
  disp ' '
  disp ' '
  pause
  
% Plotten des Zwischenergebnisses
  figure
  mesh(fx,xxs,abs(transpose(H1)))
  shading interp
%  colormap(parula)
  colormap(jet)
  view(37.5,30)
  axis([-0.5 0.5 -128 128 0 0.15])  
  title('Zwischenergebnis: FFT der Spalten');
  xlabel('Ortsfrequenz fx in 1/m')
  ylabel('y in m')
  zlabel('Amplitude');
  pause

%  H1T=H1.';
%  H1T(1:10,1:10)
%  pause
%  HT=fft(H1T);             %transpose 1-D Spectrum H1 and 1-D fft again 
  H2=fft(H1,[],2);                  %transpose back; result H2 = f(fx,fy)
  disp(t3)
  for i=123:134
      for j=123:134
         fprintf('%6.3f%6.3f%s',real(fftshift(H2(i,j))),imag(fftshift(H2(i,j))),'i')
      end
      fprintf('\n')
  end
%  H2(1:10,1:10)
  pause

% 2D-FFT using Matlab-command fft2
  H=fft2(h);
%  Z=real(fftshift(H));      %Spectrum centered at f=0; symmetric for -N/2..N/2
  Z=abs(fftshift(H));

% plot 2-D Spectrum of cube
  figure
  surf(fx,fy,Z)
  shading flat
%  axis([-0.5 0.5 -0.5 0.5 0 1])
  colormap(jet)
  view(37.5,30)
  title('2D-FFT eines Rechtecks');
  xlabel('Ortsfrequenz fx in 1/m')
  ylabel('Ortsfrequenz fy in 1/m')
  zlabel('amplitude');
  pause
%  plot(fx,real(fftshift(H(:,1))))
  figure
  plot(fx,abs(fftshift(H(:,1))))
  title('einzelne Spalte des 2D Spektrums');
  xlabel('Ortsfrequenz fx in 1/m')
%  ylabel('Realteil');
  ylabel('Amplitude');
  pause
%  plot(xx-n/2,h0(:,128))  %Würfel in Mitte, h symmetrisch
  plot(xx-n/2,h0(:,129))  
  axis([-128 128 0 0.02])
  title('einzelne Spalte der 2D-Impulsantwort (gleitendes Mittel)');
  xlabel('x in m')
  ylabel('amplitude');
  pause

  close all
% end of MATLAB exercise DEMO2FFT