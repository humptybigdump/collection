% MATLAB exercise GRAVCOR, status 20070522
%
% CORRELATION AND POWER SPECTRUM OF AIRPRESSURE AND GRAVITY
% ========================================================= 
%
% noch in Denglisch

% 2009: wegen schwacher Beamer auf weissen Hintergrund umgestellt
%  colordef black      

% Length of a segment for PSD estimation
  seg=512;
  nover=seg/2;  % statistics valid only for nover=0 and approx. for nover=seg/2
 
% load the recorded gravity tides, air pressure and computed gravity tides
% from file bfet19097.TID (286 days of hourly data from LaCoste earth tide
% gravimeter ET19 at station BFO Schiltach, 1991:
  load bfet1907.tid -ascii
  n=length(bfet1907);

% extract recorded gravity tides:
  grec=bfet1907(1:n,3);

% extract recorded air pressure:
  x=bfet1907(1:n,4);

% extract synthetic gravity tides:
  gsynt=bfet1907(1:n,5);
  y=grec-gsynt;
% detrend x and y:
  x=detrend(x);
  y=detrend(y);
 
% construct a vector containing the time in hours :
  dt=1;
  t=0:dt:n-1;
  
% construct a vector containing frequency in 1/hours (for segments!):
  Fs=1/dt;
  f0=1/(seg*dt);
  fny=1/(2*dt);
  f=0:f0:fny;

% Plot x-vector=air pressure :
  clf
  plot(t,x,'b')
  title('GRAVCOR: Airpressure (2nd variable in xcov)')
  xlabel('time in hours')
  ylabel('hPa')
  disp 'enter any key to continue'
  pause

% Plot y-vector:
  figure
  plot(t,y,'k')
  title('GRAVCOR: Residual gravity (1st variable in xcov)')
  xlabel('time in hours')
  ylabel('nm/s**2')
  disp 'enter any key to continue'
  pause

% Compute autocorrelation function of x-variable:
  cxx=xcov(x,'unbiased');
  varx=cxx(n);
  rxx=cxx/varx;

% Prepare for the plot of autocorrelation functions:
% we will use 20% of the maximum lag only for the plot
% 
  m=fix(0.75*n);
% Compute symmetric lag-vector (unit is in hours):
  lag=-(n-1):1:(n-1);

% Plot autocorrelation function of x-variable:
  figure
  plot(lag(m:2*n-1-m),rxx(m:2*n-1-m),'b') 
  title('Autocorrelation function of airpressure / comp. with normally distrib. var. ') 
  xlabel('time lag in hours')
  ylabel('correlation coefficient')
  grid
  disp 'enter any key to continue'
  hold on
  pause
  
% For comparison: auto correlation of a normal distributed
% data series
  xr=randn(n,1);
  rxr=xcov(xr,'unbiased')/var(xr);
  plot(lag(m:2*n-1-m),rxr(m:2*n-1-m),'r')
  hold off
  pause
 
% Comparison of 1st and 2nd half of air pressure data set;
% Autocorrelation should be similar
  n2=n/2;
  x1=x(1:n2);
  x2=x((n2+1):n);
  plot(t(1:n2),x1,'b',t(1:n2),x2,'r')
  title('airpressure: comparison 1st and 2nd half of dataset')
  xlabel('time in hours')
  ylabel('hPa')
  disp 'enter any key to continue'
  pause

  cxx1=xcov(x1,'unbiased');
  cxx2=xcov(x2,'unbiased');
  varx1=cxx1(n2);
  varx2=cxx2(n2);
  rxx1=cxx1/varx1;
  rxx2=cxx2/varx2;
  m2=fix(0.75*n2);
% Compute symmetric lag-vector (unit is in hours):
  lag2=-(n2-1):1:(n2-1);

% Plot autocorrelation function of x-variable:
  figure
  plot(lag2(m2:2*n2-1-m2),rxx1(m2:2*n2-1-m2),'b',lag2(m2:2*n2-1-m2),rxx2(m2:2*n2-1-m2),'r') 
  title('Autocorrelation functions of airpressure, 1st and 2nd half of dataset') 
  xlabel('time lag in hours')
  ylabel('correlation coefficient')
  grid
  disp 'enter any key to continue'
  pause

% Compute autocorrelation function of y-variable:
  cyy=xcov(y,'unbiased');
  vary=cyy(n);
  ryy=cyy/vary;

% Plot autocorrelation function of y-variable:
  figure
  plot(lag(m:2*n-1-m),ryy(m:2*n-1-m),'k')
  title('GRAVCOR: Autocorrelation function of gravity')
  xlabel('time lag in hours')
  ylabel('correlation coefficient')
  grid
  disp 'enter any key to continue'
  pause

% Compute cross-correlation function:
  cxy=xcov(y,x,'unbiased');
  rxy=cxy/sqrt(varx*vary);        %hier wie Korr.koeff, siehe Bronstein, S.801
%  rxy=cxy/cxy(n);                %hier rxy(0)=1
% Plot cross-correlation function of x- and y-variables: 
  plot(lag(m:2*n-1-m),rxy(m:2*n-1-m),'m')
  title('GRAVCOR: Crosscorrelation function of airpressure and gravity')
  xlabel('time lag in hours')
  ylabel('correlation coefficient')
  grid
  disp 'enter any key to continue'
  pause

% Compute power spectral densities of x- and y-series
% using the Welch method:
  ww=hanning(seg);                 %see below
  Pxx=pwelch(x,ww,nover,[],Fs);
  Pyy=pwelch(y,ww,nover,[],Fs);
  Pxy=cpsd(x,y,ww,nover,[],Fs);
  Cxy=mscohere(x,y,ww,nover,[],Fs);
  Txy=tfestimate(x,y,ww,nover,[],Fs);

% ----------------------------------------------------------------
% Bemerkung zu PWELCH 

% Achtung: das Defaultfenster Hamming in pwelch geht an den Enden
% eines Segmentes nicht ganz auf Null; dadurch gibt es
% (unterschiedlich hohe) Jumps am Anfang und Ende eines
% Segmentes. Dies erhöht das Rauschniveau der Schwere ganz
% erheblich. Krass: der Test mit Rechteckfenster: das geht
% gar nicht!! Erhöhung des Rauschens vor allem im hochfrequenten
% Bereich. Der Befehl Spektrum aus Matlab 4 hat Default mäßig
% ein Hanningfenster verwendet => niedriges Rauschniveau.

% Korrekturfaktor wg. Verwendung eines Fensters (s. Bendat&Piersol)
% wird in Subroutine 'computeperiodogram.m' unter
% C:\Programme\MATLAB71\toolbox\signal\signal\private
% direkt aus der übergebenen Fensterfunktion ermittelt und an den 
% Leistungsspektren angebracht.

% guckst du hier: voll der krasse Test mit Rechteckfenster
%  wr=1:1:seg;
%  TxyR=tfestimate(x,y,wr,nover,[],Fs);

% Und hier ersmal: Test2, affengeil, bringt aber ganix
%  tt=0:dt:seg-1;
%  Pysm=zeros((seg/2+1),1);
%  for i=1:6
%    ys=y((seg*i-seg+1):(seg*i));
%    ys=ys-mean(ys);
%    Pys=pwelch(ys,ww,0,[],Fs);
%    Pysm=Pysm+Pys;
%   end
%  Pysm=Pysm/i;
%  loglog(f,Pyy,f,Pysm);
%  pause
%-------------------------------------------------------------------

% Plot of Power Spectral Densities (PSD)
  figure
  loglog(f,Pxx,'b')
%  grid
  title('GRAVCOR: Pyy - Leistungsspektrum Luftdruck'), ...
  xlabel('Frequenz / 1/Stunde')
  ylabel('hPa**2 pro 1/Stunde')
  pause
%
  loglog(f,Pyy,'k')
  title('GRAVCOR: Pxx - Leistungsspektrum Schwere'), ...
  xlabel('Frequenz / 1/Stunde')
  ylabel('(nm/s**2)**2  pro 1/Stunde')
  hold on
  pause
  loglog(f,Pxx,'b')
  pause
  hold off
  
  figure
  loglog(f,abs(Pxy),'r');...
  title('GRAVCOR: Pxy - Kreuzleistungsspektrum')
  xlabel('Frequenz / 1/Stunde')
  ylabel('hPa*nm/s**2 pro 1/Stunde')
  pause
%
  semilogx(f,Cxy,'m'), ...
  title('GRAVCOR: xy - spektrale Kohärenz'), ...
  xlabel('Frequenz / 1/Stunde')
  pause

% Calculation of 95%-Confidence-Limits using Chi-Square-Distribution
% nu: Number of degrees of freedom, each spectrum adds 2 degrees
  if nover==0
     nu=fix(2*n/seg);      
  else
     nu=fix(2*2*n/seg-1);   
  end

% 0.975: 97,5 percent of data above this limit
% 0.025:  2,5 percent of data above this limit
% Attention: alpha (0.975 <-> 0.025) interchanged comparing
% table Chi2 in Bendat/Piersol ?!
  chihigh=chi2inv(0.025,nu);
  chilow=chi2inv(0.975,nu);
  
% Confidence Interval for x-variable
  clow=nu*Pxx/chilow;
  chigh=nu*Pxx/chihigh;
  figure
  loglog(f,Pxx,'b',f,clow,'--m',f,chigh,'--c');
  title('Pxx - X Power Spectral Density with 95% confidence limits'), ...
  xlabel('Frequenz / 1/Stunde')
  ylabel('hPa**2  pro 1/Stunde')
  pause
  
  clear clow
  clear chigh
% Confidence Interval for y-variable
  clow=nu*Pyy/chilow;
  chigh=nu*Pyy/chihigh;
  loglog(f,Pyy,'k',f,clow,'--m',f,chigh,'--c');
  title('Pyy - Y Power Spectral Density with 95% confidence limits'), ...
  xlabel('cycle per hour')
  ylabel('(nm/s**2)**2 per cph')
  pause
    
% Standarddeviation of Transferfunction
% remark: the error estimate is strictly speaking only valid for
% a box car window and no overlap. Acc. to Bendat & Piersol
% (pp. 398), Hanning windowing increases the relativ error of
% the PSD by sqrt(2); 90% of this increase is retrieved by
% a 50% overlap. Don't know if this holds also for the Transfer
% function, but it is assumed here (as in many papers using
% the Bendat/Piersol equations)

  k=fix(length(x-nover)/(seg-nover));
  e=sqrt(1-Cxy)./(sqrt(Cxy)*sqrt(2*k));
  ab=abs(Txy);
  st=ab.*e;
  ph=unwrap(angle(Txy));
  plot(f(20:80),ab(20:80),'b');
  hold on
  errorbar(f(20:80),ab(20:80),st(20:80),'ok'), ...
%  grid
%  axis([0.01 0.08 1 5]);
  title('GRAVCOR: Txy - Transfer function magnitude'), ...
  xlabel('cycle per hour')
  ylabel('nm/s**2 per hPa')
  hold off
  pause

  plot(f(20:80),180/pi*ph(20:80),'r');
  hold on
  errorbar(f(20:80),180/pi*ph(20:80),180/pi*e(20:80),'ok'), ...  
%  grid
%  axis([0.01 0.07 140 220]);
  title('GRAVCOR: Txy - Transfer function phase'), ...
  xlabel('cycle per hour'), ...
  ylabel('Degrees')
  hold off
  pause
 
% Correction of gravity due to air pressure variations
  mf=mean(ab(20:80));
  yc=y+mf*x;
  PC=pwelch(yc,ww,0,[],Fs);;

  loglog(f,Pyy,'k')
  title('GRAVCOR: Corrected Power Spectral Density'), ...
  xlabel('cycle per hour')
  ylabel('nm/s**2 per cph')
  pause
  hold on
  loglog(f,PC,'r')
  pause
  hold off

  
% End MATLAB exercise GRAVCOR