  clear
  close all

%% constants and parameters
  c = 3*10^8;                 % velocity of light [c] = m/s
  v = 7098.0194;              % velocity of satllite [v] = m/s

%% Sensorparameters, taken from file-header of the ERS-scene
% Range Chirp
  lambda=0.05656;             % wavelength of carrier wave
  fs=18.962468*10^6;          % sampling Frequency
  dtau = 1/fs;                % sampling period
  k=(4.18989015*10^(11));     % FM-rate range chirp [k] = Hz/s = 1/(s^2)
  tau_p=37.12*10^(-6);        % chirp length [tau_p] = s

% Azimut Chirp
  ta=0.627;                   % aperture time (theor.: theta_a*r0/v=lambda/D_a*r0/v = 0.627s )
  prf=1679.902;               % pulse repitition frequency (theor. azimuth bandwidth W_a=2*v/L=1420Hz)
% Test image                  % for simplicity a constant range distance r0 can be used
  r0=852358.15;               % range to middle of the scene [r0] = m
% ERS-scene                   % use variable range distance
  r_near_far = r0 + ([-1023:1:1024])*1/(2*fs)*3e8;
                              
%% Loading and plotting of data scene
  raw = load('test_image.mat'); 
  raw_image = raw.img_az_rg_conv;
  
  [N_az,N_rg] = size(raw_image);
  
  figure
  imagesc(abs(raw_image));
  colormap('gray')
  title('Raw_image')
  xlabel('range')
  ylabel('azimuth')
  
 

%% creating the (complex!) range chirp 

  tau=-tau_p/2:dtau:tau_p/2;
  range_chirp=exp(i*pi*k.*tau.^2);

% steps R1-R7 are only for plotting the chirp signal, they are not use
% for the actual filtering (see section "range focusing" below)

% R1: Plotting the chirp-signal
  figure;
  subplot(3,1,1); plot(tau.*10^6, real(range_chirp));  title('Real Range Chirp'); xlabel('\tau /탎'); ylabel('Amplitude');
  subplot(3,1,2); plot(tau.*10^6, imag(range_chirp),'-r');  title('Imag Range Chirp'); xlabel('\tau /탎'); ylabel('Amplitude');
  subplot(3,1,3); plot(tau.*10^6,(angle((range_chirp)))); title('Wrapped Phase'); xlabel('\tau /탎'); ylabel('Phase /rad');
  
% R2: calculation of the Fourier spectrum of the chirp
  range_chirp_fft=fft(ifftshift(range_chirp));


% R3: Calculation of amplitude spectrum
  range_chirp_fft_ab=abs(range_chirp_fft);


% R4: calculation of frequency axis
  N_rgchrp=numel(range_chirp);
  fny=1/(2*dtau);
  df=1/(N_rgchrp*dtau);
  f_rg=-fny:df:fny-df;


% R5: for the graph: re-sorting the amplitude spectrum with fftshift
  range_chirp_fft_abs=fftshift(range_chirp_fft_ab);


% R6: represention of unwrapped phase- und frequency of the chirp and plot 
% of the amplitude spectrum (normalized to 1)  
  figure;
  subplot(1,3,1); plot(tau.*10^6, unwrap(angle(range_chirp))-min(unwrap(angle(range_chirp))));  title('Unwrapped Phase'); xlabel('\tau /탎'); ylabel('Phase /rad'); grid on;
  subplot(1,3,2); plot(tau.*10^6, k.*tau.*10^(-6));  title('Frequenzy'); xlabel('\tau /탎'); ylabel('Frequenzy /MHz'); grid on;
  subplot(1,3,3); plot(f_rg.*10^-6,10*log10(range_chirp_fft_abs/max(range_chirp_fft_abs(N_rgchrp/2))));  title('Amplitude spectrum'); xlabel('f /MHz'); ylabel('Amplitude /dB'); grid on;
  
% R7: representation acc. to page 15 in the script SAR basics and normalization 
% (SAR-Grundlagen und Normierung) including normalization of the 
% amplitude spectrum to 1
  figure 
  subplot(1,2,1); plot((range_chirp_fft_abs/max(range_chirp_fft_abs(N_rgchrp/2))),f_rg.*10^-6);  title('Amplitude spectrum'); xlabel('Modulus'); ylabel('f /MHz'); grid on;
  subplot(1,2,2); plot(tau(1:end-1).*10^6, (diff(unwrap(angle((range_chirp))))./dtau./(2*pi)).*10^(-6));  title('Frequenzy'); xlabel('\tau /탎'); ylabel('f /MHz'); grid on;
  axis([-20,20,-10,10]);

%% creatig the azimut chirp

%% range focusing (~compression, ~deconvolution)

% S1:
% Bring range chirp to the same (time) length as raw data matrix, i.e. fill
% up with zeros, since no signal is sent
% Zeropadding is done symmetrically along the horizontal dimension 
% (since range_chirp is a row vector.)
  N_rgpad=(N_rg-N_rgchrp)/2; 
  range_chirp_ref=padarray(range_chirp,[0 N_rgpad]); 

% S2: FFT of the range_reference_function
  f_range_chirp_ref = fft(ifftshift(range_chirp_ref));

% S3: taking the complex conjugate 
  fcon_range_chirp_ref = conj(f_range_chirp_ref);

% S4: duplicate the range reference function for all azinuth times
  fcon_range_chirp_ref = repmat(fcon_range_chirp_ref,[N_az,1]);

% S5: FFT of the image
  fft_raw_image = fft(raw_image,[],2);

% S6: correlation and iFFT
  fft_raw_image_rg_focussed=fft_raw_image.*fcon_range_chirp_ref;     %Deconvolutio
  img_rg_focussed = ifft(fft_raw_image_rg_focussed,[],2);
  
  figure
  imagesc(abs(img_rg_focussed))
  colormap('gray')
  pause



%% azimut focusing
% Notes and hints: 
% 1. azimuth-focusing is done for each column of the image matix (in the
% direction of dimension 1). To do this, row-vectors (as e.g. reference functions) 
% have to be transposed. Attention: using the apostrophe ' for transposing
% changes the sign of the imaginary part! Better use .' or the MATLAB
% command transpose.
%
% 2. when focusing the ERS scene: the frequency modulation rate (FM) for 
% the azimuth chirp changes acc. to r_near_far resulting in a FM-vector. It
% should enter the expression to calculate the chirp as a column vector. 
% The command padarry works as before, repmat is not necessary in this case.


%% Multi-Looking