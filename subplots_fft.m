% R6: representaion Darstellung of phase- und frequency of the chirp and 
% plot of the amplitude spectrum  
  figure;
  subplot(1,3,1); plot(tau.*10^6, unwrap(angle(range_chirp))-min(unwrap(angle(range_chirp))));  title('Unwrapped Phase'); xlabel('\tau /µs'); ylabel('Phase /rad'); grid on;
  subplot(1,3,2); plot(tau.*10^6, k.*tau.*10^(-6));  title('Frequenzy'); xlabel('\tau /µs'); ylabel('Frequenzy /MHz'); grid on;
  subplot(1,3,3); plot(f_rg.*10^-6,10*log10(range_chirp_fft_abs/max(range_chirp_fft_abs(N_rgchrp/2))));  title('Amplitude spectrum'); xlabel('f /MHz'); ylabel('Amplitude /dB'); grid on;
  
% R7: representation acc. to page 15 in the script SAR basics and normalization 
% (SAR-Grundlagen und Normierung) including normalization of the 
% amplitude spectrum to 1
  figure 
  subplot(1,2,1); plot((range_chirp_fft_abs/max(range_chirp_fft_abs(N_rgchrp/2))),f_rg.*10^-6);  title('Amplitude spectrum'); xlabel('Modulus'); ylabel('f /MHz'); grid on;
  subplot(1,2,2); plot(tau(1:end-1).*10^6, (diff(unwrap(angle((range_chirp))))./dtau./(2*pi)).*10^(-6));  title('Frequenzy'); xlabel('\tau /µs'); ylabel('f /MHz'); grid on;
  axis([-20,20,-10,10]);

