% R4: Darstellung von Phasen- und Frequenzgang des chirps sowie des 
% Amplitudenspektrums  
  figure;
  subplot(1,3,1); plot(tau.*10^6, unwrap(angle((range_chirp)))-min(unwrap(angle((range_chirp)))));  title('Unwrapped Phase'); xlabel('\tau /µs'); ylabel('Phase /rad'); grid on;
  subplot(1,3,2); plot(tau.*10^6, k.*tau.*10^(-6));  title('Frequenz'); xlabel('\tau /µs'); ylabel('Frequenz /MHz'); grid on;
  subplot(1,3,3); plot(f_rg.*10^-6,10*log10(range_chirp_fft_abs_shift));  title('Amplitudenspektrum'); xlabel('f /MHz'); ylabel('Amplitude /dB'); grid on;

% R5: Darstellung analog zu Seite 15 im Skript SAR-Grundlagen
% einschlieﬂlich Normierung des Amplitudenspektrums auf 1
  figure
  subplot(1,2,1); plot((range_chirp_fft_abs_shift/max(range_chirp_fft_abs_shift(N_rgchrp/2))),f_rg.*10^-6);  title('Amplitudenspektrum'); xlabel('Modulus'); ylabel('f /MHz'); grid on;
  subplot(1,2,2); plot(tau(1:end-1).*10^6, (diff(unwrap(angle((range_chirp))))./dtau./(2*pi)).*10^(-6));  title('Frequenz'); xlabel('\tau /µs'); ylabel('f /MHz'); grid on;
  axis([-20,20,-10,10]);

