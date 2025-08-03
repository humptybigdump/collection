% MATLAB Script to Demonstrate Discrete Fourier Transformation
% Clear previous data
clear; clc; close all;

% Define sampling parameters
Fs = 64; % Sampling frequency (Hz), to cover frequencies up to 10 rad/s
T = 1/Fs; % Sampling period
T_total = 30; % Total time in seconds for desired frequency resolution
L = T_total * Fs; % Total number of samples

% Define frequency vector with desired resolution in rad/s
f_rad = (0:L/2-1) * (2 * pi / T_total); % Frequency vector in rad/s

%% 1. Gaussian Bell-Shaped Signal (Normal Distribution)
% For Gaussian, we use a time vector centered around zero to approximate -∞ to ∞
t_gauss = (-L/2:L/2-1)*T; % Time vector for Gaussian centered around zero
sigma = 10; % Standard deviation for Gaussian (variance is sigma^2)
gauss_bell = exp(-t_gauss.^2 / (2*sigma^2));
gauss_bell = gauss_bell / max(gauss_bell); % Normalize to 1

% Plot Gaussian in Time Domain
figure;
subplot(4,2,1);
plot(t_gauss, gauss_bell);
title('Gaussian Bell-Shaped Signal (Time Domain)');
xlabel('Time (s)');
ylabel('Amplitude');

% FFT of Gaussian Bell-Shaped Signal
Y1 = fftshift(fft(gauss_bell)); % Shift zero frequency to center
P1 = abs(Y1)/L; % Amplitude spectrum

% Plot Gaussian FFT with Frequency Axis Limited to [0, 10] rad/s
subplot(4,2,2);
plot(f_rad(f_rad <= 10), P1(L/2+1:L/2+length(f_rad(f_rad <= 10))));
title('Gaussian Bell-Shaped Signal (Frequency Domain)');
xlabel('Frequency (rad/s)');
ylabel('|P1(\omega)|');
xlim([0, 10]);

%% 2. Polyharmonic Oscillation (Incommensurable Frequencies around 1 rad/s)
t = (0:L-1)*T; % Time vector from 0 to positive time
f1 = 1; % Frequency of first component in rad/s
f2 = sqrt(2); % Frequency of second component (incommensurable)
f3 = sqrt(3); % Frequency of third component (incommensurable)
polyharmonic = sin(f1 * t) + sin(f2 * t) + sin(f3 * t);

% Plot Polyharmonic in Time Domain
subplot(4,2,3);
plot(t, polyharmonic);
title('Polyharmonic Oscillation (Time Domain)');
xlabel('Time (s)');
ylabel('Amplitude');

% FFT of Polyharmonic Oscillation
Y2 = fft(polyharmonic);
P2 = abs(Y2(1:L/2))/L;

% Plot Polyharmonic FFT with Frequency Axis Limited to [0, 10] rad/s
subplot(4,2,4);
plot(f_rad(f_rad <= 10), P2(f_rad <= 10));
title('Polyharmonic Oscillation (Frequency Domain)');
xlabel('Frequency (rad/s)');
ylabel('|P2(\omega)|');
xlim([0, 10]);

%% 3. Damped Oscillator with Harmonic Force (Analytical Amplitude)
f3 = 1; % Frequency of the forcing in rad/s
zeta = 0.05; % Damping ratio
omega_n = 1; % Natural frequency in rad/s
damped_oscillator = exp(-zeta*omega_n*t) .* sin(f3 * t);

% Plot Damped Oscillator in Time Domain
subplot(4,2,5);
plot(t, damped_oscillator);
title('Damped Oscillator with Harmonic Force (Time Domain)');
xlabel('Time (s)');
ylabel('Amplitude');

% FFT of Damped Oscillator
Y3 = fft(damped_oscillator);
P3 = abs(Y3(1:L/2))/L;

% Analytical Fourier Transform of Damped Oscillator
b = zeta * omega_n; % Damping factor
lambda = omega_n; % Angular frequency
analytic_damped = (1/sqrt(2*pi)) * abs((b + 1j*f_rad) ./ ((b + 1j*f_rad).^2 + lambda^2));

% Plot Damped Oscillator FFT with Analytical Amplitude Spectrum
subplot(4,2,6);
plot(f_rad(f_rad <= 10), P3(f_rad <= 10), 'b', 'DisplayName', 'FFT Amplitude');
hold on;
%plot(f_rad(f_rad <= 10), analytic_damped(f_rad <= 10), 'r--', 'DisplayName', 'Analytical Amplitude');
title('Damped Oscillator with Harmonic Force (Frequency Domain)');
xlabel('Frequency (rad/s)');
ylabel('|P3(\omega)|');
legend;
xlim([0, 10]);

%% 4. Stochastically Excited Harmonic Oscillator with More Noise
f4 = 1; % Natural frequency of oscillator in rad/s
sigma_noise = 10; % Noise level
stochastic_excitation = sin(f4 * t) + sigma_noise * randn(size(t));

% Plot Stochastic Oscillator in Time Domain
subplot(4,2,7);
plot(t, stochastic_excitation);
title('Stochastically Excited Harmonic Oscillator (Time Domain)');
xlabel('Time (s)');
ylabel('Amplitude');

% FFT of Stochastically Excited Harmonic Oscillator
Y4 = fft(stochastic_excitation);
P4 = abs(Y4(1:L/2))/L;

% Plot Stochastic Oscillator FFT with Frequency Axis Limited to [0, 10] rad/s
subplot(4,2,8);
plot(f_rad(f_rad <= 10), P4(f_rad <= 10));
title('Stochastically Excited Harmonic Oscillator (Frequency Domain)');
xlabel('Frequency (rad/s)');
ylabel('|P4(\omega)|');
xlim([0, 10]);
