close all;
clearvars;

% Parameters
omega_n = 1;           % Natural frequency
zeta = 0.1;           % Small damping ratio
A = 1;                 % Constant amplitude of forcing
omega_start = 0.1;     % Starting frequency of driving force
omega_end = 2;         % Final frequency of driving force
T_end = 3000;          % Total simulation time for gradual frequency increase
dt = 0.05;             % Time step for simulation

% Time vector
time = 0:dt:T_end;

% Frequency sweep - slowly increase the frequency over time
omega_t = omega_start + (omega_end - omega_start) * (time / T_end);

% Define the driving force with a slowly varying frequency
forcing = A * sin(cumtrapz(time, omega_t)); % Integrate omega_t to get phase

% Theoretical Amplification Function for Comparison
omega_range = linspace(omega_start, omega_end, 500);
H_theoretical = 1 ./ sqrt((1 - (omega_range / omega_n).^2).^2 + (2 * zeta * omega_range / omega_n).^2);

% Solve the differential equation for the damped oscillator
osc_eq = @(t, y) [y(2); -2 * zeta * omega_n * y(2) - omega_n^2 * y(1) + interp1(time, forcing, t)];

% Initial conditions [y(0), y'(0)]
[t, y] = ode45(osc_eq, time, [0, 0]);

% Extract the envelope by taking the maximum absolute value of the response over time
envelope_amplitude = abs(hilbert(y(:, 1))); % Hilbert transform for envelope

figure;
% Top subplot: Time-domain response
subplot(2, 1, 1);
plot(t, y(:,1), 'k', 'LineWidth', 1);
xlabel('$t$', 'Interpreter', 'latex');
ylabel('$x$', 'Interpreter', 'latex');
title('System Response in Time Domain');
legend(['Damping $\zeta =$ ', num2str(zeta)], 'Interpreter', 'latex');
grid on;

% Bottom subplot: Momentary driving frequency
subplot(2, 1, 2);
plot(t, omega_t, 'k', 'LineWidth', 1);
xlabel('$t$', 'Interpreter', 'latex');
ylabel('Driving Frequency $\omega(t)$', 'Interpreter', 'latex');
title('Instantaneous Driving Frequency');
grid on;

% Plotting
figure;

% Plot theoretical amplification function
plot(omega_range, H_theoretical, 'r', 'LineWidth', 1.5);
title('Theoretical Amplification Function');
xlabel('Driving Frequency \omega');
ylabel('Amplification Factor H(\omega)');
grid on;
hold on;
% Interpolating the envelope amplitude data to frequency
% Create a mapping from time to frequency
frequency_range = omega_start + (omega_end - omega_start) * (t / T_end);

% Plot frequency sweep response with envelope detection in terms of frequency
plot(frequency_range, envelope_amplitude, 'b', 'LineWidth', 1.5);
hold on;
%plot(frequency_range, frequency_range / max(frequency_range) * max(envelope_amplitude), 'g--'); % Scaled frequency for reference
title('Frequency Sweep Response - Envelope Amplitude for $\zeta=$ ', num2str(zeta));
xlabel('Driving Frequency $\eta$');
ylabel('Envelope Amplitude of Response');
legend('Theoretical Amplification','Experimental Amplification');
grid on;
% Add the analytical amplification function formula as LaTeX text
text(0.2, max(envelope_amplitude)*1.1, ...
    '$$H(\omega) = \frac{1}{\sqrt{(1 - \eta^2)^2 + (2 \zeta \eta)^2}}$$', ...
    'Interpreter', 'latex', 'FontSize', 12, 'Color', 'k');
