% Simple Forced Undamped Oscillation Simulation

% Parameters
m = 1;                % Mass (kg)
k = 1;                % Spring constant (N/m)
f_hat = 1;            % Amplitude of the forcing function
eta = 1;            % Frequency ratio (Omega / omega)
omega = sqrt(k / m);  % Natural frequency
Omega = eta * omega;  % Driving frequency

% Initial conditions
initial_displacement = 10;  % Initial displacement
initial_velocity = 0;      % Initial velocity
initial_conditions = [initial_displacement; initial_velocity];

% Time span for simulation
t_span = [0, 500];

% Define the ODE for forced undamped oscillations
odefun = @(t, y) [y(2); (f_hat * cos(Omega * t) - k * y(1)) / m];

% Solve the ODE
[t, y] = ode45(odefun, t_span, initial_conditions);

% Plot the solution
figure;
plot(t, y(:, 1), 'LineWidth', 1.5);
title(['System Response with $\eta = $', num2str(eta), ...
       ', $f =$ ', num2str(f_hat), ', $ m = $', num2str(m), ', $ k = $', num2str(k)]);
xlabel('$t$');
ylabel('$x$');
grid on;
