clear all;
close all;;

% Define parameters for the oscillator
m = 1;               % Mass (kg)
k = 1;             % Stiffness (N/m)
omega_n = sqrt(k/m); % Natural frequency (rad/s)

% Time settings
tspan = [0, 100];     % Time range (s)
y0 = [11; 0];         % Initial conditions [position; velocity]

%% Case 1: Viscous Damping
c_viscous = 0.2; % Damping coefficient for viscous damping (N·s/m)

% Define ODE for viscous damping
viscous_ode = @(t, y) [y(2); -k/m * y(1) - c_viscous/m * y(2)];

% Solve ODE
[t_viscous, y_viscous] = ode45(viscous_ode, tspan, y0);

%% Case 2: Quadratic Air-Drag Damping
c_quadratic = 0.5; % Damping coefficient for quadratic air-drag damping

% Define ODE for quadratic air-drag damping
quadratic_ode = @(t, y) [y(2); -k/m * y(1) - c_quadratic/m * y(2) * abs(y(2))];

% Solve ODE
[t_quadratic, y_quadratic] = ode45(quadratic_ode, tspan, y0);

%% Case 3: Coulomb Damping (with sliding and sticking phases)
F_coulomb = 0.05; % Coulomb friction force (N)

% Define ODE for Coulomb damping
coulomb_ode = @(t, y) [y(2); -k/m * y(1) - F_coulomb/m * sign(y(2)) * (y(2) ~= 0)];

% Solve ODE
[t_coulomb, y_coulomb] = ode45(coulomb_ode, tspan, y0);

% Handle sticking phase in post-processing
for i = 1:length(y_coulomb)-1
    if abs(y_coulomb(i, 2)) < 1e-3 && abs(y_coulomb(i, 1)) < F_coulomb/k
        y_coulomb(i+1:end, 2) = 0;
        y_coulomb(i+1:end, 1) = y_coulomb(i, 1);
        break;
    end
end

%% Plot Results
figure;

% Plot Viscous Damping
subplot(3, 1, 1);
plot(t_viscous, y_viscous(:, 1), 'k');
title('1-DOF Harmonic Oscillator with Viscous Damping');
xlabel('Time (s)');
ylabel('Displacement (m)');
grid on

% Plot Quadratic Air-Drag Damping
subplot(3, 1, 2);
plot(t_quadratic, y_quadratic(:, 1), 'k');
title('1-DOF Harmonic Oscillator with Quadratic Air-Drag Damping');
xlabel('Time (s)');
ylabel('Displacement (m)');
grid on

% Plot Coulomb Damping
subplot(3, 1, 3);
plot(t_coulomb, y_coulomb(:, 1), 'k');
title('1-DOF Harmonic Oscillator with Coulomb Damping');
xlabel('Time (s)');
grid on;
ylabel('Displacement (m)');