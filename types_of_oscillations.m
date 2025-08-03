clear;
clc;

% Time vector
tspan = [0 50]; % time interval

%% Free Oscillations: d²x/dt² + w0² * x = 0
% Differential equation: d²x/dt² = -w0² * x
% Rewritten as a system of first-order ODEs:
% dx1/dt = x2
% dx2/dt = -w0² * x1

w0 = 2; % natural frequency
free_osc_eq = @(t, y) [y(2); -w0^2 * y(1)];

% Initial conditions [x0, v0] (displacement and velocity)
y0_free = [1; 0]; % starts from position 1 with velocity 0
[t_free, y_free] = ode45(free_osc_eq, tspan, y0_free);

% Plot Free Oscillations
figure;
subplot(5,1,1);
plot(t_free, y_free(:,1), 'b', 'LineWidth', 1.5);
title('Free Oscillations: d²x/dt² + w0² * x = 0');
xlabel('Time');
ylabel('Displacement');
grid on;

%% Damped Oscillations: d²x/dt² + 2*b*dx/dt + w0² * x = 0
% Differential equation: d²x/dt² = -2*b*dx/dt - w0² * x
% Rewritten as a system of first-order ODEs:
% dx1/dt = x2
% dx2/dt = -2*b*x2 - w0² * x1

b = 0.1; % damping coefficient
damped_osc_eq = @(t, y) [y(2); -2*b*y(2) - w0^2 * y(1)];

% Initial conditions [x0, v0]
y0_damped = [1; 0];
[t_damped, y_damped] = ode45(damped_osc_eq, tspan, y0_damped);

% Plot Damped Oscillations
subplot(5,1,2);
plot(t_damped, y_damped(:,1), 'r', 'LineWidth', 1.5);
title('Damped Oscillations: d²x/dt² + 2*b*dx/dt + w0² * x = 0');
xlabel('Time');
ylabel('Displacement');
grid on;

%% Parametrically Driven Oscillations: d²x/dt² + w0²*(1 + h*cos(omega*t))*x = 0
% Differential equation: d²x/dt² = -w0²*(1 + h*cos(omega*t)) * x
% Rewritten as a system of first-order ODEs:
% dx1/dt = x2
% dx2/dt = -w0²*(1 + h*cos(omega*t)) * x1

h = 0.5; % modulation depth
omega = 2.; % driving frequency
parametric_osc_eq = @(t, y) [y(2); -(w0^2 * (1 + h*cos(omega*t))) * y(1)];

% Initial conditions [x0, v0]
y0_param = [0.01; 0];
[t_param, y_param] = ode45(parametric_osc_eq, tspan, y0_param);

% Plot Parametrically Driven Oscillations
subplot(5,1,3);
plot(t_param, y_param(:,1), 'g', 'LineWidth', 1.5);
title('Parametrically Driven Oscillations: d²x/dt² + w0²*(1 + h*cos(omega*t))*x = 0');
xlabel('Time');
ylabel('Displacement');
grid on;

%% Van der Pol Oscillator: d²x/dt² - mu*(1 - x^2)*dx/dt + w0² * x = 0
% Differential equation: d²x/dt² = mu*(1 - x^2)*dx/dt - w0² * x
% Rewritten as a system of first-order ODEs:
% dx1/dt = x2
% dx2/dt = mu*(1 - x1^2)*x2 - w0² * x1

mu = 1; % nonlinearity parameter
van_der_pol_eq = @(t, y) [y(2); mu*(1 - y(1)^2)*y(2) - w0^2 * y(1)];

% Initial conditions [x0, v0]
y0_vdp = [.1; 0];
[t_vdp, y_vdp] = ode45(van_der_pol_eq, tspan, y0_vdp);

% Plot Van der Pol Oscillator
subplot(5,1,4);
plot(t_vdp, y_vdp(:,1), 'm', 'LineWidth', 1.5);
title('Van der Pol Oscillator: d²x/dt² - mu*(1 - x^2)*dx/dt + w0² * x = 0');
xlabel('Time');
ylabel('Displacement');
grid on;

%% Oscillations with External Forcing: d²x/dt² + 2*b*dx/dt + w0² * x = A*cos(omega*t)
% Differential equation: d²x/dt² = -2*b*dx/dt - w0² * x + A*cos(omega*t)
% Rewritten as a system of first-order ODEs:
% dx1/dt = x2
% dx2/dt = -2*b*x2 - w0² * x1 + A*cos(omega*t)

A = 0.5; % forcing amplitude
external_osc_eq = @(t, y) [y(2); -2*b*y(2) - w0^2 * y(1) + A * cos(omega*t)];

% Initial conditions [x0, v0]
y0_ext = [1; 0];
[t_ext, y_ext] = ode45(external_osc_eq, tspan, y0_ext);

% Plot Oscillations with External Forcing
subplot(5,1,5);
plot(t_ext, y_ext(:,1), 'k', 'LineWidth', 1.5);
title('Oscillations with External Forcing: d²x/dt² + 2*b*dx/dt + w0² * x = A*cos(omega*t)');
xlabel('Time');
ylabel('Displacement');
grid on;
