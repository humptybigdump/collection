% Exercise 1.5 of the Computer Exercise 1 in Estimator and Observer design
close all
clear all

% Parameters
t    = (0:1:12);     % Time vector (start:step:end)
lam1 = -2;           % First eigenvalue
lam2 = -0.5;         % Second eigenvalue
b    = 1;            % Input vector
x0   = [0;0];        % Initial condition
p.u  = 1;            % Heaviside step function

% The system can be written in matrix notation as 
p.A = [lam1, 1; 0, lam2];
p.B = [0; 1];
p.C = [1, 0];
p.D = 0;

% Call ode45 function for numerical integration
[t,x] = ode45(@(t,x)lti_sys(t,x,p),t,x0);

% Extract system output
y = p.C*x';

% Plot
figure('Name','Exercise 1.5')
plot(t,y,'LineWidth',2)
grid on
xlabel('$t$','Interpreter','latex','FontSize',16);
ylabel('$y(t)$','Interpreter','latex','FontSize',16);

% Function for system dynamics
function dx = lti_sys(t,x,p)
    % System dynamics
    dx = p.A*x + p.B*p.u;
end
