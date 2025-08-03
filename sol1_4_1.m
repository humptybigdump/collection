% Exercise 1.4 of the Computer Exercise 1 in Estimator and Observer design
close all
clear all

% Parameters
t    = (0:0.1:12);   % Time vector (start:step:end)
lam1 = -2;           % First eigenvalue
lam2 = -0.5;         % Second eigenvalue
b    = 1;            % Input vector

% Analytical step response (Part I)
y =  b/(lam1*lam2)*(1 + (lam2/(lam1-lam2)).*exp(lam1.*t) - ...
                         (lam1/(lam1-lam2)).*exp(lam2.*t) );

% The system can be written in matrix notation as 
A = [lam1, 1; 0, lam2];
B = [0; 1];
C = [1, 0];
D = 0;

% Step response via numerical Euler discretization (Part III)
% Parameter for dt = 0.001
dt  = 0.001;                % Discretization step size
xk  = [0;0];                % Initial condition
u   = 1;                    % Heaviside-stepfunction
td1 = (0:dt:12);            % Discretized time vector

for i = 2:1:length(td1)     % Loop over time vector
    % Euler function call
    [y1(i),xk(:,i)] = expl_euler(A,B,C,D,xk(:,i-1),u,dt);

end

% Parameter for dt = 0.01
dt  = 0.01;                 % Discretization step size
xk  = [0;0];                % Initial condition
u   = 1;                    % Heaviside-stepfunction
td2 = (0:dt:12);            % Discretized time vector

for i = 2:1:length(td2)     % Loop over time vector
    % Euler function call
    [y2(i),xk(:,i)] = expl_euler(A,B,C,D,xk(:,i-1),u,dt);

end

% Parameter for dt = 0.1
dt  = 0.1;                  % Discretization step size
xk  = [0;0];                % Initial condition
u   = 1;                    % Heaviside-stepfunction
td3 = (0:dt:12);            % Discretized time vector

for i = 2:1:length(td3)     % Loop over time vector
    % Euler function call
    [y3(i),xk(:,i)] = expl_euler(A,B,C,D,xk(:,i-1),u,dt);

end

% Parameter for dt = 1
dt  = 1;                    % Discretization step size
xk  = [0;0];                % Initial condition
u   = 1;                    % Heaviside-stepfunction
td4 = (0:dt:12);            % Discretized time vector

for i = 2:1:length(td4)     % Loop over time vector
    % Euler function call
    [y4(i),xk(:,i)] = expl_euler(A,B,C,D,xk(:,i-1),u,dt);

end

% Calculate analytical solution at same time instances as discretization
% Delta t = 0.001
t1     = (0:0.001:12);     % Time vector
y1ana =  b/(lam1*lam2)*(1 + (lam2/(lam1-lam2)).*exp(lam1.*t1) - ...
                         (lam1/(lam1-lam2)).*exp(lam2.*t1) );
% Delta t = 0.01
t2     = (0:0.01:12);      % Time vector
y2ana =  b/(lam1*lam2)*(1 + (lam2/(lam1-lam2)).*exp(lam1.*t2) - ...
                         (lam1/(lam1-lam2)).*exp(lam2.*t2) );
% Delta t = 0.1
t3     = (0:0.1:12);        % Time vector
y3ana =  b/(lam1*lam2)*(1 + (lam2/(lam1-lam2)).*exp(lam1.*t3) - ...
                         (lam1/(lam1-lam2)).*exp(lam2.*t3) );

% Calculating the approximation of the L^1 norm
eps1 = sum(abs(y1ana - y1))*0.001;   % For Delta t = 0.001
eps2 = sum(abs(y2ana - y2))*0.01;    % For Delta t = 0.01
eps3 = sum(abs(y3ana - y3))*0.1;     % For Delta t = 0.1

% Ratios of the approximation error 
ratio12 = eps2/eps1;
ratio13 = eps3/eps1;
ratio23 = eps3/eps2;
% In explicit Euler the error scales lienarly w.r.t. to step size

% Plot
figure('Name','Exercise 1.4')
plot(t,y,'LineWidth',2)
hold on
plot(td1,y1,'--','LineWidth',2)
plot(td2,y2,'--','LineWidth',2)
plot(td3,y3,'--','LineWidth',2)
plot(td4,y4,'--x','LineWidth',2)
grid on
legend('Analytic','$\Delta t=0.001$','$\Delta t=0.01$','$\Delta t=0.1$',...
    '$\Delta t=1$','Location','SouthEast','Interpreter','latex')
xlabel('$t$','Interpreter','latex','FontSize',16);
ylabel('$y(t)$','Interpreter','latex','FontSize',16);

% Function for explicit Euler
function [y,xk] = expl_euler(A,B,C,D,x,u,dt)
    % Explicit Euler discretization algorithm
    xk = x + dt*(A*x + B*u);
    y  = C*x + D*u;
end
