% Exercise 1.3 of the Computer Exercise 1 in Estimator and Observer design
close all
clear all

% Parameters
t    = (0:0.1:12);   % Time vector (start:step:end)
lam1 = -2;           % First eigenvalue
lam2 = -0.5;         % Second eigenvalue
b    = 1;            % Input vector

% Step response
y =  b/(lam1*lam2)*(1 + (lam2/(lam1-lam2)).*exp(lam1.*t) - ...
                         (lam1/(lam1-lam2)).*exp(lam2.*t) );

% The system can be written in matrix notation as 
A = [lam1, 1; 0, lam2];
B = [0; 1];
C = [1, 0];
D = 0;

% Calculate transfer function by ss2tf
[n,d] = ss2tf(A,B,C,D);     % Get coefficients of transfer function
sys   = tf(n,d);            % Set up transfer function by coefficients
y1    = step(sys,t);        % Calculate step response

% Plot
figure('Name','Exercise 1.3')
plot(t,y,'LineWidth',2)
hold on
plot(t,y1,'--','LineWidth',2)  % Dashed line style
grid on
legend('Analytic','Laplace','Interpreter','latex')
xlabel('$t$','Interpreter','latex','FontSize',16);
ylabel('$y(t)$','Interpreter','latex','FontSize',16);



