% Exercise 1.2 of the Computer Exercise 1 in Estimator and Observer design
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

% Plot
figure('Name','Exercise 1.2')
plot(t,y,'LineWidth',2)
grid on
xlabel('$t$','Interpreter','latex','FontSize',16);
ylabel('$y(t)$','Interpreter','latex','FontSize',16);