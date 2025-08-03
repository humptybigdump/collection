clear
close all

% Time vector 
t_sim = [0,7];
% Initial states
x_0 = [2;4];

% Coefficients of feedback controllers
c = [2,3];                           % Integrator backstepping
p = [c(1)*c(2)+1,c(1)+c(2)];         % so that I-S-Lin equals backstepping

% Input-/State-Linearization
[t_lin,x_lin] = ode45(@(t,x)ISLinearization(t,x,p),t_sim,x_0);

% Backstepping
[t_bac,x_bac] = ode45(@(t,x)IntBackstepping(t,x,c),t_sim,x_0);

% Plots
figure('Name','Linearization vs Backstepping')
x1 = plot(t_lin,x_lin(:,1),'LineWidth',2,'Color',[0 0.4470 0.7410]);
hold on
x2 = plot(t_lin,x_lin(:,2),'LineWidth',2,'Color',[0.8500 0.3250 0.0980]);
bs = plot(t_bac,x_bac(:,1),'--','LineWidth',2,'Color',...
    [0.9290 0.6940 0.1250]);
plot(t_bac,x_bac(:,2),'--','LineWidth',2,'Color',[0.9290 0.6940 0.1250])
hold off
grid on
xlabel('Time','Interpreter','latex')
ylabel('$x_1$, $x_2$','Interpreter','latex')
legend([x1,x2,bs],'$x_1$ Linearization','$x_2$ Linearization',...
    '$x_1$, $x_2$ Backstepping',...
    'Location','NorthEast','Interpreter','latex')

function dx = IntBackstepping(t,x,c)
% Parameter
c0 = c(1);
c1 = c(2);

% Input based on integrator backstepping
u = -2*x(1)^3 - 2*x(1)*x(2) - x(2)*(c1+c0) - x(1)*(c1*c0+1+(c1+c0)*x(1));

% ODEs
dx(1)   = x(1)^2 + x(2);
dx(2,1) = u;
end

function dx = ISLinearization(t,x,p)
% Parameter
p0 = p(1);
p1 = p(2);

% Input based on input to state linearization
u = -2*x(1)^3 - 2*x(1)*x(2) - x(1)*(p0 + p1*x(1)) - x(2)*p1;

% ODEs
dx(1)   = x(1)^2 + x(2);
dx(2,1) = u;
end
