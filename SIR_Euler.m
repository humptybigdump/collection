%
% Explicit Euler for SIR model
%
%%
clc; close all;  clear;
%% Parameter discretisation

T = 60; % length time intervall
h = 0.01; % step size
n = round(T/h)+1; 

%% Problem parameter

S_0 = 0.9;
I_0 = 0.01;
R_0 = 1 - S_0 - I_0;

rho = 5; % R-value
gamma = 0.0; % loss of immunity

% ODE y' = f(t,y) 

f = @(tt,yy) [-rho*yy(1)*yy(2)+gamma*yy(3); 
    rho*yy(1)*yy(2) - yy(2); 
    yy(2)-gamma*yy(3)];

%% Explicit Euler

y_sol = zeros(3,n); t_int = linspace(0,T,n); % assemble solution and time vector
y_sol(:,1) = [S_0; I_0; R_0]; % inital condition

% time integration

for i = 1:n-1
    y_sol(:,i+1) = y_sol(:,i) + h*f(t_int(i),y_sol(:,i));
end

%% Plot solutions and invariant
figure1 =figure(1);clf;%figure1.WindowState = 'maximized';
hold on;
    pbaspect([4 3 1]);
    set(gca,'FontSize', 20);
    xlabel('$t$','Interpreter', 'latex');
    plot(t_int, y_sol(1,:),'Color','green','LineWidth',2);
    plot(t_int, y_sol(2,:),'Color','red','LineWidth',2);
    plot(t_int, y_sol(3,:),'Color','blue','LineWidth',2);
    legend('S','I','R');
    xlim([0,T]);
    ylim([0,1]);
hold off;

figure2 =figure(2);clf;%figure1.WindowState = 'maximized';
hold on;
    pbaspect([4 3 1]);
    set(gca,'FontSize', 20);
    xlabel('$t$','Interpreter', 'latex');
    ylabel('$S+I+R$','Interpreter', 'latex');
    plot(t_int, y_sol(1,:)+y_sol(2,:)+y_sol(3,:),'Color','black','LineWidth',2);
    xlim([0,T]);
    ylim([0,1.6]);
hold off;
