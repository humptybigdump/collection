function fmms0401
%% Script zu MMS-04/01

%% Initialisierung
clear
% clc
close all

set(groot,'defaultTextInterpreter','Latex');
set(groot,'defaultAxesTickLabelInterpreter','Latex');
set(groot,'defaultLegendInterpreter','Latex');
set(groot,'defaultAxesFontSize',16)

%% Parameter
J_A = 1;
k_d = 0.1;
a   = 1;
c1  = 1;
c2c1= 5/2;      % Bifurkationsparameter, c2c1 = c2/c1

%% Numerische Integration
ic      = [2.7,0];
t_start = 0;
t_end   = 50;

ode_opt = odeset('RelTol',1e-6,'AbsTol',1e-6);

tic
[t,x] = ode45(@fode_federpendel,[t_start,t_end],ic,ode_opt);
toc

phi = x(:,1);
phi_dot = x(:,2);

figure
plot(t,phi,'k')
xlabel('$t$')
ylabel('$\varphi$','Rotation',0)
% ylim([min(inf,0),inf])

% figure
% plot(t,phi_dot)

figure
plot(phi,phi_dot,'k')
xlabel('$\varphi$')
ylabel('$\dot \varphi$','Rotation',0)
% xlim([min(inf,0),inf])

%% Ergebnisse aus Stabilitätsbetrachtung
vekc1 = 0:2;
vekc2 = 2:0.01:5;

vekphi11 = 0*vekc1;
vekphi12 = 0*vekc2;

vekphi21 = pi*ones(1,length(vekc1));
vekphi22 = pi*ones(1,length(vekc2));

vekphi3  = acos(1./(1-vekc2));
vekphi4  = 2*pi-vekphi3;

figure
plot(vekc1,vekphi11,'r',vekc2,vekphi12,'r',vekc1,vekphi21,'b--',vekc2,vekphi22,'b',vekc2,vekphi3,'k--',vekc2,vekphi4,'c--',c2c1,ic(1),'x')
ylim([0,2*pi])
ylabel('$\varphi_0$','Rotation',0)
xlabel('$\frac{c_2}{c_1}$')
yticks(0:pi:2*pi)
yticklabels({'0','$\pi$','$2\pi$'})
xtext = [1,1,4,4];
ytext = [0.5,3.5,1.7,4.7];
strtex= {'$\varphi_{01}$','$\varphi_{02}$','$\varphi_{03}$','$\varphi_{04}$'};
txt = text(xtext,ytext,strtex,'FontSize',16);
txt(1).Color = 'r';
txt(2).Color = 'b';
txt(3).Color = 'k';
txt(4).Color = 'c';




function dydt = fode_federpendel(~,y)
    dydt(1) = y(2);
    dydt(2) = -1/J_A*(k_d*y(2) + 4*a^2*sin(y(1))*(c1*(1 - cos(y(1))) + c1*c2c1*cos(y(1))));
    dydt    = dydt';
end
end