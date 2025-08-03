clear all
close all
clc

% Solve the Lorenz equations
% dx/dt = -\sigmax + \sigmay
% dy/dt = rx-y -xz
% dz/dt = -bz +xy
% for sigma = 10;  b=8/3; r=28;
% with initial conditions: x=y=z=5;
yo=[5 5 5]; % row vector!
% integrate over the timespan t = 0 to 20,
to=0; tn=20;
% use a timestep dt=0.01.
dt=[0.01];
trange=[to:dt:tn]; % row vector, while the code needs column vector!

% do rk4
[t,y]=rk4(@fun,trange,yo);

% do rk4 once more, but add small perturbation to x
[t,y2]=rk4(@fun,trange,[5.001 5 5]');

% To use the multistep Adams-Bashforth method
%   we need to prepare two initial values of yo
%   (so that size(yo) = (n,2), n is the number of ode's to be solved)
%   For that we can use a one-step method.
%   Let us use the rk4 to estimate y at to+dt
% [tinit,yinit]=rk4(@fun,[to to+dt],yo)
%--Now we can call our 2nd order Adams-Bashforth
% [t,y]=ab2(@fun,trange,yinit);

% Do forward euler
% [t,y]=feuler(@fun,trange,yo);


figure
WinOnTop(gcf); %force figure always on top
set(gcf,'defaultaxesfontsize',14); %huge font size for presentation purpose
hold on
xlim([-30 30])
ylim([-30 30])
zlim([0 60])
%axis image
xlabel('x')
ylabel('y')
zlabel('z')
grid on
plot3(y(1,1),y(2,1),y(3,1),'>b','markerfacecolor','b','markersize',10)
plot3(y(1,1),y(2,1),y(3,1),'xr','markerfacecolor','r','markersize',10)
view([38 29]);
% title({'$$\frac{dx}{dt} = -\sigma x + \sigma y$$',...
%    '$$\frac{dy}{dt} = rx-y -xz$$',...
%    '$$\frac{dz}{dt} = -bz +xy$$'},...
%    'interpreter','latex','fontsize',12)
pause

step=5;
for i=1:step:length(t)-step
    plot3(y(1,i:i+step),y(2,i:i+step),y(3,i:i+step),'-b','linewidth',1.5)
    plot3(y2(1,i:i+step),y2(2,i:i+step),y2(3,i:i+step),'--r','linewidth',1.5)
    view(40+360*i/(length(t)-step),25); %rotating view
    axis([-30 30 -30 30 0 60])
    drawnow
end
plot3(y(1,end),y(2,end),y(3,end),'ob','markerfacecolor','b','markersize',10)
plot3(y2(1,end),y2(2,end),y2(3,end),'or','markerfacecolor','r','markersize',10)

%
function f=fun(t,y)
% dx/dt = -\sigmax + \sigmay
% dy/dt = rx-y -xz
% dz/dt = -bz +xy
% solve for
sigma = 10;  b=2.6666667; r=28;
f(1) = sigma*(y(2)-y(1));
f(2) =  y(1)*(r-y(3)) - y(2);
f(3) = -b*y(3) + y(1)*y(2);
end
