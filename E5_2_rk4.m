clear all
close all
clc

% ODE : dy/dt = -y^2, y(1) = 1.
% The exact solution is y(t) = 1/t.
% Solve for the timespan 1 to 10.
f = @(t,y) -y.^2;
fexact = @(t) 1./t;


yo=[1]'; % has to be in vector form
to=1;
tn=10;
dt=[0.2 0.02 0.002];

% plot exact solution
tt=linspace(to,tn,100);
plot(tt,fexact(tt),'k','linewidth',1.5);
hold on
for i = 1:length(dt)
    clear t y
    trange=[to:dt(i):tn];
    % do runga-kutta
    [t,y]=rk4(f,trange,yo)
    E(i)=abs(y(end)-fexact(tn));
    plot(t,y)
    
    % do forward euler
    [tf,yf]=feuler(f,trange,yo);
    Ef(i)=abs(yf(end)-fexact(tn));
    plot(tf,yf)
end

format shorte
% compare error feuler vs rk4
[dt' E' Ef']
