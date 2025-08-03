clear all
close all
clc

% Studying stability of explicit vs implicit Euler method
% This is an extended version of the example stability.m
% !! Notice here we use feuler.m and beuler.m 
% which is not identical to the functions contained 
% in stability.m

% Solve the ode below
f=@(t,y) -20*y; 
%
% with an  initial condition y(0)=1
yo=1;
% for the range t=0 to 0.5
to=0; tn=0.5; 
% use the following time step(s)
dt=[0.11]; %try from 0.001 ...0.1
% generate column vector trange
trange = [to:dt:tn];

% Let's plot the exact solution first
yexact=@(t) exp(-20*t);
hfig=figure(1);
set(hfig,'position',[900 30 800 800]);
set(gcf,'defaultaxesfontsize',24); 
plot(linspace(to,tn,300),yexact(linspace(to,tn,300)),...
    'b','linewidth',1.5);
hold on

%------------
% Forward :
% We march forward in time
% y(i+1) = y(i) + dt * f(t(i),y(i)); %see function feuler below
%   Stability: forward euler is stable only when ...
%     dt < abs(2/20)=0.1.
%   Try out different dt !11
tic
[tef,yef]=feuler(f,trange,yo);
Elapsed_time_forward=toc
plot(tef,yef,'-o','linewidth',1.25)

%------------
% Backward :
% y(i+1) = y(i) + dt * f(t(i+1),y(i+1))
% Equation to solve :
%   y(i) - 20y(i+1)*dt - y(i+1) = 0
% In this case, the resulting equation is linear, 
%   so we can isolate y(i+1) and obtain an explicit expression: 
%   y(i+1)=y(i)/(1+20*dt)
fp_ex=@(tnow,ynow,dt) ynow/(1+20*dt); %fp is the function to be solved,
%                               i.e. fp computes y(i+1)
%
% However!!! typically we get a nonlinear equation.
%   Thus, to solve for y(i+1) we need to use e.g. Newton-Raphson.
%   To illustrate the much higher computing cost needed when 
%   solving non-linear equation is required, let us pretend 
%   that we can not isolate y(i+1) and that we have to solve 
%   y(i)-20*yp*dt-yp = 0; where yp=y(i+1)
%   In this example, we use the built-in matlab function : fzero 
%   to solve for yp implicitly.
fp_im=@(tnow,ynow,yp,dt) ynow-20*yp*dt-yp;

tic
[teb,yeb]=beuler(fp_im,trange,yo);
Elapsed_time_backward=toc
plot(teb,yeb,'-x','linewidth',1.25)

xlabel('t'); 
ylabel('y(t)');
legend('exact','forward','backward')

tic
[teb2,yeb2]=beuler_ex(fp_ex,trange,yo);
Elapsed_time_backward_ex=toc