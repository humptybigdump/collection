clear all
close all
clc

% Exercise : stiff ode

% Given the ODE:
f=@(t,y) -1000*y + 3000 - 2000*exp(-t); 
% with initial condition
yo=0;
% The exact solution is known : 
yexact=@(t) 3 - 0.998*exp(-1000*t) - 2.002*exp(-t);

% Apply the forward Euler method to 
% solve the above ODE for the time span
to=0; tn=0.01; 

% To study the stability 
% use different timesteps dt
dt=[0.0005 0.001 0.0015 0.002 0.0022];

% Use forward Euler
[tf1,yf1]=feuler(f,[to:dt(1):tn], yo);
[tf2,yf2]=feuler(f,[to:dt(2):tn], yo);
[tf3,yf3]=feuler(f,[to:dt(3):tn], yo);
[tf4,yf4]=feuler(f,[to:dt(4):tn], yo);
[tf5,yf5]=feuler(f,[to:dt(5):tn], yo);

% Stability :
%   look at the fast changing part exp(-1000*t) ...
%   forward euler is stable only when dt < abs(2/1000)=0.002.
% Equation to solve :
%   In this example we can isolate y(i+1), 
%   so we can solve explicitly using the equation:
%   y(i+1) = (y(i)+3000*dt-2000*dt*exp(-t(i+1))/(1+1000*dt))
fp_ex = @(tnow,ynow,dt) (ynow+3000*dt-2000*dt.*exp(-(tnow+dt)))./(1+1000*dt);
%   The implicit equation would be:
fp_im = @(tp,yp,ynow,dt) ynow + dt * (-1000*yp + 3000 - 2000*exp(-tp)) -yp;


[tb1,yb1]=beuler(fp_im, [to:dt(1):tn], yo);
[tb2,yb2]=beuler(fp_im, [to:dt(2):tn], yo);
[tb3,yb3]=beuler(fp_im, [to:dt(3):tn], yo);
[tb4,yb4]=beuler(fp_im, [to:dt(4):tn], yo);
[tb5,yb5]=beuler(fp_im, [to:dt(5):tn], yo);

% try the tic and toc command to make a rough 
% comparison of the computing time when 
% using explicit vs implicit methods.
% --> implicit is slower.
tic
[tf_ex,yf_ex]=feuler(f, [to:dt(1):tn], yo); 
Elapsed_explicit=toc

tic
[tb_im,yb_im]=beuler(fp_im, [to:dt(1):tn], yo); 
Elapsed_implicit=toc

% tic
% [t_ex,y_ex]=beuler_ex(fp_ex, [to:dt(1):tn], yo); 
% Elapsed_explicit=toc



hfig=figure(1);
set(hfig,'position',[500 30 1000 800]);
set(gcf,'defaultaxesfontsize',18)

subplot(1,2,1)
plot(linspace(to,tn,200),yexact(linspace(to,tn,200)),...
    'b','linewidth',1.5);
hold on
plot(tf1,yf1,'--','linewidth',1.5)
plot(tf2,yf2,'--','linewidth',1.5)
plot(tf3,yf3,'--','linewidth',1.5)
plot(tf4,yf4,'--','linewidth',1.5)
plot(tf5,yf5,'--','linewidth',1.5)
axis([0 tn -1 2.5])
title('forward')

subplot(1,2,2)
plot(linspace(to,tn,200),yexact(linspace(to,tn,200)),...
    'b','linewidth',1.5);
hold on
plot(tb1,yb1,'-','linewidth',1.5)
plot(tb2,yb2,'-','linewidth',1.5)
plot(tb3,yb3,'-','linewidth',1.5)
plot(tb4,yb4,'-','linewidth',1.5)
plot(tb5,yb5,'-','linewidth',1.5)
axis([0 tn -1 2.5])
title('backward')

xlabel('t'); 
ylabel('y(t)');
%ylim([0 1.2])

legendtxt{1}='exact';
for i=1:length(dt)
    legendtxt{i+1}=['h=' num2str(dt(i)','%2.4f')];
end

legend(legendtxt,'location','southeast')