clear all
close all
clc

% demo stability.m (compact)
% See also stability_plus.m !
% WARNING!! : the very simple functions feuler & beuler
%             in this example   
%             are not identical to 
%             the ones used in stability_plus.m and in all 
%             other exercises of this problem sessions!!!!

% solve the ode
f = @(t,y) -20*y;

% initial condition
yo=1;

% for the range
to=0;
tn=0.5;

% timestep
dt =0.12;

% generate the points
trange = [to:dt:tn];

% plot exact solution
yexact = @(t) exp(-20*t);
texact = linspace(to,tn,200);
plot(texact,yexact(texact),'b');
hold on

% forward 
% y(i+1) = y(i) + dt * f(t(i),y(i));
[t,y] = feuler(f, trange, yo);
plot(t,y,'o-r')

% backward 
% y(i+1) = y(i) + dt * f(t(i+1),y(i+1));
% fp_im : y(i) -20*y(i+1)*dt-y(i+1)
fp_im = @(yp,ynow) ynow-20*yp*dt-yp;
[teb,yeb] = beuler(fp_im, trange, yo);
plot(teb,yeb,'o-k')

function [t,y] = beuler(fp, t, yo)
y(1) = yo;
for i = 1:length(t)-1
  y(i+1) = fzero(@(yp) fp(yp,y(i)),y(i));
end
end

function [t,y] = feuler(f, t, yo)
dt = t(2)-t(1);
y(1) = yo;
for i = 1:length(t)-1
  y(i+1) = y(i) + dt * f(t(i),y(i));
end
end
