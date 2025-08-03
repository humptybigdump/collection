% example taylor series 
% this is not an efficient script 
% it serves as a simple illustration
% 
% given : f(x) = x^3 + 2x^2 - 5x + 3
% estimate the f(x) at x = xp = 1
% use the Taylor series expansion
% expand f(xp) about the point xo=0
% to construct the taylor series, 
% we need the derivatives of f(x) at xo
% fd1(xo) = 3xo^2 + 4xo - 5 = -5
% fd2(xo) = 6xo + 4         = 4
% fd3(xo) = 6
%
% let's expand f(xp) about the point xo=0;
% f(xp) = f(xo) + fd1(xo)*(xp-xo) +
%                 fd2(xo)*(xp-xo)^2/factorial(2) + ... +
%                 fdn(xo)*(xp-xo)^n/factorial(n) 
%
% NOTICE : because the true function is a 3rd degree polynomial
%          the third-order approximation is exact!

clear all; close all; clc;
% expand about 
xo = 0;
% estimate the value at 
xp = 1;  %one point

% plot the exact function f(x) 
x = [-2:0.05:2];
ytrue = x.^3 + 2*x.^2 - 5*x + 3;
plot(x,ytrue,'b');
grid on;
xlabel('x')
ylabel('f(x)');
hold on; %superimpose next data points

%zeroth-order approximation
%f_0(xp) = f(xo)
%generate a zero matrix yo with a size same as xp
yo = zeros(size(xp));
yo = yo + 3
plot(xp,yo,'-or','markerfacecolor','r')

%first-order approximation
%f_1(xp) = yo + fd1(xo)*(xp-xo)  
y1 = yo - 5*xp
plot(xp,y1,'-og','markerfacecolor','g')

%second-order approximation
%f_2(xp) = y1 + fd2(xo)*(xp-xo)^2/factorial(2)  
y2 = y1 + 4.*xp.^2/factorial(2)
plot(xp,y2,'-om','markerfacecolor','m')

%third-order approximation
%f_3(x) = y2 + fd3(xo)*(xp-xo)^2/factorial(3) 
y3 = y2 + 6*xp.^3/factorial(3)
plot(xp,y3,'-ok','markerfacecolor','k')

legend('true function','zero-order','first-order','second-order','third-order')

%-----
% the same as above but for several points
xxp = [-2:0.2:2]; %several points

figure
% plot the true function
plot(x,ytrue,'b');
grid on;
xlabel('x')
ylabel('f(x)');
hold on

%zero-order approximation
%f_0(xp) = f(xo)
%generate a zero matrix yo with a size same as xxp
yyo = zeros(size(xxp));
yyo = yyo + 3;
plot(xxp,yyo,'xr')

%first-order approximation
%f_1(xp) = yo + fd1(xo)*(xp-xo)  
yy1 = yyo - 5*xxp;
plot(xxp,yy1,'xg')

%second-order approximation
%f_2(xp) = y1 + fd2(xo)*(xp-xo)^2/factorial(2)  
yy2 = yy1 + 4.*xxp.^2/factorial(2);
plot(xxp,yy2,'xm')

%third-order approximation
%f_3(x) = y2 + fd3(xo)*(xp-xo)^2/factorial(3) 
yy3 = yy2 + 6*xxp.^3/factorial(3);
plot(xxp,yy3,'xk')

legend('true function','zero-order','first-order','second-order','third-order')