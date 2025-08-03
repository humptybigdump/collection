clear all
close all
clc
% Given the points : (1,1), (2,3), (4,3),
% use the monomial basis
% to obtain a polynomial fit 
% that goes through all points

% For each data point we have :
% (xo,yo) : a0 + a1*1 + a2*1^2 = 1
% (x1,y1) : a0 + a1*2 + a2*2^2 = 3
% (x2,y2) : a0 + a1*4 + a2*4^2 = 3
% 
% a0 +   a1 +  1*a2 = 1
% a0 + 2*a1 +  4*a2 = 3
% a0 + 4*a1 + 16*a2 = 3
%
% in matrix form : 
% Va=b;
V= [1 1 1
    1 2 4
    1 4 16];
%
b=[1; 3; 3];

% to solve in matlab we can simply use the
% statement 
a = V\b

% plot the available (given) data points
xa = [1 2 4];
ya = [1 3 3];
plot(xa,ya,'ob')
xlabel('x')
ylabel('y')

hold on % to superimpose another data set
        
%plot the polynomial 
% let's say for x=[0:0.1:5]
xpol=[0:0.1:5];
ypol=a(1)+a(2)*xpol+a(3)*xpol.^2;
plot(xpol,ypol,'-r')







