clear all
close all
clc

%--------------------------------------------------
% problem statement 
% use the newton-raphson method to
% find the roots of f(x)

% Derive analytical expressions for 
% f(x) and it's first derivative fd
f = @(x) -0.5*x.^2 + 2.5*x + 4.5;
fd= @(x) -x + 2.5;
%another example: 
%f  = @(x) 2*x.^3 - 11.7*x.^2 + 17.7*x -5;
%fd = @(x) 6*x.^2 - 23.4*x +17.7;

% Plot and visually find the roots 
% to help obtaining a good initial guess
x=linspace(-5,10,100);
y=f(x);
plot(x,y,'r','linewidth',2)
xlabel('x')
ylabel('f(x)')
drawnow

%---------
% result given by matlab built-in function
xr1_matlab=fzero(f,[-3, 1])
xr2_matlab=fzero(f,[5 10])

% Define an initial guess
% try e.g. xo=1 and xo=6
xo1 = -1;
xo2 =  6;
et = 1e-5;
itmax=50;

[xr1,it1]=newraph(f,fd,xo1,et,itmax)
[xr2,it2]=newraph(f,fd,xo2,et,itmax)

function [xr,it]=newraph(f,fd,xold,et,itmax)
dx = 2*et;
it = 0;
while abs(dx) > et && it < itmax
    it=it+1;
    dx = f(xold)/fd(xold);
    xnew=xold-dx;
    %xr(it) = xnew; %saves result of every iteration
    xold=xnew;
end
xr = xnew;
%disp(['My solution xr = ' num2str(xr)])
%disp(['Number of iterations = ' num2str(it)])
end

