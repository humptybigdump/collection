clear all
close all
clc

%--------------------------------------------------
% problem statement 
% use the fixed-point iteration method to 
% find the root(s) of the function
f = @(x) -0.5*x.^2 + 2.5*x + 4.5;

% manipulate f(x)=0 to derive an
% analytical expression in the form x = ...
% notice that f(x) = 0 is equivalent to x = f(x) + x
g = @(x) (0.5*x.^2-4.5)/2.5;

% plot and visually find the roots 
% to help obtaining a good initial guess
x=linspace(-5,10,100);
y=f(x);
plot(x,y,'r','linewidth',2)
grid on
xlabel('x')
ylabel('f(x)')
%
drawnow

%---------
% result given by fzero
xr1_matlab=fzero(f,[-3, 1])
xr2_matlab=fzero(f,[5 10])

%--------------
% with the help of the above plot choose 
% an initial guess that is close to the root 
% try e.g. xo=1 and then xo=6 
xo1 = 1;
xo2 = 6.35;  
et  = 1e-5;
itmax=100;
[xr1,it1]=fixedpoint(g,xo1,et,itmax)
[xr2,it2]=fixedpoint(g,xo2,et,itmax)
%--------
% Ooops! Why is xr2 not 6.4051?


%--------------------------------------------------------
function [xr,it]=fixedpoint(g,xold,et,itmax)
% a routine for finding the roots of f(x)
% using the fixed-point iteration
% Inputs: 
   % g(x) : the iterative fixed point formulation $g(x)$ 
   % xold : the initial guess 
   % et   : the error tolerance
   % itmax: the maximum iteration allowed
% Outputs:
   % xr : root
   % it : number of iteration performed

it=0;
Ea=2*et;
while (Ea > et) && (it < itmax)
    % compute next estimate of the root xr
    it=it+1;
    xnew = g(xold);
    Ea = abs((xnew-xold)/xnew);
    xold=xnew;
    %xr(it)=xnew; % saves result of every iteration
end
xr = xnew; % only final solution
%disp(['Numerical solution xr = ' num2str(xr)])
%disp(['Number of iterations = ' num2str(it)])
end
