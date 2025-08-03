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

% to check convergent/divergent beforehand
% we need the first derivative of g(x)
gd = @(x) x/2.5;

% plot and visually find the roots 
% to help obtaining a good initial guess
x=linspace(-5,10,100);
y=f(x);
plot(x,y,'r','linewidth',2)
grid on
grid minor
xlabel('x')
ylabel('f(x)')
%
drawnow

%---------
% result given by fzero
xr1_matlab=fzero(f,[-2, -1])
xr2_matlab=fzero(f,[6 7])

%--------------
% with the help of the above plot choose 
% an initial guess that is close to the root 
% try for example xo=1 and xo=6
xo1 = -1;
xo2 =  6; 
et = 1e-5;
itmax=100;
[xr1,it1]=fixedpoint2(g,xo1,et,itmax,gd)
[xr2,it2]=fixedpoint2(g,xo2,et,itmax,gd)

%--------------------------------------------------------
function [xr,it]=fixedpoint2(g,xold,et,itmax,gd)
% a routine for finding the roots of f(x)
% using the fixed-point iteration
% Inputs: 
   % g(x) : the iterative fixed point formulation g(x) 
   % gd(x): the first derivative of g(x) 
   % xold : the initial guess 
   % et   : the error tolerance
   % itmax: the maximum iteration allowed
% Outputs:
   % xr : root
   % it : number of iterations performed

it=0;
Ea=2*et;
while (Ea > et) && (it < itmax)
    % use theorem to check whether g(x) is always convergent 
    rho = abs(gd(xold));
    %[xold rho]
    if rho > 1
        xr=xold;
        disp('error : no iteration performed !!')
        disp(['rho = ' num2str(rho,'%2.2f') ', rho > 1 !!'])
        disp('Please provide an iteration formula g(x) that is guaranteed to be convergent!!')
        return
    end
    % compute next estimate of the root xr
    it=it+1;
    xnew = g(xold);
    Ea = abs((xnew-xold)/xnew);
    xold=xnew;
    %xr(it)=xnew; % saves result of every iteration
end
xr = xnew; % only final solution
%disp(['My solution xr = ' num2str(xr)])
%disp(['Number of iterations = ' num2str(it)])
end
