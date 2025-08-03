function [xr,it] = bisection(f,xl,xu,et)
% NOTE : this is not the most efficient algorithm!!!
% It can be made more efficient.

% This routine performs the bisection method
% Inputs : 
%   f   = function handle of f(x)
%   xl  = lower bound
%   xu  = upper bound
%   et  = tolerance of absolute error 
% Outputs :
%   xr  = root found between xl and xu
%   it  = number of iterations performed

% check whether there is a change in sign
if f(xl)*f(xu) > 0
    disp('Error : there is no root')
    return
end

tol=1;it=0;
while tol > et %&& itermax < 100 %also good to add max. iteration
    % compute next estimate for the root
    % and evaluating the solution
    it=it+1;
    xr=(xl+xu)/2;
    tol = abs(f(xr));
    % adjust the new bounds
    % first check in which range there is a change of sign
    if f(xl)*f(xr) < 0
        xu=xr; 
    else
        xl=xr; 
    end
end
end