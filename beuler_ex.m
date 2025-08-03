function [t,y]=beuler_ex(fp,t,yo)

% Backward Euler scheme for cases when ...
% explicit expression is available. 
% NOTE : typically, when using backward euler
%        we need to solve nonlinear equation(s) 
%        cf. beuler.m
%
% inputs:
%   fp = the explicit function to be solved
%   t = [to t1 t2 .... tn] i.e. values of the independent variable
%   yo = initial value(s) of y, must be a row vector
%
% outputs:
%   t = vector of independent variable (in our example time)
%   y = vector of y(t) (our numerical solution)
%

% compute timestep dt
dt = t(2)-t(1);

% allocate matrix y beforehand to speed-up computation
y = zeros(length(yo),length(t)); 

% fill first column of y with the initial condition(s)
y(:,1)=yo;

% compute next y
% using the explicit expression fp
for i = 1:length(t)-1
    y(:,i+1)=fp(t(i),y(:,i),dt);
end
end
