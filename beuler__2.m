function [t,y]=beuler(fp,t,yo)
%
% Backward Euler scheme
% inputs:
%   fp = the nonlinear function to be solved
%   t = [to t1 t2 .... tn] i.e. values of the independent variable
%   yo = initial value(s) of y, must be a row vector
%
% outputs:
%   t = vector of independent variable (in our example time)
%   y = vector / matrix of y(t) (our numerical solution)
%

% allocate matrix y beforehand to speed-up computation
y = zeros(length(yo),length(t)); 

% fill first column of y with the initial condition(s)
y(:,1)=yo;

% compute timestep dt
dt = t(2)-t(1);

% compute next y by solving 
% the nonlinear equation fp 
% using the built-in fzero function
for i = 1:length(t)-1
    y(:,i+1)=fzero(@(yp) fp(t(i+1),yp,y(:,i),dt),y(:,i));
end
end
