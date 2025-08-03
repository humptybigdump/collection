function [t,y]=feuler(f,t,yo)
%
% Forward Euler scheme
% this routine can be used for multiple ode(s)
%
% inputs:
%   f = ode function(s) to be solved (f = f(t,y)= dy/dt)
%   t = [to t1 t2 .... tn] i.e. values of the independent variable
%   yo = initial value(s) of y, must be a row vector
%
% outputs:
%   t = vector of independent variable (in our example time)
%   y = vector (matrix) of y(t) (our numerical solution)
%

% compute timestep dt
dt = t(2)-t(1);

% allocate matrix y beforehand to speed-up computation
y = zeros(length(yo),length(t));

% fill first column of y with the initial condition(s)
y(:,1)=yo;

% 
for i = 1:length(t)-1
    y(:,i+1)=y(:,i) + f(t(i),y(:,i))' * dt;
end
%
end