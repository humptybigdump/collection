function[t,y]=ab2(f,t,yo)
%
% 2nd order Adams-Bashforth
%
% inputs:
%   f = ode function to be solved (f = f(t,y)= dy/dt)
%   t = [to t1 t2 .... tn] i.e. values of the independent variable
%   yo = initial values of y (yo = y(0)); ab2 needs two initial values !!
%
% outputs:
%   t = vector of independent variable (in our example time)
%   y = vector of y(t) (our numerical solution)
%
y = zeros(size(yo,1),length(t)); % allocating the y beforehand speeds up computation
y(:,1:2)=yo;

% compute timestep dt
dt = t(2)-t(1);

%
for i = 2:length(t)-1
    slope(:,1)=1.5 * f(t(i),   y(:,i)) - ...
               0.5 * f(t(i-1), y(:,i-1));
    y(:,i+1) = y(:,i) + dt*slope;
end
%
end