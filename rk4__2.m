
function[t,y]=rk4(f,t,yo)
%
% 4-stage Runge-Kutta method 
% this routine can be used for multiple ode(s)
%
% inputs:
%   f = ode function(s) to be solved (f = f(t,y)= dy/dt)
%   trange = [to t1 t2 .... tn] i.e. values of the independent variable
%   yo = initial value(s) of y, must be a row vector
%
% outputs:
%   t = vector of independent variable (in our example time)
%   y = vector (or matrix if solving a system of ode-s) of y(t)
%       (i.e. our numerical solution(s))
%

% compute timestep dt
dt= t(2)-t(1);

% allocate y beforehand to speed up computation!!!
y = zeros(length(yo),length(t)); 

% fill first column of y with the initial condition(s)
yo=yo(:); % ensure it is a column vector
y(:,1)=yo;
%
% perform Runge-Kutta
for i = 1:length(t)-1
    % Calculate the four stages
    K1 = f(t(i),        y(:,i));
    K2 = f(t(i)+0.5*dt, y(:,i)+0.5*dt*K1);
    K3 = f(t(i)+0.5*dt, y(:,i)+0.5*dt*K2);
    K4 = f(t(i)+dt,     y(:,i)+dt*K3);
    
    % Evaluate solution(s) at t(i+1)
    y(:,i+1) = y(:,i) + dt/6 * (K1 + 2*K2 + 2*K3 + K4)';
end
%
end