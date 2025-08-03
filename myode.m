function [t,x] = myode(a,b)

% ODE: 
% xdot = f(t,x) = [a*x(1)*x(2)*sin(x(1)); b*exp(-x(2))]
% x(0) = [1;1];

%Solve the ODE

    x0 = [1;1];
    tspan = 0:0.01:5.0;
    p.a = a;
    p.b = b; 
    
    [t,x] = ode45(@(t,x)rhs(t,x,p),tspan,x0);
    
end


function xdot = rhs(t,x,p)
    xdot = [p.a*x(1)*x(2)*sin(x(1)); p.b*exp(-x(2))];
end
