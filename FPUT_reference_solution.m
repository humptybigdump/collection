function [x,y] = FPUT_reference_solution(x0,y0,w,t_end)
        % initial vector
        v0 = [x0; y0];
        % apply ode45
        odefun = @(t,v) dt_xy(v,w);
        opts = odeset('RelTol',1e-10,'AbsTol',1e-12);
        [~,v] = ode45(odefun, [0, t_end], v0,opts);
        % extract solution at time t_end
        v = transpose(v);
        x = v(1:6,end); y = v(7:12,end);
end

function [dvdt] = dt_xy(v,w)
    x = v(1:6); y = v(7:12);
    % axuiliary variables
    y1 = (x(1)-x(4))^3;             % (x_{0,1} - x_{1,1})^3
    y2 = (x(2)-x(5)-x(1)-x(4))^3;   % (x_{0,2} - x_{1,2} - x_{0,1} - x_{1,1})^3
    y3 = (x(3)-x(6)-x(2)-x(5))^3;   % (x_{0,3} - x_{1,3} - x_{0,2} - x_{1,2})^3
    y4 = (x(3)+x(6))^3;             % (x_{0,3} - x_{1,3})^3
    % nonlinearity
    nonlin = - [ y1 - y2;
                 y2 - y3;
                 y3 + y4;
                -y1 - y2;
                -y2 - y3;
                -y3 + y4 ];
    % right hand side of ODE (as first order system) at time point v=(x,y)^T
    dvdt = zeros(12,1);
    dvdt(1:6) = y;
    dvdt(7:12) = [zeros(3,1);-w^2*x(4:6)] + nonlin;
end
