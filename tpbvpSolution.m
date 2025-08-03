%% Parameters
global isConstrained
isConstrained = false;
tf = 2;
m = 1;
x0 = [0; 0];
x1f = 4;

%% Initial guess for bvp4c
tInit = linspace(0,tf,100);
% make a quadratic fit for first state fulfilling the boundary conditions
% x1(t) = a*t^2 + b*t + c
% x2(t) = dx1dt =  2*a*t + b
a = (x1f-x0(1))/(tf^2);
b = 0;
c = x0(1);
pInit = zeros(4,1); % [lambda_1(t_0),lambda_2(t_0),x_2(t_f),mu].'
solinit = bvpinit(tInit,@(t_)initialGuess(t_,a,b,c),pInit);

%% Solve the TPBVP
sol = bvp4c(@(t_,y_,p_)tpbvpRightHandSideFun(t_,y_,p_,m),... % odefun
            @(yt0_,ytf_,p_)bcsFun(yt0_,ytf_,p_,x0,x1f),...   % bcfun
            solinit);                                        % solinit

uOptBvp4c=-1/(2*m)*sol.y(4,:);
if isConstrained
    disp('Constrained optimal control')
    uOptBvp4c(uOptBvp4c <= -0.5) = -0.5;
    uOptBvp4c(uOptBvp4c >= 0.5) = 0.5;
end
% simulate the system using ode45 with optimal input (might differ from the bvp4c solution due to numerical inaccuracies)
[tSim,xSim] = ode45(@(t_,x_)pointMassRightHandSideFun(t_,x_,struct('time',sol.x,'data',uOptBvp4c),m),0:1e-2:tf,x0);

%% Plot
figure
subplot(5,1,1)
plot(tSim,xSim(:,1), 'LineWidth', 2)
grid on
ylabel('x_1')
subplot(5,1,2)
plot(tSim,xSim(:,2), 'LineWidth', 2)
grid on
ylabel('x_2')
subplot(5,1,3)
plot(sol.x,sol.y(3,:), 'LineWidth', 2)
grid on
ylabel('\lambda_1')
subplot(5,1,4)
plot(sol.x,sol.y(4,:), 'LineWidth', 2)
grid on
ylabel('\lambda_2')
subplot(5,1,5)
stairs(sol.x,uOptBvp4c, 'LineWidth', 2)
grid on
ylabel('u^*')
xlabel('Time in s')

%% Local functions
function yGuess_ = initialGuess(t_,a_,b_,c_)
    yGuess_ = [a_*t_.^2 + b_*t_ + c_; 
               2*a_*t_ + b_;
               0;
               0];
end

function boundaryCondition_ = bcsFun(yt0_,ytf_,p_,x0_,x1f_)
    % This function evaluates the boundary condition for the bvp4c solver based
    % on the initial and final states (if known/present) and parameters that 
    % are calculated in the process.
    % inputs: yt0_ ... y(t_0) with y(t_0) = [x(t_0).', lambda(t_0).']
    %         ytf_ ... y(t_f) with y(t_f) = [x(t_f).', lambda(t_f).']
    %         p_   ... unknown parameters p_= [lambda_1(t_0), lambda_2(t_0), x_2(t_f), mu].'
    %         x0_  ... initial state (user defined)
    %         x1f_ ... final position (user defined)
    % output: boundaryCondition_ ... boundary condition for bvp4c
    
    %
    % unknowns
    % lambda_1(t_0) and lambda_2(t_0)
    lambda0 = p_(1:2);
    % x_2(t_f)
    x2f = p_(3);
    % (time-invariant) adjoint variable associated with final constraint psi(t_f,x(t_f)) = 0
    mu = p_(4);
    % y_0
    y0 = [x0_;
          lambda0];
    % y_f
    yf = [x1f_;
          x2f;
          mu;
          2*x2f];
    % boundary conditions
    boundaryCondition_ = [yt0_-y0;
                          ytf_-yf];
end

function dydt_ = tpbvpRightHandSideFun(t_,y_,p_,m_)
    % This function evaluates the right-hand-side of the two-point boundary 
    % value problem (Euler Lagrange Equations or canonical equations) for the 
    % point mass. Note that you should make use of the pointMassRightHandSide 
    % function in here to  avoid code duplication.
    % inputs: t_ ... time
    %         y_ ... canonical state vector [x_1, x_2, lambda_1, lambda_2], position, velocity, costate
    %         p_ ... unknown parameters p_= [lambda_1(t_0), lambda_2(t_0), x_2(t_f), mu].'
    %         m_ ... mass
    % output: dydt_ ... time derivative of canonical state vector 
    %
    % system state
    global isConstrained
    xStar = y_(1:2);
    % costate
    lambdaStar = y_(3:4);
    % optimal control
    uStar = -1/(2*m_)*lambdaStar(2);
    if isConstrained
        if uStar <= -0.5
            uStar = -0.5;
        elseif uStar >= 0.5
            uStar = 0.5;
        end
    end
    % time derivative of system state with optimal control
    dxStardt = pointMassRightHandSideFun(t_,xStar,uStar,m_);
    % time derivative of costate
    dlambdaStardt = -[2*xStar(1);
                      lambdaStar(1)];
    % time derivative of canonical state vector
    dydt_ = [dxStardt;
             dlambdaStardt];
end

function dxdt_ = pointMassRightHandSideFun(t_,x_,u_,m_)
    % A function that evaluates the right-hand-side of the point mass system.
    % inputs: t_ ... time
    %         x_ ... state vector [x_1,x_2], position, velocity
    %         u_ ... control force as scalar or struct with fields "time" and "data"
    %         m_ ... mass of the point
    % output: dxdt_ ... time derivative of state vector
    %
    if isstruct(u_)
        u = interp1(u_.time,u_.data.',t_,'previous','extrap').';
    else
        u = u_;
    end
    A = [0 1;
         0 0];
    b = [0;1/m_];
    dxdt_ = A*x_ + b*u;
end
