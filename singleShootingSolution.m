%% Parameters
tf = 2; % final time
m = 1; % mass
x0 = [0; 0]; % initial state
x1f = 4; % x_1(tf) = x_{1f}
dt = 1e-1; % sample time/granularity of solution 
initialGuessStrategy = 'better'; % either 'superStupid', 'stupid', or 'better' 
integrateFun = @(fFun_,tSpan_,x0_)euler1e(fFun_,tSpan_,x0_,dt); % either @ode45 or @(fFun_,tSpan_,x0_)euler1e(fFun_,tSpan_,x0_,dt)

%% Input parameterization
% time vector for input t_u = [t_0,t_1,...,t_{N-1}]
N = tf/dt;
tu = linspace(0,tf-dt,N); % remember the parameterization used (piece-wise constant) has N points at t_0=0, t_1=dt, ..., t_{N-1}=t_f-dt
switch initialGuessStrategy
    case 'superStupid'
        w0 = zeros(1,N);
    case 'stupid'
        w0 = ones(1,N);
    case 'better'
        % interpolate (zero-order hold) from u_0=3 to u_{N-1}=-2
        w0 = interp1(linspace(0,tf,1000),linspace(3,-2,1000),tu,'previous','extrap');
    otherwise
        error('initialGuessStrategy not recognized')
end

% % figure('Name','Initial guess for u')
% stairs(linspace(0,tf,N+1),[w0(:);w0(end)],'LineWidth',2)

%% fmincon call
tic % use this to measure the time it takes to solve the problem
[uOpt,fOpt,exFlag,out,lambda] = fmincon(@(w_)mayerCost(w_,x0,tf,m,integrateFun),... % cost
               w0,... % u_0,u_1,...,u_{N-1}      N in total
               [],[],... % no linear inequalities
               [],[],... % no linear equalities
               [],[],... % no bounds
               @(w_)constraints(w_,x0,x1f,tf,m,integrateFun),... % combined nonlinear (or linear) inequality and equality constraints
               optimoptions('fmincon',...
                            'Display','iter',...
                            'Algorithm','sqp',...
                            'MaxFunctionEvaluations',1e5,...
                            'MaxIter',2500));
toc

% simulate again using ode45 with optimal input
[tOpt,xOpt] = integrateFun(@(t_,x_)pointMassRightHandSideFun(t_,x_,struct('time',tu,'data',uOpt),m),[0,tf],x0);
xOpt = xOpt.'; 
[tOde,xOde] = ode45(@(t_,x_)pointMassRightHandSideFun(t_,x_,struct('time',tu,'data',uOpt),m),0:1e-2:tf,x0);
xOde = xOde.';
               
%% Plot
figure('Name','Optimal solution')
subplot(3,1,1)
plot(tOpt,xOpt(1,:), 'LineWidth', 2)
hold on
plot(tOde,xOde(1,:), 'LineWidth', 2)
grid on
ylabel('x_1')
subplot(3,1,2)
plot(tOpt,xOpt(2,:), 'LineWidth', 2)
hold on
plot(tOde,xOde(2,:), 'LineWidth', 2)
grid on
ylabel('x_2')
subplot(3,1,3)
stairs([tu,tf],[uOpt(:);uOpt(end)], 'LineWidth', 2)
grid on
ylabel('u^*')
xlabel('Time in s')

%% Local (in-script) functions
function cost_ = mayerCost(w_,x0_,tf_,m_,integrateFun_)
    % A function that evaluates the Mayer cost.
    arguments % use the arguments block to validate inputs
        w_            (:,1) double {mustBeVector} % decision variables
        x0_           (:,1) double {mustBeVector} % initial state
        tf_           (1,1) double {mustBeNumeric} % final time
        m_            (1,1) double {mustBeNumeric} % mass of point
        integrateFun_ (1,1) function_handle = @ode45 % integration function 
    end
    N = length(w_);
    dt = tf_/N;
    tu = linspace(0,tf_-dt,N);
    % integrate system over entire horizon
    [~,zOde] = integrateFun_(@(t_,z_)extendedDynamicsRightHandSideFun(t_,z_,...
                             struct('time',tu,'data',w_),m_),...
                             [0,tf_],... % integrate from 0 to tf
                             [x0_;0]); % initial state = z0 = [x0;0], i.e., integrand starts with q(0)=0
    %       x_2(tf)^2      + q(tf)
    cost_ = zOde(end,2).^2 + zOde(end,3); 
end

function [ineqs_,eqs_] = constraints(w_,x0_,x1f_,tf_,m_,integrateFun_)
    % A function that evaluates the constraints.
    arguments % use the arguments block to validate inputs
        w_            (:,1) double {mustBeVector} % decision variables
        x0_           (:,1) double {mustBeVector} % initial state
        x1f_          (1,1) double {mustBeNumeric} % final position
        tf_           (1,1) double {mustBeNumeric} % final time
        m_            (1,1) double {mustBeNumeric} % mass of point
        integrateFun_ (1,1) function_handle = @ode45 % integration function
    end
    N = length(w_);
    tu = linspace(0,tf_-tf_/N,N);
    [~,zOde] = integrateFun_(@(t_,z_)extendedDynamicsRightHandSideFun(t_,z_,...
                             struct('time',tu,'data',w_),m_),...
                             [0,tf_],... % integrate from 0 to tf
                             [x0_;0]); % initial state = z0 = [x0;0], i.e., integrand starts with q(0)=0
    ineqs_ = []; % no inequalities
    % note that these are linear so you could also have expressed them using Aeq*w=beq
    eqs_ = [zOde(1,1:2).'-x0_; % x_1(t_0) = x_{10} and x_2(t_0) = x_{20}
            zOde(end,1)-x1f_]; % x_1(tf) = x_{1f}
end

function dzdt_ = extendedDynamicsRightHandSideFun(t_,z_,u_,m_)
    % A function that evaluates the right-hand-side of the extended system.
    arguments
        t_            (1,1) double {mustBeNumeric} % time 
        z_            (3,1) double {mustBeNumeric} % extended state z=[x',q]
        u_            (:,:) {mustBeNumericOrStruct} % input
        m_            (1,1) double {mustBeNumeric} % mass of point
    end
    if isstruct(u_)
        % interpolate using zero-order hold (needed by ODE45)
        % and extrapolate if necessary (take into account numeric inaccuracies)
        u = interp1(u_.time,u_.data.',t_,'previous','extrap').';
    else
        u = u_;
    end
    dxdt = pointMassRightHandSideFun(t_,z_(1:2),u,m_);
    dqdt = z_(1).^2 + u.^2;
    dzdt_ = [dxdt;dqdt];
end

function dxdt_ = pointMassRightHandSideFun(t_,x_,u_,m_)
    % A function that evaluates the right-hand-side of the point mass system.
    arguments
        t_            (1,1) double {mustBeNumeric} % time
        x_            (2,1) double {mustBeNumeric} % state x = [pos, vel]
        u_            (:,:) {mustBeNumericOrStruct} % input
        m_            (1,1) double {mustBeNumeric} % mass of point
    end
    if isstruct(u_)
        % interpolate using zero-order hold (needed by ODE45)
        u = interp1(u_.time,u_.data.',t_,'previous','extrap').'; 
    else
        u = u_;
    end
    A = [0 1;
         0 0];
    b = [0;1/m_];
    dxdt_ = A*x_ + b*u;
end

function [t_,y_] = euler1e(fFun_,tSpan_,y0_,h_)
    % A function that integrates a system of ODEs with the explicit Euler method.
    arguments
        fFun_         (1,1) function_handle % right-hand-side of ODE
        tSpan_        (1,2) double {mustBeNumeric,mustBeReal} % time span
        y0_           (:,1) double {mustBeNumeric,mustBeReal} % initial condition of IVP
        h_            (1,1) double {mustBeNumeric,mustBeReal} % step size
    end
    t = tSpan_(1):h_:tSpan_(end);
    y = zeros(length(y0_),length(t));
    y(:,1) = y0_;
    for kk = 1:length(t)-1
        yk = y(:,kk);
        tk = t(kk);
        k1 = fFun_(tk,yk);
        y(:,kk+1) = yk + h_*k1;
    end
    if size(tSpan_,2) > 2
        % return interpolated solution. THIS IS PROBABLY NOT ACCURATE
        t_ = tSpan_;
        y_ = interp1(t,y.',tSpan_);
    else
        % return "internal" solution
        t_ = t.';
        y_ = y.';
    end
end

function mustBeNumericOrStruct(u_)
    % Custom validation function for input u_.
    %
    if ~isnumeric(u_) && ~isstruct(u_)
        error('Input must be numeric or struct.')
    end
end
