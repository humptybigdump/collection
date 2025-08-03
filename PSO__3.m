

%fun = @(x) -1/2*x(1)^2+x(2);

%Rastrigin function 2D
fun = @(x) 10*2+x(1)^2-10*cos(2*pi*x(1))+x(2)^2-10*cos(2*pi*x(2));

lb = [-5.12,-5.12];
ub = [5.12,5.12];

%lb = [-100,-100];
%ub = [100,100];

%options = optimoptions('particleswarm','SwarmSize',20,'HybridFcn',@fmincon);

options = optimoptions('particleswarm','SwarmSize',100,'MaxIterations',200,'FunctionTolerance',0.1,'HybridFcn',[],'PlotFcn','pswplotbestf');


%rng default  % For reproducibility
nvars = 2;
[x,fval,exitflag,output] = particleswarm(fun,nvars,lb,ub,options)


z=10*2+x(1)^2-10*cos(2*pi*x(1))+x(2)^2-10*cos(2*pi*x(2));