clear all
close all

% Starting value for iterations
xStart = [-1 -1]';

% Pass problem function handles to struct
handles.fFun     = @rosen;
handles.gradfFun = @gradRosen;

% Line search parameters
% general parameters
param.maxIter = 1500;   % maximum number of iterations
param.tol     = 1e-5;   % stopping criterion for norm of step
param.xStart  = xStart; % starting point for x (initial guess)
% search direction parameter
param.sk.method = 'steepestDescent'; 
% step length method
param.alphak.method = 'wolfe';
param.alphak.wolfe.epsilon0 = .7;
param.alphak.wolfe.alpha0   = 5;
param.alphak.wolfe.rho      = .8;

% Execute line search
res = lineSearch(handles, param)

% Plotting
% function handle for easy plotting
rosenPlot=@(x1,x2) (100.0 * (x2 - x1.^2).^2 + (x1 - 1).^2);

function f_ = rosen(x_)
  f_ =  (100*(x_(2) - x_(1)^2)^2 + (x_(1) - 1)^2);
end

function gradf_ = gradRosen(x_)
  gradf_ =  [-400*(x_(2) - x_(1)^2)*x_(1) + 2*(x_(1) - 1);
              200*(x_(2) - x_(1)^2)];
end 
