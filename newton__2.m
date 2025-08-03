function root = newton(fun, x0, tol, maxiter, dx)
% Newton solver for finding the root of a function f
% f:        function
% x0:       initial value for the variable x 
% tol:      tolerance (optional)
% max_iter: maximum number of iterations (optional)
% dx:       size of the finite element for the numerical derivative (optional)


% Set a standard tolerance in case tol is not passed
if nargin < 3 || isempty(tol)
    tol = 1e-6;
end

% Set a standard maximum number of iterations in case it is not passed
if nargin < 4 || isempty(maxiter)
    maxiter = 1e3;
end

% Set a dx in case it is not passed
if nargin < 5 || isempty(dx)
    dx = 1e-6;
end

% Initialization
x    = x0;
iter = 0;
    
    while iter < maxiter
        
        f  = fun(x);
    
        % Numerical derivative
        df = ( fun(x + dx) - fun(x) ) / dx; 
    
        x_new = x - f/df;
    
        % Check the convergence
        if abs(x_new - x) < tol
            break;
        end 
    
        % Update the values
        x    = x_new;
        iter = iter + 1;
    
    end

    root = x;

end