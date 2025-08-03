% Script for testing convergence rates of quadrature rules.
%
% A sequence of rules of the same type is applied to compute the same integral.
% The number of quadrature points is doubled each time. The results are used to
% obtain an estimated order of convergence.

% The test is carried out for the following function f. Any function can be
% used here that for some input vector returns a result vector of the same length.
% f = @(t) exp(sin(t));
%f = @(t) log(1.01 - t) .* exp(t);
f = @(t) exp(t);

% Interval end points
a = 0;
b = 1;

% Assign the quadrature rule to be used to a function handle. The rule function
% must return two vectors of equal length: the first contains the quadrature points,
% the second one the correspoinding weights.
%quadRule = @(N) composite_trapezoidal_rule(N, a, b);
%quadRule = @(N) composite_simpson_rule(N, a, b);
%quadRule = @(N) gauss_legendre(N, a, b);
%quadRule = @(N) composite_gauss_leg_rule(N, 3, a, b);
%quadRule = @(N) prodquad_log(N, 0, 0, 1);
quadRule = @(N) gauss_log(N);

% Initial number of quadrature points and the number of doublings.
%N0 = 4;
%num_double = 6;
N0 = 1;
num_double = 5;

% ------------------------------------------------------------------------
% from here on, the script does not need to be changed

% Compute numbers of quadrature points
N = N0 * 2.^(0:num_double);

% Initialise result vectors
I = zeros(size(N));
Q2n_min_Qn = NaN(size(N));
eoc = NaN(size(N));

% Print result table header
fprintf('\n   N           I         | Q(N) - Q(2N) |       EOC\n');
fprintf('----------------------------------------------------------\n');


for j=1:length(N)
    
    % Determine quadrature points and weights
    [x, w] = quadRule(N(j));
    
    % Approximation of the integral
    I(j) = w * f(x).';
    
    % Approximation of the error and convergence rate
    if (j > 1)
        Q2n_min_Qn(j-1) = abs( I(j) - I(j-1) );
    end    
    if (j > 2)
        eoc(j-2) = log( Q2n_min_Qn(j-2) / Q2n_min_Qn(j-1) ) / log(2);
    end
end

% Print table rows
for j=1:length(N)
    fprintf(' %4d   %14.10f   %14.10f   %14.10f\n', N(j), I(j), Q2n_min_Qn(j), eoc(j));
end
fprintf('\n\n');
