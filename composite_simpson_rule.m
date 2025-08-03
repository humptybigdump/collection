function [x, w] = composite_simpson_rule(N, a, b)
%
% Compute the quadrature nodes and weights of the N+1 point composite trapezoidal rule for the
% interval [a, b].
% 
% Inputs:
%  a   starting point of the interval [a, b].
%  b   ending point of the interval [a, b].
%  N   N+1 points on interval [a, b].
%
% Output:
%  x, w   nodes and weights are row vectors of length N+1 containing the quadrature points and the 
%         corresponding weights.
%

assert(mod(N, 2) == 0, 'For the composite Simpson rule, the parameter N must be an even number.');

h = (b - a) / N;

x = linspace(a, b, N+1);

% this is the weight for all odd indeces 3, 5, ..., N-1
w = 2*h/3 * ones(size(x)); 

% this is the weight for all even indeces 2, 4, ..., N
w(2:2:end) = 4*h/3;

% value at end points a, b is h/3.
w(1) = h/3;
w(end) = h/3;

end
