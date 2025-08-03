function [x, w] = composite_trapezoidal_rule(N, a, b)
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

h = (b - a) / N;

x = linspace(a, b, N+1);
w = h * ones(size(x));
w(1) = h/2;
w(end) = h/2;

end