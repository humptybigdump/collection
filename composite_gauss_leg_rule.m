function [x, w] = composite_gauss_leg_rule(N, M, a, b)
%
% Compute the quadrature nodes and weights of a composite Gauss-Legendre rule on the interval (a,
% b). The interval is subdivided into N subintervals of equal length. On each subinterval, an M+1
% point Gauss-Legendre rule is used.
% 
% Inputs:
%  a   starting point of the interval [a, b].
%  b   ending point of the interval [a, b].
%  N   number of subintervals.
%  M   M+1 point Gauss-Legendre rule on each subinterval.
%
% Output:
%  x, w   nodes and weights are row vectors of length N*(M+1) containing the quadrature points and 
%         the corresponding weights.
%

% length of subintervals
h = (b - a) / N;

% Gauss-Legendre rule on (0, h)
[t_k, w_k] = gauss_legendre(M, 0, h);

x = zeros(1, N*(M+1));
w = zeros(1, N*(M+1));

for j=1:N
    range = (j-1)*(M+1) + (1:M+1);
    x(range) = a + (j-1)*h + t_k;
    w(range) = w_k;
end

end