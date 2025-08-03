function [x, w] = gauss_legendre(N, a, b)
%
% Computes an N+1 point Gauss-Legendre rule on the interval (a, b).
%
% The recurrence relation for Legendre polynomials on (-1, 1) is
% 
% q_{n+1}(x) = x q_n(x) - n^2 / (2n-1) / (2n+1) q_{n-1}(x)
%
% Thus we have alpha_n = 0, beta_n = n / ((2n-1)*(2n+1))^{1/2}.
%
% For standard Gauss-Legendre quadrature, the integration interval is (-1, 1), the 
% weight function is omega = 1, thus int_w = 2.
%
% Finally the points need to be translated and the weights to be scaled for the interval
% (a, b).
%

% Compute coefficients of 3 term recurrence relation
n = 1:N;
alpha_n = zeros(1, N+1);
beta_n = n ./ sqrt((2*n-1) .* (2*n+1));

% Compute points/weights for interval (-1, 1)
[x, w] = gauss_points_weights(alpha_n, beta_n, 2);

% Scale for (a, b)
x = a + (b-a) / 2 * (1 + x);
w = (b-a)/2 * w;
end