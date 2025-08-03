function [t, W] = quad_periodic_log(N)
%
% This function implements the quadrature rule based on interpolation by trigonometric polynomials
% for integral operators of the type
%
% A phi(t) = int_{-pi}^pi 1/(2 pi) log( 4 sin^2 (t-t')/2 ) K(t, t') phi(t') dt'
%
% with continuous and 2pi-periodic functions K and phi.
%
% Given N, we use the quadrature points
%
% t_k = -pi + k pi/N, k = 1, ...., 2N.
%
% Define R_k(t) = int_{-pi}^pi 1/(2 pi) log( 4 sin^2 (t-t')/2 ) L_k(t') dt'.
%
% Then A_N phi(t) = \sum_k R_k(t) K(t, t_k) phi(t_k). To discretize the operator A_N in a Nyström
% method, one needs to evaluate this at t = t_j, hence the values
% 
% R_k(t_j) = -1/N ( sum_{m=1}^{N-1} 1/m cos(m (j-k) pi / N) + (-1)^{j-k}/(2N)  )
% 
% are required. As is obvious from the formula, these form a symmetric Toeplitz matrix.
%
% The function returns the vector t = [t_1, ..., t_{2N}] als well as the matrix W = (R_k(t_j)).
%

% vector of quadrature points including BOTH endpoints of [-pi, pi].
t = linspace(0,2*pi,2*N+1);

% compute the first row R of the symmetric Toepliz matrix W.
R = (-1).^(2:2*N+1) / (2*N);
for m = 1:N-1
    R = R + 1/m * cos(m * t(1:end-1));
end
R = -1/N * R;
W = toeplitz(R);

% set qudrature points, remove the end point -pi from the vector t.
t = linspace(-pi, pi, 2*N+1);
t = t(2:end);

end