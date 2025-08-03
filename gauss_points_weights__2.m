function [x_n, w_n] = gauss_points_weights(alpha_n, beta_n, int_w)
%
% Compute Gaussian quadrature points and weights using the method
% of Golub/Welsh (Math. Comp. 23, 1969).
%
% The method uses the three term recurrence relation for the
% orthogonal polynomials q_n underlying the quadrature rule
%
% q_{n+1}(x) = ( x - alpha_n ) q_n(x) - beta_n^2 q_{n-1}(x)
% 
% with inital values q_{-1}(x) = 0, q_0(x) = 1.
%
% The final parameter is int_w = \int_a^b \omega(x) dx for the weight function \omega.
%
% The function returns a row vector x_n of quadrature points and a row vector w_n of
% quadrature weights, both of length N+1.

J = diag(alpha_n) + diag(beta_n,1) + diag(beta_n,-1);

[V,D] = eig(J);

x_n = diag(D).';
w_n = abs(V(1,:)).^2 * int_w; 

end