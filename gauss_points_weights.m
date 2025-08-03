function [ x_n, w_n ] = gauss_points_weights( alpha_n, beta_n, int_w )
% function [ x_n, w_n ] = gauss_points_weights( alpha_n, beta_n, int_w )
%

J = diag(alpha_n) + diag(beta_n,1) + diag(beta_n,-1);

[V,D] = eig(J);

x_n = diag(D).';
w_n = abs(V(1,:)).^2 * int_w;

end

