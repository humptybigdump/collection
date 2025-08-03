function [alpha_n, beta_n] = mod_cheb_alg(a_n, b_n, nu_n)
%
% Compute the coefficients alpha_n, beta_n used in the three term recurrence relation for a family
% of orthogonal polynomials
% 
% q_{n+1}(x) = ( x - alpha_n ) q_n(x) - beta_n^2 q_{n-1}(x)
% 
% with inital values q_{-1}(x) = 0, q_0(x) = 1.
%
% The algorithm uses the recurrence coefficients a_n, b_n for a different sequence of polynomials 
% and the moments nu_n of these polynomials according to the modified Chebychev algorithm
% (See Gautschi SIAM J. SCI. STAT. COMPUT. 3, 1982, for an exposition).
%
% The vectors a_n, b_n and nu_n should all have length 2*N (n = 0,...,2*N-1) if the 
% first N coefficients (n = 0,...,N-1) are to be computed.

n = length(nu_n)/2;

sigma_k_min_1 = zeros(1, 2*n);
sigma_k = nu_n;
sigma_k_plus_1 = zeros(1, 2*n);
alpha_n = zeros(1, n);
beta_n = zeros(1, n);

alpha_n(1) = a_n(1) + nu_n(2) / nu_n(1);
beta_n(1) = nu_n(1);

for k=1:n-1
    l_range = k+1:2*n-k;
    sigma_k_plus_1(l_range) = ...
        sigma_k(l_range+1) ...
        - (alpha_n(k) - a_n(l_range)) .* sigma_k(l_range) ...
        + b_n(l_range).^2 .* sigma_k(l_range-1) ...
        - beta_n(k) * sigma_k_min_1(l_range);

    alpha_n(k+1) = a_n(k+1) + sigma_k_plus_1(k+2) / sigma_k_plus_1(k+1) - sigma_k(k+1) / sigma_k(k);
    beta_n(k+1) = sigma_k_plus_1(k+1) / sigma_k(k);

    sigma_k_min_1 = sigma_k;
    sigma_k = sigma_k_plus_1;
end

beta_n = sqrt(beta_n(2:end));
end