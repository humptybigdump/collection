% N = 2^nmax is the maximum input vector length for which we can compute
% the DFT using one of the generated matrices
nmax = 10;

W = cell(1, nmax);
W{1} = exp(-1j*2*pi*0/1);

for n = 1:nmax
    % current DFT order
    N = 2^n;
    
    % permutation matrix
    Pn = [upsample(eye(N/2), 2, 0).'; upsample(eye(N/2), 2, 1).'];
    % Two-N/2-DFT matrix
    Fn = [W{n} zeros(N/2); zeros(N/2) W{n}];
    
    % Twiddle-Factor matrix
    Tn = [eye(N/2) diag(exp(-1j*2*pi*(0:N/2-1)/N)); eye(N/2) -diag(exp(-1j*2*pi*(0:N/2-1)/N))];
    
    % N-point DFT matrix
    W{n+1} = Tn*Fn*Pn;
end