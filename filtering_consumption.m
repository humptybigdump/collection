% program to demonstrate impact of filtering consumption data

% choose mean, standard deviation of consumption and dividend growth and the noise term
mu  = 0.018;
sig = 0.036;

sig_n = 0.06; % noise

sig_d = 0.12;
corr  = 0.5;

nu = sig^2/(sig^2+sig_n^2); % filter parameter suggested by Kroencke (2016)

years = 10000;
dt    = 1;

% generate random numbers
eps_c = randn(years/dt,1);
eps_d = randn(years/dt,1);
eps_n = randn(years/dt,1);

% simulate consumption and dividend growth and the noisy signal
dLogC = mu*dt + sig*sqrt(dt)*eps_c;
dLogD = mu*dt + sig_d*sqrt(dt)*(corr*eps_c + sqrt(1-corr^2)*eps_d);
dLogY = dLogC + sig_n*sqrt(dt)*eps_n;

% filter consumption data
dLogF = nan(years/dt,1);
dLogF(1) = dLogY(1);
for t = 2:length(dLogF)
    dLogF(t) = dLogF(t-1) + nu*(dLogY(t) - dLogF(t-1));
end

% display results
disp(' ');
disp('    Mean(g)   Std(g)   corr(g,R)');
% true consumption
disp('True consumption growth:')
CC = corrcoef(dLogC,dLogD);
disp([mean(dLogC),std(dLogC),CC(1,2)]);
% filtered consumption 
disp('Filtered consumption growth:')
CC = corrcoef(dLogF,dLogD);
disp([mean(dLogF),std(dLogF),CC(1,2)]);