% program to demonstrate time aggregation bias in consumption data

% choose mean, standard deviation of consumption and dividend growth
mu  = 0.018;
sig = 0.036;

sig_d = 0.12;
corr  = 0.5;

years = 10000;
dt    = 1/100;

% generate random numbers
eps_c = randn(years/dt,1);
eps_d = randn(years/dt,1);

% simulate consumption and dividend growth
dLogC = mu*dt + sig*sqrt(dt)*eps_c;
dLogD = mu*dt + sig_d*sqrt(dt)*(corr*eps_c + sqrt(1-corr^2)*eps_d);

dLogC_sum = cumsum(dLogC);
dLogD_sum = cumsum(dLogD);

% level of consumption
C = exp(dLogC_sum);

% log returns
dLogD_sum_an = dLogD_sum((1/dt):(1/dt):end);
R_annual = dLogD_sum_an(2:end)-dLogD_sum_an(1:end-1);


% CORRECT
C_end_of_year = C((1/dt):(1/dt):end);
dLogC_annual = log(C_end_of_year(2:end)) - log(C_end_of_year(1:end-1));


% TIME AGGREGATION
C_aggregated = sum(lagmatrix(C,0:(1/dt)-1),2);
C_aggregated = C_aggregated((1/dt):(1/dt):end);

dLogC_aggr = log(C_aggregated(2:end)) - log(C_aggregated(1:end-1));


% 4th QUARTER OVER 4th QUARTER
C_4th_quarter = sum(lagmatrix(C,0:(1/(4*dt)-1)),2);
C_4th_quarter = C_4th_quarter((1/dt):(1/dt):end);

dLogC_4qtr = log(C_4th_quarter(2:end)) - log(C_4th_quarter(1:end-1));


% DISPLAY RESULTS
disp(' ');
disp('    Mean(g)   Std(g)   corr(g,R)');

% true consumption moments:
disp('True moments:')
CC = corrcoef(dLogC_annual,R_annual);
disp([mean(dLogC_annual), std(dLogC_annual), CC(1,2)]);

% time aggregated consumption moments:
disp('Time-aggregated moments:');
CC = corrcoef(dLogC_aggr,R_annual);
disp([mean(dLogC_aggr), std(dLogC_aggr), CC(1,2)]);

% Q4/Q4 consumption moments:
disp('Q4/Q4 moments:');
CC = corrcoef(dLogC_4qtr,R_annual);
disp([mean(dLogC_4qtr), std(dLogC_4qtr), CC(1,2)]);

