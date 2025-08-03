%% Exercise 4: Extreme value statistics. Water and Energy Cycles 24/25
%% Task 1: Import of data
% read in text file with year and HQ value as table
table_data = readtable('data\HQ_Elbe_Extreme.txt');

% define separate arrays for HQ and year extracted from table
HQ = table2array(table_data(:,{'HQ'}));
year = table2array(table_data(:,{'year'}));

% plot of HQ time series 
figure
plot(year, HQ, 'o');
xlabel('year');
ylabel('HQ [m/s]');
title('HQ time series')

% basic statistics of HQ time series (functions: max, min, median)
max_HQ = max(HQ)
date_max_HQ = year(HQ == max_HQ)

min_HQ = min(HQ)
date_min_HQ = year(HQ == min_HQ)

median_HQ = median(HQ)

%% Task 2: Generate and plot a histogram of HQ data
% giving the left and right edges of all bins (= 46) with a bin size of 100.
bin_edges = 0:100:max(HQ); 

% plot histogram
figure
histogram(HQ,bin_edges)
xlabel('HQ bins/classes (m/s)')
ylabel('counts of HQ values in bins/classes (-)')
title('Histogram of HQ data')

%% Task 3: Generate and plot an empirical, cumulative distribution function (ecdf)
% sort HQ values from small to large values ("sorted_HQ"), in the following
% tasks always use the sorted HQ values.
sorted_HQ = sort(HQ);

% calculate frequencies of observed HQ values; each sorted HQ value gets a certain frequency
% , e.g. smallest HQ value gets the frequency = 1/N , and the highest HQ value gets the frequency = N/N (= 1).
N = length(sorted_HQ); % total number of HQ values
PE = [1:N] / N; % frequencies of observed values 

% plot cdf
figure
plot(sorted_HQ, PE, 'o')
title('Empirical, cumulative distribution function of HQ values')
xlabel('HQ (m3/s)')
ylabel('frequencies (-)')

%% Task 4: Generate theoretical, cumulative distribution function (tcdf) with "Normal","Gamma","Weibull" distribution
% Normal distribution (no approximation, uses mean and STD of HQ data)
normal_cdf_fit = fitdist(sorted_HQ,'Normal'); % function to fit a normal cdf to sorted HQ data to find mean and standard deviation as input for generating a normal cdf
                                              % look into the created object and its parameter values


sorted_HQ_mean = normal_cdf_fit.mu; % mean as first input parameter for creating normal cdf
sorted_HQ_STD = normal_cdf_fit.sigma;  % standard deviation as second input parameter for creating normal cdf

normal_cdf = cdf('Normal',sorted_HQ,sorted_HQ_mean,sorted_HQ_STD); % creating normal cdf

% Gamma distribution (maximum likelihood approximation to generate a and b
% parameter)
gamma_cdf_fit = fitdist(sorted_HQ,'Gamma'); % function to fit a Gamma cdf to HQ data to find a and b parameter as input for generating a Gamma cdf
                                            % look into the created object and its parameter values                                            

a_Gamma = gamma_cdf_fit.a; % a parameter as first input parameter for creating Gamma cdf
b_Gamma = gamma_cdf_fit.b;  % b parameter as first input parameter for creating Gamma cdf

gamma_cdf = cdf('Gamma',sorted_HQ,a_Gamma,b_Gamma); %creating normal cdf

% Weibull distribution(maximum likelihood approximation to generate a and b
% parameter)
weibull_cdf_fit = fitdist(sorted_HQ,'Weibull'); % function to fit a Weibull cdf to HQ data to find a and b parameter as input for generating a Weibull cdf
                                            % look into the created object and its parameter values
                                            
a_Weibull = weibull_cdf_fit.a; % a parameter as first input parameter for creating Weibull cdf
b_Weibull = weibull_cdf_fit.b;  % b parameter as first input parameter for creating Weibull cdf

weibull_cdf = cdf('Weibull',sorted_HQ,a_Weibull,b_Weibull); %creating Weibull cdf

% plot all three tcdf, together with the ecdf for comparison
figure
plot(sorted_HQ, PE,'o');  %ecdf
xlabel('HQ [m/s]')
ylabel('frequencies')
hold on
plot(sorted_HQ, normal_cdf,'r','LineWidth',1.5) % normal tcdf
plot(sorted_HQ, gamma_cdf,'g','LineWidth',1.5) % gamma tcdf
plot(sorted_HQ, weibull_cdf,'k','LineWidth',1.5) % Weibull tcdf
title('Fit of three tcdf to ecdf')
legend('ecdf','normal tcdf','gamma cdf','weibull cdf')

%% Task 5: Compute flood return periods RP
% use Eq. 2 to calculate RP based on ecdf and the three tcdf
RP_empirical_cdf = (1./(1-PE));
RP_normal_cdf = 1./(1-normal_cdf); 
RP_gamma_cdf = 1./(1-gamma_cdf);
RP_weibull_cdf = 1./(1-weibull_cdf);

% plot of return periods
figure
plot(sorted_HQ, RP_empirical_cdf, 'o')
set(gca, 'YScale', 'log')
xlabel('HQ (m/s)')
ylabel('return period [a]')
hold on
plot(sorted_HQ, RP_normal_cdf, 'r')
plot(sorted_HQ, RP_gamma_cdf, 'g')
plot(sorted_HQ, RP_weibull_cdf, 'k')
yline(1000)
legend('ecdf','normal tcdf','gamma tcdf','weibull tcdf','HQ 1000')