%% Script to check if the seismic noise follows a normal distribution by applying the chi-square test

%% load the data
load('D3D.mat');

%% work on first column only
Data = D3D(:,1);

%% plot default histogram
histogram(Data);

%% calculate the goodness-of-fit with a Gaussian distribution
[h, p, stats] = chi2gof(Data);

%% compute the chi square value

% one way is to use a for loop
tic
chitwo=0;
for ii=1:length(stats.O)
    chitwo = chitwo + (stats.O(ii)-stats.E(ii))^2/stats.E(ii);
end
toc
disp(chitwo);

% second way is to use vector multiplication
tic
chitwo2 = ((stats.O-stats.E)./stats.E)*(stats.O-stats.E)';
toc
disp(chitwo2);

% vector multiplication is at least twice faster