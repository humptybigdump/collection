%% load data
load('D3D.mat');

%% make the histograms: 3 different ones depending on the bin-size rule

% default histogram first
% we will use subplots to compare them
subplot(1,3,1);
histogram(D3D(:,1), 'Normalization','pdf');
title('Default bin size');
grid on;

% histogram using Scott's bin size rule
subplot(1,3,2);
histogram(D3D(:,1), 'BinMethod','scott', 'Normalization','pdf');
title('Scott''s rule for bin size');
grid on;

% histogram using Freedman-Diaconis' bin size rule
subplot(1,3,3);
histogram(D3D(:,1), 'BinMethod','fd', 'Normalization','pdf');
title('Freedman-Diaconis'' rule for bin size');
grid on;
% hh = histogram(D3D(:,1), 'BinMethod','fd');

%% fit the data with a normal distribution
pd = fitdist(D3D(:,1),'Normal');

% estimate pdf of pd on the range of data we have
Xpdf = linspace(min(D3D(:,1)), max(D3D(:,1)), 100);
Ypdf = pdf(pd, Xpdf);

% overlay the previous (sub)plot
hold on;
plot(Xpdf, Ypdf, 'r', 'LineWidth',3);

%% compare data mean and standard deviation with those given by the fit
d3dmu=mean(D3D(:,1));
d3dstd=std(D3D(:,1));

fprintf(1, 'Mean from fit = %f and from data = %f\n',...
    pd.mu, d3dmu);
fprintf(1, 'Standard deviation from fit = %f and from data = %f\n',...
    pd.sigma, d3dstd);