% This is a script

% read input data
load('D3D.mat');

% make default histogram
histogram(D3D(:,1));

% will overlay another histogram
hold on;

% give the bin size according to Scott's rule and plot again
histogram(D3D(:,1), 'BinWidth', 3.49*std(D3D(:,1))/size(D3D,1)^(1/3));

% put legend
legend('auto', 'Scott');

% fit the 1st column data with a normal distribution
pd=fitdist(D3D(:,1),'Normal');

% compare mean and std with those from pd
mean(D3D(:,1));
std(D3D(:,1));

% make a new graph and overlay histogram represented in pdf with the pd
% distribution data

figure();
histogram(D3D(:,1),...
    'BinWidth', 3.49*std(D3D(:,1))/size(D3D,1)^(1/3),...
    'Normalization', 'pdf');

hold on;
plot(-10:0.1:30, pdf(pd, -10:0.1:30), 'k');
