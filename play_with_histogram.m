% load matlab data
load('D3D');
% quick vizualization of the data
plot(D3D);
plot(D3D(:,1)); % 1st column only
% plot (default) histogram
histogram(D3D(:,1));
% compute mean
mean(D3D(:,1))
% compute standard deviation
std(D3D(:,1))
% look where 99.73% of data could be. Is it consistent with histogram?
3*std(D3D(:,1)) + mean(D3D(:,1))
mean(D3D(:,1)) - 3*std(D3D(:,1))
% change normalization of the histogram
histogram(D3D(:,1), 'Normalization', 'count')
histogram(D3D(:,1), 'Normalization', 'pdf')
histogram(D3D(:,1), 'Normalization', 'cdf')
% try to fit our data with a Normal distribution
pd = fitdist(D3D(:,1), 'Normal') % look at results
% plot histogram in pdf and overalay with theoretical distribution
histogram(D3D(:,1), 'Normalization', 'pdf')
hold on;
plot([-10:30],pdf(pd, [-10:30]),'r','LineWidth',2);
ylabel('pdf');

% change number of bin for the histogram
figure;
histogram(D3D(:,1), 200, 'Normalization', 'pdf');
hold on;
% check that with a 'pdf' scale, the histogram have comparable
% "amplitudes"
histogram(D3D(:,1), 'Normalization', 'pdf');
% apply Scott's rule for number of bins
histogram(D3D(:,1), round((max(D3D(:,1))-min(D3D(:,1)))/(3.49*std(D3D(:,1))/size(D3D,1)^(1/3))), 'Normalization', 'pdf');
ylabel('pdf');
hold off