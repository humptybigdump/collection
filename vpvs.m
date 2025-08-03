%% vpvs script
%   Generate a script to
%       Load the file “vpvs.xlsx” using the function readtable
%       This Excel tables contains 2 columns: one with 150 P-wave velocity
%       values (Vp) and one with 150 S-wave velocity values. These values
%       are those of measurements performed on many granitic samples.
%       Create a first figure composed of 2 subplots that show the
%       box-and-whiskers plots of Vp and of Vs, with indication of Q1, Q2,
%       Q3 and largest value between min(Vel) and LIF, and smallest value
%       between max(Vel) and UIF
%       Create a second figure
%           The first subplot shows:
%               A cross plot of the observed velocity values
%               The best fit of the Vp(Vs)
%               The best fit equation
%           The second subplot shows:
%               The histogram of the fit residuals, i.e. the histogram of
%               the observed Vp values minus the predicted ones
%               Overlaid with a red line is the best Gaussian fit of this
%               distribution
%               The plot title gives the standard deviation with which the
%               Vp values are predicted
%       Save all figures
%

%% Load the file “vpvs.xlsx” using the function readtable
T = readtable('vpvs.xlsx');

%% Create a first figure composed of 2 subplots
% Show box-and-whiskers plots of Vp and of Vs, with indication of Q1, Q2,
% Q3 and largest value between min(Vel) and LIF, and smallest value between
% max(Vel) and UIF

% This is not done here...

%% Create a second figure
% The first subplot shows:
%   A cross plot of the observed velocity values
%   The best fit of the Vp(Vs)
%   The best fit equation
% The second subplot shows:
%   The histogram of the fit residuals, i.e. the histogram of the observed
%   Vp values minus the predicted ones
%   Overlaid with a red line is the best Gaussian fit of this distribution
%   The plot title gives the standard deviation with which the Vp values
%   are predicted

figure;
% 1st subplot
subplot(1,2,1);

% plot(T.Vs__m_s_, T.Vp__m_s_, 'ob'); % not so robust, you need to know the name of the variables
plot(T.(T.Properties.VariableNames{2}), T.(T.Properties.VariableNames{1}), 'ob'); % super robust

axis equal;
grid on;
xlabel(T.Properties.VariableDescriptions{2}, 'Interpreter', 'none');
ylabel(T.Properties.VariableDescriptions{1}, 'Interpreter', 'none');

% fit a line to explain Vp from Vs
pf = polyfit(T.Vs__m_s_, T.Vp__m_s_,1);

hold on;
plot(T.Vs__m_s_, polyval(pf, T.Vs__m_s_), '-r', 'Linewidth', 2);
linetext = sprintf('Vp = %.2f + %.2f * Vs', pf(2), pf(1));
text(quantile(T.Vs__m_s_, 0.25), min(T.Vp__m_s_)+0.1*range(T.Vp__m_s_), linetext);
legend('Observed data', 'Linear fit', 'Location', 'northwest');

% now compute the residuals
Res = T.Vp__m_s_ - polyval(pf, T.Vs__m_s_);

% 2nd subplot
subplot(1,2,2);
histogram(Res, 'Normalization', 'pdf'); % normaization is important for later overlay
grid on;
xlabel('Residual (m/s)');
ylabel('PDF');

% now fit the data with a Normal distribution
pd = fitdist(Res, 'Normal');
% overlay with observed data
hold on;
plot([min(Res):max(Res)], pdf(pd, [min(Res):max(Res)]), '-r', 'LineWidth', 2);
title(sprintf('Vp +/- %.2f', std(Res)));

% save figure locally (i.e. in folder where we run the script
saveas(gcf, 'vpvs.jpg');