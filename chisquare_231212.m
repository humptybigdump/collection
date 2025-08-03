% chisquare exercise

% load the data first
load("D3D.mat");

% keep the first column
Xx = D3D(:,1);

% check we have at least 30 values
if numel(Xx)<30
    fprintf(1, "There are less than 30 values => check D3D.mat file\n");
    return;
end

% compute the histogram
Hh = histogram(Xx);
hold on;

% check that all bins have at least 5 values
% adapt histogram if necessary
while (min(Hh.Values)<5)
    Hh = histogram(Xx, Hh.NumBins-1);
end

% compute the expected bin counts from the normal distribution
% we use the mean and the standard deviation from the observations
Emid = Hh.BinWidth*0.5+Hh.BinEdges(1:Hh.NumBins); % Vector of bin middles
Ee = normpdf(Emid, mean(Xx), std(Xx));

% move from pdf to counts
Ee = Ee/sum(Ee)*sum(Hh.Values);

% overlay "expectations" with observed values 
% hold on;
plot(Emid, Ee, 'o-k', 'LineWidth', 3);

% calculate the chi-square value
chi2 = sum((Hh.Values-Ee).^2./Ee);

% define the critical value
chi2crit = icdf('chi2', 0.95, Hh.NumBins-2-1);

% compare both values
% print whether we have a normally distributed sample or not
if chi2<=chi2crit
    fprintf(1, 'Our sample follows a Normal distribution with a 95%% significance level\n');
    fprintf(1, 'Observed chi2 = %.2f <= %.2f = critical chi2\n', chi2, chi2crit);
else
    fprintf(1, 'Our sample DOES NOT follow a Normal distribution with a 95%% significance level\n');
    fprintf(1, 'Observed chi2 = %.2f > %.2f = critical chi2\n', chi2, chi2crit);
end

title('Chi-square test - Normal distribution');
xlabel('Value');
ylabel('Count');
grid on;
set(gca, "FontWeight", 'Bold', 'FontSize', 14);

% save figure
saveas(gcf,'chisquare_231212', 'jpg');