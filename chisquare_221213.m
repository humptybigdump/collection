% In this script, we want to check with the chi2-test if the data in the
% first column of the D3D are following a Normal distribution

% 1- load the data and keep first column only
load('D3D.mat');
Xx = D3D(:,1);

% 2- make the histogram and keep info on the bins
hi = histogram(Xx);
% check that all bins have more than 4 counts otherwise decrease the number
% of bins
while min(hi.BinCounts)<4
    hi=histogram(Xx, hi.NumBins-1);
end
fprintf('Histogram number of bins is: %d\n', hi.NumBins);

% 3- fit our data with a normal distribution
ND = fitdist(Xx,"Normal");

% 4- compute the theoretical histogram from the Normal distribution
% at the middle of each bin of the "observed" histogram
Midx = hi.BinEdges + ((hi.BinEdges(2)-hi.BinEdges(1))/2);
Midx = Midx(1:end-1); % we must remove the last element
Ex = pdf(ND, Midx);

% 5- rescale the pdf to "fit" the observed histogram
% because it is a pdf, the surface is used to rescale
Ex = Ex/(sum(Ex))*sum(hi.BinCounts);

% 6- plot result on original/observed histogram
hold on;
plot(Midx, Ex, 'LineWidth', 2, 'Color', 'k');

% 7- calculate the observed chisquare
% (remember elementwise arithmetic operations)
ochi2 = sum((hi.BinCounts-Ex).^2./Ex);

% 8- get the critical chisquare value at 95% significant level
critchi2 = chi2inv(0.95, hi.NumBins-3);

% 9- see if we accept or reject "our statistical data sample follows the
% proposed Normal distribution"
if (critchi2>ochi2)
    fprintf(1, 'The observed statistical sample is WELL represented by a Normal distribution, with 95%% significance level\n');
else
    fprintf(1, 'The observed statistical sample is NOT WELL represented by a Normal distribution, with 95%% significance level\n');
end
