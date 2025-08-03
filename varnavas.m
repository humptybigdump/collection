MM = readmatrix("Varnavas geochem data EG.xlsx", "Range",[2 5]);

CC = corrcoef(cov(MM));

imagesc(CC);

colorbar

gplotmatrix(MM(:,1:8))