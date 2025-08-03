%% script to test different linear fits of 2D data
%--------------------------------------------------

close all;
clearvars;

%% load the data
load('D3D.mat');

%% plot 2nd column as a function of 1st column
plot(D3D(:,1), D3D(:,2), 'ob');
axis equal;
grid on;
xlabel('First column variable');
ylabel('Second column variable');

%% fit y (2nd col.) from x (1st col.)
Pp = polyfit(D3D(:,1), D3D(:,2), 1);

% now get modelled y values
Ymod = polyval(Pp, D3D(:,1));

% overlay the fitted line on the points
hold on;
plot(D3D(:,1), Ymod, 'g-', 'LineWidth', 3);

%% fit x from y
Pp2 = polyfit(D3D(:,2), D3D(:,1), 1);

% now get modelled y values
Xmod = polyval(Pp2, D3D(:,2));

% overlay the fitted line on the points
plot(Xmod, D3D(:,2), 'r-', 'LineWidth', 3);

%% apply the principal component analysis
[EVEC, EVAL] = eig(cov(D3D(:,1), D3D(:,2)));

% the best line fit is colinear to the eigenvector corresponding to the
% largest eigenvalue
% identify "largest" eigenvector
[l1, col] = max(max(EVAL));

% plot the eigenvector on the graph. It has to go through the mean value of
% our cloud
x0 = mean(D3D(:,1));
y0 = mean(D3D(:,2));
plot([x0-3*sqrt(l1)*EVEC(1,col), x0+3*sqrt(l1)*EVEC(1,col)], ...
     [y0-3*sqrt(l1)*EVEC(2,col), y0+3*sqrt(l1)*EVEC(2,col)], 'k-', 'LineWidth', 3);

%% finalize figure with legend and title and save it
hold off;
legend('Original samples', ...
    'Fit of Y from X', ...
    'Fit of X from Y', ...
    'Fit with PCA');

title('Different fitting or linear regressions on the same 2D data set');

saveas(gcf, 'd3dfit', 'jpg');