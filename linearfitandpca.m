%% Exercise: 08/12/2020
% Objective: fit a cloud of 2D points

% load data
load('D3D.mat');

% just look at data, put legend and grid
plot(D3D,'-');
legend('1st column', '2nd column', '3rd column');

% make cross-plot of the 1st and 2nd columns of matrix D3D
plot(D3D(:,1), D3D(:,2), 'o');
set grid,
grid on;
axis equal; % to have ortho-normal coordinate system
xlabel('X value'); ylabel('Y value'); % we can write several commands on the same line in Matlab

% linearly fit Y values (2nd col.) of the cloud from the X values (1st col.)
px2y = polyfit(D3D(:,1),D3D(:,2),1);
% compute the predicated Y values according to fit
Ycapx2y = polyval(px2y,D3D(:,1));
% show results on the same graph
hold on;
plot(D3D(:,1), Ycapx2y, 'r-', 'LineWidth', 3);

% now linearly fit X values from Y values and plot result
py2x = polyfit(D3D(:,2),D3D(:,1),1);
Xcapy2x = polyval(py2x, D3D(:,2));
plot(Xcapy2x, D3D(:,2), 'm-', 'LineWidth', 3);

% move slowly to Principal Component Analysis (PCA)
% first compute covariance matrix and observe
CC = cov(D3D(:,1), D3D(:,2))
% compare with variances
var(D3D(:,1))
var(D3D(:,2))
% get correlation coefficient between X and Y
corrcoef(D3D(:,1), D3D(:,2))

% compute PCA, i.e. find eigensystem
[V,D] = eig(CC);
% look at the diagonal matrix and the rotation matrix
D
V
% vectors of the rotation matrix are orthogonal
dot(V(:,1), V(:,2))
% plot best fit of cloud points according to PCA (i.e. eigenvector
% associated with largest eigenvalue)
% scale the eigenvector by 3 times the standard deviation (to cover 99.7%
% of the point range along that axis)
plot(mean(D3D(:,1))+[0, V(2,1)*3*sqrt(D(2,2))], mean(D3D(:,2))+[0, V(2,2)*3*sqrt(D(2,2))], 'k-', 'Linewidth', 3)
plot(mean(D3D(:,1))+[0, -V(2,1)*3*sqrt(D(2,2))], mean(D3D(:,2))+[0, -V(2,2)*3*sqrt(D(2,2))], 'k-', 'Linewidth', 3)
% plot the second eigenvector and scale it also properly
plot(mean(D3D(:,1))+[0, V(1,1)*3*sqrt(D(1,1))], mean(D3D(:,2))+[0, V(1,2)*3*sqrt(D(1,1))], 'g-', 'Linewidth', 3)
plot(mean(D3D(:,1))+[0, -V(1,1)*3*sqrt(D(1,1))], mean(D3D(:,2))+[0, -V(1,2)*3*sqrt(D(1,1))], 'g-', 'Linewidth', 3)
