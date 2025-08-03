%% load the file
close all;
load('D3D.mat');

%% create variables for each of the 2 columns
Xx=D3D(:,1);
Yy=D3D(:,2);

%% make the cross-plot of the data
% plot(Xx, Yy, 'o', 'MarkerEdgeColor','yellow', 'MarkerFaceColor','magenta', 'LineStyle','none');
plot(Xx, Yy, 'o', 'MarkerEdgeColor','blue', 'MarkerFaceColor','cyan');
grid on;
axis equal;
xlabel('Xx-values');
ylabel('Yy-values');
title('Regression on the D3D data');

%% fit the y-data with a polynom of order 1 using x-data
Pp = polyfit(Xx,Yy,1);
Ymod = Pp(1)*Xx+Pp(2);

% plot the result as a line segment (red color)
hold on;
plot(Xx, Ymod, 'LineStyle','-', 'Color', 'red', 'LineWidth', 2)

%% fit the x-data with a polynom of order 1 using y-data
Pp = polyfit(Yy,Xx,1);
Xmod = Pp(1)*Yy+Pp(2);

% plot the result as a line segment (green color)
plot(Xmod, Yy, 'LineStyle','-', 'Color', 'magenta', 'LineWidth', 2)
% plot(Xmod, Yy, '.k');

% put a legend
legend('Original data', 'Ymod(Xx)', 'Xmod(Yy)');

%save the figure
saveas(gcf,'fit2d_231128', 'jpg');

%% use the Principal Component Analysis to get best line fit

% first compute covariance matrix
CC = cov(D3D(:,[1,2]));
% then go to eigensystem and get eigenvectors and eigenvalues
[Eivec, Eival] = eig(CC);
% identify eigenvector with the highest eigenvalue
[val1, i1] = max(max(Eival));
% plot the direction on our 2D cloud
% figure();
plot([0, Eivec(1,i1)], [0, Eivec(2,i1)], 'o-');

% hold off;