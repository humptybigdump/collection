%% load the file
load('D3D.mat');

%% create variables for each of the 2 columns
Xx=D3D(:,1);
Yy=D3D(:,2);

%% make the cross-plot of the data
% plot(Xx, Yy, 'o', 'MarkerEdgeColor','yellow', 'MarkerFaceColor','magenta', 'LineStyle','none');
plot(Xx, Yy, 'o', 'MarkerEdgeColor','yellow', 'MarkerFaceColor','magenta');
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
plot(Xmod, Yy, 'LineStyle','-', 'Color', 'green', 'LineWidth', 2)
% plot(Xmod, Yy, '.k');

% % put a legend
% legend('Original data', 'Ymod(Xx)', 'Xmod(Yy)');
% 
% %save the figure
% saveas(gcf,'fit2d_221206', 'jpg');

%% fit a line using the PCA
% compute the covariance matrix of the 2D data
% make the eigenvalue analysis
[EIVEC, EIVAL] = eig(cov(D3D(:,1:2)));
% keep eigenvector corresponding to largest eigenvalue
% get index of the largest eigenvector
[lambda, ind] = max(max(EIVAL));
% plot the eigenvector in both directions, starting at the mid point and
% over 3 times the square root of the corresponding eigenvalue
Seg = 3*sqrt(lambda)*EIVEC(:,ind);
plot([mean(Xx)-Seg(1) mean(Xx)+Seg(1)], [mean(Yy)-Seg(2) mean(Yy)+Seg(2)], '-k', 'LineWidth',2);

% put a legend
legend('Original data', 'Ymod(Xx)', 'Xmod(Yy)', 'PCA');

%save the figure
saveas(gcf,'fit2d_221206', 'jpg');

hold off;

%%Eureka!
