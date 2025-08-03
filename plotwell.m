%% Task 1
% Plot density as a function of depth

% First of all, remove all existing variables from the workspace
clearvars;
% load the well data into the workspace
WELL = readtable('well.txt'); % WELL is a table
% get data for the depth and for the density from the table
% make the cross-plot: x-axis for density and y-axis for depth
% data plotted as a red line with red circles
plot(WELL.rho__g_cc_, WELL.depth__ft_, '-or');

%% Task 2
% In another figure, create 3 subplots with depth as vertical axis
% increasing down, and as horizontal axes density, slowness P and slowness
% S respectively.
% Put labels to the axes.
% Put horizontal axes on the top.
% Put a grid.
% Save figure.

% create new figure
figure();
%create 1st subplot (density/depth)
subplot(1,3,1);
plot(WELL.rho__g_cc_, WELL.depth__ft_, '-or'); % plot line with red circles
% reverse direction of the depth
set(gca,'YDir', 'reverse');
% put x-axis on top of graph
set(gca,'XAxisLocation', 'top');
% label the axes
xlabel('Density [g/cc]');
ylabel('Depth [ft]');
% put grid;
grid on;

%create 2nd subplot
subplot(1,3,2);
plot(WELL.slowp__us_ft_, WELL.depth__ft_, '-+b');
set(gca,'YDir', 'reverse');
set(gca,'XAxisLocation', 'top');
xlabel('Slowness P [us/ft]');
ylabel('Depth [ft]');
grid on;

%create 3rd subplot
subplot(1,3,3);
plot(WELL.slows__us_ft_, WELL.depth__ft_, '-^g');
% hold on;
% plot(WELL.slowp__us_ft_, WELL.depth__ft_, '-+b');
% hold off;
set(gca,'YDir', 'reverse');
set(gca,'XAxisLocation', 'top');
xlabel('Slowness S [us/ft]');
ylabel('Depth [ft]');
grid on;

% Save figure
saveas(gcf, 'plotwell-2', 'jpeg');

%% Task 3
% In another figure, make a polar plot of the azimuth and inclination of
% the fracture (given in the last 2 columns of the file).
% Give a title

% create new figure
figure();

% initiate the polarplot
% inclination has to be converted to degrees (from radians)
% data displayed as red circles
polarplot(WELL.azi__rad_, WELL.inc__rad_*180/pi(), 'or');
% set 0 deg azimuth at the top of the graph
set(gca,'ThetaZeroLocation', 'top');
% azimuth is increasing clockwise
set(gca, 'ThetaDir', 'clockwise');
% give title
title('Polarplot of azimuth and inclination');
