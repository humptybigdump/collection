%% plotwell exercise
% This script proposes solutions of the "plotwell" exercise

%% read file, load content in matrix MAT and close file
MAT = readmatrix('well.txt');

%% Task 1
% plot depth as a function of density
plot(MAT(:,2),MAT(:,1),'-or');
xlabel('Density [g/cc]');
ylabel('Depth [ft]');
grid on;
title('Well log');
set(gca,'ydir','reverse')
set(gca,'xaxislocation','top');

%% Task 2
% create 3 subplots in the same figure with depth as vertical axis
% increasing down
% 1 subplot for density, 1 subplot for the P-slowness and 1 subplot for the
% S-slowness

figure;
subplot(1,3,1);
% plot depth as a function of density
plot(MAT(:,2),MAT(:,1),'-or');
xlabel('Density [g/cc]');
ylabel('Depth [ft]');
grid on;
set(gca,'ydir','reverse')
set(gca,'xaxislocation','top');

subplot(1,3,2);
% plot depth as a function of P slowness
plot(MAT(:,3),MAT(:,1),'--+g');
xlabel('P slowness [micro.s/ft]');
ylabel('Depth [ft]');
grid on;
title('Well log');
set(gca,'ydir','reverse')
set(gca,'xaxislocation','top');

subplot(1,3,3);
% plot depth as a function of S-slowness
plot(MAT(:,4),MAT(:,1),'-.db');
xlabel('S slowness [micro.s/ft]');
ylabel('Depth [ft]');
grid on;
set(gca,'ydir','reverse')
set(gca,'xaxislocation','top');
% save figure
saveas(gcf,'subplotwell','tiff');

%% Task 3
% make a polarplot of the azimuth and inclination
figure;
% use polarplot function. Be careful, 2nd axis ("radius") should be in the
% units you want but 1st axis ("angle") should be given in radians.
polarplot(MAT(:,6),...
    180/pi*MAT(:,6+1),'o');
set(gca,'thetadir','clockwise'); % azimuth should be plotted clockwise
set(gca,'thetazerolocation','top'); % azimuth zero=North is at the top
title('Fracture azimuth and inclination');
% save figure
saveas(gcf,'polarplot','jpg');