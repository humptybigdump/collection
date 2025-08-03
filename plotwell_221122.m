%% Task 1: plot the density as a function of depth

% Version 1: with readmatrix
% % read the file
% WELL=readmatrix('well.txt');
% 
% % plot depth as a function of density
% plot(WELL(:,2), WELL(:,1), "o-", "LineWidth", 2, "Color", "r");
% xlabel('Density [g/cc]');
% ylabel('Depth [ft]');
% title('Cross-plot');
% 
% set(gca,'YDir','reverse');
% set(gca,'XAxisLocation','top');

% Version 2: with readtable
% read the file
WELL=readtable('well.txt');

% plot depth as a function of density
plot(WELL.rho__g_cc_, WELL.depth__ft_, "o-", "LineWidth", 2, "Color", "r");
xlabel('Density [g/cc]');
ylabel('Depth [ft]');
title('Cross-plot');

set(gca,'YDir','reverse');
set(gca,'XAxisLocation','top');

saveas(gcf,'plotwell_1','jpg');
% make the 3 subplots now
figure();

% subplot 1: plot depth as a function of density
subplot(1,3,1);
plot(WELL.rho__g_cc_, WELL.depth__ft_, "o-", "LineWidth", 2, "Color", "r");
xlabel('Density [g/cc]');
ylabel('Depth [ft]');
title('Cross-plot');

set(gca,'YDir','reverse');
set(gca,'XAxisLocation','top');

% subplot 2: plot depth as a function of P-slowness
subplot(1,3,2);
plot(WELL.slowp__us_ft_, WELL.depth__ft_, "o-", "LineWidth", 2, "Color", "b");
xlabel('P-slowness [us/ft]');
ylabel('Depth [ft]');
title('Cross-plot');

set(gca,'YDir','reverse');
set(gca,'XAxisLocation','top');

% subplot 3: plot depth as a function of S-slowness
subplot(1,3,3);
plot(WELL.slows__us_ft_, WELL.depth__ft_, "o-", "LineWidth", 2, "Color", "g");
xlabel('S-slowness [us/ft]');
ylabel('Depth [ft]');
title('Cross-plot');

set(gca,'YDir','reverse');
set(gca,'XAxisLocation','top');

saveas(gcf,'plotwell_2','jpg');
% now make the polarplot
figure();

polarplot(WELL.azi__rad_, WELL.inc__rad_*180/pi, 'bo');
set(gca,'ThetaDir','clockwise');
set(gca,'ThetaZeroLocation','top');

saveas(gcf,'plotwell_3','jpg');