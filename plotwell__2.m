%% first task

% with readtable below (uncomment if necessary) vvvvvvvvvvvvvvvvvvvvvvvvvvv
%
% TW=readtable('well.txt');
% plot(TW.rho__g_cc_, TW.depth__ft_, 'LineWidth', 3);
% set(gca, 'Fontsize', 18, 'Fontweight', 'bold');
% grid on;
% saveas(gcf,'plotwell_1','jpg');
%
% with readtable above ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
% with readmatrix below

TW=readmatrix('well.txt');
plot(TW(:,2), TW(:,1), 'LineWidth', 3);
set(gca, 'Fontsize', 18, 'Fontweight', 'bold');
grid on;
saveas(gcf,'plotwell_1','jpg');

%% second task (with the subplots)
% subplots
figure('Units','normalized',...
    'Position',[0.05, 0.05, 0.8, 0.8]);
% density
subplot(1,3,1);
plot(TW(:,2), TW(:,1), 'LineWidth', 3);
set(gca, 'Fontsize', 18, 'Fontweight', 'bold');
grid on;
xlabel('Density (g/cc)');
ylabel('Depth (ft)');
set(gca,'YDir','reverse');
set(gca,'XAxisLocation','top');

% slowp
subplot(1,3,2);
plot(TW(:,3), TW(:,1), 'r', 'LineWidth', 3);
set(gca, 'Fontsize', 18, 'Fontweight', 'bold');
grid on;
xlabel('Slowness P (micros/ft)');
% ylabel('Depth (ft)');
set(gca,'YDir','reverse');
set(gca,'XAxisLocation','top');

% slows
subplot(1,3,3);
plot(TW(:,4), TW(:,1), 'm', 'LineWidth', 3);
set(gca, 'Fontsize', 18, 'Fontweight', 'bold');
grid on;
xlabel('Slowness S (micros/ft)');
% ylabel('Depth (ft)');
set(gca,'YDir','reverse');
set(gca,'XAxisLocation','top');

saveas(gcf,'plotwell_2','jpg');

%% third task: the polar plot

figure();
polarplot(TW(:,6), TW(:,7)*180/pi, 'o');
set(gca,"ThetaDir","clockwise");
set(gca,"ThetaZeroLocation","top");
set(gca, 'Fontsize', 18, 'Fontweight', 'bold');
set(gca, "RLim", [0, 90]);
title('Fracture normal azimuth and inclination (�)')
saveas(gcf,'plotwell_3', 'jpg');