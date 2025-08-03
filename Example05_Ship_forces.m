%% Lock Levlling 12/2023
%
clc;
clear;
close all;

% BAW-Colors
BAWHELLGRAU = [135 135 135]/256;
BAWGRAU = [79 79 78]/256;
BAWDUNKELGRAU = [79 79 78]/256;
BAWHELLBLAU = [95 191 237]/256;
BAWBLAU = [0 68 122]/256;
BAWTUERKIS = [0 168 184]/256;
BAWORANGEROT = [233 79 53]/256;

HGRAU = [0.901960784313726 0.901960784313726 0.901960784313726];

%% Data

h_u     = 12.0;       % [m] Upper water level
h_l     = 4.0;        % [m] Lower water level
mue     = 0.6;        % [-] Discharge coefficent
A_f     = 6;          % [m²] Gate cross section
t_A     = 180;        % [s] Time until fully opening
l_c     = 200;        % [m] Length of lock chamber
b_c     = 12.5;       % [m] Width of lock chamber
A_c     = l_c*b_c;    % [m²] Ground area of lock chamber
g       = 9.81;       % [m/s²] Gravitational constant

m_Ship = 2300*1000;   % [kg] Ship Weight

% Time step of calculation
dt      = 30;         % [s] Time step
t_sim   = 1200;
numSteps   = t_sim/dt;

% Allocate data
A   = zeros(1,numSteps);
h   = zeros(1,numSteps);
h_c = zeros(1,numSteps);
Q   = zeros(1,numSteps);

t   = zeros(1,numSteps);

F_1 = zeros(1,numSteps); % Force on slope
F_2 = zeros(1,numSteps); % Surge force

% Initial Values
h_c(1)  = h_l;        % [m] Chamber water level for t = 0
h(1)    = h_u-h_c(1); % [m] Head for t = 0





% Calculation in a loop up to 6000 time steps
for n=2:1:numSteps
    t(n)   = t(n-1)+dt;
    
    if t(n) < t_A
        A(n)   = (t(n)/t_A)*A_f; % filling valves opening
    else
        A(n)   = A_f; % maximum opening width
    end
    
    h_c(n) = h_c(n-1)+ dt*Q(n-1)/A_c;
    h(n)   = h_u-h_c(n);
    Q(n)   = A(n)*mue*sqrt(2*g*h(n));
    
    F_1(n) = (Q(n)/(b_c*h_c(n)))^2/(2*l_c)*m_Ship;
    F_2(n) = (Q(n)-Q(n-1))/dt*-m_Ship/(b_c*h_c(n));
end

fileName = ['LockLevelling'];

%% Plot

fig1 = figure;
    
fig_title = ['Forces on a ship during lock filling'];
    
tt = sgtitle(fig_title);
tt.FontName = 'Cambria';

farben      = colormap(jet);

%% Subplot Nr. 1: Cross section filling valves

ax(1) = subplot(2,2,1);
plot(ax(1),t,A,'DisplayName','A_f','LineWidth',2,'LineStyle','-','Color',BAWGRAU);

% Legend
h1 = legend (ax(1), 'show', 'Location','SouthEast');
set(h1,'FontSize',10, 'FontName', 'Cambria');

%title and lables
title('a) Cross section filling valves A_f')
xlabel('t [s]')
ylabel('A_f [m²]')

%limits
xlim([0,1200]);
ylim([0,10]);
xticks([0:300:1200]);
yticks([0:2:10]);

%% Subplot Nr. 2: Chamber water level
ax(2) = subplot(2,2,2);

plot(ax(2),[t(1) t(end)],[h_u h_u],'DisplayName','h_u','LineWidth',2,'LineStyle','-','Color',BAWHELLBLAU);
hold on;
plot(ax(2),t,h_c,'DisplayName','h_c','LineWidth',2,'LineStyle','-','Color',BAWBLAU);
plot(ax(2),[t(1) t(end)],[h_l h_l],'DisplayName','h_l','LineWidth',2,'LineStyle','-','Color',BAWHELLGRAU);

% Legend
h2 = legend (ax(2), 'show', 'Location','SouthEast');
set(h2,'FontSize',10, 'FontName', 'Cambria');

%title and lables
title('b) Water levels h_u, h_c and h_l')
xlabel('t [s]')
ylabel('h [m]')

%limits
xlim([0,1200]);
ylim([2,14]);
xticks([0:300:1200]);
yticks([0:2:14]);

%% Subplot Nr. 3: Inflow

ax(3) = subplot(2,2,3);
plot(ax(3),t,Q,'DisplayName','Q','LineWidth',2,'LineStyle','-','Color',BAWORANGEROT);

% Legend
h3 = legend (ax(3), 'show', 'Location','NorthEast');
set(h3,'FontSize',10, 'FontName', 'Cambria');

%title and lables
title('d) Flow into chamber')
xlabel('t [s]')
ylabel('Q [m³/s]')

%limits
xlim([0,1200]);
ylim([0,80]);
xticks([0:300:1200]);
yticks([0:20:80]);

%% Subplot Nr. 4: Kräfte auf das Schiff

ax(4) = subplot(2,2,4);
plot(ax(4),t,F_1/1000,'DisplayName','F_1 (slope)','LineWidth',2,'LineStyle','-','Color',BAWORANGEROT);
hold on;
plot(ax(4),t,F_2/1000,'DisplayName','F_2 (surge)','LineWidth',2,'LineStyle','-','Color',BAWTUERKIS);
plot(ax(4),t,(F_1+F_2)/1000,'DisplayName','F_{total}','LineWidth',2,'LineStyle','-','Color',BAWDUNKELGRAU);

% Legend
h4 = legend (ax(4), 'show', 'Location','NorthEast');
set(h4,'FontSize',10, 'FontName', 'Cambria');


%title and lables
title('d) Ship forces')
xlabel('t [s]')
ylabel('F [kN]')

%limits
xlim([0,1200]);
ylim([-50,50]);
xticks([0:300:1200]);
yticks([-50:25:50]);

axis(ax(1:4),'square');

%% Ausgabe / Abspeichern als Figure bzw. png / noch zu ergänzen: Querdruck mit drehen

% Ploteinstellungen und speichern
baw.base.plot.BawFigureDefaults.setPrintDpi(600)
baw.base.plot.BawFigureDefaults.setFontName('Cambria')
baw.base.plot.BawFigureDefaults.setFontSize(10)
baw.base.plot.BawFigureDefaults.setDefaults(fig1,[14 14])

ax(1).TitleFontWeight = 'normal';
ax(2).TitleFontWeight = 'normal';
ax(3).TitleFontWeight = 'normal';
ax(4).TitleFontWeight = 'normal';

baw.base.plot.BawFigureDefaults.writeFigureToFile(fig1, fileName, 'jpeg', true)
