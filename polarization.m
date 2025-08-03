%% Hinweis: Klicken Sie auf die Legendeneinträge um diese Aus-/Einzublenden
% Version: 16.11.2021

%% Parameter: Polarisation
ax = 1/sqrt(3);
ay = sqrt(1 - ax^2);
phix = pi; 
phiy = 0;

%% Parameter: Simulation & Visualisierung
% allemeine Parameter
N = 501;
E0 = 1/2;
f = 3e8;
w = 2*pi*f;
k = w/3e8;
t = linspace(0,4/f,N);
N = length(t);
z = transpose(linspace(0,4,N));
zth = 10;

% Feldkomponenten Ex/Ey
Ex = ax*E0*cos(w*t+phix - k*z);
Ey = ay*E0*cos(w*t+phiy - k*z);

% Zeichne nur an jeder zth'ten Position Pfeile
zd = z(1:zth:end);
zer = zeros(size(zd));
Exd = Ex(1:zth:end,:);
Eyd = Ey(1:zth:end,:);

% eigene Farben
colors = [102 51 102; ...
    239 85 59; ...
    0 204 150; ...
    171 99 250; ...
    255 161 90; ...
    25 211 243; ...
    255 102 146; ...
    182 232 128; ...
    255 151 255; ...
    254 203 82] / 255;

%% Plot
% Plot Layout
figure;
set(gcf, 'WindowState', 'maximized');
tl = tiledlayout(1,3);

% 3D-Plot (links)
ax1 = nexttile(1,[1 2]);
hold(ax1, 'on');
view(ax1, 3);
set(ax1, 'XTickLabel', [], 'YTickLabel', [], 'ZTickLabel', []);
set(ax1, 'YDir', 'reverse');
grid(ax1)
xlabel(ax1, "z");
ylabel(ax1, "x");
zlabel(ax1, "y");
axis(ax1,"equal");
xlim(ax1,[0 z(end)]);
ylim(ax1,E0*[-1 1]);
zlim(ax1,E0*[-1 1]);

% 2D-Plot (rechts)
ax2 = nexttile(3);
hold(ax2, 'on');
view(ax2, 2);
set(ax2, 'XTickLabel', [], 'YTickLabel', [], 'XTick', (-1:1)*E0, 'YTick', (-1:1)*E0);
grid(ax2)
xlabel(ax2, "x");
ylabel(ax2, "y");
axis(ax2,"equal");
xlim(ax2,E0*[-1 1]);
ylim(ax2,E0*[-1 1]);

% Initiale Darstellung
% 3D
env3 = plot3(ax1, z, Ex(:,1), Ey(:,1), '--','DisplayName','Pfad','Color',colors(1,:),'LineWidth',1.5);
arr3x = quiver3(ax1,zd,zer,zer,zer,Exd(:,1),zer,0,'MaxHeadSize',0.1,'DisplayName','Ex','Color',colors(3,:),'LineWidth',1.5);
arr3y = quiver3(ax1,zd,zer,zer,zer,zer,Eyd(:,1),0,'MaxHeadSize',0.1,'DisplayName','Ey','Color',colors(4,:),'LineWidth',1.5);
arr3 = quiver3(ax1,zd,zer,zer,zer,Exd(:,1),Eyd(:,1),0,'MaxHeadSize',0.1,'DisplayName','E','Color',colors(5,:),'LineWidth',1.5);
% 3D: z-Achse
axline3 = plot3(ax1, [0 z(end)], [0 0], [0 0], 'Color', 'black'); axline3.Annotation.LegendInformation.IconDisplayStyle = 'off';
% 3D: Legende
lgd1 = legend(ax1);
lgd1.Orientation = 'horizontal';
lgd1.Location = 'northoutside';
lgd1.Box = 'off';
lgd1.ItemHitFcn = @hitcallback;


% 2D
env2 = plot(ax2, Ex(:,1), Ey(:,1), '--','DisplayName','Pfad','Color',colors(1,:),'LineWidth',1.5);
arr2x = quiver(ax2,0,0,Exd(1,1),0,0,'MaxHeadSize',0.5,'DisplayName','Ex','Color',colors(3,:),'LineWidth',1.5);
arr2y = quiver(ax2,0,0,0,Eyd(1,1),0,'MaxHeadSize',0.5,'DisplayName','Ey','Color',colors(4,:),'LineWidth',1.5);
arr2 = quiver(ax2,0,0,Exd(1,1),Eyd(1,1),0,'MaxHeadSize',0.5,'DisplayName','E','Color',colors(5,:),'LineWidth',1.5);
% 2D: Beschriftung
text(-0.875*E0,0.875*E0, "z=0");
% 2D: Legende
lgd2 = legend(ax2);
lgd2.Orientation = 'horizontal';
lgd2.Location = 'northoutside';
lgd2.Box = 'off';
lgd2.ItemHitFcn = @hitcallback;

% Passe alle Schriften in der Figur an
set(findall(gcf, 'type', 'text'), 'FontName', 'Arial', 'FontSize', 14)
set(findall(gcf, 'type', 'legend'), 'FontName', 'Arial', 'FontSize', 14)
set(findall(gcf, 'type', 'axes'), 'FontName', 'Arial', 'FontSize', 14)


%% Animation
pause(1)

for k = 2:length(t)  
    % 3D: Ex
    arr3x.VData = Exd(:,k);
    % 3D: Ey
    arr3y.WData = Eyd(:,k);
    % 3D: E
    arr3.VData = Exd(:,k); arr3.WData = Eyd(:,k);
    % 3D: Pfad
    env3.YData = Ex(:,k); env3.ZData = Ey(:,k);
      
    % 2D: Ex
    arr2x.UData = Exd(1,k);
    % 2D: Ey
    arr2y.VData = Eyd(1,k);
    % 2D: E
    arr2.UData = Exd(1,k); arr2.VData = Eyd(1,k);
    
    drawnow
end


%% Ausblenden Legendeneinträge
function hitcallback(src,evnt)
if strcmp(evnt.Peer.Visible,'on')
    evnt.Peer.Visible = 'off';
else 
    evnt.Peer.Visible = 'on';
end
end
