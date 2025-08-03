function EMW_Uebung_02_Matlab_Polarisation(ax, dphi, options)
%% Hinweis: Klicke auf die Legendeneinträge um diese Aus-/Einzublenden
% Version: 07.11.2023
    arguments
        ax = 1/sqrt(2)
        dphi = pi/3
        options.animate = true
        options.fps = 30
    end

    %% Parameter: Polarisation
    % ax und dphi übergeben wir beim Funktionsaufruf
    ay = sqrt(1 - ax^2);
    % Vergleiche Vorlesung: dφ = φy - φx, wir setzen hier φx = 0!
    phix = 0;
    phiy = dphi;
    
    %% Parameter: Simulation & Visualisierung
    % Parameter
    Nz = 201;
    Nt = 101;
    
    f = 3e8;
    w = 2*pi*f;
    k = w/3e8;
    T = 1/f;
    t = (0:Nt-1) * T/Nt;
    z = transpose(linspace(-4,0,Nz));
    zth = 10;
    
    % Feldkomponenten Ex und Ey
    Ex = ax*cos(w*t + phix - k*z);
    Ey = ay*cos(w*t + phiy - k*z);
    
    % Zeichne nur an jeder zth'ten Position Pfeile
    zd = z(1:zth:end);
    dummy = zeros(size(zd));
    Exd = Ex(1:zth:end,:);
    Eyd = Ey(1:zth:end,:);
    
    % eigene Farben
    color1 = [0.902,0.624,0.0];
    color2 = [0.337,0.706,0.914];
    color3 = [0.0,0.62,0.451];
    
    %% Plot
    % Plot Layout
    figure;
    tiledlayout(1,3);
    
    % 3D-Plot (links)
    ax1 = nexttile(1,[1 2]);
    hold(ax1, 'on');
    view(ax1, -37.5, 30);
    set(ax1, XDir="reverse", YDir="reverse");
    grid(ax1)
    xlabel(ax1, "z"); ylabel(ax1, "E_x"); zlabel(ax1, "E_y");
    axis(ax1, "equal");
    xlim(ax1, [z(1) z(end)]); ylim(ax1, [-1 1]); zlim(ax1, [-1 1]);
    % 3D: z-Achse
    axline3 = plot3(ax1, [z(1) z(end)], [0 0], [0 0], Color="black"); axline3.Annotation.LegendInformation.IconDisplayStyle = 'off';
    
    % 2D-Plot (rechts)
    ax2 = nexttile(3);
    hold(ax2, 'on');
    view(ax2, 2);
    set(ax2, XTickLabel=["-1", "", "1"], YTickLabel=[], XTick=-1:1, YTick=-1:1);
    grid(ax2)
    xlabel(ax2, "E_x"); ylabel(ax2, "E_y");
    axis(ax2,"equal");
    xlim(ax2,[-1 1]); ylim(ax2,[-1 1]);
    % 2D: Beschriftung
    text(-0.875,0.875, "z=0");
    % 2D: Koordinatenursprung zentriert
    set(ax2, XAxisLocation='origin', YAxisLocation='origin')
    
    % Initiale Darstellung
    % 3D
    env3 = plot3(ax1, z, Ex(:,1), Ey(:,1), DisplayName="Pfad", Color="black", LineWidth=0.5);
    arr3x = quiver3(ax1, zd, dummy, dummy, dummy, Exd(:,1), dummy, 0, MaxHeadSize=0.1, DisplayName='Ex', Color=color2, LineWidth=1);
    arr3y = quiver3(ax1, zd, dummy, dummy, dummy, dummy, Eyd(:,1), 0, MaxHeadSize=0.1, DisplayName='Ey', Color=color3, LineWidth=1);
    arr3 = quiver3(ax1, zd, dummy, dummy, dummy, Exd(:,1), Eyd(:,1), 0, MaxHeadSize=0.1, DisplayName='E', Color=color1, LineWidth=1);
    
    % 2D
    env2 = plot(ax2, Ex(:,1), Ey(:,1), DisplayName="Pfad", Color="black", LineWidth=0.5);
    arr2x = quiver(ax2, 0, 0, Exd(1,1), 0, 0, MaxHeadSize=0.5, DisplayName='Ex', Color=color2, LineWidth=1);
    arr2y = quiver(ax2, 0, 0, 0, Eyd(1,1), 0, MaxHeadSize=0.5, DisplayName='Ey', Color=color3, LineWidth=1);
    arr2 = quiver(ax2, 0, 0, Exd(1,1), Eyd(1,1), 0, MaxHeadSize=0.5, DisplayName='E', Color=color1, LineWidth=1);

    % 3D: Legende
    lgd = legend(ax1);
    lgd.Orientation = 'horizontal';
    lgd.Layout.Tile = "South";
    lgd.Box = 'off';
    lgd.ItemHitFcn = @hitcallback;

    % here, it is super important to specify the output variables
    hlink1 = linkprop([env3, env2], "Visible");
    hlink2 = linkprop([arr3x, arr2x], "Visible");
    hlink3 = linkprop([arr3y, arr2y], "Visible");
    hlink4 = linkprop([arr3, arr2], "Visible");

    % default: do not show Ex/Ey arrows
    arr3x.Visible = "off";
    arr3y.Visible = "off";
    
    %% Animation
    
    if options.animate
        pause(1)
        k = 1;
        while true
            k = mod(k, Nt) + 1;
            try
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
    
                pause(1/options.fps)
            catch
                break
            end
        end
    end
end

%% Ausblenden Legendeneinträge
function hitcallback(src,evnt)
    if strcmp(evnt.Peer.Visible,'on')
        evnt.Peer.Visible = 'off';
    else
        evnt.Peer.Visible = 'on';
    end
end