function interactiveSVD
    % GUI zur vollständigen SVD-Darstellung einer Matrix H
    % Zeigt die Transformationen der Punkte durch V^T, S und U
    % Zeigt zusätzlich den Rang der Matrix H

    % Startkoordinaten des Vierecks
    defaultX = [1, 1, -1, -1];
    defaultY = [1, -1, -1, 1];
    xPoints = defaultX;
    yPoints = defaultY;

    % GUI-Fenster
    fig = figure('Name', 'SVD Visualization', 'NumberTitle', 'off', 'Position', [100, 100, 1000, 700]);

    % Eingabefelder für die Matrix H (2x2)
    uicontrol('Style', 'text', 'Position', [20, 650, 80, 20], 'String', 'H(1,1):');
    h11 = uicontrol('Style', 'edit', 'Position', [100, 650, 50, 20], 'String', '1');
    uicontrol('Style', 'text', 'Position', [160, 650, 80, 20], 'String', 'H(1,2):');
    h12 = uicontrol('Style', 'edit', 'Position', [240, 650, 50, 20], 'String', '0');
    uicontrol('Style', 'text', 'Position', [20, 620, 80, 20], 'String', 'H(2,1):');
    h21 = uicontrol('Style', 'edit', 'Position', [100, 620, 50, 20], 'String', '0');
    uicontrol('Style', 'text', 'Position', [160, 620, 80, 20], 'String', 'H(2,2):');
    h22 = uicontrol('Style', 'edit', 'Position', [240, 620, 50, 20], 'String', '1');
    
    % Buttons
    uicontrol('Style', 'pushbutton', 'Position', [600, 650, 80, 30], 'String', 'Calculate', 'Callback', @startCallback);
    uicontrol('Style', 'pushbutton', 'Position', [700, 650, 80, 30], 'String', 'Reset', 'Callback', @resetCallback);

    % Textfeld für Rang der Matrix
    rankText = uicontrol('Style', 'text', 'Position', [600, 620, 180, 20], ...
        'String', 'Matrix condition: ', 'FontSize', 10, 'HorizontalAlignment', 'left');

    % Plot-Achsen
    ax1 = axes('Position', [0.1, 0.5, 0.35, 0.3]); title('Rectangle to be transformed');
    ax2 = axes('Position', [0.55, 0.5, 0.35, 0.3]); title('Transformation using $V^T$', 'Interpreter', ' Latex');
    ax3 = axes('Position', [0.55, 0.1, 0.35, 0.3]); title('Transformation using $S$', 'Interpreter', ' Latex'); 
    ax4 = axes('Position', [0.1, 0.1, 0.35, 0.3]); title('Transformation using $U$', 'Interpreter', ' Latex');
    hold(ax1, 'on'); hold(ax2, 'on'); hold(ax3, 'on'); hold(ax4, 'on');

    % Initiales Viereck anzeigen
    updateInteractivePoints();

    % Funktionen für Interaktivität
    function updateInteractivePoints()
        cla(ax1);
        plot(ax1, [xPoints, xPoints(1)], [yPoints, yPoints(1)], 'o-', ...
            'MarkerSize', 10, 'MarkerFaceColor', 'r', 'ButtonDownFcn', @startDragFcn);
        axis(ax1, [-10 10 -10 10])
        grid(ax1, "on")
        title(ax1, 'Interaktives Viereck');
    end

    function startCallback(~, ~)
        % Lese Matrix H aus Eingabefeldern
        H = [str2double(h11.String), str2double(h12.String); ...
             str2double(h21.String), str2double(h22.String)];
        
        % Berechne Rang der Matrix        
        rankText.String = sprintf('Matrix condition: %.2f', cond(H));

        % Berechne SVD der Matrix H
        [U, S, V] = svd(H);
        
        % Punkte in Matrixform
        points = [xPoints; yPoints];
        
        % Transformation durch V^T
        vtTransformed = V' * points;
        cla(ax2);
        plot(ax2, [vtTransformed(1, :), vtTransformed(1, 1)], ...
                  [vtTransformed(2, :), vtTransformed(2, 1)], 'o-', ...
                  'MarkerSize', 10, 'MarkerFaceColor', 'b');
        axis(ax2, [-10 10 -10 10])
        grid(ax2, "on")
        title('Transformation using $V^T$', 'Interpreter', ' Latex');

        % Transformation durch S
        sTransformed = S * vtTransformed;
        cla(ax3);
        plot(ax3, [sTransformed(1, :), sTransformed(1, 1)], ...
                  [sTransformed(2, :), sTransformed(2, 1)], 'o-', ...
                  'MarkerSize', 10, 'MarkerFaceColor', 'g');
        axis(ax3, [-10 10 -10 10])
        grid(ax3, "on")
        title('Transformation using $S$', 'Interpreter', ' Latex'); 

        % Transformation durch U
        uTransformed = U * sTransformed;
        cla(ax4);
        plot(ax4, [uTransformed(1, :), uTransformed(1, 1)], ...
                  [uTransformed(2, :), uTransformed(2, 1)], 'o-', ...
                  'MarkerSize', 10, 'MarkerFaceColor', 'm');
        axis(ax4, [-10 10 -10 10])
        grid(ax4, "on")
        title('Transformation using $U$', 'Interpreter', ' Latex');
    end

    function resetCallback(~, ~)
        % Setze Punkte zurück auf die Startkoordinaten
        xPoints = defaultX;
        yPoints = defaultY;
        updateInteractivePoints();
        cla(ax2); cla(ax3); cla(ax4);
        rankText.String = 'Rang der Matrix: ';
    end

    % Dragging-Funktionen
    function startDragFcn(~, ~)
        set(fig, 'WindowButtonMotionFcn', @draggingFcn);
        set(fig, 'WindowButtonUpFcn', @stopDragFcn);
    end

    function draggingFcn(~, ~)
        cp = get(ax1, 'CurrentPoint');
        x = cp(1, 1);
        y = cp(1, 2);
        [~, idx] = min(hypot(xPoints - x, yPoints - y));
        xPoints(idx) = x;
        yPoints(idx) = y;
        updateInteractivePoints();
    end

    function stopDragFcn(~, ~)
        set(fig, 'WindowButtonMotionFcn', '');
        set(fig, 'WindowButtonUpFcn', '');
    end
end
