% Interactive Matlab GUI for visualizing two communication cells

function interactive_cells_gui

    function updateRightFigure(~, ~)
        % Placeholder for right figure update logic
        cla(hAxes2); % Clear the right figure
        % Add your custom implementation here to update the right figure
        
        Ptx = 10^(20/10);
        PN = 10^((-174 + 10 * log10(20000))/10);
        pathlossExponent = str2double(get(hPathLoss, 'String'));
        distances = [sqrt(diff(get(connection1, 'XData'))^2 + diff(get(connection1, 'YData'))^2), ...
            sqrt(diff(get(connection2, 'XData'))^2 + diff(get(connection2, 'YData'))^2), ...
            sqrt(diff(get(interference1, 'XData'))^2 + diff(get(interference1, 'YData'))^2), ... 
            sqrt(diff(get(interference2, 'XData'))^2 + diff(get(interference2, 'YData'))^2)];        

        lambda = 0.33; % 900MHz
        pl = (lambda ./ (4 * pi * 250)).^pathlossExponent;
        np = -174 + 10 * log10(20000);
        pathloss = (lambda ./ (4 * pi * distances)).^pathlossExponent;
        if get(modeDropdown, 'Value') == 1
            t1 = min(23, 10 + np - 10 * log10(pathloss(1)));
            t2 = min(23, 10 + np - 10 * log10(pathloss(2)));
            txPower = [t1, t2, t1, t2];            
        else
            txPower = [1 1 1 1] * min(40, 10 + np - 10 * log10(pl)); % Achieve 10dB at cell edge        
        end        
        receivedPower = 10.^(txPower ./ 10) .* pathloss;        

        % No coordination, cooperation
        R1i = log2(1 + receivedPower(1)/(receivedPower(4) + 10^(np/10)));
        R2i = log2(1 + receivedPower(2)/(receivedPower(3) + 10^(np/10)));        

        plot(hAxes2, [R1i R1i 0], [0 R2i R2i], 'g-')
        hold on

        % Time sharing
        R1 = 1/log(2.0) * log(1 + receivedPower(1)/10^(np/10));
        R2 = 1/log(2.0) * log(1 + receivedPower(2)/10^(np/10));
        plot(hAxes2, [R1 0], [0 R2], 'r-')

        % Cut-Set Bound
        % Derivation based on paper by Saon, 2004
        c1 = receivedPower(1);        
        c2 = receivedPower(4);
        d1 = receivedPower(2);
        d2 = receivedPower(3);
        np_lin = 10^(np/10);
        a12 = c2/d2;
        a21 = d1/c1;
        P1 = receivedPower(1)/np_lin;
        P2 = receivedPower(2)/np_lin;
        R = [0 0; R1i R2i; R1 0; 0 R2];
        delta = 0.05;        
        for alpha = [0.000001:delta:1]
            for beta = [0.000001:delta:1]
                for lambda = [0.000001:delta:1]
                    R1 = lambda*log2(1+alpha*P1/lambda)+(1-lambda)*min(log2(1 + (1-alpha)*P1/(1-lambda+a12*(1-beta)*P2)), log2(1 + a21*(1-alpha)*P1/(1-lambda+(1-beta)*P2)));
                    R2 = (1-lambda)*log2(1 + (1-beta)*P2/(1-lambda)) + lambda*min(log2(1 + beta*P2/(lambda+a21*alpha*P1)), log2(1 + a12*beta*P2/(lambda+alpha*P1)));
                    R = [R; R1 R2];
                end
            end
        end
        k = convhull(R);
        plot(hAxes2, R(k, 1), R(k, 2), ' b-');
        legend(hAxes2, 'No cooperation', 'Time Sharing', 'Cooperation [Sason, 2004]')
        limit_range = max(ceil(log2(1 + receivedPower(1)/10^(np/10))), ceil(log2(1 + receivedPower(2)/10^(np/10))));        
        axis(hAxes2, [0 limit_range 0 limit_range])    
        
    end
    % Create the GUI figure
    hFig = figure('Name', 'Interference Channel', 'NumberTitle', 'off', 'Position', [100, 100, 800, 600], 'WindowButtonMotionFcn', @onMouseMove);

    % Create input field for Path-loss
    uicontrol('Style', 'text', 'Position', [20, 20, 100, 25], 'String', 'Path-loss:');
    hPathLoss = uicontrol('Style', 'edit', 'Position', [120, 20, 100, 25], 'String', '3.5');

    % Axes for the communication cells
    hAxes1 = axes('Parent', hFig, 'Position', [0.1, 0.2, 0.4, 0.7]);
    title(hAxes1, 'Communication Cells');
    xlabel(hAxes1, 'X Coordinate');
    ylabel(hAxes1, 'Y Coordinate');
    axis(hAxes1, [-500 500 -250 250]);
    grid(hAxes1, 'on');
    hold(hAxes1, 'on');

    % Axes for the second figure
    hAxes2 = axes('Parent', hFig, 'Position', [0.55, 0.2, 0.4, 0.7]);
    title(hAxes2, 'Achievable Rates');
    xlabel(hAxes2, 'Left terminal');
    ylabel(hAxes2, 'Right terminal');
    axis(hAxes2, [-500 500 -250 250]);
    grid(hAxes2, 'on');
    
    % Add button to update the right figure
    updateButton = uicontrol('Style', 'pushbutton', 'String', 'Update', 'Position', [450, 20, 120, 30], 'Callback', @updateRightFigure);    

    % Add dropdown for Uplink/Downlink selection
    uicontrol('Style', 'text', 'Position', [250, 20, 100, 25], 'String', 'Mode:');
    modeDropdown = uicontrol('Style', 'popupmenu', 'String', {'Uplink', 'Downlink'}, 'Position', [330, 20, 100, 25]);

    hold(hAxes2, 'on');

    % Plot cells and terminals
    viscircles(hAxes1, [-250, 0], 250, 'LineStyle', '-', 'EdgeColor', 'b'); % Cell 1
    viscircles(hAxes1, [250, 0], 250, 'LineStyle', '-', 'EdgeColor', 'b'); % Cell 2
    plot(hAxes1, -250, 0, 'kx', 'MarkerSize', 10, 'LineWidth', 1.5); % Base station 1
    plot(hAxes1, 250, 0, 'kx', 'MarkerSize', 10, 'LineWidth', 1.5); % Base station 2
    terminal1 = plot(hAxes1, -10, 0, 'bo', 'MarkerSize', 8, 'MarkerFaceColor', 'b', 'ButtonDownFcn', @startDragFcn); % Terminal 1
    terminal2 = plot(hAxes1, 10, 0, 'bo', 'MarkerSize', 8, 'MarkerFaceColor', 'b', 'ButtonDownFcn', @startDragFcn); % Terminal 2

    % Initialize connections
    connection1 = plot(hAxes1, [-250, -10], [0, 0], 'b-', 'LineWidth', 1.5); % Connection from Base station 1 to Terminal 1
    connection2 = plot(hAxes1, [250, 10], [0, 0], 'b-', 'LineWidth', 1.5); % Connection from Base station 2 to Terminal 2
    interference1 = plot(hAxes1, [-10, 250], [0, 0], 'r--', 'LineWidth', 1.5); % Interference from Terminal 1 to Base station 2
    interference2 = plot(hAxes1, [10, -250], [0, 0], 'r--', 'LineWidth', 1.5); % Interference from Terminal 2 to Base station 1   

    % Initialize terminals in both cells
    
    % Initialize connections
    

    % Variables to keep track of dragging
    draggingTerminal = [];

    function startDragFcn(src, ~)
        draggingTerminal = src;
    end

    function onMouseMove(~, ~)
        if isempty(draggingTerminal)
            return;
        end
        % Get current mouse position
        currentPoint = get(gca, 'CurrentPoint');
        newX = currentPoint(1, 1);
        newY = currentPoint(1, 2);

        % Update terminal position
        set(draggingTerminal, 'XData', newX, 'YData', newY);

        % Update connections
        if draggingTerminal == terminal1
            set(connection1, 'XData', [-250, newX], 'YData', [0, newY]);
            set(interference2, 'XData', [newX, 250], 'YData', [newY, 0]);
        elseif draggingTerminal == terminal2
            set(connection2, 'XData', [250, newX], 'YData', [0, newY]);
            set(interference1, 'XData', [newX, -250], 'YData', [newY, 0]);
        end
    end

    % Stop dragging when mouse button is released
    set(hFig, 'WindowButtonUpFcn', @stopDragFcn);
    function stopDragFcn(~, ~)
        draggingTerminal = [];
    end
end
