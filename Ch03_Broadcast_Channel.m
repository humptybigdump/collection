% Interactive Matlab GUI to visualize Broadcast Channel (BC) Capacity for one transmitter and two receivers

function bc_capacity_visualizer
    % Create the GUI figure
    hFig = figure('Name', 'Broadcast Channel Capacity', 'NumberTitle', 'off', 'Position', [100, 100, 1000, 600]);
    
    % Create UI controls for transmit power and channel amplification
    uicontrol('Style', 'text', 'Position', [20, 550, 150, 25], 'String', 'Transmit Power for User 1 (W):');
    hPower1 = uicontrol('Style', 'edit', 'Position', [180, 550, 100, 25], 'String', '1');
    uicontrol('Style', 'text', 'Position', [20, 510, 150, 25], 'String', 'Transmit Power for User 2 (W):');
    hPower2 = uicontrol('Style', 'edit', 'Position', [180, 510, 100, 25], 'String', '1');
    uicontrol('Style', 'text', 'Position', [20, 470, 150, 25], 'String', 'Channel Gain g1 (User 1):');
    hPathLoss1 = uicontrol('Style', 'edit', 'Position', [180, 470, 100, 25], 'String', '1');
    uicontrol('Style', 'text', 'Position', [20, 430, 150, 25], 'String', 'Channel Gain g2 (User 2):');
    hPathLoss2 = uicontrol('Style', 'edit', 'Position', [180, 430, 100, 25], 'String', '1');
    
    % Create UI controls for access scheme and slider for resource sharing
    uicontrol('Style', 'text', 'Position', [300, 550, 150, 25], 'String', 'Access Scheme:');
    hScheme = uicontrol('Style', 'popupmenu', 'String', {'Time Sharing', 'Frequency Sharing', 'Interference Cancellation'}, 'Position', [460, 550, 150, 25]);
    uicontrol('Style', 'text', 'Position', [300, 510, 150, 25], 'String', 'Resource Share (a1):');
    hSlider = uicontrol('Style', 'slider', 'Position', [460, 510, 200, 25], 'Min', 0, 'Max', 1, 'Value', 0.5, 'Callback', @updateCapacity);
    hButtonRestart = uicontrol('Style', 'pushbutton', 'Position', [700, 550, 100, 25], 'String', 'Restart', 'Callback', @restartSimulation);
    
    % Axes for resource allocation (time-frequency grid)
    hAxes1 = axes('Parent', hFig, 'Position', [0.55, 0.1, 0.4, 0.6]);
    xlabel(hAxes1, 'Time');
    ylabel(hAxes1, 'Frequency');
    title(hAxes1, 'Resource Allocation');
    grid(hAxes1, 'on');
    
    % Axes for BC capacity region
    hAxes2 = axes('Parent', hFig, 'Position', [0.1, 0.1, 0.35, 0.6]);
    xlabel(hAxes2, 'Rate R1 (bps)');
    ylabel(hAxes2, 'Rate R2 (bps)');
    
    title(hAxes2, 'Broadcast Channel Capacity Region');
    grid(hAxes2, 'on');
    hold(hAxes2, 'on');
    
    % Variables to store data points
    capacityPoints = [];
    
    % Callback function to update the capacity plot
    function updateCapacity(~, ~)
        % Get the values from UI controls
        P1 = str2double(get(hPower1, 'String'));
        P2 = str2double(get(hPower2, 'String'));
        g1 = str2double(get(hPathLoss1, 'String'));
        g2 = str2double(get(hPathLoss2, 'String'));
        a1 = get(hSlider, 'Value');
        a2 = 1 - a1;
        scheme = get(hScheme, 'Value');
        xlim(hAxes2, [0, ceil(log2(1+g1*P1))])
        ylim(hAxes2, [0, ceil(log2(1+g2*P2))])

        % Calculate rates based on the selected scheme
        switch scheme
            case 1 % Time Sharing
                R1 = a1 * log2(1 + g1 * P1);
                R2 = a2 * log2(1 + g2 * P2);
                % Update resource allocation plot
                cla(hAxes1);
                % Plot time-sharing allocation as a rectangle
                rectangle(hAxes1, 'Position', [0, 0, a1, 1], 'FaceColor', 'r', 'EdgeColor', 'k'); % User 1 allocation
                rectangle(hAxes1, 'Position', [a1, 0, a2, 1], 'FaceColor', 'b', 'EdgeColor', 'k'); % User 2 allocation
                text(hAxes1, a1/2, 0.5, sprintf('P1: %.2f', P1), 'HorizontalAlignment', 'center', 'Color', 'w');
                text(hAxes1, a1 + a2/2, 0.5, sprintf('P2: %.2f', P2), 'HorizontalAlignment', 'center', 'Color', 'w');
                set(hAxes1, 'XLim', [0, 1], 'YLim', [0, 1]);
                xlabel(hAxes1, 'Time');
                ylabel(hAxes1, 'Frequency');
                title(hAxes1, 'Time Sharing Allocation');
            case 2 % Frequency Sharing
                R1 = a1 * log2(1 + g1 * P1);
                R2 = a2 * log2(1 + g2 * P2);
                % Update resource allocation plot
                cla(hAxes1);
                % Plot frequency-sharing allocation as rectangles
                rectangle(hAxes1, 'Position', [0, 0, 1, a1], 'FaceColor', 'r', 'EdgeColor', 'k'); % User 1 allocation
                rectangle(hAxes1, 'Position', [0, a1, 1, a2], 'FaceColor', 'b', 'EdgeColor', 'k'); % User 2 allocation
                text(hAxes1, 0.5, a1 / 2, sprintf('1.0 * P1: %.2f', P1), 'HorizontalAlignment', 'center', 'Color', 'w');
                text(hAxes1, 0.5, a1 + a2 / 2, sprintf('1.0 * P2: %.2f', P2), 'HorizontalAlignment', 'center', 'Color', 'w');
                set(hAxes1, 'XLim', [0, 1], 'YLim', [0, 1]);
                xlabel(hAxes1, 'Time');
                ylabel(hAxes1, 'Frequency');
                title(hAxes1, 'Frequency Sharing Allocation');               
            case 3 % Interference Cancellation
                % Calculate rates with interference cancellation
                R1 = a1 * log2(1 + P1) + (1 - a1) * log2(1 + P1 / (1 + P2));
                R2 = (1 - a1) * log2(1 + P2) + a1 * log2(1 + P2 / (1 + P1));
                
                R1 = log2(1 + a1 * g1 * P1);
                R2 = min(log2(1 + (1 - a1) * g2 * P2 / (1 + a1 * g2 * P1)), log2(1 + (1 - a1) * g1 * P2 / (1 + a1 * g1 * P1)));
                
                % Update resource allocation plot
                cla(hAxes1);
                % Plot interference cancellation allocation as rectangles
                rectangle(hAxes1, 'Position', [0, 0, 1, 1], 'FaceColor', 'r', 'EdgeColor', 'k'); % User 1 allocation
                % Add blue stripes for User 2 overlay on User 1 allocation
                linspace(0, 1, ceil(1/a1 * 10))
                for x = linspace(0, 1, ceil(1/a1 * 10))
                    line(hAxes1, [x, x], [0, 1], 'Color', 'b', 'LineWidth', 1);
                end
                
                set(hAxes1, 'XLim', [0, 1], 'YLim', [0, 1]);
                xlabel(hAxes1, 'Time');
                ylabel(hAxes1, 'Frequency');
                title(hAxes1, 'Interference Cancellation Allocation');                
        end
        
        % Plot the new capacity point
        capacityPoints = [capacityPoints; R1, R2];
        plot(hAxes2, capacityPoints(:, 1), capacityPoints(:, 2), 'bo');
    end
    
    % Callback function to restart the simulation
    function restartSimulation(~, ~)
        % Clear capacity points and reset plots
        capacityPoints = [];
        cla(hAxes1);
        cla(hAxes2);
        xlabel(hAxes2, 'Rate R1 (bps)');
        ylabel(hAxes2, 'Rate R2 (bps)');
        set(hAxes2, 'XLim', [0, 1], 'YLim', [0, 1]);
        title(hAxes2, 'BC Capacity Region');
        grid(hAxes2, 'on');
        hold(hAxes2, 'on');
    end
end
