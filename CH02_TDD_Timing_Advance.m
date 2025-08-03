% Interactive Matlab GUI to show a base station and terminals transmitting frames

function interactive_base_station
    % Initialize GUI for interactive base station simulation
    % Create the GUI figure
    hFig = figure('Name', 'Timing Advance Simulation', 'NumberTitle', 'off', 'Position', [100, 100, 1000, 800]);
    
    % Create UI controls for number of terminals, maximum distance, and Start button
    hTextNumTerminals = uicontrol('Style', 'text', 'String', 'Number of Terminals:', 'Parent', hFig, 'Position', [20, 750, 150, 25]);
    hNumTerminals = uicontrol('Style', 'edit', 'String', '30', 'Parent', hFig, 'Position', [180, 750, 100, 25]);
    hTextMaxDistance = uicontrol('Style', 'text', 'String', 'Max Distance (m):', 'Parent', hFig, 'Position', [20, 710, 150, 25]);
    hMaxDistance = uicontrol('Style', 'edit', 'String', '5000', 'Parent', hFig, 'Position', [180, 710, 100, 25]);
    hButtonStart = uicontrol('Style', 'pushbutton', 'String', 'Start', 'Callback', @startSimulation, 'Parent', hFig, 'Position', [300, 750, 100, 25]);
    hCheckboxTimingAdvance = uicontrol('Style', 'checkbox', 'String', 'Timing Advance', 'Parent', hFig, 'Position', [450, 750, 150, 25]);
    
    % Axes for the base station and terminals
    hAxes1 = axes('Parent', hFig, 'Position', [0.1, 0.1, 0.35, 0.6]);
    xlabel(hAxes1, 'X Coordinate');
    ylabel(hAxes1, 'Y Coordinate');
    title(hAxes1, 'Base Station and Terminals');
    grid(hAxes1, 'on');
    axis(hAxes1, 'equal');
    
    % Axes for frame arrival times
    hAxes2 = axes('Parent', hFig, 'Position', [0.6, 0.1, 0.35, 0.6]);
    xlabel(hAxes2, 'Time (\mus)');
    ylabel(hAxes2, 'Received Power');
    title(hAxes2, 'Frame Arrival at Base Station');
    grid(hAxes2, 'on');
    
    % Base station coordinates
    baseStation = [0, 0];
    speed = 300; % Speed of signal propagation (in m/us)

    % Callback function for Start button
    function startSimulation(~, ~)
        % Get the number of terminals and max distance from the input fields
        nTerminals = str2double(get(hNumTerminals, 'String'));
        radius = str2double(get(hMaxDistance, 'String'));
        
        % Set axis limits for the base station and terminals plot
        axis(hAxes1, [-radius, radius, -radius, radius]);
        
        % Randomly generate terminal positions within the area
        terminalPositions = -radius + 2 * radius * rand(nTerminals, 2);
        
        % Calculate distances from terminals to the base station
        distances = sqrt((terminalPositions(:, 1) - baseStation(1)).^2 + (terminalPositions(:, 2) - baseStation(2)).^2);
        
        % Calculate the time it takes for frames to arrive at the base station
        maxDistance = max(distances);
        if get(hCheckboxTimingAdvance, 'Value')
            timingAdvanceDelays = (maxDistance - distances) / speed;
            arrivalTimes = distances / speed + timingAdvanceDelays;
            transmissionDelays = timingAdvanceDelays;
        else
            arrivalTimes = distances / speed;
            transmissionDelays = zeros(size(distances));
        end
        maxArrivalTime = max(arrivalTimes);
        
        % Plot base station and terminals
        cla(hAxes1);
        plot(hAxes1, baseStation(1), baseStation(2), 'ro', 'MarkerSize', 10, 'LineWidth', 2); % Base station
        hold(hAxes1, 'on');
        plot(hAxes1, terminalPositions(:, 1), terminalPositions(:, 2), 'b^', 'MarkerSize', 8, 'LineWidth', 1.5); % Terminals
        legend(hAxes1, {'Base Station', 'Terminals'}, 'Location', 'northeast', 'AutoUpdate', 'off');
        
        % Animate frame transmission from all terminals simultaneously to base station
        numSteps = 100; % Number of steps for the animation
        totalTime = maxArrivalTime; % Total time for animation is based on the longest arrival time
        timeStep = totalTime / numSteps; % Time increment for each step
        hFrames = gobjects(nTerminals, 1); % Handle array for frame markers
        for i = 1:nTerminals
            hFrames(i) = plot(hAxes1, terminalPositions(i, 1), terminalPositions(i, 2), 'go', 'MarkerSize', 6, 'LineWidth', 1.5, 'HandleVisibility', 'off'); % Initial frame position
        end
        timeVector = linspace(0, maxArrivalTime + 100, 2000); % Time vector for plotting
        frameDuration = 66.67; % Each frame lasts for 70 microseconds
        
        for t = 0:timeStep:totalTime
            for i = 1:nTerminals
                if t >= transmissionDelays(i) && t <= (arrivalTimes(i) + transmissionDelays(i))
                    progressFactor = (t - transmissionDelays(i)) / (arrivalTimes(i) - transmissionDelays(i));
                    xPos = terminalPositions(i, 1) * (1 - progressFactor) + baseStation(1) * progressFactor;
                    yPos = terminalPositions(i, 2) * (1 - progressFactor) + baseStation(2) * progressFactor;
                    set(hFrames(i), 'XData', xPos, 'YData', yPos); % Update frame position
                end
            end
            % Update the power plot dynamically on the right side
            receivedPower = zeros(size(timeVector));
            for i = 1:nTerminals
                arrivalTime = arrivalTimes(i);
                if t >= arrivalTime
                    receivedPower = receivedPower + (timeVector >= arrivalTime & timeVector <= (arrivalTime + frameDuration));
                end
            end
            cla(hAxes2);
            plot(hAxes2, timeVector, receivedPower, 'LineWidth', 1.5);
            pause(0.01); % Pause to create animation effect
        end
        delete(hFrames); % Remove frames after reaching base station
        hold(hAxes1, 'off');
        
        % Plot frame arrivals over time
        cla(hAxes2);
        receivedPower = zeros(size(timeVector));
        for i = 1:nTerminals
            arrivalTime = arrivalTimes(i);
            % Add power contribution from each frame
            receivedPower = receivedPower + (timeVector >= arrivalTime & timeVector <= (arrivalTime + frameDuration));
        end
        plot(hAxes2, timeVector, receivedPower, 'LineWidth', 1.5);
        hold on;
        first_pos = find(receivedPower > 0);
        first_time = timeVector(first_pos(1));
        plot(hAxes2, first_time + [0, 0, 66.67, 66.67], [0, nTerminals, nTerminals, 0], 'r-');
        plot(hAxes2, first_time + [66.67, 66.67, 66.67 + 4.69, 66.67 + 4.69], [0, nTerminals, nTerminals, 0], 'r-');
        hold off;
    end

    % Callback function to resize UI elements when figure size changes
    function resizeUI(~, ~)
        figPos = get(hFig, 'Position');
        margin = 20;
        elementHeight = 25;
        spacing = 10;
        % Update positions of GUI elements
        set(hTextNumTerminals, 'Position', [margin, figPos(4) - margin - elementHeight, 150, elementHeight]);
        set(hNumTerminals, 'Position', [180, figPos(4) - margin - elementHeight, 100, elementHeight]);
        set(hTextMaxDistance, 'Position', [margin, figPos(4) - margin - 2 * (elementHeight + spacing), 150, elementHeight]);
        set(hMaxDistance, 'Position', [180, figPos(4) - margin - 2 * (elementHeight + spacing), 100, elementHeight]);
        set(hButtonStart, 'Position', [300, figPos(4) - margin - elementHeight, 100, elementHeight]);
        set(hCheckboxTimingAdvance, 'Position', [450, figPos(4) - margin - elementHeight, 150, elementHeight]);
    end
    % Set resize callback after figure and UI elements are created
    set(hFig, 'SizeChangedFcn', @resizeUI);
end
