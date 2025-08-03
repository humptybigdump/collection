% Interactive Matlab GUI to place base stations according to a 2D Poisson process

function interactive_poisson_base_stations
    close all
    % Interactive Matlab GUI for Base Station Placement
    % Create the GUI figure
    hFig = figure('Name', 'Cell Deployment', 'NumberTitle', 'off', 'Position', [100, 100, 1000, 800]);
    set(hFig, 'SizeChangedFcn', @resizeUI)

    % Create UI controls for number of base stations, path-loss exponent, and Start button
    hTextNumBaseStations = uicontrol('Style', 'text', 'Position', [20, 750, 150, 25], 'String', 'Number of Base Stations:', 'Parent', hFig);
    hNumBaseStations = uicontrol('Style', 'edit', 'Position', [180, 750, 100, 25], 'String', '10', 'Parent', hFig);
    hTextPathLoss = uicontrol('Style', 'text', 'Position', [20, 710, 150, 25], 'String', 'Path-loss exponent (a):', 'Parent', hFig);
    hPathLoss = uicontrol('Style', 'edit', 'Position', [180, 710, 100, 25], 'String', '2', 'Parent', hFig);
    hTextNumUsers = uicontrol('Style', 'text', 'Position', [20, 670, 150, 25], 'String', 'Number of users:', 'Parent', hFig);
    hNumUsers = uicontrol('Style', 'edit', 'Position', [180, 670, 100, 25], 'String', '1000', 'Parent', hFig);
    hButtonGenerate = uicontrol('Style', 'pushbutton', 'Position', [300, 750, 100, 25], 'String', 'Generate', 'Callback', @generateBaseStations, 'Parent', hFig);
    hTextDeployment = uicontrol('Style', 'text', 'Position', [450, 710, 150, 25], 'String', 'Deployment Type:', 'Parent', hFig);
    hDropdownDeployment = uicontrol('Style', 'popupmenu', 'String', {'Random', 'Diamond', 'Hexagonal'}, 'Position', [610, 710, 150, 25], 'Value', 1, 'Parent', hFig);
    hTextSumPower = uicontrol('Style', 'text', 'Position', [450, 750, 150, 25], 'String', 'Sum Power (dBm):', 'Parent', hFig);
    hSumPower = uicontrol('Style', 'text', 'Position', [610, 750, 100, 25], 'String', '0', 'Parent', hFig);
    hDropdownDisplay = uicontrol('Style', 'popupmenu', 'String', {'None', 'Colormap', 'Rate'}, 'Position', [750, 750, 150, 25], 'Value', 1, 'Parent', hFig);
    hButtonUpdate = uicontrol('Style', 'pushbutton', 'Position', [910, 750, 100, 25], 'String', 'Update', 'Callback', @updateDisplay, 'Parent', hFig);
    
    % Axes for base station placement
    hAxes = axes('Parent', hFig, 'Position', [0.1, 0.1, 0.8, 0.7]);
    xlabel(hAxes, 'X Coordinate');
    ylabel(hAxes, 'Y Coordinate');
    title(hAxes, 'Base Stations Placement');
    grid(hAxes, 'on');
    axis(hAxes, [0, 10000, 0, 10000]); % Set fixed range for consistent visualization
    
    % Callback function for Generate button
    function generateBaseStations(~, ~)
        % Get the deployment type from the dropdown
        deploymentType = get(hDropdownDeployment, 'Value');
        % Get the number of base stations and path-loss exponent from the input fields
        nBaseStations = str2double(get(hNumBaseStations, 'String'));
        pathLossExponent = str2double(get(hPathLoss, 'String'));
        nUsers = str2double(get(hNumUsers, 'String'));
        
        % Define the area dimensions
        areaWidth = 10000;
        areaHeight = 10000;
        
        % Generate base station positions based on deployment type
        switch deploymentType
            case 1 % Random (Poisson Process)
                baseStationPositionsX = areaWidth * rand(nBaseStations, 1);
                baseStationPositionsY = areaHeight * rand(nBaseStations, 1);            
            case 2 % Diamond Grid                                               
                numRows = ceil(sqrt(nBaseStations));
                numCols = ceil(nBaseStations / numRows);
                spacingX = areaWidth / (numCols - 1);
                spacingY = areaHeight / (numRows - 1)
                [X, Y] = meshgrid(areaWidth/numRows/2+linspace(0, numRows-1, numRows)*areaWidth/numRows, areaHeight/numCols/2+linspace(0, numCols-1, numCols)*areaHeight/numCols);                
                baseStationPositionsX = X(:);
                baseStationPositionsY = Y(:);                
                baseStationPositionsX = baseStationPositionsX(1:nBaseStations);
                baseStationPositionsY = baseStationPositionsY(1:nBaseStations);
            case 4 % Hexagonal Grid
                numRows = ceil(sqrt(nBaseStations));
                numCols = ceil(nBaseStations / numRows);
                %[X, Y] = meshgrid(linspace(0, areaWidth, numCols), linspace(0, areaHeight, numRows));
                [X, Y] = meshgrid(areaWidth/numRows/2+linspace(0, numRows-1, numRows)*areaWidth/numRows, areaHeight/numCols/2+linspace(0, numCols-1, numCols)*(areaHeight - areaHeight/(2*numCols))/numCols);
                baseStationPositionsX = X(:);
                baseStationPositionsY = Y(:);
                % Adjust positions to form a diamond grid
                for row = 2:2:numRows
                    baseStationPositionsY((row-1)*numCols + 1:row*numCols) = baseStationPositionsY((row-1)*numCols + 1:row*numCols) + areaHeight / (2 * numRows);
                end
                baseStationPositionsX = baseStationPositionsX(1:nBaseStations);
                baseStationPositionsY = baseStationPositionsY(1:nBaseStations);
            otherwise
                error('Deployment type not yet implemented');
        end
        
        % Store base station positions for future updates
        setappdata(hFig, 'baseStationPositionsX', baseStationPositionsX);
        setappdata(hFig, 'baseStationPositionsY', baseStationPositionsY);
        setappdata(hFig, 'nBaseStations', nBaseStations);
        setappdata(hFig, 'pathLossExponent', pathLossExponent);
        setappdata(hFig, 'nUsers', nUsers);
        
        % Call updateDisplay to draw the initial state
        updateDisplay();
    end

    function updateDisplay(~, ~)
        % Retrieve base station positions and other parameters
        baseStationPositionsX = getappdata(hFig, 'baseStationPositionsX');
        baseStationPositionsY = getappdata(hFig, 'baseStationPositionsY');
        nBaseStations = getappdata(hFig, 'nBaseStations');
        pathLossExponent = getappdata(hFig, 'pathLossExponent');
        nUsers = getappdata(hFig, 'nUsers');
        
        % Define the area dimensions
        areaWidth = 10000;
        areaHeight = 10000;
        
        % Plot the base stations
        cla(hAxes);
        hold(hAxes, 'on');
        plot(hAxes, baseStationPositionsX, baseStationPositionsY, 'ro', 'MarkerSize', 8, 'LineWidth', 1.5);
        title(hAxes, sprintf('Base Stations Placement (%d stations)', nBaseStations));
        
        % Create Voronoi tessellation based on the base station positions
        [vCells, vCentroids] = voronoin([baseStationPositionsX, baseStationPositionsY]);        
        voronoi(hAxes, baseStationPositionsX, baseStationPositionsY);        
        
        % Calculate the maximum distance within the cell for each base station
        maxDistances = zeros(nBaseStations, 1);
        for i = 1:nBaseStations
            edges = vCells(vCentroids{i}, :);            
            edges = edges(find(isfinite(edges(:, 1))), :);
            distances = sqrt((edges(:, 1) - baseStationPositionsX(i)).^2 + (edges(:, 2) - baseStationPositionsY(i)).^2);
            maxDistances(i) = max(distances);
        end
        
        % Calculate path-loss, noise power, and required transmission power
        validDistances = maxDistances(isfinite(maxDistances));
        lambda = 4 * pi;
        pl = (lambda ./ (4 * pi * validDistances)).^pathLossExponent;
        np = -174 + 10 * log10(20000);
        txPower = 10 + np - 10 * log10(pl);
        
        % Annotate the transmission power next to each base station
        for i = 1:nBaseStations
            if isfinite(maxDistances(i))
                text(hAxes, baseStationPositionsX(i), baseStationPositionsY(i), sprintf('%.2f dBm', txPower(i)), 'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right');
            else
                text(hAxes, baseStationPositionsX(i), baseStationPositionsY(i), 'Inf dBm', 'VerticalAlignment', 'bottom', 'HorizontalAlignment', 'right');
            end
        end
        
        % Update sum power text field
        sumPower = sum(10.^(txPower./10)/1000);
        sumPower = 10*log10(1000*sumPower);
        set(hSumPower, 'String', sprintf('%.2f dBm', sumPower));
        
        % Handle dropdown selection for displaying additional information
        dropdownValue = get(hDropdownDisplay, 'Value');
        if dropdownValue == 1 % Only Histogram        
            % Create histogram of transmission power
            figure;
            histogram(txPower);
            title('Histogram of Transmission Power');
            xlabel('Transmit Power (dBm)');
            ylabel('Frequency');
        elseif dropdownValue == 2 % Show Colormap
            % Create a grid for calculating signal-to-interference ratio (SIR)
            gridResolution = 200;
            [gridX, gridY] = meshgrid(linspace(0, areaWidth, gridResolution), linspace(0, areaHeight, gridResolution));
            gridPoints = [gridX(:), gridY(:)];
            
            % Calculate received power at each grid point from each base station
            receivedPower = zeros(size(gridX, 1), size(gridX, 2), nBaseStations);
            for i = 1:nBaseStations
                distances = sqrt((gridX - baseStationPositionsX(i)).^2 + (gridY - baseStationPositionsY(i)).^2);
                pathLoss = (1 ./ (4 * pi * distances)).^pathLossExponent;
                receivedPower(:, :, i) = 10.^(txPower(i) / 10) .* pathLoss;
            end
            
            % Compute SIR at each grid point
            signalPower = max(receivedPower, [], 3);
            interferencePower = sum(receivedPower, 3) - signalPower;
            SIR = signalPower ./ interferencePower;
            
            % Plot SIR as a colormap
            SIRdB = 10 * log10(SIR);
            imagesc(hAxes, [0, areaWidth], [0, areaHeight], SIRdB);
            set(hAxes, 'YDir', 'normal');
            colormap(hAxes, 'jet');
            colorbar;
        elseif dropdownValue == 3 % Show Rate
            % Calculate area for each Voronoi cell and assign 100/area value
            cellValues = zeros(nBaseStations, 1);
            for i = 1:nBaseStations                            
                if all(isfinite(vCells(vCentroids{i}, :))) % Ignore cells that extend to infinity
                    cellCoords = vCells(vCentroids{i}, :);
                    cellArea = polyarea(cellCoords(:, 1), cellCoords(:, 2));                    
                    cellValues(i) = 100 / max(1, nUsers * cellArea / (10000 * 10000));
                end
            end
            
            % Plot the cells with colors representing the rate values
            cmap = colormap(hAxes, 'parula');
            for i = 1:nBaseStations
                if all(isfinite(vCells(vCentroids{i}, :))) % Ignore cells that extend to infinity
                    fill(hAxes, vCells(vCentroids{i}, 1), vCells(vCentroids{i}, 2), cellValues(i), 'FaceAlpha', 0.6);
                end
            end
            colorbar;
        end
        hold(hAxes, 'off');

    end

    % Callback function to resize UI elements when figure size changes
    function resizeUI(~, ~)
        figPos = get(hFig, 'Position');
        margin = 20;
        elementHeight = 25;
        spacing = 10;
        % Update positions of GUI elements
        set(hTextNumBaseStations, 'Position', [margin, figPos(4) - margin - elementHeight, 150, elementHeight]);
        set(hNumBaseStations, 'Position', [180, figPos(4) - margin - elementHeight, 100, elementHeight]);
        set(hTextPathLoss, 'Position', [margin, figPos(4) - margin - 2 * (elementHeight + spacing), 150, elementHeight]);
        set(hPathLoss, 'Position', [180, figPos(4) - margin - 2 * (elementHeight + spacing), 100, elementHeight]);
        set(hTextNumUsers, 'Position', [margin, figPos(4) - margin - 3 * (elementHeight + spacing), 150, elementHeight]);
        set(hNumUsers, 'Position', [180, figPos(4) - margin - 3 * (elementHeight + spacing), 100, elementHeight]);
        set(hButtonGenerate, 'Position', [300, figPos(4) - margin - elementHeight, 100, elementHeight]);
        set(hTextDeployment, 'Position', [450, figPos(4) - margin - 2 * (elementHeight + spacing), 150, elementHeight]);
        set(hDropdownDeployment, 'Position', [610, figPos(4) - margin - 2 * (elementHeight + spacing), 150, elementHeight]);
        set(hTextSumPower, 'Position', [450, figPos(4) - margin - elementHeight, 150, elementHeight]);
        set(hSumPower, 'Position', [610, figPos(4) - margin - elementHeight, 100, elementHeight]);
        set(hDropdownDisplay, 'Position', [750, figPos(4) - margin - elementHeight, 150, elementHeight]);
        set(hButtonUpdate, 'Position', [910, figPos(4) - margin - elementHeight, 100, elementHeight]);
        set(hAxes, 'Position', [0.1, 0.1, 0.8, 0.7]);
    end

end
