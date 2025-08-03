% Interactive Matlab GUI to visualize Antenna Array Scaling

function interactive_antenna_gui
    % Create the GUI figure
    hFig = figure('Name', 'Antenna Array Scaling', 'NumberTitle', 'off', 'Position', [100, 100, 800, 600]);
    
    % Create UI controls for number of antennas and array scaling options
    uicontrol('Style', 'text', 'Position', [20, 550, 150, 25], 'String', 'Number of Antennas:');
    hNumAntennas = uicontrol('Style', 'edit', 'Position', [180, 550, 100, 25], 'String', '10');
    
    hCheckboxScale = uicontrol('Style', 'checkbox', 'Position', [20, 510, 150, 25], 'String', 'Scale Antenna Array', 'Callback', @toggleScale);
    
    uicontrol('Style', 'text', 'Position', [20, 470, 150, 25], 'String', 'Antenna Spacing (if scaled):');
    hAntennaSpacing = uicontrol('Style', 'edit', 'Position', [180, 470, 100, 25], 'String', '0.5', 'Enable', 'off');
    
    uicontrol('Style', 'text', 'Position', [20, 430, 150, 25], 'String', 'Array Size (if not scaled):');
    hArraySize = uicontrol('Style', 'edit', 'Position', [180, 430, 100, 25], 'String', '5', 'Enable', 'on');
    
    % Create UI control for the Show button
    hButtonShow = uicontrol('Style', 'pushbutton', 'Position', [300, 550, 100, 25], 'String', 'Show', 'Callback', @updateFigure);
    % Create UI control for the Redraw button
    hButtonRedraw = uicontrol('Style', 'pushbutton', 'Position', [420, 550, 100, 25], 'String', 'Redraw', 'Callback', @updateFigure);
    
    % Axes for the antenna array visualization
    hAxes = axes('Parent', hFig, 'Position', [0.1, 0.1, 0.8, 0.5]);
    xlabel(hAxes, 'Incoming Angle (degrees)');
    ylabel(hAxes, 'Normalized Received Power');
    title(hAxes, 'Antenna Array Response');
    grid(hAxes, 'on');
    
    % Callback function for toggling between scale options
    function toggleScale(~, ~)
        if get(hCheckboxScale, 'Value')
            set(hAntennaSpacing, 'Enable', 'on');
            set(hArraySize, 'Enable', 'off');
        else
            set(hAntennaSpacing, 'Enable', 'off');
            set(hArraySize, 'Enable', 'on');
        end
    end

    % Callback function for updating the figure
    function updateFigure(~, ~)
        % Get the values from UI controls
        numAntennas = str2double(get(hNumAntennas, 'String'));
        isScaled = get(hCheckboxScale, 'Value');
        
        if isScaled
            antennaSpacing = str2double(get(hAntennaSpacing, 'String'));
            arraySize = antennaSpacing * (numAntennas - 1); % Compute array size based on spacing
        else
            arraySize = str2double(get(hArraySize, 'String'));
            antennaSpacing = arraySize / (numAntennas - 1); % Compute antenna spacing based on array size
        end
        
        % Define angles for visualization
        angles = linspace(-90, 90, 1000);
        
        % Calculate the normalized received power (array factor)
        k = 2 * pi; % Wavenumber (assuming unit wavelength)
        arrayFactor = zeros(size(angles));
        for n = 0:(numAntennas - 1)
            arrayFactor = arrayFactor + exp(1i * k * antennaSpacing * n * sind(angles));
        end
        arrayFactor = abs(arrayFactor / numAntennas).^2; % Normalize and take magnitude squared
        
        % Update the figure
        %if get(hButtonRedraw, 'Value') == 1
        %    cla(hAxes);
        %end

        % Plot the received power
        % If Redraw is pressed, clear the current plot, otherwise overlay
        if strcmp(get(gcbo, 'String'), 'Redraw')
            cla(hAxes);
        end
        x = [-pi:0.01:pi];
        if isScaled
            Lr = numAntennas*antennaSpacing;
        else
            Lr = arraySize;
        end
        ylim(hAxes, [0, 1]);
        y = 1/numAntennas^2 * abs(sin(pi*Lr*cos(x)) ./ sin(pi*Lr/numAntennas*cos(x))).^2;
        plot(hAxes, x, y, 'k-', 'LineWidth', 1.5)        
        hold on
        xlabel(hAxes, 'Incoming angle $\phi$', 'Interpreter','latex');
        ylabel(hAxes, 'Normalized received power $|y|^2/n_r^2$', 'Interpreter','latex')
        title(hAxes, 'Antenna Array Response');
        grid(hAxes, 'on');
    end
end
