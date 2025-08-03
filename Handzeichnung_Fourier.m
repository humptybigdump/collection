function Hand_Written_Fourier
% Step 0: Clean up workspace and close all open figures
clear;
close all;

% Step 1: Initialize figure and set axis limits
figure('position',[100  100 1600 800]);
axis([0 10 -5 5]); % Set axis limits for x and y
title('Click to start, move, and click again to stop (at least 10 pixels away)');
xlabel('x');
ylabel('y');
grid on;
hold on;

x = [];
y = [];

% Set up the mouse button callback for registering the start click
set(gcf, 'WindowButtonDownFcn', @startDrawing);

% Variables to track mouse movement and starting position
isDrawing = false;
x_start = 0;
y_start = 0;
last_x = -Inf; % Initialize with a very small value for comparison

    function startDrawing(~, ~)
        % Left-click to start tracking mouse movement
        if strcmp(get(gcf, 'SelectionType'), 'normal') && ~isDrawing
            isDrawing = true;
            set(gcf, 'WindowButtonMotionFcn', @mouseMotion);  % Track mouse motion
            set(gcf, 'WindowButtonUpFcn', @stopDrawing);       % Track stop drawing with click
            [x_start, y_start] = getMousePosition();          % Get start point
            x = [x; x_start];
            y = [y; y_start];
            last_x = x_start; % Set last_x as the starting point
            plot(x_start, y_start, 'bo', 'MarkerFaceColor', 'b'); % Plot starting point
        end
    end

    function stopDrawing(~, ~)
        % Stop tracking mouse movement when releasing the click
        % Ensure the second click is at least 10 pixels away from the start point
        [x_current, y_current] = getMousePosition();
        if isDrawing && sqrt((x_current - x_start)^2 + (y_current - y_start)^2) > 10/72 % 10 pixels tolerance
            isDrawing = false;
            set(gcf, 'WindowButtonMotionFcn', '');  % Stop tracking mouse motion
            set(gcf, 'WindowButtonUpFcn', '');      % Stop tracking release of mouse button

            % Clean the data by keeping unique x-values (bijective function)
            [x, unique_indices] = unique(x, 'stable');
            y = y(unique_indices);

            % Now proceed with Fourier and plotting
            plotFourierSeries(x, y); % Call the Fourier processing and plotting function
        end
    end

    function mouseMotion(~, ~)
        % This function tracks the mouse motion while the button is held
        if isDrawing
            [x_current, y_current] = getMousePosition();    % Get current mouse position

            % Only record points if x_current is greater than the last recorded x
            if x_current > last_x
                x = [x; x_current]; % Collect points as the mouse moves
                y = [y; y_current];
                last_x = x_current; % Update last recorded x-value
                plot(x_current, y_current, 'b.', 'MarkerSize', 10); % Plot the motion point
                drawnow; % Update the plot in real-time
            end
        end
    end

    function [x_pos, y_pos] = getMousePosition()
        % Helper function to get the mouse position in axis coordinates
        mouse_pos = get(gca, 'CurrentPoint');
        x_pos = mouse_pos(1, 1);
        y_pos = mouse_pos(1, 2);
    end

    function plotFourierSeries(x, y)
        % Step 2: Linear interpolation on the collected data
        x = x - min(x); % Normalize x-axis to start from 0
        L = max(x); % Period length of the curve
        xx = linspace(0, L, 1000); % Fine grid for interpolation
        yy_linear = interp1(x, y, xx, 'linear'); % Linear interpolation

        % Step 3: Calculate Fourier coefficients (harmonic representation)
        N = 20; % Number of Fourier coefficients to calculate
        a0 = (2/L) * trapz(xx, yy_linear); % DC component (mean value)
        a = zeros(1, N); % Cosine coefficients
        b = zeros(1, N); % Sine coefficients

        % Fourier coefficients for cosine and sine terms
        for n = 1:N
            a(n) = (2/L) * trapz(xx, yy_linear .* cos(2 * pi * n * xx / L));
            b(n) = (2/L) * trapz(xx, yy_linear .* sin(2 * pi * n * xx / L));
        end

        % Step 4: Plot the original function and Fourier approximations
        figure;
        subplot(2, 1, 1); % Original vs Fourier approximation plot
        hold on;
        plot(xx, yy_linear, 'k-', 'LineWidth', 2, 'DisplayName', 'Original Curve'); % Original curve

        % Fourier series reconstruction
        yy_approx = a0 / 2; % Start with the DC component (a0/2)
        colors = jet(N); % Color scheme for different harmonics

        for n = 1:N
            yy_approx = yy_approx + a(n) * cos(2 * pi * n * xx / L) + b(n) * sin(2 * pi * n * xx / L);
            plot(xx, yy_approx, 'Color', colors(n, :), 'DisplayName', sprintf('k = %d', n),'LineWidth',1); % Approximation curve
            pause(0.5); % Pause to show each harmonic being added
        end
        title('Original Function and Fourier Series Approximation');
        xlabel('x');
        ylabel('y');
        legend('show','Location','eastoutside');
        grid on;
        hold off;

        % Step 5: Energy calculation for the signal and Fourier components

        % Total energy of the original signal
        total_energy = trapz(xx, yy_linear.^2);

        % Energy of the DC component
        E0 = (L / 2) * (a0 / 2)^2;

        % Energy of the Fourier components
        E_components = zeros(1, N);
        for n = 1:N
            E_components(n) = (L / 2) * (a(n)^2 + b(n)^2);
        end

        % Total energy of the Fourier series (sum of all components)
        total_energy_fourier = E0 + sum(E_components);

        % Compare first harmonic's energy to the total energy
        E_first_mode = E_components(1);
        energy_ratio = E_first_mode / total_energy;

        % Display the energy comparison
        disp(['Total energy of the original signal: ', num2str(total_energy)]);
        disp(['Total energy of the Fourier series: ', num2str(total_energy_fourier)]);
        disp(['Energy of the first mode: ', num2str(E_first_mode)]);
        disp(['Ratio of first mode energy to total energy: ', num2str(energy_ratio)]);

        % Step 6: Plot the strength of Fourier components in 3D with consistent colors
        figure;
        hold on;

        % Color scheme for different harmonics (reuse the 'colors' from previous plots)
        colors = jet(N); % Use the same color map as before

        % For each harmonic order n, plot a line and marker with the same color
        for n = 1:N
            % Plot the stem from [n, 0, 0] to [n, a(n), b(n)] using the same color
            plot3([n, n], [0, a(n)], [0, b(n)], 'Color', colors(n, :), 'LineWidth', 2);
            % Plot the head of the stem with the same color
            plot3(n, a(n),  b(n), 'o', 'MarkerFaceColor', colors(n, :), 'MarkerEdgeColor', colors(n, :));
        end

        title('Fourier Component Amplitudes in 3D');
        xlabel('Harmonic Order (k)');
        ylabel('Cosine Coefficient (C_k)');
        zlabel('Sine Coefficient (S_k)');
        grid on;
        hold off;
        view(3)
        % Set equal scaling for y and z, and independent scaling for x
daspect([1 1 1]); % Set equal aspect ratio for all axes initially
pbaspect([1 1 1]); % Keep plot box proportions the same
ax = gca; 
ax.DataAspectRatio = [2 1 1]; % This sets x to have a different scaling (2:1:1), but y and z to have equal scaling (1:1)




        % Step 7: Final plot with original points, linear interpolation, and Fourier after 20 terms
        figure;
        hold on;
        plot(x, y, 'ro', 'MarkerSize', 8, 'DisplayName', 'Collected Points'); % Collected points
        plot(xx, yy_linear, 'k-', 'LineWidth', 2, 'DisplayName', 'Linear Interpolation'); % Linear interpolation

        yy_final = a0 / 2;
        for n = 1:N
            yy_final = yy_final + a(n) * cos(2 * pi * n * xx / L) + b(n) * sin(2 * pi * n * xx / L);
        end
        plot(xx, yy_final, 'color',[0 150 130]/255, 'LineWidth', 2, 'DisplayName', 'Fourier (20 terms)'); % Fourier after 20 terms

        title('Collected Points, Linear Interpolation, and Fourier (20 terms)');
        xlabel('x');
        ylabel('y');
        legend('show');
        grid on;
        hold off;
    end
end
