% Interactive GUI to compare the total power needed to cover an area
% with N base stations in mobile communications.

function interactive_power_comparison
    % Create the figure window
    fig = figure('Name', 'Total Power Comparison', 'NumberTitle', 'off', 'Position', [100 100 600 400]);

    % Constants
    L = 1;       % Length of the line (e.g., in kilometers)
    N0 = 1;      % Noise power (arbitrary units)
    
    % Default parameters
    n_default = 3.5;
    SNR_edge_default = 10;
    N_default = 5;
    
    % Create UI controls for path loss exponent (n)
    uicontrol('Style', 'text', 'Position', [20 350 150 20], 'String', 'Path Loss Exponent (n):', 'HorizontalAlignment', 'left');
    n_edit = uicontrol('Style', 'edit', 'Position', [180 350 100 25], 'String', num2str(n_default), 'Callback', @update_plot);
    
    % Create UI controls for SNR at cell edge (SNR_edge)
    uicontrol('Style', 'text', 'Position', [20 310 150 20], 'String', 'SNR at Cell Edge (SNR_{edge}):', 'HorizontalAlignment', 'left');
    SNR_edit = uicontrol('Style', 'edit', 'Position', [180 310 100 25], 'String', num2str(SNR_edge_default), 'Callback', @update_plot);
    
    % Create UI controls for number of base stations (N)
    uicontrol('Style', 'text', 'Position', [20 270 150 20], 'String', 'Number of Base Stations (N):', 'HorizontalAlignment', 'left');
    N_edit = uicontrol('Style', 'edit', 'Position', [180 270 100 25], 'String', num2str(N_default), 'Callback', @update_plot);
    
    % Axes for plotting
    ax = axes('Parent', fig, 'Position', [0.4 0.2 0.55 0.7]);
    grid on;
    xlabel('Number of Base Stations (N)');
    ylabel('Total Transmit Power (P_{total})');
    title('Total Power Needed vs. Number of Base Stations');
    
    % Initial plot
    update_plot();
    
    function update_plot(~, ~)
        % Get parameters from UI controls
        n = str2double(get(n_edit, 'String'));
        SNR_edge_dB = str2double(get(SNR_edit, 'String'));
        N_input = str2double(get(N_edit, 'String'));
        
        % Validate inputs
        if isnan(n) || n <= 0
            errordlg('Please enter a valid positive number for Path Loss Exponent (n).', 'Invalid Input');
            return;
        end
        if isnan(SNR_edge_dB)
            errordlg('Please enter a valid positive number for SNR at Cell Edge (SNR_{edge}).', 'Invalid Input');
            return;
        end
        if isnan(N_input) || N_input < 2 || mod(N_input,1) ~= 0
            errordlg('Please enter an integer value greater than or equal to 2 for Number of Base Stations (N).', 'Invalid Input');
            return;
        end
        
        % Update plot data
        N_min = 2;
        N_max = max(N_input, 10);  % Ensure we have a range up to at least the input N
        N_values = N_min:N_max;
        P_total_values = zeros(size(N_values));
        
        for idx = 1:length(N_values)
            N_iter = N_values(idx);
            s_iter = L / N_iter;
            d_iter = s_iter / 2;
            Pt_iter = 10^(SNR_edge_dB/10) / (1 / d_iter)^n;
            P_total_values(idx) = N_iter * Pt_iter;
            if idx > 1
                P_total_values(idx) = P_total_values(idx) / P_total_values(1)
            end
        end
        P_total_values(1) = 1.0
        
        % Update the plot
        cla(ax);
        plot(ax, N_values, P_total_values, '-o', 'LineWidth', 2);
        hold(ax, 'on');
        % Highlight the current N
        idx_current = find(N_values == N_input);
        plot(ax, N_values(idx_current), P_total_values(idx_current), 'ro', 'MarkerSize', 10, 'MarkerFaceColor', 'r');
        hold(ax, 'off');
        grid(ax, 'on');
        xlabel(ax, 'Number of Base Stations (N)');
        ylabel(ax, 'Total Transmit Power (P_{total})');
        title(ax, 'Total Power Needed vs. Number of Base Stations');
        
        % Display the current calculation results
        s = L / N_input;
        d = s / 2;
        Pt = 10^(SNR_edge_dB/10) / (1/d)^n;
        P_total = N_input * Pt;
        
        % Update results display
        results_str = {
            sprintf('Results for N = %d:', N_input)
            sprintf('Distance between base stations (s): %.4f units', s)
            sprintf('Distance to cell edge (d): %.4f units', d)
            sprintf('Transmit power per base station (Pt): %.4f units', Pt)
            sprintf('Total transmit power (P_{total}): %.4f units', P_total)
            };
        
        % Remove previous results text if exists
        delete(findall(fig, 'Tag', 'ResultsText'));
        
        % Display the results in a text box
        annotation('textbox', [0.05 0.05 0.9 0.15], 'String', results_str, 'EdgeColor', 'none', 'FontSize', 10, 'Tag', 'ResultsText');
    end
end
