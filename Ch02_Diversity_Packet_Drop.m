function packet_transmission_simulation()
    % Packet Transmission Simulation with Multiple Methods and Restart Button

    % Create the figure and UI elements
    fig = figure('Name', 'Packet Transmission Simulation', 'NumberTitle', 'off', 'MenuBar', 'none', 'ToolBar', 'none');
    set(fig, 'Position', [100, 100, 900, 600]);

    % Initialize variables
    N = 10; % Number of rows
    M = 20; % Number of columns
    average_SNR_dB = 10; % Average SNR in dB
    p = 0.5; % Outage probability
    method_list = {'Method 1: No combining, No ARQ', 'Method 2: Chase Combining (ARQ)', 'Method 3: Selection Combining (no ARQ)', 'Method 4: Chase Combining (no ARQ)'};
    selected_method = 1; % Default to Method 1

    % Create UI controls for parameters
    uicontrol('Style', 'text', 'Position', [20 550 100 20], 'String', 'Rows (N):', 'HorizontalAlignment', 'left');
    N_edit = uicontrol('Style', 'edit', 'Position', [120 550 80 25], 'String', num2str(N));

    uicontrol('Style', 'text', 'Position', [220 550 100 20], 'String', 'Columns (M):', 'HorizontalAlignment', 'left');
    M_edit = uicontrol('Style', 'edit', 'Position', [320 550 80 25], 'String', num2str(M));

    uicontrol('Style', 'text', 'Position', [420 550 100 20], 'String', 'Average SNR (dB):', 'HorizontalAlignment', 'left');
    SNR_edit = uicontrol('Style', 'edit', 'Position', [520 550 80 25], 'String', num2str(average_SNR_dB));

    uicontrol('Style', 'text', 'Position', [620 550 120 20], 'String', 'Outage Probability (p):', 'HorizontalAlignment', 'left');
    p_edit = uicontrol('Style', 'edit', 'Position', [740 550 50 25], 'String', num2str(p));

    % Dropdown menu for method selection
    uicontrol('Style', 'text', 'Position', [20 500 150 20], 'String', 'Select Method:', 'HorizontalAlignment', 'left');
    method_menu = uicontrol('Style', 'popupmenu', 'Position', [170 500 400 25], 'String', method_list, 'Callback', @method_selection_callback);

    % Create buttons
    start_button = uicontrol('Style', 'pushbutton', 'Position', [600 500 100 30], 'String', 'Start Simulation', 'Callback', @start_simulation);
    restart_button = uicontrol('Style', 'pushbutton', 'Position', [720 500 100 30], 'String', 'Restart Simulation', 'Callback', @restart_simulation, 'Enable', 'off');

    % Axes for grid
    ax_grid = axes('Parent', fig, 'Position', [0.05 0.1 0.6 0.7]);
    axis(ax_grid, 'equal');
    axis(ax_grid, [0 M 0 N]);
    set(ax_grid, 'XTick', 0:M, 'YTick', 0:N, 'XTickLabel', [], 'YTickLabel', [], 'TickLength', [0 0], 'YDir', 'reverse');
    grid(ax_grid, 'on');
    hold(ax_grid, 'on');

    % Axes for statistics
    ax_stats = axes('Parent', fig, 'Position', [0.7 0.1 0.25 0.7], 'Visible', 'off');

    % Variables to store simulation data
    counter = 0; % Successful transmissions
    total_packets = 0;

    % Callback functions
    function method_selection_callback(source, ~)
        selected_method = source.Value;
    end

    function start_simulation(~, ~)
        % Disable parameter editing during simulation
        set([N_edit, M_edit, SNR_edit, p_edit, start_button, method_menu], 'Enable', 'off');
        set(restart_button, 'Enable', 'on');

        % Read parameters
        N = str2double(get(N_edit, 'String'));
        M = str2double(get(M_edit, 'String'));
        average_SNR_dB = str2double(get(SNR_edit, 'String'));
        p = str2double(get(p_edit, 'String'));

        % Validate inputs
        if isnan(N) || N <= 0 || isnan(M) || M <= 0 || isnan(average_SNR_dB) || isnan(p) || p <= 0 || p >= 1
            errordlg('Invalid input parameters.', 'Error');
            return;
        end

        % Clear previous grid
        cla(ax_grid);
        axis(ax_grid, [0 M 0 N]);
        set(ax_grid, 'XTick', 0:M, 'YTick', 0:N, 'XTickLabel', [], 'YTickLabel', [], 'TickLength', [0 0], 'YDir', 'reverse');
        grid(ax_grid, 'on');
        hold(ax_grid, 'on');

        % Initialize counter
        counter = 0;
        total_packets = N * M;

        % Initialize packet number
        packet_number = 0;
        n = 0;

        if selected_method == 1 || selected_method == 2
            % Loop through each cell
            for i = 1:N % For each row
                for j = 1:M % For each column
                    packet_number = packet_number + 1;
    
                    % Determine outage probability based on method
                    n = n + 1
                    switch selected_method
                        case 1 % Method 1: Constant Outage Probability
                            p_n = p; % Use the constant outage probability
                        case 2 % Method 2: Variable Outage Probability per Packet
                            % MRC
                            norm_g = -log(1 - p);
                            mrc = exp(-norm_g);                                    
                            for k = 2:n                            
                                mrc = mrc + 1/factorial(k-1) * exp(-norm_g) .* norm_g.^(k-1);
                            end
                            mrc = 1.0 - mrc;
                            
                            p_n = mrc;
                    end
    
                    % Simulate packet transmission
                    is_dropped = rand < p_n; % Packet is dropped with probability p_n
    
                    if is_dropped
                        % Color the cell red
                        color = 'red';
                        % Do not increase the counter
                    else
                        % Color the cell green
                        color = 'green';
                        % Increase the counter
                        counter = counter + 1;
                        n = 0;
                    end
    
                    % Coordinates for the cell
                    x = [j - 1, j, j, j - 1];
                    y = [i - 1, i - 1, i, i];
                    patch(ax_grid, x, y, color);
    
                    % Write the counter in the cell if not dropped
                    if ~is_dropped
                        text(ax_grid, j - 0.5, i - 0.5, num2str(counter), 'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle', 'FontSize', 12, 'FontWeight', 'bold');
                    end
    
                    % Pause briefly to animate
                    pause(0.02);
    
                    % Update the plot
                    drawnow;
                end
            end
        else % Method 3 or 4
            % Loop through each cell                        
            for i = 1:N % For each row
                j = 0
                while j < M                
                    j = j+1;
                    packet_number = packet_number + 1;
                    if selected_method == 3
                        p_n = p*p
                    else
                        % MRC
                        norm_g = -log(1 - p);
                        mrc = exp(-norm_g);        
                        l = 2
                        for k = 2:l                            
                            mrc = mrc + 1/factorial(k-1) * exp(-norm_g) .* norm_g.^(k-1);
                        end
                        mrc = 1.0 - mrc;
                        
                        p_n = mrc;
                    end
    
                    % Simulate packet transmission
                    is_dropped = rand < p_n; % Packet is dropped with probability p_n
    
                    if is_dropped
                        % Color the cell red
                        color = 'red';
                        % Do not increase the counter
                    else
                        % Color the cell green
                        color = 'green';                        
                        counter = counter + 1;
                    end
                        
                    % Coordinates for the cell
                    x = [j - 1, j, j, j - 1];
                    y = [i - 1, i - 1, i, i];
                    patch(ax_grid, x, y, color);
    
                    % Write the counter in the cell if not dropped
                    if ~is_dropped
                        text(ax_grid, j - 0.5, i - 0.5, num2str(counter), 'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle', 'FontSize', 12, 'FontWeight', 'bold');
                    end

                    % Always two packets ...
                    j = j+1
                    % Coordinates for the cell
                    x = [j - 1, j, j, j - 1];
                    y = [i - 1, i - 1, i, i];
                    patch(ax_grid, x, y, color);
    
                    % Write the counter in the cell if not dropped
                    if ~is_dropped
                        text(ax_grid, j - 0.5, i - 0.5, num2str(counter), 'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle', 'FontSize', 12, 'FontWeight', 'bold');
                    end

                    % Pause briefly to animate
                    pause(0.04);
    
                    % Update the plot
                    drawnow;
                end
            end
        end

        % Calculate Target SNR
        target_SNR_linear = (-log(1 - p) / log(exp(1))) * 10^(average_SNR_dB / 10);
        target_SNR_dB = 10 * log10(target_SNR_linear);

        % Calculate Rate per Packet
        R = log2(1 + target_SNR_linear);

        % Calculate Total Rate
        total_rate = counter * R;

        % Display statistics
        cla(ax_stats);
        set(ax_stats, 'Visible', 'on', 'XLim', [0 1], 'YLim', [0 1], 'XTick', [], 'YTick', []);
        stats_text = {
            sprintf('Total Packets: %d', total_packets),
            sprintf('Successful Transmissions: %d', counter),
            sprintf('Outage Probability (p): %.4f', p),
            sprintf('Average SNR (dB): %.2f', average_SNR_dB),
            sprintf('Target SNR (Linear): %.4f', target_SNR_linear),
            sprintf('Target SNR (dB): %.2f dB', target_SNR_dB),
            sprintf('Rate per Packet (R): %.4f bits/symbol', R),
            sprintf('Total Rate: %.4f bits/symbol', total_rate)
        };
        text(ax_stats, 0.05, 0.9, stats_text, 'VerticalAlignment', 'top', 'FontSize', 12);

        % Update title
        title(ax_grid, sprintf('Total Successful Transmissions: %d out of %d', counter, total_packets));
    end

    function restart_simulation(~, ~)
        % Re-enable parameter editing
        set([N_edit, M_edit, SNR_edit, p_edit, start_button, method_menu], 'Enable', 'on');
        set(restart_button, 'Enable', 'off');

        % Clear grid and statistics
        cla(ax_grid);
        axis(ax_grid, [0 M 0 N]);
        set(ax_grid, 'XTick', 0:M, 'YTick', 0:N, 'XTickLabel', [], 'YTickLabel', [], 'TickLength', [0 0], 'YDir', 'reverse');
        grid(ax_grid, 'on');
        hold(ax_grid, 'on');

        cla(ax_stats);
        set(ax_stats, 'Visible', 'off');

        % Reset counter
        counter = 0;
        total_packets = 0;
    end
end
