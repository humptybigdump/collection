function tdd_latency_simulation()
    % TDD Packet Latency Simulation with Per-Slot Evaluation and Limited Capacity

    % Create the figure and UI elements
    fig = figure('Name', 'TDD Packet Latency Simulation', 'NumberTitle', 'off', 'MenuBar', 'none', 'ToolBar', 'none');
    set(fig, 'Position', [100, 100, 1200, 750]);

    % Initialize default parameters
    N_DL = 5; % Number of consecutive Downlink slots
    N_UL = 5; % Number of consecutive Uplink slots
    lambda_BS = 0.5; % Arrival rate per time step at base station (packets per time step)
    lambda_UE = 0.5; % Arrival rate per time step at terminal (packets per time step)
    slot_duration = 1; % Duration of one slot in milliseconds
    simulation_time = 1000; % Total simulation time in milliseconds

    % Create UI controls for parameters
    uicontrol('Style', 'text', 'Position', [20 700 200 20], 'String', 'Consecutive Downlink Slots:', 'HorizontalAlignment', 'left');
    N_DL_edit = uicontrol('Style', 'edit', 'Position', [220 700 80 25], 'String', num2str(N_DL));

    uicontrol('Style', 'text', 'Position', [320 700 200 20], 'String', 'Consecutive Uplink Slots:', 'HorizontalAlignment', 'left');
    N_UL_edit = uicontrol('Style', 'edit', 'Position', [520 700 80 25], 'String', num2str(N_UL));

    uicontrol('Style', 'text', 'Position', [20 660 200 20], 'String', 'Arrival Rate at BS (\lambda_{BS} per time step):', 'HorizontalAlignment', 'left');
    lambda_BS_edit = uicontrol('Style', 'edit', 'Position', [220 660 80 25], 'String', num2str(lambda_BS));

    uicontrol('Style', 'text', 'Position', [320 660 200 20], 'String', 'Arrival Rate at UE (\lambda_{UE} per time step):', 'HorizontalAlignment', 'left');
    lambda_UE_edit = uicontrol('Style', 'edit', 'Position', [520 660 80 25], 'String', num2str(lambda_UE));

    uicontrol('Style', 'text', 'Position', [20 620 200 20], 'String', 'Slot Duration (ms):', 'HorizontalAlignment', 'left');
    slot_duration_edit = uicontrol('Style', 'edit', 'Position', [220 620 80 25], 'String', num2str(slot_duration));

    uicontrol('Style', 'text', 'Position', [320 620 200 20], 'String', 'Simulation Time (ms):', 'HorizontalAlignment', 'left');
    simulation_time_edit = uicontrol('Style', 'edit', 'Position', [520 620 80 25], 'String', num2str(simulation_time));

    % Create buttons
    start_button = uicontrol('Style', 'pushbutton', 'Position', [650 680 120 30], 'String', 'Start Simulation', 'Callback', @start_simulation);
    stop_button = uicontrol('Style', 'pushbutton', 'Position', [790 680 120 30], 'String', 'Stop Simulation', 'Callback', @stop_simulation, 'Enable', 'off');
    restart_button = uicontrol('Style', 'pushbutton', 'Position', [930 680 120 30], 'String', 'Restart Simulation', 'Callback', @restart_simulation, 'Enable', 'off');

    % Axes for histograms
    ax_hist_BS = axes('Parent', fig, 'Position', [0.05 0.05 0.4 0.35]);
    title(ax_hist_BS, 'Latency Histogram at Base Station');
    xlabel(ax_hist_BS, 'Latency (ms)');
    ylabel(ax_hist_BS, 'Number of Packets');
    grid(ax_hist_BS, 'on');

    ax_hist_UE = axes('Parent', fig, 'Position', [0.55 0.05 0.4 0.35]);
    title(ax_hist_UE, 'Latency Histogram at Terminal');
    xlabel(ax_hist_UE, 'Latency (ms)');
    ylabel(ax_hist_UE, 'Number of Packets');
    grid(ax_hist_UE, 'on');

    % Axes for visualization
    ax_queue = axes('Parent', fig, 'Position', [0.05 0.45 0.9 0.3], 'Visible', 'off');

    % Text handles for displaying slot status and simulation time
    slot_status_text = uicontrol('Style', 'text', 'Position', [650 630 300 30], 'String', '', 'FontSize', 12, 'HorizontalAlignment', 'left');
    simulation_time_text = uicontrol('Style', 'text', 'Position', [650 600 300 30], 'String', '', 'FontSize', 12, 'HorizontalAlignment', 'left');

    % Variables to store simulation data
    latency_BS = []; % Latencies for packets at BS
    latency_UE = []; % Latencies for packets at UE

    % Variables for visualization
    queue_BS = []; % Packet queue at BS
    queue_UE = []; % Packet queue at UE

    % Stop flag
    stop_flag = false;

    % Callback functions
    function start_simulation(~, ~)
        % Disable parameter editing during simulation
        set([N_DL_edit, N_UL_edit, lambda_BS_edit, lambda_UE_edit, slot_duration_edit, simulation_time_edit, start_button], 'Enable', 'off');
        set([stop_button, restart_button], 'Enable', 'on');
        stop_flag = false;

        % Read parameters
        N_DL = str2double(get(N_DL_edit, 'String'));
        N_UL = str2double(get(N_UL_edit, 'String'));
        lambda_BS = str2double(get(lambda_BS_edit, 'String'));
        lambda_UE = str2double(get(lambda_UE_edit, 'String'));
        slot_duration = str2double(get(slot_duration_edit, 'String'));
        simulation_time = str2double(get(simulation_time_edit, 'String'));

        % Validate inputs
        if isnan(N_DL) || N_DL <= 0 || isnan(N_UL) || N_UL <= 0 || isnan(lambda_BS) || lambda_BS < 0 || isnan(lambda_UE) || lambda_UE < 0 || isnan(slot_duration) || slot_duration <= 0 || isnan(simulation_time) || simulation_time <= 0
            errordlg('Invalid input parameters.', 'Error');
            return;
        end

        % Initialize simulation
        latency_BS = [];
        latency_UE = [];
        queue_BS = [];
        queue_UE = [];

        % Build TDD frame structure
        frame_slots = [repmat({'DL'}, 1, N_DL), repmat({'UL'}, 1, N_UL)];
        total_slots = length(frame_slots);

        % Initialize variables
        current_time = 0; % Current simulation time in ms
        slot_end_time = slot_duration; % Time at which current slot ends
        slot_index = 1; % Index of current slot in the frame_slots

        % Counters for transmitted packets in the current DL/UL period
        dl_packets_transmitted = 0;
        ul_packets_transmitted = 0;

        % Visualization setup
        cla(ax_queue);
        set(ax_queue, 'Visible', 'off');

        % Main simulation loop
        while current_time < simulation_time && ~stop_flag
            % Update slot status
            current_slot_type = frame_slots{slot_index};
            time_remaining_in_slot = slot_end_time - current_time;
            set(slot_status_text, 'String', sprintf('Current Slot: %s | Time Remaining: %.2f ms', current_slot_type, time_remaining_in_slot));
            set(simulation_time_text, 'String', sprintf('Simulation Time: %.2f / %.2f ms', current_time, simulation_time));

            % Time step
            time_step = 1; % 1 ms time step
            current_time = current_time + time_step;

            % Generate packet arrivals for this time step
            if rand() < lambda_BS
                % Packet arrives at BS
                packet = struct('arrival_time', current_time, 'latency', 0);
                queue_BS = [queue_BS, packet];
            end
            if rand() < lambda_UE
                % Packet arrives at UE
                packet = struct('arrival_time', current_time, 'latency', 0);
                queue_UE = [queue_UE, packet];
            end

            % Transmit packets if possible
            if strcmp(current_slot_type, 'DL')
                % DL slot
                if dl_packets_transmitted < N_DL && ~isempty(queue_BS)
                    % Transmit one packet
                    packet = queue_BS(1);
                    packet.latency = current_time - packet.arrival_time;
                    latency_BS = [latency_BS; packet.latency];
                    queue_BS(1) = []; % Remove transmitted packet from queue
                    dl_packets_transmitted = dl_packets_transmitted + 1;
                end
            elseif strcmp(current_slot_type, 'UL')
                % UL slot
                if ul_packets_transmitted < N_UL && ~isempty(queue_UE)
                    % Transmit one packet
                    packet = queue_UE(1);
                    packet.latency = current_time - packet.arrival_time;
                    latency_UE = [latency_UE; packet.latency];
                    queue_UE(1) = []; % Remove transmitted packet from queue
                    ul_packets_transmitted = ul_packets_transmitted + 1;
                end
            end

            % Update visualization
            update_visualization();

            % Advance to next slot if current time reaches slot end time
            if current_time >= slot_end_time
                slot_index = mod(slot_index, total_slots) + 1;
                slot_end_time = slot_end_time + slot_duration;

                % Reset counters at the beginning of DL or UL periods
                if strcmp(frame_slots{slot_index}, 'DL') && ~strcmp(current_slot_type, 'DL')
                    dl_packets_transmitted = 0;
                elseif strcmp(frame_slots{slot_index}, 'UL') && ~strcmp(current_slot_type, 'UL')
                    ul_packets_transmitted = 0;
                end

                % Check if a full frame has been completed
                if slot_index == 1
                    % Update histograms after each full frame
                    update_histograms();
                end
            end

            % Pause to simulate real-time visualization
            pause(0.001);

            % Check for stop flag
            if stop_flag
                break;
            end
        end

        % Final update of histograms
        update_histograms();

        % Disable slot status text after simulation
        set(slot_status_text, 'String', 'Simulation Complete');
        set(simulation_time_text, 'String', '');

        % Nested function for updating visualization
        function update_visualization()
            % Clear previous visualization
            cla(ax_queue);
            set(ax_queue, 'Visible', 'on');
            axis(ax_queue, [0 100 0 100]);
            set(ax_queue, 'XTick', [], 'YTick', []);
            title(ax_queue, 'Packet Queues Visualization');

            % Display BS queue
            text(ax_queue, 10, 80, 'BS Queue:', 'FontSize', 12, 'FontWeight', 'bold');
            for idx = 1:min(10, length(queue_BS))
                packet = queue_BS(idx);
                text(ax_queue, 10, 80 - idx*5, sprintf('Packet %d: Arrival Time = %.2f ms', idx, packet.arrival_time), 'FontSize', 10);
            end
            if length(queue_BS) > 10
                text(ax_queue, 10, 80 - 11*5, sprintf('... (%d more)', length(queue_BS) - 10), 'FontSize', 10);
            end

            % Display UE queue
            text(ax_queue, 60, 80, 'UE Queue:', 'FontSize', 12, 'FontWeight', 'bold');
            for idx = 1:min(10, length(queue_UE))
                packet = queue_UE(idx);
                text(ax_queue, 60, 80 - idx*5, sprintf('Packet %d: Arrival Time = %.2f ms', idx, packet.arrival_time), 'FontSize', 10);
            end
            if length(queue_UE) > 10
                text(ax_queue, 60, 80 - 11*5, sprintf('... (%d more)', length(queue_UE) - 10), 'FontSize', 10);
            end

            % Display current slot type
            rectangle(ax_queue, 'Position', [45, 50, 10, 10], 'FaceColor', 'yellow');
            text(ax_queue, 50, 55, current_slot_type, 'HorizontalAlignment', 'center', 'VerticalAlignment', 'middle', 'FontSize', 12, 'FontWeight', 'bold');

            drawnow;
        end

        % Nested function to update histograms
        function update_histograms()
            % Update histograms with current data
            cla(ax_hist_BS);
            histogram(ax_hist_BS, latency_BS, 'BinWidth', slot_duration / 2);
            title(ax_hist_BS, 'Latency Histogram at Base Station');
            xlabel(ax_hist_BS, 'Latency (ms)');
            ylabel(ax_hist_BS, 'Number of Packets');
            grid(ax_hist_BS, 'on');

            cla(ax_hist_UE);
            histogram(ax_hist_UE, latency_UE, 'BinWidth', slot_duration / 2);
            title(ax_hist_UE, 'Latency Histogram at Terminal');
            xlabel(ax_hist_UE, 'Latency (ms)');
            ylabel(ax_hist_UE, 'Number of Packets');
            grid(ax_hist_UE, 'on');
        end
    end

    function stop_simulation(~, ~)
        stop_flag = true;
        set(stop_button, 'Enable', 'off');
    end

    function restart_simulation(~, ~)
        % Re-enable parameter editing
        set([N_DL_edit, N_UL_edit, lambda_BS_edit, lambda_UE_edit, slot_duration_edit, simulation_time_edit, start_button], 'Enable', 'on');
        set([stop_button, restart_button], 'Enable', 'off');
        stop_flag = false;

        % Clear histograms
        cla(ax_hist_BS);
        cla(ax_hist_UE);

        % Clear visualization
        cla(ax_queue);
        set(ax_queue, 'Visible', 'off');
        set(slot_status_text, 'String', '');
        set(simulation_time_text, 'String', '');

        % Reset latency data
        latency_BS = [];
        latency_UE = [];
        queue_BS = [];
        queue_UE = [];
    end
end
