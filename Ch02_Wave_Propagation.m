function wave_propagation_moving_source_gui
    % Create the figure window
    fig = figure('Name', 'Wave Propagation Simulation with Moving Source', 'NumberTitle', 'off', ...
        'Position', [100 100 800 600]);

    % Axes for wave propagation visualization
    ax_wave = axes('Parent', fig, 'Position', [0.05 0.3 0.6 0.65]);
    axis(ax_wave, [-10 10 -5 5]);
    axis equal;
    grid(ax_wave, 'on');
    xlabel(ax_wave, 'X');
    ylabel(ax_wave, 'Y');
    title(ax_wave, 'Wave Propagation Simulation with Moving Source');
    hold(ax_wave, 'on');  % Ensure hold is on

    % Axes for received amplitude over time
    ax_signal = axes('Parent', fig, 'Position', [0.7 0.3 0.25 0.65]);
    grid(ax_signal, 'on');
    xlabel(ax_signal, 'Time');
    ylabel(ax_signal, 'Received Amplitude');
    title(ax_signal, 'Received Signal at Sink');

    % Default parameters
    source_initial_pos = [-8, 0]; % Starting position of the source
    sink_pos = [0, 0];
    num_reflectors = 5;
    reflector_radius = 0.5;
    wave_speed = 5;     % Speed of the wave
    source_speed = 2;   % Speed of the source (must be less than wave_speed)
    frequency = 1;      % Frequency of the wave
    time_step = 0.01;   % Time step for the simulation
    cycles_to_display = 20;  % Number of wave cycles to display at the sink

    % Flags to track simulation state
    is_simulation_running = false;
    stop_simulation = false;

    % Create UI controls
    uicontrol('Style', 'text', 'Position', [20 100 100 20], 'String', 'Wave Speed:', 'HorizontalAlignment', 'left');
    speed_edit = uicontrol('Style', 'edit', 'Position', [120 100 80 25], 'String', num2str(wave_speed), 'Callback', @update_params);

    uicontrol('Style', 'text', 'Position', [220 100 120 20], 'String', 'Source Speed:', 'HorizontalAlignment', 'left');
    source_speed_edit = uicontrol('Style', 'edit', 'Position', [340 100 80 25], 'String', num2str(source_speed), 'Callback', @update_params);

    uicontrol('Style', 'text', 'Position', [440 100 120 20], 'String', 'Number of Reflectors:', 'HorizontalAlignment', 'left');
    num_reflectors_edit = uicontrol('Style', 'edit', 'Position', [560 100 80 25], 'String', num2str(num_reflectors), 'Callback', @update_params);

    uicontrol('Style', 'text', 'Position', [20 60 100 20], 'String', 'Reflector Radius:', 'HorizontalAlignment', 'left');
    reflector_radius_edit = uicontrol('Style', 'edit', 'Position', [120 60 80 25], 'String', num2str(reflector_radius), 'Callback', @update_params);

    uicontrol('Style', 'text', 'Position', [220 60 120 20], 'String', 'Frequency:', 'HorizontalAlignment', 'left');
    frequency_edit = uicontrol('Style', 'edit', 'Position', [340 60 80 25], 'String', num2str(frequency), 'Callback', @update_params);

    start_button = uicontrol('Style', 'pushbutton', 'Position', [440 60 100 25], 'String', 'Start Simulation', 'Callback', @start_simulation);

    % Create "Stop Simulation" button
    stop_button = uicontrol('Style', 'pushbutton', 'Position', [560 60 100 25], 'String', 'Stop Simulation', 'Callback', @stop_simulation_callback, 'Enable', 'off');

    % Variables to store reflectors and plotting handles
    reflectors = [];
    source_handle = [];
    sink_scatter_handle = [];
    reflector_handles = [];
    signal_line = [];
    source_pos_history = [];

    % Draw initial setup
    draw_setup();

    % Callback functions
    function update_params(~, ~)
        if is_simulation_running
            errordlg('Cannot change parameters during simulation.', 'Error');
            return;
        end

        % Update parameters based on user input
        wave_speed = str2double(get(speed_edit, 'String'));
        source_speed = str2double(get(source_speed_edit, 'String'));
        num_reflectors = str2double(get(num_reflectors_edit, 'String'));
        reflector_radius = str2double(get(reflector_radius_edit, 'String'));
        frequency = str2double(get(frequency_edit, 'String'));

        % Validate inputs
        if isnan(wave_speed) || wave_speed <= 0
            errordlg('Please enter a valid positive number for Wave Speed.', 'Invalid Input');
            return;
        end
        if isnan(source_speed) || source_speed <= 0 || source_speed >= wave_speed
            errordlg('Source Speed must be positive and less than Wave Speed.', 'Invalid Input');
            return;
        end
        if isnan(num_reflectors) || num_reflectors < 0 || mod(num_reflectors,1) ~= 0
            errordlg('Please enter a valid non-negative integer for Number of Reflectors.', 'Invalid Input');
            return;
        end
        if isnan(reflector_radius) || reflector_radius <= 0
            errordlg('Please enter a valid positive number for Reflector Radius.', 'Invalid Input');
            return;
        end
        if isnan(frequency) || frequency <= 0
            errordlg('Please enter a valid positive number for Frequency.', 'Invalid Input');
            return;
        end

        % Redraw the setup with new parameters
        draw_setup();
    end

    function draw_setup()
        % Avoid clearing the axes if simulation is running
        if ~is_simulation_running
            cla(ax_wave);  % Clear axes before drawing
            axis(ax_wave, [-10 10 -5 5]);
            hold(ax_wave, 'on');
        end

        % Reset source position history
        source_pos_history = [];

        % Delete existing source_handle if it exists
        if ~isempty(source_handle) && isvalid(source_handle)
            delete(source_handle);
        end

        % Draw initial source position
        source_handle = plot(ax_wave, source_initial_pos(1), source_initial_pos(2), 'bo', 'MarkerSize', 10, 'MarkerFaceColor', 'b');
        text(ax_wave, source_initial_pos(1)-0.5, source_initial_pos(2)+0.5, 'Source', 'Color', 'b');

        % Draw sink using scatter and store handle
        if ~isempty(sink_scatter_handle) && isvalid(sink_scatter_handle)
            delete(sink_scatter_handle);
        end
        sink_scatter_handle = scatter(ax_wave, sink_pos(1), sink_pos(2), 100, 'r', 'filled');
        text(ax_wave, sink_pos(1)-0.5, sink_pos(2)+0.5, 'Sink', 'Color', 'r');

        % Generate random reflectors
        reflectors = [];
        if ~isempty(reflector_handles)
            for i = 1:length(reflector_handles)
                if isvalid(reflector_handles{i})
                    delete(reflector_handles{i});
                end
            end
        end
        reflector_handles = cell(num_reflectors, 1);
        rng(1); % For reproducibility
        for i = 1:num_reflectors
            theta = rand * 2 * pi;
            r = 3 + rand * 2;  % Distance from sink
            x = sink_pos(1) + r * cos(theta);
            y = sink_pos(2) + r * sin(theta);
            reflectors = [reflectors; x, y];
            h = viscircles(ax_wave, [x, y], reflector_radius, 'EdgeColor', 'k');
            reflector_handles{i} = h;
        end
        drawnow;
    end

    function start_simulation(~, ~)
        if is_simulation_running
            return;  % Prevent starting multiple simulations
        end
        draw_setup();

        is_simulation_running = true;
        stop_simulation = false;  % Reset the stop flag

        % Disable UI controls during simulation
        set([speed_edit, source_speed_edit, num_reflectors_edit, reflector_radius_edit, frequency_edit, start_button], 'Enable', 'off');
        set(stop_button, 'Enable', 'on');  % Enable the stop button

        % Ensure hold is on for ax_wave
        hold(ax_wave, 'on');

        % Adjust max_time based on maximum possible time for waves to reach sink
        max_possible_distance = 0;
        for i = 1:size(reflectors, 1)
            total_distance = norm(source_initial_pos - reflectors(i, :)) + norm(reflectors(i, :) - sink_pos);
            if total_distance > max_possible_distance
                max_possible_distance = total_distance;
            end
        end
        % Also consider direct path
        direct_distance = norm(source_initial_pos - sink_pos);
        if direct_distance > max_possible_distance
            max_possible_distance = direct_distance;
        end
        % Calculate maximum delay
        max_delay = max_possible_distance / wave_speed;

        % Adjust max_time to include maximum delay and desired cycles
        period = 1 / frequency;
        max_time = max_delay + period * cycles_to_display;

        % Set simulation start time
        t_start = 0;  % Start from time zero

        % Simulation parameters
        t = t_start:time_step:max_time;
        num_frames = length(t);

        % Prepare for animation
        wave_lines = [];
        reflector_wave_lines = cell(size(reflectors, 1), 1);

        % Initialize received signal array
        received_signal = zeros(1, num_frames);

        % Ensure sink_scatter_handle and source_handle are valid
        if isempty(sink_scatter_handle) || ~isvalid(sink_scatter_handle) || isempty(source_handle) || ~isvalid(source_handle)
            errordlg('Graphics handles are invalid. Please restart the simulation.', 'Error');
            % Re-enable UI controls
            set([speed_edit, source_speed_edit, num_reflectors_edit, reflector_radius_edit, frequency_edit, start_button, stop_button], 'Enable', 'on');
            is_simulation_running = false;
            return;
        end

        % Calculate total distance between source and sink
        total_source_sink_distance = norm(source_initial_pos - sink_pos);

        % Direction vector from source to sink
        direction_vector = (sink_pos - source_initial_pos) / total_source_sink_distance;

        % Simulation loop
        for frame = 1:num_frames
            % Check if the stop button has been pressed
            if stop_simulation
                break;  % Exit the simulation loop
            end

            % Calculate current time
            current_time = t(frame);

            % Update source position
            source_pos = source_initial_pos + source_speed * current_time * direction_vector;
            source_pos_history = [source_pos_history; source_pos];

            % Update source marker position
            if isvalid(source_handle)
                set(source_handle, 'XData', source_pos(1), 'YData', source_pos(2));
            else
                errordlg('Source marker handle is invalid. Please restart the simulation.', 'Error');
                % Re-enable UI controls
                set([speed_edit, source_speed_edit, num_reflectors_edit, reflector_radius_edit, frequency_edit, start_button, stop_button], 'Enable', 'on');
                is_simulation_running = false;
                return;
            end

            % Draw wavefronts from source (crests and troughs)
            theta_vals = linspace(0, 2*pi, 200);
            wave_number = 2 * pi * frequency / wave_speed;
            num_wavefronts = 15; % Number of visible wavefronts

            % Clear previous wave lines
            if ~isempty(wave_lines)
                delete(wave_lines);
            end
            wave_lines = [];

            for k = -num_wavefronts:num_wavefronts
                % Calculate radius for each crest and trough
                radius = wave_speed * current_time + k * (wave_speed / frequency);
                if radius >= 0
                    x_wave = source_pos(1) + radius * cos(theta_vals);
                    y_wave = source_pos(2) + radius * sin(theta_vals);
                    if mod(k, 2) == 0
                        % Crest
                        line_style = '-';
                    else
                        % Trough
                        line_style = '--';
                    end
                    wave_line = plot(ax_wave, x_wave, y_wave, 'Color', 'b', 'LineStyle', line_style);
                    wave_lines = [wave_lines; wave_line];
                end
            end

            % Initialize total wave at sink
            total_wave = 0;

            % Calculate contributions from direct wave
            % Time delay depends on the moving source
            source_to_sink_distance = norm(source_pos - sink_pos);
            time_delay = source_to_sink_distance / wave_speed;
            if current_time >= time_delay
                phase_delay = -wave_number * wave_speed * time_delay;
                amplitude = cos(wave_number * wave_speed * (current_time - time_delay) + phase_delay);
                total_wave = total_wave + amplitude;
            end

            % Draw and calculate reflected waves
            for i = 1:size(reflectors, 1)
                reflector_pos = reflectors(i, :);
                % Path from source to reflector
                source_to_reflector_distance = norm(source_pos - reflector_pos);
                % Check if wave has reached reflector
                if current_time >= source_to_reflector_distance / wave_speed
                    % Time since wave hit reflector
                    time_since_reflection = current_time - source_to_reflector_distance / wave_speed;
                    % Path from reflector to sink
                    reflector_to_sink_distance = norm(reflector_pos - sink_pos);
                    total_distance = source_to_reflector_distance + reflector_to_sink_distance;
                    total_time_delay = total_distance / wave_speed;
                    if current_time >= total_time_delay
                        % Calculate phase delay
                        phase_delay = -wave_number * wave_speed * total_time_delay;
                        amplitude = cos(wave_number * wave_speed * (current_time - total_time_delay) + phase_delay);
                        total_wave = total_wave + amplitude;
                    end

                    % Draw reflected wavefronts
                    reflected_radius = wave_speed * time_since_reflection;
                    if reflected_radius >= 0
                        % Clear previous reflected wave lines
                        if ~isempty(reflector_wave_lines{i})
                            delete(reflector_wave_lines{i});
                        end
                        reflector_wave_lines{i} = [];

                        for k = -num_wavefronts:num_wavefronts
                            radius = reflected_radius + k * (wave_speed / frequency);
                            if radius >= 0
                                x_reflected = reflector_pos(1) + radius * cos(theta_vals);
                                y_reflected = reflector_pos(2) + radius * sin(theta_vals);
                                if mod(k, 2) == 0
                                    % Crest
                                    line_style = '-';
                                else
                                    % Trough
                                    line_style = '--';
                                end
                                wave_line = plot(ax_wave, x_reflected, y_reflected, 'Color', 'g', 'LineStyle', line_style);
                                reflector_wave_lines{i} = [reflector_wave_lines{i}; wave_line];
                            end
                        end
                    end
                end
            end

            % Update received signal
            received_signal(frame) = total_wave;

            % Update received signal plot
            if frame == 1
                hold(ax_signal, 'off');
                signal_line = plot(ax_signal, t(1:frame), received_signal(1:frame), 'r');
                xlabel(ax_signal, 'Time');
                ylabel(ax_signal, 'Received Amplitude');
                title(ax_signal, 'Received Signal at Sink');
                grid(ax_signal, 'on');
            else
                % Update the data of the existing line
                set(signal_line, 'XData', t(1:frame), 'YData', received_signal(1:frame));
            end

            % Adjust y-limits to fit the data
            current_ydata = received_signal(1:frame);
            ymin = min(current_ydata);
            ymax = max(current_ydata);
            if ymin == ymax
                ymin = ymin - 1;
                ymax = ymax + 1;
            else
                margin = 0.1 * (ymax - ymin);
                ymin = ymin - margin;
                ymax = ymax + margin;
            end
            ylim(ax_signal, [ymin ymax]);
            xlim(ax_signal, [t_start max_time]);

            drawnow;
            pause(0.01);
        end

        % Re-enable UI controls after simulation
        set([speed_edit, source_speed_edit, num_reflectors_edit, reflector_radius_edit, frequency_edit, start_button], 'Enable', 'on');
        set(stop_button, 'Enable', 'off');  % Disable the stop button
        is_simulation_running = false;
    end

    function stop_simulation_callback(~, ~)
        if is_simulation_running
            stop_simulation = true;  % Set the flag to stop the simulation
        end
    end
end
