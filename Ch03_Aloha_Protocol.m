% Interactive Matlab Script for Packet Transmission (No Collisions, Exponential Arrival with Animation)

function interactive_packet_simulation
    % Create the GUI figure
    hFig = figure('Name', 'Packet Transmission Simulation', 'NumberTitle', 'off', 'Position', [100, 100, 1200, 800]);
    
    % Create UI controls for parameters
    uicontrol('Style', 'text', 'Position', [20, 750, 200, 25], 'String', 'Number of Users (N):', 'HorizontalAlignment', 'left');
    hNumUsers = uicontrol('Style', 'edit', 'Position', [230, 750, 100, 25], 'String', '10');
    
    uicontrol('Style', 'text', 'Position', [20, 710, 200, 25], 'String', 'Average Arrival Rate (\lambda):', 'HorizontalAlignment', 'left');
    hLambda = uicontrol('Style', 'edit', 'Position', [230, 710, 100, 25], 'String', '0.5');
    
    uicontrol('Style', 'text', 'Position', [20, 670, 200, 25], 'String', 'Packet Length (D) in Time Units:', 'HorizontalAlignment', 'left');
    hPacketLength = uicontrol('Style', 'edit', 'Position', [230, 670, 100, 25], 'String', '3');
    
    % Create buttons
    hAlohaCheckbox = uicontrol('Style', 'checkbox', 'Position', [580, 750, 150, 25], 'String', 'Slotted Aloha', 'Value', 0);
    uicontrol('Style', 'pushbutton', 'Position', [350, 750, 100, 30], 'String', 'Run Simulation', 'Callback', @run_simulation);
    hStopButton = uicontrol('Style', 'pushbutton', 'Position', [460, 750, 100, 30], 'String', 'Stop', 'Callback', @stop_simulation, 'Enable', 'off');
    
    % Axes for the packet transmission visualization
    hAxes = axes('Parent', hFig, 'Position', [0.1, 0.1, 0.8, 0.7]);
    xlabel(hAxes, 'Time');
    ylabel(hAxes, 'Terminals');
    title(hAxes, 'Packet Transmission Visualization');
    grid(hAxes, 'on');
    hold(hAxes, 'on');
    
    % Callback function for stopping the simulation
    function stop_simulation(~, ~)
        global stop_flag;
        stop_flag = true;
        set(hStopButton, 'Enable', 'off');
        set(hNumUsers, 'Enable', 'on');
        set(hLambda, 'Enable', 'on');
        set(hPacketLength, 'Enable', 'on');
    end

    % Callback function for running the simulation
    
    function run_simulation(~, ~)
        global stop_flag;
        stop_flag = false;
        stop_flag = false;
        % Get the values from UI controls
        N = str2double(get(hNumUsers, 'String'));
        lambda = str2double(get(hLambda, 'String'));
        D = str2double(get(hPacketLength, 'String'));
        
        % Validate inputs
        if isnan(N) || N <= 0 || isnan(lambda) || lambda <= 0 || isnan(D) || D <= 0
            errordlg('Invalid input parameters.', 'Error');
            return;
        end
        
        % Initialize simulation parameters
        % Delete old success rate text fields if they exist
        if exist('hSuccessRate', 'var') && isvalid(hSuccessRate)
            delete(hSuccessRate);
        end
        
        % Create text fields for success rate of each user
        hSuccessRate = gobjects(N+1, 1);
        for user = 1:N
            hSuccessRate(user) = uicontrol('Style', 'text', 'Units', 'normalized', 'Position', [0.9, 0.75 - 0.05 * user, 0.08, 0.03], 'String', sprintf('User %d Success Rate: 0%%', user), 'HorizontalAlignment', 'left');
        end
        hSuccessRate(N+1) = uicontrol('Style', 'text', 'Units', 'normalized', 'Position', [0.9, 0.75 - 0.05 * (N+1), 0.08, 0.03], 'String', sprintf('Sum: 0%%', user), 'HorizontalAlignment', 'left');

        num_slots = 100; % Visible number of time slots
        total_time = 300; % Total simulation time for longer duration
        packet_times = cell(N, 1); % Store packet arrival times for each user
        next_arrival_times = zeros(N, 1); % Next packet arrival time for each user
        success_count = zeros(N, 1); % Count of successful transmissions per user
        successful_packets = cell(N, 1); % Store successful packet start times for each user
        total_transmissions = zeros(N, 1); % Count of total transmissions per user
        
        % Generate initial packet arrival times for each user
        for user = 1:N
            if get(hAlohaCheckbox, 'Value')
                next_arrival_times(user) = ceil(exprnd(1 / lambda) / D) * D; % Align to slotted Aloha
            else
                next_arrival_times(user) = exprnd(1 / lambda);
            end
        end
        
        % Clear previous visualization
        cla(hAxes);
        set(hAxes, 'YLim', [0, N + 1], 'XLim', [0, num_slots]);
        xlim_range = [0, num_slots];
        set(hStopButton, 'Enable', 'on');
        set(hNumUsers, 'Enable', 'off');
        set(hLambda, 'Enable', 'off');
        set(hPacketLength, 'Enable', 'off');
        
        % Run the simulation
        current_time = 0;
        while current_time < total_time
            % Find the next packet to be transmitted
            [min_time, user_idx] = min(next_arrival_times);
            current_time = min_time;
            
            % Schedule the packet for transmission
            packet_times{user_idx} = [packet_times{user_idx}; current_time];
            total_transmissions(user_idx) = total_transmissions(user_idx) + 1;
                        
            % Update next arrival time for the user, ensuring it starts after the current packet ends
            if get(hAlohaCheckbox, 'Value')
                next_arrival_times(user_idx) = ceil((current_time + D + exprnd(1 / lambda)) / D) * D; % Align to slotted Aloha
            else
                next_arrival_times(user_idx) = current_time + D + exprnd(1 / lambda);
            end
            
            % Animate the packet transmission
            % Check for active transmissions from other terminals
            is_collision = false;
            for other_user = 1:N
                if other_user ~= user_idx && any(packet_times{other_user} <= current_time & (packet_times{other_user} + D) > current_time)
                    is_collision = true;
                    break;
                end
            end
            if ~is_collision
                successful_packets{user_idx} = [successful_packets{user_idx}; current_time];
            end
            
            % Draw packet in red if there is a collision, otherwise green
            % Overlay all previous colliding packets in red if there is a collision
            if is_collision
                % Mark the current packet as not successful if previously marked
                if ismember(current_time, successful_packets{user_idx})
                    success_count(user_idx) = success_count(user_idx) - 1;
                    successful_packets{user_idx}(successful_packets{user_idx} == current_time) = [];
                end

                for other_user = 1:N
                    if other_user ~= user_idx && any(packet_times{other_user} <= current_time & (packet_times{other_user} + D) > current_time)
                        start_time = packet_times{other_user}(find(packet_times{other_user} <= current_time & (packet_times{other_user} + D) > current_time, 1));
                        if ismember(start_time, successful_packets{other_user})
                            success_count(other_user) = success_count(other_user) - 1;                            
                            successful_packets{other_user}(successful_packets{other_user} == start_time) = [];                            
                        end
                        rectangle('Position', [start_time, other_user - 0.5, D, 1], 'FaceColor', 'r', 'EdgeColor', 'k');
                    end
                end
                rectangle('Position', [current_time, user_idx - 0.5, D, 1], 'FaceColor', 'r', 'EdgeColor', 'k');
            else
                rectangle('Position', [current_time, user_idx - 0.5, D, 1], 'FaceColor', 'g', 'EdgeColor', 'k');
                success_count(user_idx) = success_count(user_idx) + 1;
            end
            pause(0.1); % Pause to create animation effect
            drawnow;

            % Update success rate text fields
            sum_occupation = 0.0
            for user = 1:N                                    
                user_occupation = 100.0*length(successful_packets{user})*D/current_time;
                sum_occupation = sum_occupation + user_occupation;
                set(hSuccessRate(user), 'String', sprintf('User %d: %.2f%%', user, user_occupation));                                
            end
            set(hSuccessRate(N+1), 'String', sprintf('Sum: %.2f%%', sum_occupation));                                

            % Shift the visualization window to the left if current_time exceeds visible range
            if current_time > xlim_range(2)
                xlim_range = xlim_range + (current_time - xlim_range(2));
                set(hAxes, 'XLim', xlim_range);
                
            end
            if stop_flag
                break;
            end
        end
        % Update success rate text fields
        sum_occupation = 0.0
        for user = 1:N                                    
            user_occupation = 100.0*length(successful_packets{user})*D/total_time;
            sum_occupation = sum_occupation + user_occupation;
            set(hSuccessRate(user), 'String', sprintf('User %d: %.2f%%', user, user_occupation));                                
        end
        set(hSuccessRate(N+1), 'String', sprintf('Sum: %.2f%%', sum_occupation));
    end
end
