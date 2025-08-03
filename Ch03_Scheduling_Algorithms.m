% Interaktives Matlab Script zur Simulation von Schedulingalgorithmen in einem Mobilfunksystem

function interactive_scheduling_simulation
    % Create the GUI figure
    hFig = figure('Name', 'Scheduling Algorithm Simulation', 'NumberTitle', 'off', 'Position', [100, 100, 1200, 800]);
    
    % Create UI controls for average arrival rate for each user
    userArrivalRates = zeros(1, 5);
    for user = 1:5
        uicontrol('Style', 'text', 'Position', [20, 750 - 40 * user, 200, 25], 'String', sprintf('Average Arrival Rate User %d (λ):', user), 'HorizontalAlignment', 'left');
        userArrivalRates(user) = uicontrol('Style', 'edit', 'Position', [230, 750 - 40 * user, 100, 25], 'String', '0.5');
        uicontrol('Style', 'text', 'Position', [350, 750 - 40 * user, 120, 25], 'String', 'Path Loss (PL in dB):', 'HorizontalAlignment', 'left');
        userPathLoss(user) = uicontrol('Style', 'edit', 'Position', [480, 750 - 40 * user, 80, 25], 'String', '0');    
        userQueueSize(user) = uicontrol('Style', 'text', 'Position', [580, 750 - 40 * user, 100, 25], 'String', 'Queue: 0', 'HorizontalAlignment', 'left');
        userRate(user) = uicontrol('Style', 'text', 'Position', [690, 750 - 40 * user, 150, 25], 'String', 'Rate: 0', 'HorizontalAlignment', 'left');
    end
    
    % Create dropdown menu for scheduling algorithm selection
    uicontrol('Style', 'text', 'Position', [20, 500, 200, 25], 'String', 'Scheduling Algorithm:', 'HorizontalAlignment', 'left');
    hAlgorithmDropdown = uicontrol('Style', 'popupmenu', 'Position', [230, 500, 150, 25], 'String', {'Round Robin', 'Max-Min', 'Max-SNR', 'Proportional Fair'}, 'Callback', @update_algorithm);
    
    % Create buttons
    uicontrol('Style', 'pushbutton', 'Position', [20, 450, 100, 30], 'String', 'Run Simulation', 'Callback', @run_simulation);
    hStopButton = uicontrol('Style', 'pushbutton', 'Position', [140, 450, 100, 30], 'String', 'Stop', 'Callback', @stop_simulation, 'Enable', 'off');
    
    % Axes for the packet buffer visualization
    num_time_steps = 100;
    hAxes = axes('Parent', hFig, 'Position', [0.1, 0.1, 0.8, 0.5], 'XLim', [1, num_time_steps]);
    xlabel(hAxes, 'Time Slots');
    ylabel(hAxes, 'Frequency Resources');
    title(hAxes, 'Time-Frequency Resource Allocation');
    grid(hAxes, 'on');
    hold(hAxes, 'on');
    
    % Scheduling algorithm type
    schedulingAlgorithm = 'Round Robin';
    
    % Callback function for updating algorithm
    function update_algorithm(src, ~)
        val = get(src, 'Value');
        if val == 1
            schedulingAlgorithm = 'Round Robin';
        elseif val == 2
            schedulingAlgorithm = 'Max-Min';
        elseif val == 3
            schedulingAlgorithm = 'Max-SNR';
        elseif val == 4
            schedulingAlgorithm = 'Proportional Fair';        
        end
    end

    % Callback function for stopping the simulation
    function stop_simulation(~, ~)
        global stop_flag;
        stop_flag = true;
        set(hStopButton, 'Enable', 'off');
    end

    % Callback function for running the simulation
    function run_simulation(~, ~)
        global stop_flag;
        stop_flag = false;
        set(hStopButton, 'Enable', 'on');
        
        % Get the arrival rates for each user
        lambda = zeros(1, 5);
        for user = 1:5
            lambda(user) = str2double(get(userArrivalRates(user), 'String'));
            if isnan(lambda(user)) || lambda(user) < 0
                errordlg(sprintf('Invalid arrival rate for User %d.', user), 'Error');
                return;
            end
        end
        
        % Clear old simulation
        cla(hAxes);

        % Simulation parameters
        num_time_steps = 100;
        num_frequency_resources = 10;
        buffer_sizes = zeros(5, num_time_steps); % Buffer size (in units of data) for each user over time
        channel_gain = 1/sqrt(2)*exprnd(1, 5, num_frequency_resources); % Initial channel gain for each user and frequency resource
        resource_allocation = zeros(num_time_steps, num_frequency_resources); % Store the user allocation for each time slot and frequency resource % Initial channel gain for each user and frequency resource
        
        % Run the simulation
        user_rates = zeros(1, 5);                  
        user_metrics = zeros(1, 5); % Proportional Fair metric for each user
        alpha = 0.9; % Weight for updating the metric in Proportional Fair
        path_losses = zeros(1, 5);
        for user = 1:5
            path_losses(user) = 10^(str2double(get(userPathLoss(user), 'String')) / 10);
        end
        temp = 0;
        for t = 1:num_time_steps
            if stop_flag
                break;
            end
            % Generate new packets for each user (Poisson distributed)
            for user = 1:5
                new_packets = poissrnd(lambda(user));
                buffer_sizes(user, t) = buffer_sizes(user, max(t-1, 1)) + new_packets; % Increase data in buffer
                set(userQueueSize(user), 'String', sprintf('Queue: %.2f', buffer_sizes(user, t)));
                set(userRate(user), 'String', sprintf('Rate: %.2f', user_rates(user)/(t)));
            end
            
            % Update channel gain (correlated in time)
            rho = 0.9;
            channel_gain = rho*1/sqrt(2)*exprnd(1, 5, num_frequency_resources) + sqrt(1 - rho^2) * 1/sqrt(2) * exprnd(1, 5, num_frequency_resources);
            temp = temp + abs(channel_gain(1, 1))^2;
            
            % Scheduling (Round Robin, Max-Min, or Max-SNR)
            if strcmp(schedulingAlgorithm, 'Round Robin')
                last_user_idx = mod(t, 5);
                for res = 1:num_frequency_resources                    
                    user_idx = mod(last_user_idx, 5) + 1;
                    for i = 1:5
                        if buffer_sizes(user_idx, t) > 0
                            break;
                        else
                            user_idx = mod(user_idx, 5) + 1;
                        end
                    end
                    last_user_idx = user_idx;
                    if ~isempty(user_idx)
                        rate = min(buffer_sizes(user_idx, t), log2(1 + channel_gain(user_idx, res) * path_losses(user_idx))); % Calculate rate for the resource
                        buffer_sizes(user_idx, t) = max(0, buffer_sizes(user_idx, t) - rate); % Decrease data in buffer by rate
                        user_rates(user_idx) = user_rates(user_idx) + rate; %log2(1 + channel_gain(user_idx, res) * path_losses(user_idx)); % Update rate for the user
                        resource_allocation(t, res) = user_idx; % Allocate the resource to the user
                    end
                end            
            elseif strcmp(schedulingAlgorithm, 'Max-Min')
                for res = 1:num_frequency_resources
                    ind = find(buffer_sizes(:, t) > 0);
                    if ~isempty(ind)
                        [~, max_ind] = min(user_rates(ind)); % Find user with minimum rate and non-empty buffer                    
                        user_idx = ind(max_ind);                
                        if buffer_sizes(user_idx, t) > 0
                            rate = min(buffer_sizes(user_idx, t), log2(1 + channel_gain(user_idx, res) * path_losses(user_idx))); % Calculate rate for the resource
                            buffer_sizes(user_idx, t) = max(0, buffer_sizes(user_idx, t) - rate); % Decrease data in buffer by rate
                            user_rates(user_idx) = user_rates(user_idx) + rate; % Update rate for the user
                            resource_allocation(t, res) = user_idx; % Allocate the resource to the user
                        end
                    end
                end
            elseif strcmp(schedulingAlgorithm, 'Max-SNR')
                for res = 1:num_frequency_resources
                    ind = find(buffer_sizes(:, t) > 0);
                    if ~isempty(ind)
                        [~, max_ind] = max(channel_gain(ind, res) .* path_losses(ind)'); % Find user with maximum SNR and non-empty buffer
                        user_idx = ind(max_ind);
                        if buffer_sizes(user_idx, t) > 0
                            rate = min(buffer_sizes(user_idx, t), log2(1 + channel_gain(user_idx, res) * path_losses(user_idx))); % Calculate rate for the resource
                            buffer_sizes(user_idx, t) = max(0, buffer_sizes(user_idx, t) - rate); % Decrease data in buffer by rate
                            user_rates(user_idx) = user_rates(user_idx) + rate; % Update rate for the user
                            resource_allocation(t, res) = user_idx; % Allocate the resource to the user
                        end
                    end
                end
            elseif strcmp(schedulingAlgorithm, 'Proportional Fair')
                user_metrics = alpha * user_metrics;
                for res = 1:num_frequency_resources
                    ind = find(buffer_sizes(:, t) > 0);
                    if ~isempty(ind)
                        % Calculate metric for Proportional Fair scheduling
                        metrics = (channel_gain(ind, res)' .* path_losses(ind)) ./ (user_metrics(ind) + 1e-6);
                        [~, max_ind] = max(metrics); % Find user with maximum metric and non-empty buffer
                        user_idx = ind(max_ind);
                        if buffer_sizes(user_idx, t) > 0                            
                            rate = min(buffer_sizes(user_idx, t), log2(1 + channel_gain(user_idx, res) * path_losses(user_idx))); % Calculate rate for the resource                                                        
                            buffer_sizes(user_idx, t) = max(0, buffer_sizes(user_idx, t) - rate); % Decrease data in buffer by rate
                            user_rates(user_idx) = user_rates(user_idx) + rate; % Update rate for the user
                            user_metrics(user_idx) = user_metrics(user_idx) + (1 - alpha) * rate; % Update Proportional Fair metric
                            resource_allocation(t, res) = user_idx; % Allocate the resource to the user
                        end
                    end
                end                
            end
                   
            % Update the plot for time-frequency resource allocation
            colors = lines(5); % Assign unique colors to each user
            for res = 1:num_frequency_resources
                t_slot = t;
                    user_idx = resource_allocation(t_slot, res);
                    if user_idx > 0
                        rectangle('Position', [t_slot - 0.5, res - 0.5, 1, 1], 'FaceColor', colors(user_idx, :), 'EdgeColor', 'k');
                    end                
            end
            drawnow;
            pause(0.1); % Pause for visualization effect
        end
        temp / num_time_steps
        
        set(hStopButton, 'Enable', 'off');
    end
end
