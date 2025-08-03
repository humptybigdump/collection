function ofdm_rayleigh_fading_simulation()
    % OFDM Rayleigh Fading Simulation with multiple users and interactive parameters

    % Create the figure and UI elements
    fig = figure('Name', 'OFDM Rayleigh Fading Simulation', 'NumberTitle', 'off', 'MenuBar', 'none', 'ToolBar', 'none', 'Resize', 'on');
    set(fig, 'Position', [100, 100, 1200, 800]);

    % Default parameters
    N_subcarriers = 64;          % Number of subcarriers
    T_sym = 70e-6;               % OFDM symbol duration (70 us)
    delta_f = 15e3;              % Subcarrier spacing (15 kHz)
    N_symbols = 100;             % Number of OFDM symbols to simulate
    N_users = 1;                 % Number of users

    % Default channel parameters
    N_paths = 5;                 % Number of multipath components
    tau_max = 5e-6;              % Maximum delay spread (5 us)
    v = 30;                      % Speed (m/s)
    f_c = 2e9;                   % Carrier frequency (Hz)
    c = 3e8;                     % Speed of light (m/s)

    % Create UI controls with normalized units
    uicontrol('Style', 'text', 'Units', 'normalized', 'Position', [0.02 0.94 0.15 0.03], 'String', 'Number of Paths:', 'HorizontalAlignment', 'left');
    N_paths_edit = uicontrol('Style', 'edit', 'Units', 'normalized', 'Position', [0.18 0.94 0.1 0.035], 'String', num2str(N_paths));

    uicontrol('Style', 'text', 'Units', 'normalized', 'Position', [0.32 0.94 0.15 0.03], 'String', 'Maximum Delay (\mus):', 'HorizontalAlignment', 'left');
    tau_max_edit = uicontrol('Style', 'edit', 'Units', 'normalized', 'Position', [0.48 0.94 0.1 0.035], 'String', num2str(tau_max * 1e6));

    uicontrol('Style', 'text', 'Units', 'normalized', 'Position', [0.02 0.89 0.15 0.03], 'String', 'Speed (m/s):', 'HorizontalAlignment', 'left');
    v_edit = uicontrol('Style', 'edit', 'Units', 'normalized', 'Position', [0.18 0.89 0.1 0.035], 'String', num2str(v));

    uicontrol('Style', 'text', 'Units', 'normalized', 'Position', [0.32 0.89 0.15 0.03], 'String', 'Carrier Frequency (GHz):', 'HorizontalAlignment', 'left');
    f_c_edit = uicontrol('Style', 'edit', 'Units', 'normalized', 'Position', [0.48 0.89 0.1 0.035], 'String', num2str(f_c / 1e9));

    uicontrol('Style', 'text', 'Units', 'normalized', 'Position', [0.62 0.94 0.15 0.03], 'String', 'Number of Users:', 'HorizontalAlignment', 'left');
    N_users_edit = uicontrol('Style', 'edit', 'Units', 'normalized', 'Position', [0.78 0.94 0.1 0.035], 'String', num2str(N_users));

    % Start button
    start_button = uicontrol('Style', 'pushbutton', 'Units', 'normalized', 'Position', [0.65 0.89 0.1 0.05], 'String', 'Start Simulation', 'Callback', @start_simulation);

    % Axes for plotting
    ax_fading = axes('Parent', fig, 'Units', 'normalized', 'Position', [0.1 0.1 0.8 0.75]);
    xlabel(ax_fading, 'Time (ms)');
    ylabel(ax_fading, 'Subcarrier Index');
    title(ax_fading, 'Maximum Fading Power over Users per Subcarrier over Time');
    colorbar(ax_fading);
    colormap(ax_fading, jet);

    % Callback function
    function start_simulation(~, ~)
        % Read parameters
        N_paths = str2double(get(N_paths_edit, 'String'));
        tau_max = str2double(get(tau_max_edit, 'String')) * 1e-6; % Convert from µs to s
        v = str2double(get(v_edit, 'String'));
        f_c = str2double(get(f_c_edit, 'String')) * 1e9; % Convert from GHz to Hz
        N_users = str2double(get(N_users_edit, 'String'));

        % Validate inputs
        if isnan(N_paths) || N_paths <= 0 || isnan(tau_max) || tau_max < 0 || isnan(v) || v < 0 || isnan(f_c) || f_c <= 0 || isnan(N_users) || N_users <= 0
            errordlg('Invalid input parameters.', 'Error');
            return;
        end

        % Derived parameters
        fd_max = (v / c) * f_c;      % Maximum Doppler shift (Hz)

        % Time vector for OFDM symbols
        t_symbols = (0:N_symbols-1) * T_sym;  % Start time of each symbol in seconds
        t_symbols_ms = t_symbols * 1e3;       % Convert time to milliseconds for plotting

        % Subcarrier frequencies
        f = (-N_subcarriers/2:N_subcarriers/2 - 1).' * delta_f;

        % Delays for each path (evenly spaced)
        if N_paths == 1
            delays = 0;
        else
            delays = linspace(0, tau_max, N_paths);  % Delays in seconds
        end

        % Initialize fading matrices for all users (subcarriers x symbols x users)
        fading_matrices = zeros(N_subcarriers, N_symbols, N_users);

        % Loop over users
        for user = 1:N_users
            % Random initial phases and Doppler shifts for each path for this user
            rng('shuffle');  % For randomness
            theta = 2 * pi * rand(N_paths, 1);       % Random initial phases
            fd = fd_max * cos(2 * pi * rand(N_paths, 1));  % Random Doppler shifts within fd_max

            % Loop over OFDM symbols
            for n = 1:N_symbols
                % Time at symbol n
                t_n = t_symbols(n);

                % Initialize channel frequency response
                H_n = zeros(N_subcarriers, 1);

                % Loop over paths
                for k = 1:N_paths
                    % Fading coefficient for path k at time t_n
                    h_k = sqrt(1/N_paths) * exp(1j * (2 * pi * fd(k) * t_n + theta(k)));

                    % Frequency response for path k across subcarriers
                    % Apply delay effect
                    H_k = h_k * exp(-1j * 2 * pi * f * delays(k));

                    % Accumulate the contributions from each path
                    H_n = H_n + H_k;
                end

                % Store the magnitude (fading power) for this symbol and user
                fading_matrices(:, n, user) = abs(H_n);
            end
        end

        % Compute the maximum fading power over all users for each subcarrier and symbol
        max_fading_matrix = max(fading_matrices, [], 3);

        % Plot the maximum fading power per subcarrier over time
        cla(ax_fading);
        imagesc(ax_fading, t_symbols_ms, 1:N_subcarriers, 20*log10(max_fading_matrix));
        set(ax_fading, 'YDir', 'normal');  % Set Y-axis direction to normal
        xlabel(ax_fading, 'Time (ms)');
        ylabel(ax_fading, 'Subcarrier Index');
        title(ax_fading, 'Maximum Fading Power over Users per Subcarrier over Time');
        colorbar(ax_fading);
        colormap(ax_fading, jet);
        caxis(ax_fading, [-40, 0]);  % Adjust color axis limits for better visibility
    end
end
