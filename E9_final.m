%% Design of flood-defence reservoir Water and Energy Cycles WS 24/25
% We will work in the catchment Colpach 

% create a string containing the path to the data, folder "data" has to
% be in the same location as this MATLAB script!
file_path_colpach = fullfile([pwd '/data'], 'Colpach.txt');

%% import hydrological time series
% table with all given data
colpach_climate = readtable(file_path_colpach,'Delimiter', ',');

% assigning observed runoff given in mm/h (= L/m^2*hr) to variable
colpach_runoff = table2array(colpach_climate(:,{'runoff_mm_h_'}));

% assigning observed rainfall given in mm/h (= L/m^2*hr) to variable
colpach_precip = table2array(colpach_climate(:,{'precip_mm_h_'}));

% define the date column as data type date 
format_date = 'yyyy-mm-dd HH:MM:SS';
date_vec = table2array(colpach_climate(:,1));
dn = datetime(date_vec,'InputFormat',format_date); % array containing only the dates of time series

% catchment area (m) of Colpach to convert units
A_catchment_colpach = 1.9e+07; 

%% Task 1: Determine runoff at exceedance probability 1% in Colpach
% generate flow duration curve (cf. exercise 1)
[sorted_runoff_colpach, ranks_colpach] = sort(colpach_runoff,'descend');
prob_colpach = 100 * (ranks_colpach / (length(ranks_colpach) + 1));
sorted_prob_colpach = sort(prob_colpach);

% plot flow duration curve
figure
semilogy(sorted_prob_colpach, sorted_runoff_colpach)
xlabel('exceedance probability')
ylabel('runoff [mm/h]')
legend('colpach')
title('Flow duration curve')

% find all positions of runoff values higher than 1 % exceedance
% probability (analytical solution)
pos_runoff_higher_1 = find(sorted_prob_colpach > 1);

% take the first position to determine the exact exceedance probability slightly higher 1 and
% respective runoff value (in mm/h)
runoff_ex_prob_1 = sorted_runoff_colpach(pos_runoff_higher_1(1));

% convert this runoff value from mm/h (= L/m^2*h) into m/s by using the Colpach catchment area --> this runoff value in m/s
% is then the maximum capacity of the ouflow channel behind the reservoir
runoff_ex_prob_1 = (runoff_ex_prob_1 * A_catchment_colpach) / (1000 * 3600);

%% Task 2: Design of flood_defence reservoir
% General setup
% select runoff event 
event_begin = datetime(2011, 01, 06, 09, 00, 00);    
event_end = datetime(2011, 01, 27, 09, 00, 00);     
event_idx = isbetween(dn, event_begin, event_end); 
time = dn(event_idx);

% reservoir inflow Q_in = event runoff; conversion of event runoff (mm/h
% into m/s)
Q_in = colpach_runoff(event_idx); % reservoir inflow = observed runoff of selected flood event 
Q_in = (Q_in * A_catchment_colpach) / (1000 * 3600); % observed event runoff = reservoir inflow in m/s

% predefined dam height (m)
Dam_height = 3;

% maximum possible release of reservoir (m/s) determined by the maximum capacity
% of the subsequent channel or stream (= runoff at 1 % exceedance
% probability in catchment; cf. Task 1)
max_reservoir_outflow = runoff_ex_prob_1; 

% acceleration of the earth m/s
g = 9.81; 

% time step of given time series in seconds
diff_time = diff(table2array(colpach_climate(:,{'date'}))); 
dt = seconds(diff_time(1));

% Setup of Monte Carlo algorithm --> for an automatic calibration of A_out
% and alpha
MC_N = 1000000; % total number of Monte Carlo runs

% random value ranges for parameters A_out and alpha
A_out_range = 0.001 + (1.00-0.001) * rand(MC_N,1);
alpha_range = 0.0005 + (0.01-0.0005) * rand(MC_N,1);

% Initialise/preallocate arrays for parameters, same length as input time series
ntime = length(time);
S = zeros(ntime,MC_N); % reservoir storage volume
h = zeros(ntime,MC_N); % reservoir water level height
Q_out = zeros(ntime,MC_N); % reservoir outflow

% Initial states of reservoir, empty reservoir
S(1,1) = 0; % zero water storage
h(1,1) = 0; % zero water level
Q_out(1,1) = 0; % no outflow
eps = 0.1; % calculation error
error_threshold = 0.05; % maximum error value
runoff_coefficient = 1; % scaling parameter for flow in outlet pipe

% MC loop
for MC_run = 1 : MC_N
    
    % select new values for parameters at beginning of each Monte Carlo run
    A_out = A_out_range(MC_run);
    alpha = alpha_range(MC_run);
    
    % for-loop to determine reservoir outflow in each time step
    for t = 1 : ntime-1 
    
        % first assumption: new outflow = old outflow
        Q_out(t+1,MC_run) = Q_out(t,MC_run); 

        while eps > error_threshold % iteration with calculation of new outflow if error between calculated outflow (Q_out_it) and previously assumed outflow (Q_out) is above threshold
       
            % calculates storage volume change of reservoir by balancing inflow and outflow and mean
            % between current and next time step
            S(t+1,MC_run) = S(t,MC_run) + (dt * 0.5 * (Q_in(t+1) + Q_in(t) - Q_out(t+1,MC_run) - Q_out(t,MC_run)));
        
            % avoids negative storage within reservoir
            if S(t+1,MC_run) < 0
                S(t+1,MC_run) = 0;
                Q_out(t+1,MC_run) = 0;
            end
        
            % calculates water height in reservoir
            h(t+1,MC_run) = alpha * sqrt(S(t+1,MC_run)); 
        
            % calculates actual outflow out of reservoir
            Q_out_it = runoff_coefficient * A_out * sqrt(2 * g * h(t+1,MC_run));
        
            % updates error value of current iteration
            eps = abs(Q_out_it - Q_out(t+1,MC_run)) / Q_out(t+1,MC_run); % error of calculated outflow (Q_out_it) related to previously assumed outflow (Q_out = equal to outflow in time step before) 
            Q_out(t+1,MC_run) = Q_out_it;


        end

        eps = 0.1; % resets actual error value for the next time step
    
    end

end

%%
% Analysis of Monte-Carlo runs and finding parameter setup with suitable and cheapest reservoir design

% costs
A_out_cm2_cost = 8; % cost of 1 cm outlet pipe
alpha_m2 = 130; % cost of 1 m surface area 

% create array with the maximum Q_out and h values of each Monte Carlo run
max_Q_h = [max(Q_out);max(h)]'; 

% find in this "max_Q_h" array the index/position of Monte Carlo runs, which
% have (i) maximum h values below dam height, (ii) maximum outflow Q_out values
% below maximum possible reservoir outflow/channel capacity and 
% simultaneously (iii) only outflow Q_out values above 0 (we call these
% Monte Carlo runs, which satisfy these criteria: valid Monte Carlo runs)
range_Q_h = find(max_Q_h(:,1) < max_reservoir_outflow & max_Q_h(:,1) > 0 &  max_Q_h(:,2) < Dam_height);

% select the respective A_out and alpha values of these valid Monte Carlo
% runs
A_out = A_out_range(range_Q_h);
alpha = alpha_range(range_Q_h);

% calculate total costs of each valid Monte Carlo run
A_out_cost = (A_out * 10000) * A_out_cm2_cost; % cost for building outlet pipe
alpha_cost = (1 ./ alpha) * alpha_m2; % cost for building reservoir over certain surface area; 1/alpha corresponds to actual surface area in m

total_cost = A_out_cost + alpha_cost;

% find the minimum cost of valid Monte Carlo runs and the respective
% position/index of the Monte Carlo run, which produces this cheapest
% parameter setup and hence cheapest but still suitable reservoir
[min_cost, pos_min_cost] = min(total_cost);

% corresponding outlet area A_out and alpha of cheapest setup
A_out_cheapest = A_out_range(range_Q_h(pos_min_cost))
alpha_cheapest = alpha_range(range_Q_h(pos_min_cost))

% Q_out and h time series of cheapest setup for plotting
Q_out_final = Q_out(:,(range_Q_h(pos_min_cost)));
h_final = h(:,(range_Q_h(pos_min_cost)));

% plot of reservoir water level and outflow time series of cheapest but
% suitable reservoir design
figure;
subplot(2,1,1);
h1=plot(time,Q_in,'r-', 'linewidth',2);
hold on;
h2=plot(time,Q_out_final,'b-', 'linewidth',2);
h3=plot([time(1) time(ntime)],[max_reservoir_outflow max_reservoir_outflow],'r --', 'linewidth',2);
xlabel( 'time [h]','fontsize',16);
ylabel( 'Q [m^3/s]','fontsize',16);
legend('Inflow', 'Release','Channel capacity');
set(gca,'fontsize',16);
subplot(2,1,2); 
plot(time,h_final,'r-', 'linewidth',2);
hold on;
plot([time(1) time(ntime)],[Dam_height Dam_height],'r --', 'linewidth',2);
xlabel( 'time [h]','fontsize',16); 
ylabel( 'water level h [m]','fontsize',16); 
set(gca,'fontsize',16);  
legend('Water level', 'Dam height','Location','northwest');



