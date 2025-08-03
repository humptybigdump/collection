%% Conceptual modelling using a HBV model, Part 1. Water and Energy Cycles WS 21/22
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

% select runoff event period in Colpach
event_begin = datetime(2012, 07, 13, 10, 00, 00);   % begin runoff event
event_end = datetime(2012, 07, 20, 14, 00, 00);     % end runoff event
event_idx = isbetween(dn, event_begin, event_end);  % time range from runoff event

%% Task 1: HBV model only with a linear reservoir to simulate event runoff in Colpach

% observed runoff and precipitation of selected event period for later
% comparison with simulated runoff and for precipitation input in the HBV
% model (Eq. 2)
obs_event_runoff = colpach_runoff(event_idx);   
obs_event_precip = colpach_precip(event_idx);
ntime = length(obs_event_runoff);         % length of observed runoff event array (each line corresponds to one hour: length of array is duration of runoff event in hours)

% initializing the linear reservoir
% setting initial values for filling of linear reservoir SLR [mm] and the
% kLR parameter [-] and calculate hourly ET values from daily values
SLR_initial = 4;  % can be adjusted!
kLR = 0.008;        % can be adjusted!
ET = (3 / 24); % hourly ET value calculated from given daily value of 3 mm/d, this value is in each time step subtracted in Eq. 2 and 4

% defining and preallocating the arrays for the filling SLR and runoff QLR
% of the linear reservoir. The arrays have the same length and time stepping (each line contains a value for this hour) as the observed
% time series and contain, up to now, only zeros in each line/time step as
% placeholder. In the subsequent simulation runs, these zeros will be
% replaced by the calculated filling SLR and runoff QLR in each time step.
SLR = zeros(ntime,1);       % array containing values for filling of linear reservoir SLR [mm] in each time step
SLR(1)  = SLR_initial;      % writes into the first line/time step the initial value for the filling of linear reservoir SLR_initial 

QLR = zeros(ntime,1);        % array containing values for runoff out of linear reservoir QLR [mm] in each time step


% HBV model simulation runs (basically just a loop which calculates for
% each time step of the event period the runoff QLR and filling SLR of
% linear reservoir, i.e. the Colpach catchment, using Eq. 1 and 2).

for t = 1:ntime % time loop from hour 1 to total duration of selected runoff event in hours

    QLR(t) = SLR(t) * kLR; % Eq. 1 from exercise sheet; runoff out of linear reservoir in each time step (hour) of event
    SLR(t+1) = SLR(t) + obs_event_precip(t) - ET - QLR(t); % Eq. 2 from exercise sheet; new filling of linear reservoir SLR in the next time step (hour) of event
    
    % check to avoid negative filling/storage of linear reservoir
    if SLR(t+1) < 0
       QLR(t) = 0;
       SLR(t+1) = 0;
    end
end

% after the HBV model run, plot the simulated runoff time series QLR
% together with the observed runoff time series for comparison. Check if
% both curves match well. If not, go back and adjust the initial values of
% SLR and kLR above, run the HBV model again and check the result. Repeat
% this until you find a satisfying match between simulated and observed
% hydrograph. What is a satisfying match? In this case, we only use a
% visual evaluation of the fit between simulation and observation. This
% method is highly subjective and hence, every person has its own criteria of a
% satisfying match.
figure
plot(dn(event_idx), obs_event_runoff);              % observed runoff time series
hold on
plot(dn(event_idx), QLR)        % simulated runoff time series
ylim([0, max(obs_event_runoff)*1.4])
title('Comparison of simulated and observed runoff for selected runoff event in Colpach (HBV model only with linear reservoir')
legend('observed','simulated')
xlabel('Date')
ylabel('runoff [mm/h])')

%% Task 2: HBV model with beta-store on top of a linear reservoir

% observed runoff and precipitation of selected event period for later
% comparison with simulated runoff and for precipitation input in the HBV
% model (Eq. 4)
obs_event_runoff = colpach_runoff(event_idx);   
obs_event_precip = colpach_precip(event_idx);
ntime = length(obs_event_runoff);         % length of observed runoff event array (each line corresponds to one hour: length of array is duration of runoff event in hours)

% initializing the beta-store and linear reservoir
% setting initial values for filling of beta-store Sbeta [mm], the maximum filling of beta-store and the
% beta parameter [-]
Sbeta_initial = 200;           % initial filling of beta-store; can be adjusted!
Sbeta_max = 0.3*1000;          % maximum filling of beta-store (product of soil porosiy and soil depth 1000 mm); can be adjusted!
beta = 1.2;                   % value of beta parameter; can be adjusted!           
 
% setting initial values for filling of linear reservoir SLR [mm] and the
% kLR parameter [-] and calculate hourly ET values from daily values
SLR_initial = 3;  % can be adjusted!
kLR = 0.013;        % can be adjusted!
ET = (3 / 24); % hourly ET value, this value is in each time step subtracted in Eq. 2 and 4

% defining and preallocating the arrays for the filling SLR and Sbeta as
% well as runoff QLR and Qbeta of the linear reservoir and beta-store,
% respectively. The arrays have the same length and time stepping (each line contains a value for this hour) as the observed
% time series and contain, up to now, only zeros in each line/time step as
% placeholder. In the subsequent simulation runs, these zeros will be
% replaced by the calculated filling and runoff of beta-store and linear reservoir in each time step.
SLR = zeros(ntime,1);       % array containing values for filling of linear reservoir SLR [mm] in each time step
SLR(1)  = SLR_initial;      % writes into the first line/time step the initial value for the filling of linear reservoir SLR_initial 

QLR = zeros(ntime,1);        % array containing values for runoff out of linear reservoir QLR [mm] in each time step

Sbeta = zeros(ntime,1);        % array containing values for filling of beta-store Sbeta [mm] in each time step
Sbeta(1) = Sbeta_initial;      % writes into the first line/time step the initial value for the filling of beta-store Sbeta_initial 

Qbeta = zeros(ntime,1);        % array containing values for runoff/flow out of beta-store Qbeta into linear reservoir [mm] in each time step

% HBV model simulation runs (basically just a loop which calculates for
% each time step of the event period the filling of the beta-store Sbeta
% and the flow from beta-store Qbeta into the linear reservoir and
% subsequently, the runoff QLR and filling SLR of
% linear reservoir, i.e. the Colpach catchment, using Eq. 1, 3-5).

for t = 1:ntime % time loop from hour 1 to total duration of selected runoff event in hours
    
    % beta-store
    Qbeta(t) = obs_event_precip(t) .* ((Sbeta(t) / Sbeta_max) ^ beta); % Eq. 3 from exercise sheet, flow/runoff from beta-store into linear reservoir
    Sbeta(t+1) = Sbeta(t) + obs_event_precip(t) - ET - Qbeta(t); % Eq. 4 from exercise sheet; new filling of beta-store Sbeta in the next time step (hour) of event

    % check to avoid negative filling/storage of beta-store
    if Sbeta(t+1) < 0
       Qbeta(t) = 0;
       Sbeta(t+1) = 0;
    end
    
    % linear reservoir
    QLR(t) = SLR(t) * kLR; % Eq. 1 from exercise sheet; runoff out of linear reservoir in each time step (hour) of event
    SLR(t+1) = SLR(t) - QLR(t) + Qbeta(t); % Eq. 5 from exercise sheet; new filling of linear reservoir SLR in the next time step (hour) of event
                                                           % this time we use Eq. 5 and not Eq. 2 as in Task 1, because the linear
                                                           % reservoir is now underneath the beta-store and only gets the outflow
                                                           % from the beta-store as input (no precipitation anymore) as well as
                                                           % has no evapotranspiration losses anymore.
    
    % check to avoid negative filling/storage of linear reservoir
    if SLR(t+1) < 0
       QLR(t) = 0;
       SLR(t+1) = 0;
    end
end

% after the HBV model run, plot the simulated runoff time series QLR
% together with the observed runoff time series for comparison. Check if
% both curves match well. If not, go back and adjust the initial values of
% SLR and kLR above, run the HBV model again and check the result. Repeat
% this until you find a satisfying match between simulated and observed
% hydrograph. What is a satisfying match? In this case, we only use a
% optically evaluation of the fit between simulation and observation. This
% method is highly subjective and hence, every person has its own criteria of a
% satisfying match.
figure
plot(dn(event_idx), obs_event_runoff);              % observed runoff time series
hold on
plot(dn(event_idx), QLR)        % simulated runoff time series
ylim([0, max(obs_event_runoff)*1.4])
title('Comparison of simulated and observed runoff for selected runoff event in Colpach (HBV model with beta-store above linear reservoir')
legend('observed','simulated')
xlabel('Date')
ylabel('runoff [mm/h])')


