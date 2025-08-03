%% Water and Energy Cycles. WS 20/21
% Exercise 9: Calculating evapotranspiration
%% Import data
% import climate file
% Structure of climate data file
% date (dd.mm.yyyy)	
% LTMI2	mean temperature [°C]
% LT2MAX max temperature [°C]
% LT2MIN min temperature [°C]
% LT14	temperature	at 14:00 o'clock [°C]	
% LF2	air humidity [%] in 2m height	
% LF14	air humidity [%] at 14 o'clock
% GLOBAL global radiation [J/cm²]		
% NIESU	precipitation height [mm]		
% WG2	wind speed [m/s] in 2 m height	
% WG10	wind speed [m/s] in 10 m height

climate = readtable('./data/climate.csv'); % table containing climate data

% import lysimeter file
% Structure of lysimeter data file
% date (dd.mm.yyyy)		
% Precipitation	[mm] P
% Seepage [mm] / groundwater recharge Q
% Storage change [mm] S
% Snowheight [mm]

lysimeter = readtable('./data/lysimeter.csv'); % table containing lysimeter data

%% Task 1: Calculate Evapotranspiration ET from the lysimeter data by closing the water balance (0 = P - Q - S - ETP)

act_ET = lysimeter.('Precipitation') - lysimeter.('Seepage') - lysimeter.('StorageChange');

% if snow on lysimeter, set ET to 0.1 mm
act_ET(lysimeter.('Snowheight')>0) = 0.1;

% plot actual evapotranspiration time series from lysimeter water balance
figure
plot(lysimeter.('date'),act_ET)
xlabel('time [days]')
ylabel('evapotranspiration [mm]')
ylim([0 max(act_ET)])

%% Task 2: Calculate Evapotranspiration using Haude method
% read in the temperature at 14:00 o'clock and air humidity in 2 m height on each day and
% assign them to variables
temp_14 = climate.('LT14');
rel_sat = climate.('LF2');

% import Haude factors from file and save them to variable (functions "readtable" and
% table2array")
haude_factor = table2array(readtable('./data/haude_factors.txt'));

% create an array containing the month number of each date/day in the given data
% time series from table column "month" and then create an array with the
% respective Haude factors for each month number
months = climate.('month');
f = haude_factor(months);

% insert HAUDE equations (Eq. 1 and 2) and calculate ET with the above determined
% parameters and Haude factors
e_s = 6.11 * exp((17.62 * temp_14) ./ (234.12 + temp_14));
    
ETP_Haude = f .* e_s .* (1 - (rel_sat ./ 100));

% plot the evapotranspiration time series of ETP lysimeter and ETP HAUDE in
% one plot
figure
plot(lysimeter.('date'),act_ET)
xlabel('time [days]')
ylabel('evapotranspiration [mm]')
hold on
plot(lysimeter.('date'), ETP_Haude)
ylim([0 max(act_ET)])
legend('act ET','ET Haude')

% calculate RMSE between ET Haude and actual evapotranspiration
RMSE_Haude = sqrt(mean((act_ET - ETP_Haude) .^2))

%% Task 3 : Calculate Evapotranspiration using Turc method
% read the global radiation and the mean air temperature in 2 m height and assign them to variables
global_radiation = climate.('GLOBAL');
temp = climate.('LTMI2');

% insert TURC equations (Eq. 3 and 4) and calculate ET Turc with the above
% determined parameters

% first, determine Turc factors C at each positio/date of time series (Eq.
% 4)
C = ones(length(rel_sat),1);
C(rel_sat < 50) = 1 + ((50 - rel_sat(rel_sat < 50)) / 70);

% then, calculate ET Turv with Eq. 3
ETP_Turc = 0.0031 .* C .* (global_radiation + 209) .* (temp ./ (temp + 15));

% check if there are negative values in the ET Turc time series and replace them with 0
ETP_Turc(ETP_Turc < 0) = 0;

% delete outliers in the ET Turc time series
ETP_Turc(ETP_Turc > 15) = 0;

% plot the evapotranspiration time series of actual evapotranspiration and ET TURC in
% one plot
figure
plot(lysimeter.('date'),act_ET)
xlabel('time [days]')
ylabel('evapotranspiration [mm]')
hold on
plot(lysimeter.('date'), ETP_Turc)
ylim([0 max(act_ET)])
legend('act ET','ET Turc')

% calculate RMSE between ET Turc and actual evapotranspiration
RMSE_Turc = sqrt(mean((act_ET - ETP_Turc).^2))

%% Task 4: Monte Carlo algorithm to calibrate Haude factors 
% number of MC runs
MC_N = 1000;

% define range with randomly distributed scaling factors between upper and
% lower limit(again, use trial and error to determine suitable upper and
% lower limits)
scaling_factor_range = 0.1 + (10 - 0.1) * rand(MC_N,1);

% initialize array for saving RMSE of each MC run
RMSE_runs = zeros(MC_N,1);

for MC_run = 1:MC_N
    
    % read in the temperature at 14:00 o'clock and air humidity in 2 m height on each day and
    % assign them to variables
    temp_14 = climate.('LT14');
    rel_sat = climate.('LF2');

    % import Haude factors from file and save them to variable (functions "readtable" and
    % table2array")
    haude_factor = table2array(readtable('./data/haude_factors.txt'));

    % create an array containing the month number of each date/day in the given data
    % time series from table column "month" and then create an array with the
    % respective Haude factors for each month number
    months = climate.('month');
    
    % select next scaling factor out of random distribution (by indexing with "MC_run") and multiply with the Haude factors
    f_new = haude_factor(months) * scaling_factor_range(MC_run);

    % insert HAUDE equations (Eq. 1 and 2) and calculate ET with the above determined
    % parameters and scaled Haude factors
    e_s = 6.11 * exp((17.62 * temp_14) ./ (234.12 + temp_14));
    
    ET_Haude = f_new .* e_s .* (1 - (rel_sat ./ 100));
    
    % save the RMSE value of each MC run 
    RMSE_runs(MC_run) = sqrt(mean((act_ET - ET_Haude).^2));
    
end

% Analyze the MC runs and find the RMSE value and position of the run with lowest RMSE
[lowest_RMSE, lowest_RMSE_pos] = min(RMSE_runs)      

% determine the new, calibrated/scaled Haude factors with the scaling factors at the position of the run with lowest RMSE
scaling_factor_range(lowest_RMSE_pos)
haude_factor_new = haude_factor * scaling_factor_range(lowest_RMSE_pos)

