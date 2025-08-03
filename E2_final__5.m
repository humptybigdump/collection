%% Time series analyis, Part 2. Water and Energy Cycles WS 20/21
% We will work in two catchments one is named Colpach the other Schwebich
% create a string containing the path to the data, folder "data" has to
% be in the same location as this MATLAB script!
file_path_colpach = fullfile([pwd '/data'], 'Colpach.txt');
file_path_schwebich = fullfile([pwd '/data'], 'Schwebich.txt');

%% import hydrological time series
% table with all given data
colpach_climate = readtable(file_path_colpach,'Delimiter', ',');
schwebich_climate = readtable(file_path_schwebich,'Delimiter', ',');

% assigning observed runoff given in mm/h (= L/m^2*hr) to variable
colpach_runoff = table2array(colpach_climate(:,{'runoff_mm_h_'}));
schwebich_runoff = table2array(schwebich_climate(:,{'runoff_mm_h_'}));

% assigning observed rainfall given in mm/h (= L/m^2*hr) to variable
colpach_precip = table2array(colpach_climate(:,{'precip_mm_h_'}));
schwebich_precip = table2array(schwebich_climate(:,{'precip_mm_h_'}));

% define the date column as data type date 
format_date = 'yyyy-mm-dd HH:MM:SS';
date_vec = table2array(colpach_climate(:,1));
dn = datetime(date_vec,'InputFormat',format_date); % array containing only the dates of time series

%% Task 1: Temperature time series and moving mean
% select the hydrological year 2014
year_begin = datetime(2013, 11, 01, 00, 00, 00); % begin of the time period
year_end = datetime(2014, 10, 31, 23, 00, 00);   % end of the time period
year_idx = isbetween(dn, year_begin, year_end);  % logical index; 1 = value within hydrological year, 0 = value not within hydrological year
% "isbetween" checks each line of the date array "dn" if the respective
% date lies between "year_begin" and "year_end".This "year_idx" can be
% used as index for arrays to extract only the respective data for the
% hydrological year from the entire time series, e.g "data_time_series(year_idx)". 

% extract the temperature from dataset and assign it to variable
colpach_temp = table2array(colpach_climate(:,{'temp__C_'}));

% apply the moving window function ("movmean") with a window size of 7 days
% (= 168 hours) to build moving temperature means
colpach_temp_year_movmean = movmean(colpach_temp(year_idx), 168);

% plot the original temperature time series together with moving mean
% temperature time series
figure
plot(dn(year_idx),colpach_temp(year_idx))
hold on
plot(dn(year_idx),colpach_temp_year_movmean,'LineWidth',2)
title('Temperature time series in Colpach for hydrological year 2014')
legend('original','moving mean')
xlabel('Date')
ylabel('Temperature °C')

%% Task 2: Combinig double mass curve and temperature periods >10°C
% double mass curve in Colpach for hydroloical year 2014 (using "year_idx"
% to identify) (see exercise 1)
colpach_cumsum_precipitation = cumsum(colpach_precip(year_idx)) / sum(colpach_precip(year_idx));
colpach_cumsum_runoff = cumsum(colpach_runoff(year_idx)) / sum(colpach_precip(year_idx));

temp_idx = colpach_temp_year_movmean > 10; %logical indexing to find all positions in the temperature array where temperature is higher than 10°C
% 0 = on this position the temperature is below; 1 = on this position the
% temperature is higher 

% plot double mass curve together with the periods where temperature is
% hgiher than 10°C
figure
plot(colpach_cumsum_precipitation, colpach_cumsum_runoff)
hold on;
plot(colpach_cumsum_precipitation(temp_idx), colpach_cumsum_runoff(temp_idx), 'o') %red dots highlight the periods with temperature above 10°C, using "temp_idx" for indexing the array with all temperature values
xlim([0 1])
ylim([0 1])
xlabel('normalized, cumulated precipitation')
ylabel('normalized, cumulated runoff')
legend('double mass curve','T>10°C')
title('Double mass curve Colpach for hydrological year 2014 with highlighted periods of T >10°C')

%% Task 3:Highest runoff event in Colpach and Schwebich for entire data time period
[max_runoff_colpach, pos_max_runoff_colpach] = max(colpach_runoff) % function "max" can return the maximum runoff value in time series together with the position of this runoff value in array
date_max_runoff_colpach = dn(pos_max_runoff_colpach) % with the position you can identify the date of maximum runoff

[max_runoff_schwebich, pos_max_runoff_schwebich] = max(schwebich_runoff)
date_max_runoff_schwebich = dn(pos_max_runoff_schwebich)

%% Task 4: Define runoff event and calculate event runoff coeffcient for both catchments
% select runoff event period based on the max. runoff date in Colpach
event_begin = datetime(2011, 01, 05, 00, 00, 00);   % 2 days before date of max. runoff in Colpach
event_end = datetime(2011, 01, 14, 00, 00, 00);     % 7 day after date of max. runoff in Colpach
event_idx = isbetween(dn, event_begin, event_end);  % time range from runoff event

% plot runoff event time series of precipitation and runoff in both
% catchments
figure
subplot(2,1,2)
plot(dn(event_idx), colpach_runoff(event_idx))              % plot runoff event in Colpach
hold on
plot(dn(event_idx), schwebich_runoff(event_idx))              % plot runoff event in Schwebich
title('runoff')
xlabel('date')
ylabel('runoff [mm/hr]')
legend('colpach','schwebich')
subplot(2,1,1)
stairs(dn(event_idx), colpach_precip(event_idx))                 % plot related precipitation in Colpach
hold on
stairs(dn(event_idx), schwebich_precip(event_idx))                 % plot related precipitation in Schwebich
title('precipitation')
xlabel('date')
ylabel('precipitation [mm]')
legend('colpach','schwebich')

% calculate event runoff coefficient for both catchments (see exercise 1)
RC_Colpach_event = sum(colpach_runoff(event_idx)) / sum(colpach_precip(event_idx))
RC_Schwebich_event = sum(schwebich_runoff(event_idx)) / sum(schwebich_precip(event_idx)) 

%% Task 5: Identify drought periods in bot catchments for entire data time period
% moving sum of precipitation in Colpach in a window of 31 days
% function calculates for each line of array the sum of the next 31 lines.
% Look up help documentation to function "movsum".
colpach_precip_movsum = movsum(colpach_precip, (31*24)); 

% logical index to find lines of moving-precipitation-sum array where moving sum is lower 10 mm = cumulated sum of precipitation over 31 days is lower than 10 mm
% example x = y < 10
drought_idx_colpach = colpach_precip_movsum < 10; 

% dates of drought period 
% select first date in first line as start of drought period
dates_drought_colpach = dn(drought_idx_colpach); 

% select drought event
drought_begin_colpach = datetime(2011, 03, 14, 00, 00, 00);   % manually select drought begin from above array (first date in array "dates_drought_colpach")
drought_end_colpach = datetime(2011, 04, 14, 00, 00, 00);     % manually select drought end by just adding 31 days to the drought start
drought_idx_colpach = isbetween(dn, drought_begin_colpach, drought_end_colpach);  % extract time range from drought period

% plot precipitation and runoff of drought period
figure
subplot(2,1,1)
plot(dn(drought_idx_colpach), colpach_precip(drought_idx_colpach))
title('precipitation colpach in drought period')
xlabel('date')
ylabel('precipitation [mm/h])')
subplot(2,1,2)
plot(dn(drought_idx_colpach), colpach_runoff(drought_idx_colpach))
ylim([0 0.1])
title('runoff colpach in drought period')
xlabel('date')
ylabel('runoff [mm/h])')
