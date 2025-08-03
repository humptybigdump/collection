%% Time series analyis, Part1. Water and Energy Cycles WS 22/23
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

%% Task 1: Plot time series of precipitation and runoff
figure
subplot(2,1,2)
title('runoff')
plot(dn, colpach_runoff)
hold on
plot(dn, schwebich_runoff)
title('runoff')
legend('colpach','schwebich')
ylabel('mm')
xlabel('date')
subplot(2,1,1)
stairs(dn, colpach_precip)                 
hold on
stairs(dn, schwebich_precip)
title('precipitation')
legend('colpach','schwebich')
ylabel('mm')
xlabel('date')

%% Task 2: Monthly runoff sums for hydrological year 2013
% selecting hydrological year 
year_begin = datetime(2012, 11, 01, 01, 00, 00); % begin of the time period
year_end = datetime(2013, 10, 31, 23, 00, 00);   % end of the time period
year_idx = isbetween(dn, year_begin, year_end);  % logical index; 1 = value within hydrological year, 0 = value not within hydrological year
% "isbetween" checks each line of the date array "dn" if the respective
% date lies between "year_begin" and "year_end".This "year_idx" can be
% used as index for arrays to extract only the respective data for the
% hydrological year from the entire time series, e.g "data_time_series(year_idx)". 

% monthly runoff sums in both catchments
hourly_runoff_Colpach = colpach_runoff(year_idx); % all hourly runoff values in hydrological year
hourly_runoff_Schwebich = schwebich_runoff(year_idx);
months = month(dn(year_idx)); % number of month of each runoff value in hydrological year

for i = 1:12
    monthly_runoff_sum_Colpach(i) = sum(hourly_runoff_Colpach(months == i)); % calculate monthly runoff sums
    monthly_runoff_sum_Schwebich(i) = sum(hourly_runoff_Schwebich(months == i));
end

% array "monthly_runoff_sum_Schwebich" is still in a wrong sequence/order
% because the 12 months span over 2 years and the months 11 and 12 are from
% the previous year 2012. So, we have to sort the array by putting the
% months 11 and 12 (from year 2012) to the front and subsequently the
% months 1 to 10 (from year 2013)
sorted_monthly_runoff_sum_Colpach = [monthly_runoff_sum_Colpach(11:12),monthly_runoff_sum_Colpach(1:10)]; % sort sequnce of months to obtain values from Nov. 2012 to Oct. 2013
sorted_monthly_runoff_sum_Schwebich = [monthly_runoff_sum_Schwebich(11:12),monthly_runoff_sum_Schwebich(1:10)]; % sort sequnce of months to obtain values from Nov. 2012 to Oct. 2013


figure
bar(sorted_monthly_runoff_sum_Colpach,'hist')
hold on 
bar(sorted_monthly_runoff_sum_Schwebich,'r')
title('Monthly runoff sums')
ylabel('runoff sum [mm]')
xlabel('months')
legend('Colpach','Schwebich')
set(gca,'xticklabel',{'Nov. 2012','Dez. 2012','Jan. 2013','Feb. 2013','Mar. 2013','Apr. 2013','May. 2013','Jun. 2013','Jul. 2013','Aug. 2013','Sep. 2013','Oct. 2013'})

%% Task 3: Runoff coefficient
% annual runoff coefficient Colpach and Schwebich 2012/2013
RC_Colpach_2013 = sum(colpach_runoff(year_idx)) / sum(colpach_precip(year_idx))
RC_Schwebich_2013 = sum(schwebich_runoff(year_idx)) / sum(schwebich_precip(year_idx)) 

%% Task 4: Water balance and calculation of evapotranspiration ET
ET_Colpach_2013 = sum(colpach_precip(year_idx)) - sum(colpach_runoff(year_idx))
ET_Schwebich_2013 = sum(schwebich_precip(year_idx)) - sum(schwebich_runoff(year_idx))

%% Task 5: Flow duration curve
% sort the runoff data as well as extract the corresponding ranks. The rank
% shows for each sorted runoff value its position/index which it had in the "original"
% unsorted array "colpach_runoff(year_idx)".
[sorted_runoff_colpach, ranks_colpach] = sort(colpach_runoff(year_idx),'descend');
[sorted_runoff_schwebich, ranks_schwebich] = sort(schwebich_runoff(year_idx),'descend');

% calculate the exceedance probability
prob_colpach = 100 * (ranks_colpach / (length(ranks_colpach) + 1));
prob_schwebich = 100 * (ranks_schwebich / (length(ranks_schwebich) + 1));

sorted_prob_colpach = sort(prob_colpach);
sorted_prob_schwebich = sort(prob_schwebich);

% plot data
figure
semilogy(sorted_prob_colpach, sorted_runoff_colpach)
hold on 
semilogy(sorted_prob_schwebich, sorted_runoff_schwebich)
xlabel('exceedance probability')
ylabel('runoff [mm/h]')
legend('colpach','schwebich')
title('Flow duration curves hydrological year 2013')

%% Task 6: Double mass curves 
colpach_cumsum_precipitation = cumsum(colpach_precip(year_idx)) / sum(colpach_precip(year_idx));
colpach_cumsum_runoff = cumsum(colpach_runoff(year_idx)) / sum(colpach_precip(year_idx));

schwebich_cumsum_precipitation = cumsum(schwebich_precip(year_idx)) / sum(schwebich_precip(year_idx));
schwebich_cumsum_runoff = cumsum(schwebich_runoff(year_idx)) / sum(schwebich_precip(year_idx));


figure
plot(colpach_cumsum_precipitation, colpach_cumsum_runoff)
hold on
plot(schwebich_cumsum_precipitation,schwebich_cumsum_runoff)
xlim([0 1])
ylim([0 1])
xlabel('normalized, cumulated precipitation')
ylabel('normalized, cumulated runoff')
legend('colpach','schwebich')
title('Double mass curves hydrological year 2013')
