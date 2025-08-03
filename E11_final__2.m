%% Exercise 11: Climate analysis of british catchments with the Budyko framework
%% Task 1: Generating Budkyo diagram and plotting the position of one catchment into Budyko diagram
% import hydrological time series of one arbitrary catchment as table (function:
% "readtable"). Simply choose one of the 50 given catchments.
tab_catchment = readtable('./data/CAMELS_GB_hydromet_timeseries_33028_19701001-20150930.csv');

% saving/assigning precipitation, potential evapotranspiration ('pet'), runoff ('discharge_spec') from
% table to variables/arrays
catchment_precip = table2array(tab_catchment(:,{'precipitation'}));
catchment_pot_evapo = table2array(tab_catchment(:,{'pet'}));
catchment_streamflow = table2array(tab_catchment(:,{'discharge_spec'}));

% the streamflow time series may contain several NaN values. Use the
% function "isnan" to find the index (=positions/rows) in the streamflow time
% series, where we have actual data and no NaN. 
nan_idx = ~isnan(catchment_streamflow);

% calculating position of catchment in Budyko framework:
% Step 1: calculating actual evapotranspiration by closing water balance
% (cf. exercise 1) assuming constant water storage change over the 45
% years. In the following, use the above defined NaN index to only do the
% calculations with actual data and ignoring rows with NaN!
catchment_act_evapo = sum(catchment_precip(nan_idx)) - sum(catchment_streamflow(nan_idx));


% Step 2: calculating aridity index of catchment (Eq. 1)
aridity_index = sum(catchment_pot_evapo(nan_idx))/sum(catchment_precip(nan_idx));

% Step 3: calculating evaporative index of catchment (Eq. 2)
evapo_index = sum(catchment_act_evapo)/sum(catchment_precip(nan_idx));

% Step 4: plot of budyko framework and position of catchment
figure
% plot of budyko curve with water and energy limits
plot([0:1], [0:1]) % line of energy limit
hold on
xlim([0 2])
ylim([0 2])
title('Position of catchment in Budyko framework')
xlabel('aridity index ETP/P')
ylabel('evaporative index ETA/P')
plot([1:2], ones(1, length([1:2]))) %line of water limit
hold on
% plot of catchment position 
plot(aridity_index,evapo_index, 'o')

%% Task 2: Plotting the positions of all given catchments automatically into the Budyko framework
% save list of all available files in folder "data", which start with a "C", in a variable  (function:"dir")
myFiles = dir('./data/C*');

% open plot of budyko curve and position of catchment. At the end of each
% for-loop run, the position of currently selected catchment will be added
% to this plot
figure
% plot of budyko curve with water and energy limits
plot([0:0.1:1], [0:0.1:1]) % line of energy limit
hold on
xlim([0 2])
ylim([0 2])
title('Position of catchment in Budyko framework')
xlabel('aridity index ETP/P')
ylabel('evaporative index ETA/P')
plot([1:0.1:2], ones(1, length([1:0.1:2]))) %line of water limit

% create for-loop to get hydrological time series data and to calculate position
% in Budyko framework of all given catchments, consecutively from first to
% last catchment
for k = 1:length(myFiles)
  % Step 1 in for-loop: at the start of each loop run, save the name of the
  % next catchment file in the previously defined list containing all
  % available files in folder "data"
  baseFileName = myFiles(k).name;
  
  % Step 2 in for-loop: set and save path to this currently selected
  % catchment file in a variable
  % (function: "fullfile")
  path_to_file = fullfile('./data/', baseFileName);

  % Step 3 in for-loop: import hydrological time series of currently selected catchment as table (function:
  % "readtable")
  tab_catchment = readtable(path_to_file);

  % Step 4 in for-loop: saving/assigning precipitation, potential
  % evapotranspiration, runoff from this table to variables/arrays. Again,
  % use "isnan" to define index/positions, where we have actual data and no
  % NaN in the streamflow timeseries
  catchment_precip = table2array(tab_catchment(:,{'precipitation'}));
  catchment_pot_evapo = table2array(tab_catchment(:,{'pet'}));
  catchment_streamflow = table2array(tab_catchment(:,{'discharge_spec'}));
  nan_idx = ~isnan(catchment_streamflow);
  
  % Step 5 in for-loop: calculating actual evapotranspiration, aridity index and evaporative
  % index of currently selected catchment. Again, use previously defined
  % Nan index to only do the calculations with actual data and no NaN
  catchment_act_evapo = sum(catchment_precip(nan_idx)) - sum(catchment_streamflow(nan_idx)); 
  aridity_index = sum(catchment_pot_evapo(nan_idx))/sum(catchment_precip(nan_idx));
  evapo_index = sum(catchment_act_evapo)/sum(catchment_precip(nan_idx));
  
  % Step 6 in for-loop: plot position of currently selected catchment into already existing plot of Budyko framework 
  hold on
  plot(aridity_index,evapo_index, 'o')
  
end  

%% ADDITION: Fitting Budyko curve to data after Zhang et al. (2004)
% qualitatively fit w to data 
% initializing theoretical ETP_P data series
ETP_P_theo = linspace(0,10,100);

% w parameter (scaling) between 1 and 5
w = 3;

% equation for calculating theoretical ETA_P data series based on theoretical ETP_P after: Zhang, L., Hickel, K., Dawes, W. R., Chiew, F. H. S., Western, A. W., & Briggs, P. R. (2004). A rational function approach for estimating mean annual evapotranspiration. Water Resources Research, 40(2), 1–14. https://doi.org/10.1029/2003WR002710
ETA_P_theo = 1 + ETP_P_theo - (1 + (ETP_P_theo) .^ w) .^ (1  /w);

% plot of theoretical, fitted Budyko curve
hold on
plot(ETP_P_theo, ETA_P_theo)

