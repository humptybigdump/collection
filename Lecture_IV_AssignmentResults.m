% ================== MATLAB ASSIGNMENT LECTURE IV ===================== %

% ================== MATLAB ASSIGNMENT LECTURE III (EnyzmeKinetics.m) ===================== %

% Initialize script
clear all; clc; close all; 

% File name of the Excel file containing the measurement data (must be in
% your current folder, left of the command window)
MyFileName = 'DataMATLAB_Experiment_1.xlsx';

% Define variable that stores the sheetnames (cell array)
MySheets = sheetnames(MyFileName);

% Calculate the time values according to the given time interval input
% 'TimeInverval': The last time point in calculated. 95 is used instead of
% 96 because the first measurement is performed a 0 seconds.
EndTime = (96 - 1) * 30;
% 'linspace' returns a row vector of (96) evenly spaced points from 0
% to EndTime
TimePoints = linspace(0, EndTime, 96);

% Execute the ImportAndPreapare function by passing the variable from above
% as input arguments. The vector containing the time points and the matrix
% containing the data are stored in 'Time' and 'Data'
MyData = ImportAndPrepare(MyFileName, MySheets);


%% --------------------------- Assignment 1  ----------------------------
figure;
% Count the concentrations --> number of lines to plot
nrConc = length(MySheets);

% Define 8 different line styles
LineStyles  = {':','-','-.','--',':','-','-.', '--'};
% Define 8 different line colors
LineColors  = {'b','g','b','k','r','k','r','b'};

% Loop over all concentrations
for ii = 1:nrConc
   
    % Plot the time vs data, using different line styles and colors. Hold
    % the figure so that all lines are plotted in one figure
    plot(TimePoints, MyData(ii,:), 'LineStyle', LineStyles{ii}, 'Color', LineColors{ii});
    hold on; 
    % Set the labels of the axis
    xlabel('Time (s)'); ylabel('Product concentration (\muM)');
    % Make the background of the plot white
    set(gcf,'color','w');
    % Change the settings of the font
    set(gca, 'FontName', 'Times New Roman', 'FontSize', 18)

end

% Add a legend, let the function but the legend outside of the plot
legend(MySheets, 'Location', 'BestOutside');

%% --------------------------- Assignment 2  ----------------------------

ColumnStartLinearRange  = 35; % this the column where 1000 (s) starts
TimeLinear              = TimePoints(1,ColumnStartLinearRange:end); % Extract linear time range

% Loop over all concentrations
for ii = 1:nrConc
    
    % Extract linear data range
    LinearData(ii,:) = MyData(ii,ColumnStartLinearRange:end);
    
    Slope(ii,1) = (LinearData(ii,end) - LinearData(ii,1)) ./ (TimeLinear(1,end) - TimeLinear(1,1));
    
end

%% --------------------------- Assignment 3  ----------------------------

% Open a new figure so that the pervious one is not overwritten
figure;

% Convert the sheetnames from strings to numbers
Concentrations = str2double(MySheets)';

% Plot the substrate concentration against the slope (reaction rate) with
% black (k) points and adjust the markersize to make the points biffer
plot(Concentrations, Slope, '.k', 'MarkerSize', 15); 
% Set the labels of the axis
xlabel('Substrate concentration (\muM)'); ylabel('Reaction rate (\muM/s)');
% Make a title for your plot
title('Michealis-Menten Kinetics');
% Make the background of the plot white
set(gcf,'color','w');
% Change the settings of the font
set(gca, 'FontName', 'Times New Roman', 'FontSize', 18)


%% ---------------------------Bonus  ----------------------------
%not finished


xDataFit = linspace( str2double(MySheets(length(MySheets))), str2double(MySheets(1)), 10000);
x0=10;
x_valuesLSQ = lsqnonlin(myfunct,x0); %dummy




plot (x,y, 'Markersize', 20, 'Marker', 'd', 'Color', 'b', 'LineStyle', '--', 'LineWidth', 2 )

