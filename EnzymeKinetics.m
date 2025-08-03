% ================== MATLAB ASSIGNMENT LECTURE III ==================== %

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


