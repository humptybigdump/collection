function [Data] = ImportAndPrepare(File, Sheets)
%ImportAndPrepare imports concentration data in 96 well format
%from an Excel file 
%
%   Input variables:
%       - FileName:     String with file name of the Excel file containing 
%                       the measurement data
%       - Sheets:       n-by-1 cell array, each cell containing a string
%                       of the corresponding name of the Excel sheet. n
%                       must be a multiple of 3 and the order of sheets
%                       matters due to averaging. The data of 3 consecutive
%                       sheets will be averaged.
%   Output variables:
%       - Data:         n-by-96 matrix, n  is the number of samples with 96
%                       values of the measurements

    % Import the data from the Excel sheets and write it into a vector
    % (linear form):
    
    % The number of substrate concentrations (number of sheets of 96 well
    % plate concentration measurements) is determined by the 'length'
    % function of MATLAB 
    nrConc = length(Sheets);
    
    % A matrix (noConc x 96) is generated to store the linearized
    % data of the 96 well plate concentration measurement.
    Data = zeros(nrConc,96);    
    
    % The first for loop counts from 1 to the number of samples to go
    % through all of the sheets named in 'Sheets'.
    for ii = 1:1:nrConc
        % The function 'xlsread' is used to read the numeric data from
        % each sheet. When ii counts up, it goes through the various 
        % elements of 'Sheets' and uses them as input arguments for
        % xlsread. The file name stays the same during the whole loop.
        % 'Sheets' is a cell array, therfore use {} for indexing.
        % Import data is overwritten with current data in each step of this
        % for loop. 
        ImportData = xlsread(File, Sheets{ii,1});
        
        % nn manually counts up from 1 to 96 (without a loop, see 'nn = nn
        % + 1'). This is required to select the elements in 'Data' to which
        % the data from the 96 well plate is assigned.
        nn = 1;
        
        % The following two for loops are going trough all 96 wells step by
        % step.
        % This for loop counts up to 8 to go through the rows of
        % 'ImportData' (data in 96 well plate format).
        for kk = 1:8
            % This for loop counts up to 12 to go through the columns of
            % 'ImportData' (data in 96 well plate format).
            for mm = 1:12
                % Here each data set from one 96 well plate/sheet is
                % assigned to one row of 'Data' as ii counts up.
                % While the two for loops above (kk & mm) go through each
                % value from the 96 well plate element by element nn counts
                % up to 96 to go through columns of 'VectorData'.
                Data(ii,nn) = ImportData(kk,mm);
                % Count up after each assignment.
                nn = nn + 1;
            end
        end
    end
    
    % Output values are 'Data'
end