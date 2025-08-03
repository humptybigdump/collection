clear all
close all
clc


data_marie = readData('data_marie.csv');
data_jocie = readData('data_jocie.csv');
data_paul = readData('data_paul.csv');
data_friederike = readData('data_friederike.csv');

tolerance = 0.3; 

resultmarie = averageCloseValues(data_marie, tolerance);
heightdiffmarie = getHeightDiff(resultmarie);
totalheightsmarie = getTotalHeight(heightdiffmarie);

resultjocie = averageCloseValues(data_jocie, tolerance);
heightdiffjocie = getHeightDiff(resultjocie);
totalheightsjocie = getTotalHeight(heightdiffjocie);

resultpaul = averageCloseValues(data_paul, tolerance);
heightdiffpaul = getHeightDiff(resultpaul);
totalheightspaul = getTotalHeight(heightdiffpaul);

resultfriederike = averageCloseValues(data_friederike, tolerance);
heightdifffriederike = getHeightDiff(resultfriederike);
totalheightsfriederike = getTotalHeight(heightdifffriederike);








function totalheights = getTotalHeight(heightdiff)
totalheights = [];
totalheightpos = 0;
totalheightsneg =0;

for i=1:length(heightdiff)
if(heightdiff(i) > 0)
totalheightpos = totalheightpos + heightdiff(i);
else
   totalheightsneg = totalheightsneg+ -1*heightdiff(i);
end
end
totalheights = [totalheightpos, totalheightsneg];
end




function height_diffs = getHeightDiff(result)
heights = [];
height_diffs = [];

for i = 1:length(result)
height = 288.15/0.0065 * (1-(result(i)/1013.25)^(1/5.255));
heights = [heights,height];
end

for i = 1:length(result)-1
height_diff = heights(i+1)-heights(i);
height_diffs = [height_diffs, height_diff];
end

end

function result = averageCloseValues(data, tolerance)
    result = [];
    currentSet = [];

    for i = 1:length(data)
        value = data(i);

        if ~isempty(currentSet) && abs(value - mean(currentSet)) <= tolerance
            currentSet = [currentSet, value];
        else
            if ~isempty(currentSet)
                result = [result, mean(currentSet)];
            end
            
            currentSet = [value];
        end
    end

   
    if ~isempty(currentSet)
        result = [result, mean(currentSet)];
    end
end

function data = readData(Data)
data = readmatrix(Data);
data = data(:,2);

end


