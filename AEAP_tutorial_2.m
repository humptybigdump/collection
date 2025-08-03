%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   AEAP - Tutorial 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Remember how to load data. We will use the same data again:
data = csvread('AEAP_data_1.csv',1); % don't forget to change the directory first!

dates = data(:,1);
MKT = data(:,2);
Returns = data(:,3:end);
constant = ones(size(MKT));

save AEAP_data_1 % save all variables in the workspace in MATLAB data file ('.mat')
% clicking AEAP_data_1.mat gives information about the data in the "current folder" window
% exporting to other formats (csv, xls, txt,...) is possible too --> google
clear
 
load AEAP_data_1


%% Data analysis
plot(MKT); % x axis is 1:length(MKT) automatically
plot(Returns(:,1)); % old data is overwritten
plot(MKT); 
hold on
plot(Returns(:,1)); % second plot is in another color
% to change color or other properties, click on window icon in plot window
% all properties can also be changed "directly"
close
plot(Returns(:,1),'Linewidth',2);
close
plot(Returns); % when entering a matrix, columns are interpreted as time series
close

% We would like to have dates on x axis
plot(dates,MKT) % looks stupid. Why?
dates(1:3)
dates_2 = datetime(1963,7,31) + calmonths(0:length(MKT)-1);
plot(dates_2,MKT)
% click on editor and look at X Limits. What do these numbers mean?
% Matlab counts days starting on January 1st in year 0.
datenum(dates_2(1))
datestr(717183)
datenum(datetime(0,1,1))
% click on file -> save as in the plot editor to save the plot
% If we save it as a MATLAB figure, we can rework it later.

%% Writing functions
% Let us write a function that calculates the mean of a time series.
% This is of course a bit stupid, because such a function already exists.
% Click on New -> Function in the editor
% All the text that you see is syntax. Fill in the following:
% 
% function y = AEAPmean(x)
% %AEAPmean Estimates the mean of a random variable with observations x.
% 
% n = length(x);
% y = sum(x)/n;
% 
% end

AEAPmean(MKT)
% works! Interesting: We defined a variable called "n" within the body of
% the function. It does not show up in the Workspace. 
% -> We can use functions as black boxes. We are not interested what
% happens inside.
% 
% mean(MKT)


%% Loops
% Imagine we want to write a function that can estimate the arithmetic and
% the geometric mean. We add a second input, by which we can decide if the
% program gives us the arithmetic or the geometric mean as output.

% function y = AEAPmean2(x,id)
% %AEAPmean Estimates the arithmetic mean of a random variable with
% % observations x (if id is equal to 1) or the geometric mean (if id is
% % equal to 2)
% 
% if id==1
%     y = mean(x);
% else
%     y = geomean(x);
% end
% 
% end

% explain if-loop and logical operator ==
t = 4==4
t = 4==5
t = 4~=5 % not equal
t = 4<5
t = 4>=4
% we saw this in first tutorial already: 
t = 1:6>=4

AEAPmean2(1:9,1)
AEAPmean(1:9)
AEAPmean2(1:9,2)
geomean(1:9)

AEAPmean2(1:9,3) % also works although id is not equal to 2. Why?
% change else into elseif id==2
AEAPmean2(1:9,3)
% nothing happens
% add: else error('Please enter an id of 1 or 2.');

AEAPmean2(-9:-1,2) % interesting error message
% let's add a check if all input variables are nonnegative:
% change body of the function to

% if sum(x<0)>0
%     error('x must contain nonnegative elements only.');
% else
%     if id==1
%         y = mean(x);
%     elseif id == 2
%         y = geomean(x);
%     else
%         error('Please enter an id of 1 or 2.');
%     end
% end
AEAPmean2(-9:-1,2)
AEAPmean2(-9:-1,1) % we don't want that. The arithmetic mean can also be calculated for negative values.
% add to first if-condition: && id==2
AEAPmean2(-9:-1,1)

% Alternatively, we can use a try-catch loop
% change body of code to:
% try 
%     if id==1
%         y = mean(x);
%     elseif id == 2
%         y = geomean(x);
%     end
% catch
%     error('You have to enter id of 1 and 2 and, in case you entered id of 2, x should only contain nonnegative values.');
% end
AEAPmean2(-9:-1,1)
AEAPmean2(-9:-1,2)

% for-loops
for i=1:10
    x(i) = log(i);
end
x

x = log((1:10)')

% avoid loops whenever possible. Loops are slow. Sometimes, however, we need loops:
clear x
x(1) = 0
x(2) = 1
for i=3:10
    x(i) = x(i-2)+x(i-1);
end
x
% Fibonacci numbers. We need to define the previous numbers to be able to
% calculate the next. 

% while-loops
% Let's calculate all Fibonacci numbers until we hit 1000
clear x
x(1) = 0; x(2) = 1;
while x(end)<1000
    newX = x(end-1)+x(end);
    x = [x,newX];
end
x

%% Large programs

% go to https://personalpages.manchester.ac.uk/staff/Alastair.Hall/GMMGUI.html
% and download GMM toolbox
% look at the program and the documentation


    

