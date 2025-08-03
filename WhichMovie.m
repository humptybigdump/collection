clear all; clc; close all; 
% Load in the data
[MovieDuration, MovieTimes] = xlsread('MovieData_Lecture2.xlsx');

% Convert the movie times to a time format in numbers
MovieStartTimes = datetime(MovieTimes(2:end, 1));
% Clear the previous MovieTimes variables in order to prevent confusion
clear MovieTimes

% The time that you will leave university
StartTime       = datetime('18:10:00');
% The time you need to travel to the cinema
TravelTime      = 10; % minutes
% The time you would like to be home
TimeThreshold   = datetime('22:30:00');

% Calculate at what time you will be at the cinema, based on the starting
% time and the travel time
TimeAtTheCinema = (addtodate (    datenum(StartTime), TravelTime, 'minute')  );

% Extract the number of movies that need to be checked, which is based on
% the loaded data
NumberOfMovies  = length(MovieStartTimes);

% Loop over the movies
for ii = 1:NumberOfMovies

    % Calculate the time at which the movie will end by adding the duration
    % time in minutes to the starting time over the movie
    MovieEndTime{ii,1}          = (addtodate (    datenum(MovieStartTimes(ii,1)), MovieDuration(ii,1), 'minute')  );
    
    % Calculate the difference between the start of the movie and the time
    % at which you will arrive at the cinema. If the movie starts later
    % than when you may arrive, then the value will be positive (>0) if you
    % arrive later than the start of the movie, then the value will be
    % negative (<0)
    StartTimeDiff(ii,1)         = datenum(MovieStartTimes(ii,1)) - TimeAtTheCinema;
    % Calculate the difference between the end of the movie and the time
    % at which you would like to be home. If the movie ends later
    % than when you want to be home, then the value will be negative (<0) 
    % if ythe movie ends before the time you want to be home, then the 
    % value will be positive (>0)    
    EndTimeDiff(ii,1)           = datenum(TimeThreshold) - datenum(MovieEndTime{ii,1});
    
    % If the movie start after I arrive at the cinema (StartTimeDiff > 0) 
    % AND (&&)
    % If the movie ends before I want to be home (EndTimeDiff > 0)
    if StartTimeDiff(ii,1) > 0 && EndTimeDiff(ii,1) > 0
        % Put a yes in the first column
       CanISeeTheMovie{ii,1} = 'yes';
       % Add the movie starting time in the second column
       CanISeeTheMovie{ii,2} = datestr(MovieStartTimes(ii,1));
    else
        % Put a no in the first column
        CanISeeTheMovie{ii,1} = 'no';
    end

end