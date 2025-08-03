%% 1- compute lowest temperature

% temperature breakthrough appears when production power = 0.9 x initial
% power
% power = rho*cp*Q*(Tprod - Tinj)

TempBT = 0.9*(165-60)+60+273.15;

% injection well position
WI = 850;

%% 2- loop over the input files
Listcsv=ls('*.csv');

figure();
hold on;
for icsv=1:size(Listcsv,1)
    % read the data
    TT=dlmread(Listcsv(icsv,:),',',1,0);
    % change first value of the temperature to initial condition
    TT(1,2) = 165+273.15;
    % get production well position and permeability for the file
    WP = str2double(Listcsv(icsv, 17:19));
    dist = WI-WP;
    PERM = str2double(Listcsv(icsv, 23:27));
    % look for breakthrough time
    done = 0;
    for iyear=1:size(TT,1)
        if(TT(iyear,2)<TempBT)
            scatter(PERM, dist, 150, TT(iyear,1)/(365*86400), 'filled', 's');
            done = 1;
            break;
        end
    end
    if ~done
        scatter(PERM, dist, 150, TT(iyear,1)/(365*86400), 'filled', 's');
    end
end

colormap(flipud(hsv(10)));
colorbar

%% 3- plot 2D map