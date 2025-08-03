% Linear Model

%% import selected data
importColpachData; 

%% Plot input data 
subplot(3,1,1)
bar(Colpach.date, Colpach.precip)
title('Precipitation')
ylabel('mm')
subplot(3,1,[2 3])
plot(Colpach.date, Colpach.runoff)
title('Discharge')
ylabel('mm')

%% Linear storage

% prepare variables 
nsteps = height(Colpach);           % number of time steps
    S = NaN(nsteps,1);              % storage (mm)
    q_sim =  NaN(nsteps,1);         % simulated discharge (mm/dt)
dt = 1;                             % time step (hours)  

%% parameters to change
tt = 115;                           % transit time (hours)
ET = 3 /24;                         % evapotranspiration (mm/dt)
    
% initial state
S(1) = 4;                           % initial filling of Soil reservoir
q_sim(1) = S(1)/tt*dt;

%% loop over time steps
for jj = 2:nsteps 
    S(jj)= S(jj-1) - q_sim(jj-1) + Colpach.precip(jj) - ET;
        if S(jj) < 0
            S(jj)=0;
        end
    q_sim(jj)= S(jj)/tt*dt;
end
%

%% plot result
plot(Colpach.date, [Colpach.runoff q_sim])
title('Discharge')
legend('observed','simulated')
ylabel('mm/h')




