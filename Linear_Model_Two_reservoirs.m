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

%% prepare variables 
nsteps = height(Colpach);           % number of time steps
    Soil = NaN(nsteps,1);           % soil storage (mm)
    GW = NaN(nsteps,1);             % groundwater storage (mm)
    q_dir = NaN(nsteps,1);          % direct runoff
    q_soil = NaN(nsteps,1);         % outflow from soil to GW
    q_gw = NaN(nsteps,1);           % discharge from GW
    q_sim =  NaN(nsteps,1);         % simulated total discharge (mm/dt)
dt = 1;                             % time step (hours)  

%% parameters to change

phi = 0.01;                         % direct runoff coefficient

tt_soil = 8;                       % transit time soil (hours)
tt_gw = 120;                         % transit time GW (hours)
ET = 3 /24;                         % evapotranspiration (mm/dt) (constant)
    
% initial states
Soil(1) = 1;
GW(1) = 2;

q_soil(1) = Soil(1)/tt_soil *dt;
q_gw(1)= GW(1)/tt_gw *dt;

%% loop over time steps
for jj = 2:nsteps 
    
    % direct runoff
    q_dir(jj) = Colpach.precip(jj) * phi;
    
    % storage change
    Soil(jj)= Soil(jj-1) - q_soil(jj-1) + Colpach.precip(jj) * (1-phi) - ET;
    GW(jj) =  GW(jj-1) - q_gw(jj-1) + q_soil(jj-1);    
     %make sure there is no negative storage
        if Soil(jj) < 0                            
                Soil(jj)=0;
        elseif GW(jj) < 0
                GW(jj)=0;
        end
        
    % fluxes from reservoirs
    q_soil(jj) = Soil(jj)/tt_soil*dt;
    q_gw(jj)= GW(jj)/tt_gw*dt;
        
    q_sim(jj)= q_dir(jj)+q_gw(jj);
end

%% plot result
plot(Colpach.date, [Colpach.runoff q_sim])
title('Discharge - two reservoirs')
legend('observed','simulated')
ylabel('mm/h')




