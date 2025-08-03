%% Water and Energy Cycles. WS 24/25
%% Exercise 9: Water movement in unsaturated soils
% Code to calculate soil water flux and change in soil moisture using
% Darcy-Richards law

%% Initialisation (setting parameters and defining variables)
% Select soil hydraulic parameters
thr = 0.065; % residual water content 
ths = 0.41; % saturated soil water content
alpha = 7.50; % air entry values
n_vg = 1.89; % shape value
ks = 1.23e-5;% saturated hydraulic conductivity

% setting the vertical length of soil domain and subdividing into grid nodes
z = -[0.00:0.025:1]'; 
dz = (diff(z));
dim = length(z);

% specify initial soil moisture profile
theta_topsoil = 0.35;  % initial soil mositure in upper soil/topsoil
theta_subsoil = 0.15;    % inital soil moisture in lower soil/subsoil
ip = find(z >= -0.2); % length of upper soil layer 20 cm, also finds the number and positions of grid nodes which are within this upper soil layer

d_theta = (theta_topsoil - theta_subsoil) / length(ip); % soil moisture difference between the grid nodes of the upper layer
theta = theta_subsoil * ones(dim,1);    % creates array, which contains theta of lower soil
theta(ip) = [theta_topsoil : -d_theta : theta_subsoil+d_theta]; % for the array positions of the upper soil, the theta values are overwritten with the respective theta of upper soil

% Preallocation of arrays
psi = zeros(dim,1); % matric potential at each grid node
k = zeros(dim,1); % soil hydraulic conductivity at each grid node
q = zeros(dim-1,1); % soil water flux after Dary-Richards law

% time setting
t_start = 0; % start of the simulation
time = t_start; % initialise simulation time
t_max = 30000;% maximum simulation time
dt = 100; % time step
i_time = 1; % counter of time steps

%% Main routine (solving Darcy-Richards law and plotting)
figure

% main routine with calculations is done as long as time is lower than
% t_max (so called while-loop)
while time < t_max
    
% Calculates psi and k based on theta values at all grid nodes, using
% function "k_psi_theta"
for i = 1:dim
    [k(i), psi(i)]=k_psi_theta(theta(i),thr,ths,alpha,n_vg,ks);
end

% Calculates flux between grid nodes with arithmetic mean (the "q" part of Eq.1)
for i = 1:dim-1 
    q(i) = (-0.5 * (k(i) + k(i+1))) * ((psi(i+1) - psi(i)) / dz(1) + 1);  % flux based on the arithmetric mean of state variables between nodes    
end

% Updates soil water content/soil moisture profile based on water fluxes between nodes
% starting loop at position 2: constant flux condition at upper boundary, mimics
% a never ending water supply/infiltration at top of the soil domain. 
for i = 2:dim-1
    theta(i) = theta(i) - dt * ((q(i-1)-q(i)) / abs(dz(1))); % continuity equation (mass conservation) to calculate change of soil moisture in each soil layer based on previously calculated fluxes q
end

% the combination of the flux equation (line 55) and
% continuity equation (line 65) gives the Darcy-Richards equation

time = time + dt;  % updating of simulation time

% Plotting of soil moisture profile
subplot(2,2,1)
plot(theta,z,'b-','linewidth',2);
xlabel('Soil water content [-]','fontsize',16);
ylabel(' z [m]','fontsize',16);
set(gca,'linewidth',2, 'fontsize',16);
axis([thr ths 1.1*min(z) 0.9*max(z) ]);
title([ num2str(time) ' s'], 'fontsize', 16);

% Plotting of matric potential profile
subplot(2,2,2)
plot(abs(psi),z,'g-','linewidth',2);
xlabel('Matric potential [m]','fontsize',16);
% axis([1.1*min(psi) 0.9*max(psi)  1.1*min(z) 0.9*max(z) ]);
ylabel(' z[m]','fontsize',16);
set(gca,'linewidth',2, 'fontsize',16);

% Plotting of Darcy fluxes at grid nodes (negative to indicate downward flux)
subplot(2,2,3)
plot(q(1:dim-1),z(1:dim-1),'r-','linewidth',2);
ylim([1.1*min(z) 0.9*max(z)]);
xlabel('Darcy flux [m/s]','fontsize',16);
ylabel('z[m]','fontsize',16);
set(gca,'linewidth',2, 'fontsize',16);

% Plotting of water flow velocity at grid nodes (negative to indicate downward flux)
subplot(2,2,4)
plot(q(1:dim-1)./theta(1:dim-1),z(1:dim-1),'r-','linewidth',2);
ylim([1.1*min(z) 0.9*max(z)]);
xlabel('v [m/s]','fontsize',16);
ylabel('z[m]','fontsize',16);
set(gca,'linewidth',2, 'fontsize',16);

MM(i_time) = getframe; % gets plots and shows them as movie
i_time=i_time+1; % time step counter for movie

end
