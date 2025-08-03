%% Water and Energy Cycles. WS 20/21
%% Exercise 10: Soil hydraulic functions
%% Task 1: Generating soil water retention and soil hydraulic conductivity curves of three soils
soil = {'S','SIL','SIC'};
% select soil hydraulic parameters from Table 1
thr = [0.045,0.067,0.07]; % residual water content of the soil
ths = [0.43,0.45,0.36];   % Porosity of the soil, water content at saturation
alpha = [14.5,2.0,0.5];  % Air entry value in 1/m
n_vg = [2.68,1.41,1.09];  % width ot the pore size distribution
ks= [8.25e-5,1.25e-6,5.83e-8];% saturated hydraulic conductivity, in m/s
m_vg = 1 - 1./n_vg;

% open figure
figure
p1=[];
p2=[];

for i = 1:3
    
% define array with soil water content from thr to ths with delt_theta step
% size of 10^-7. For the theta values in this array, you subsequently
% calculate the corresponding k(theta) and psi(theta) values
theta=[thr(i):1e-7:ths(i)]; 

% initialize/preallocate arrays for k(theta) and psi(theta) 
psi = zeros(length(theta),1); 
k = zeros(length(theta),1);   

% Calculate psi with converted Eq. 1
S = (theta - thr(i)) / (ths(i) - thr(i)); % relative saturation
psi = -1 * ((1 - S .^ (1 ./ m_vg(i))) ./ (S .^ (1 ./ m_vg(i)))) .^ (1 ./ n_vg(i)) ./ alpha(i); 

% Calculate k with Eq. 2
k = ks(i) .* S .^ 0.5 .* (1 - (1 - S .^ (1 ./ m_vg(i))) .^ m_vg(i)) .^ 2;

% Calculation of effetive field capacity of soil:
% definition of psi (m) at PWP and FC
psi_PWP = 150;
psi_FC = 0.68;

% 1: calculate relative saturation S at PWP
S_PWP = (1 ./ (1 + (abs(psi_PWP) .* alpha(i)) .^ n_vg(i))) .^ m_vg(i);

% 2: calculate soil water content theta at PWP
theta_PWP = thr(i) + (S_PWP * (ths(i) - thr(i)));

% 3: calculate relative saturation S at FC
S_FC = (1 ./ (1 + (abs(psi_FC) .* alpha(i)) .^ n_vg(i))) .^ m_vg(i);

% 4: calculate soil water content theta at FC
theta_FC = thr(i) + S_FC * (ths(i) - thr(i));

% 5: calculate effective field capacity with Eq. 3
eff_FC = theta_FC - theta_PWP

% plot of soil water retention curve and soil hydraulic conductivity curve
subplot(2,1,1);
p1 = [p1;semilogy(theta,abs(psi),'-','linewidth',2,'DisplayName',soil{1,i})];
hold on
semilogy(theta_PWP,psi_PWP,'go','linewidth',2);
semilogy(theta_FC,psi_FC,'go','linewidth',2);
xlabel('Soil water content [-]','fontsize',16);
ylabel('Psi [m]','fontsize',16);
set(gca,'linewidth',2, 'fontsize',16);
ylim([10^-2 10^7])
title('soil water retention curves')
legend([p1],'location', 'best')

subplot(2,1,2);
p2 = [p2;semilogy(theta,k,'-','linewidth',2,'DisplayName',soil{1,i})];
hold on
xlabel('Soil water content [-]','fontsize',16);
ylabel('k [m/s]','fontsize',16);
set(gca,'linewidth',2, 'fontsize',16);
ylim([10^-20 10^-4])
title('soil hydraulic conductivity curve')
legend([p2],'location', 'best')

end

%% Task 2: Sensitivity of soil water retention curves of different soils to alpha
% select soil hydraulic parameters from Table 1
soil = {'S','SIL','SIC'};
% select soil hydraulic parameters from Table 1
thr = [0.045,0.067,0.07]; % residual water content of the soil
ths = [0.43,0.45,0.36];   % Porosity of the soil, water content at saturation
n_vg = [2.68,1.41,1.09];  % width ot the pore size distribution
ks= [8.25e-5,1.25e-6,5.83e-8];% saturated hydraulic conductivity, in m/s


% initialize Monte-Carlo algorithm
% set number of MC runs
MC_N = 1000;

% create array with randomly distributed alpha values between an upper and
% lower limit
alpha_range = 0.5 + (15 - 0.5) * rand(MC_N,1);

for i = 1:3
% define array with soil water content from thr to ths with delta_theta step
% size of now 0.01. For the theta values in this array, you subsequently
% calculate the corresponding k(theta) and psi(theta) values
theta=[thr(i):0.01:ths(i)];     
        
% initialize/preallocate arrays for k(theta) and psi(theta) 
psi = zeros(length(theta),1); 
k = zeros(length(theta),1);

% initialize arrays to store result (theta, psi) of each MC run
theta_runs = zeros(length(theta),MC_N);
psi_runs = zeros(length(theta),MC_N);

% start MC loop
for MC_run = 1:MC_N
    
    % select next alpha value from predefined random distribution
    alpha = alpha_range(MC_run);
    
    % calculate m
    m_vg = 1 - 1./n_vg(i);

    % calculate psi using Eq. 1 (see procedure above)
    theta_runs(:,MC_run) = theta;
    S = (theta - thr(i)) / (ths(i) - thr(i)); 
    psi_runs(:,MC_run) = -1 * ((1 - S .^ (1 ./ m_vg)) ./ (S .^ (1 ./ m_vg))) .^ (1 ./ n_vg(i)) ./ alpha; 

end

% plot soil water retention curves for diferent alpha
figure
semilogy(theta_runs,-psi_runs,'b-','linewidth',2);
xlabel('Soil water content [-]','fontsize',16);
ylabel('Psi [m]','fontsize',16);
set(gca,'linewidth',2, 'fontsize',16);
title(soil{1,i})
ylim([10^-2 10^7])

end
