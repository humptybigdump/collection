%% Simulates solute transport under steady state conditions
% compares reactive and conservative solutes
% Erwin Zehe, Jan Wienhöfer

clearvars; close all;

%% set model parameters
D = 1e-8;   % dispersion coefficient m^2/s -> come from tracer experiments (method of moments)
v = 9.95e-7;   % average transport velocity m/s -> come from tracer experiments (method of moments)
z_fix = 1.0;  %critical depth, also depth for plotting C(Z_fix,time)

R = 2;  % Retardation coefficient [-]

lambda = log(2)/ (8 * 86400);           % degradation time scale in 1/s -> example Bromoxynil from Table 1
                                        % degradation rate = ln(2)/DT50
dt = 0.1*86400;         % timestep/s
max_time = 50*86400;     % maximum transport time/s
time = zeros((max_time/dt),1);

dz = 0.05; %grid size m
maxz = 10; %maximum depth m
z = [0+dz:dz:maxz]'; % simulation grid

% Dammköhler number of the reactor at critical depth z_fix
Damm = (z_fix*R/v) / (1/lambda);

% Initialize concentrations 
C_sim = zeros(length(z),floor(max_time/dt)); % allocate memory
C_sim_cons = zeros(length(z),floor(max_time/dt)); % transport of the conservative tracer allocate memory

% Initialise transport time
t_trans = 0; %s

figure;
n = 1; %set counter for movieframe

%% computation in nested loops
while t_trans < max_time;
time(n) = t_trans;

for i = 1:length(z)  
 % concentration of reactive solute
 C_sim(i,n) = 1/(sqrt(4*pi*D*t_trans/R))... 
  *exp(-(z(i)-v*t_trans/R)^2/(4*D*t_trans/R))...
  *exp(-lambda*t_trans/R);
 % concentration of conservative solute
 C_sim_cons(i,n) = 1/(sqrt(4*pi*D*t_trans))...
  *exp(-(z(i)-v*t_trans)^2/(4*D*t_trans));
end

% plotting
 plot(C_sim(:,n),-z,'k-','linewidth',2);
 hold on;
 plot(C_sim_cons(:,n),-z,'r-','linewidth',2);
 hold off;
 xlabel(' normalized Concentration [1/m^3],');
 ylabel(' z [m]');
 title(['Concentration profile at time ' num2str(t_trans/3600) ' h, Damm = ' num2str(Damm, 3)]);
 set(gca,'fontsize',14,'linewidth',2);
 Mbla(n)=getframe;   %Frame to capture a movie (sequence of plots) --> if not, plots are not visualized during runtime
 
t_trans=t_trans+dt;  % increase transport time 
n=n+1;     %increase counter for movieframe
end

idepth=find(abs(z-z_fix) <0.01);

%% Plot 
 figure;
 h1=plot(time(:)/(86400),C_sim(idepth,:),'k-','linewidth',2);
 hold on;
 h2=plot(time(:)/(86400),C_sim_cons(idepth,:),'r-','linewidth',2);
 title(['Concentration at ' num2str(z_fix) ' m depth as function of time, Damm = ' num2str(Damm, 3)],'fontsize',12);
 xlabel(' time [d]');
 ylabel(' normalized Concentration [1/m^3],');
 legend([h1 h2],'non conservative solute', 'conservative solute'); 
hold off;

