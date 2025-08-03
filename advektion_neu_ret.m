%% Program for simualtion of advective transport
% comparing conservative solute and solute with non-linear adsorption
% Erwin Zehe, Jan Wienhöfer

% Define grid
dx = 0.01;
x = 0:dx:10;
deltax = diff(x);

%%  Define parameters
v = 1e-5;                 % transport velocity

lambda = 0;       %1/1000;                            % first order decay
b = 0.5;      % Freundlich exponent beta
Kf = 7.;      % Freundlich parameter characterising adsorption
C_lim = 0.00001;      % minimum conc 
Cmax = 0.1;            % max conc

% Define initial states 
C = zeros(1, length(x));        % local concentrations of CONSERVATIVE solute
R = ones(1, length(x));         % local retardation coefs
C_r = zeros(1, length(x));      % local concentrations of ADSORBING solute

ip = find(x <=  0.5);           % Define a sinusoidal distribution in the first 0.5 m
C(ip) = Cmax*sin(pi*x(ip)/0.5);
C_alt = C;                      % vector to store old C
C_r(ip) = C(ip);
C_ralt = C;                     % vector to store old C_r

% Define time step (--> Courant)
tmax = 5*86400; % maximum time
time = 0.; % start time
itime = 1;
C_ave = 1;
C_amp = 0.5;

%courant criterion
dt = 0.5*min(deltax)/v;

% Time loop, solve working equation

figure;

while time < tmax
    % Define boundary conditions(0.5
    C(1) = 0; % for all times
            % C(1) = C_ave+C_amp*sin(pi*time/1000);%*rand(1) ;
    C(length(x)) = C_alt(length(x)-1);
    
    for j = 2:length(x)-1
        if b <1
            if C_ralt(j) > C_lim
                R(j) = 1+b*Kf/(C_ralt(j))^b;
            else
                R(j) = 1+b*Kf/(C_lim)^b;
            end
        else 
            R(j) = 1+Kf;
        end
        C(j) = C_alt(j)-dt*v/(x(j)-x(j-1))*(C_alt(j)-C_alt(j-1))-dt*lambda*C_alt(j);
        C_r(j) = C_ralt(j)-dt*v/(R(j)*(x(j)-x(j-1)))*(C_ralt(j)-C_ralt(j-1))-dt*lambda*C_ralt(j);
        if C(j) < 0
            C(j) = 0;
        end
        if C_r(j) < 0
            C_r(j) = 0;
        end
    end
    
    C_alt = C;
    C_ralt = C_r;
    
    % Plotting
     plot(x,C,'b-','linewidth',2);
    hold on,
    plot(x,C_r,'r-','linewidth',2);
    title([num2str(time/3600) ' h']);
    hold off;
    xlabel('x [m]');
    ylabel('C [kg/m^3]');
    axis([0 x(length(x)) 0 Cmax]);
    M(itime) = getframe;
    time = time+dt;
       itime = itime+1;
end


