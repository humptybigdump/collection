%% MODELING AND SIMULATION - POOL EXERCISE 4-1
% Models with distributed parameters - FDM
%
% Source: Joel H. Ferziger, Milovan Peric:
% Computational Methods for Fluid Dynamics, Springer, 2002
clearvars; close all; clc

%% parameters
% problem specific parameters
L = 1.0;                                    % length of the pipe
rho = 1.0;                                  % mass density
v = 2.0;                                    % convection velocity
Gamma = 1.0;                                % diffusive coefficient

% numerical parameters
delta_t = 0.0001;                           % time step
t_max = 2.0;                                % maximum simulation time
n_x = 11;                                   % number of nodes in the x-direction
iterations_max = ceil(t_max/delta_t)+1;     % maximum number of iterations
delta_x = L/(n_x-1);                        % cell size

% output parameters
f_output = 5;                               % frequency of the graphical output

%% initialization of necessary vectors and values
phi_bds = zeros(n_x,iterations_max);
phi_cds = zeros(n_x,iterations_max);
dphi_dt_bds = zeros(n_x,1);
dphi_dt_cds = zeros(n_x,1);

t = 0;  % time

% Spatial position of the grid nodes
x_disc = linspace(0, L, n_x);

%% boundary conditions
phi_bds(1) = 0;
phi_bds(n_x,:) = 1;
phi_cds(1) = 0;
phi_cds(n_x,:) = 1;

%% Peclet-number
Pe = rho*v*L/Gamma;

%% exact solution
syms x
phi_exact = (exp(x*Pe) - 1)/(exp(Pe) - 1);

%% CFL-number
CFL = 'fill in!'; % CALCULATE THE CFL NUMBER!
d = 'fill in!'; % CALCULATE THE TIMESTEP RELATIVE TO THE DIFFUSION TIME!

%% Graphical output of the solution
figure;
hold on

p_analytic = fplot(phi_exact, [0 L], 'b');
p_FDM = plot(x_disc,phi_bds(:,1),'-ro',x_disc,phi_cds(:,1),'-kx');
p_FDM(1).XDataSource = 'x_disc';
p_FDM(1).YDataSource = 'phi_bds(:,iteration)';
p_FDM(2).XDataSource = 'x_disc';
p_FDM(2).YDataSource = 'phi_cds(:,iteration)';

axis([0 L -0.2 1])

indicators = append('Pe:  ', num2str(Pe), newline, ...
    'CFL: ',num2str(CFL), newline, ...
    'd:   ',num2str(d), newline, ...
    't:   ',num2str(t));
annot = annotation('textbox',[0.15, 0.75, 0.1, 0.1],'string', indicators);
title('Concentration \phi');
xlabel('x in m');
ylabel('\phi');
legend('\phi_{exact}','\phi_{bds}','\phi_{cds}','Location','Eastoutside');

%% time loop
for iteration = 2:iterations_max
    
    for node = 2:n_x-1
        %% Calculating the time derivative
        % Convective term using BDS and CDS
        conv_bds = 'fill in!'; % IMPLEMENT THE CONVECTIVE TERM WITH BDS HERE!
        conv_cds = 'fill in!'; % IMPLEMENT THE CONVECTIVE TERM WITH CDS HERE!
        
        % Diffusive term using CDS
        diff_bds = 'fill in!'; % IMPLEMENT THE DIFFUSIVE TERM HERE USING phi_bds!
        diff_cds = 'fill in!'; % IMPLEMENT THE DIFFUSIVE TERM HERE USING phi_cds!
        
        % Time derivative
        dphi_dt_bds(node) = diff_bds-conv_bds;
        dphi_dt_cds(node) = diff_cds-conv_cds;
        
        %% Integration of phi for the next time step using the explicit
        % Euler scheme
        phi_bds(node, iteration) = 'fill in!'; % IMPLEMENT THE INTEGRATION HERE USING phi_bds!
        phi_cds(node, iteration) = 'fill in!'; % IMPLEMENT THE INTEGRATION HERE USING phi_cds!
    end
    
    % Simulation time
    t = t + delta_t;
    
    % refresh plot according to the specified frequency
    if mod(iteration, f_output)==0 || t>=t_max
        indicators = append('Pe:  ', num2str(Pe), newline, ...
            'CFL: ',num2str(CFL), newline, ...
            'd:   ',num2str(d), newline, ...
            't:   ',num2str(t));
        set(annot, 'String', indicators)
        refreshdata
        drawnow
    end
    
end % End of time loop