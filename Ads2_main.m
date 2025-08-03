% Ads.2 - Kreuzstrom Adsorption

clc;

%----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%% User Input

Fdot      = 5000; % kg/h
Sdot      = 1000;  % kg/h

W_F       = 0.08; % kg/kg_waste water
W_S       = 0.0;    % kg/kg_adsoprtion medium

w_fluid_n       = 0.1*1e-6; % kg/kg after final stage

% Langmuir Parameter
c_T = 10000; 
X_mon_T = 0.02; % Concentration of Monolayer & and limiting loading

% Simulation settings

N_max = 1000;
Plotting_stages = true;

%----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%% Calculating initially necessary values

% Transforming the flow rates to inert flows 
F_inert = Fdot / (1.0 + W_F);
S_inert = Sdot / (1.0 + W_S);

% Transforming the molar fraction to molar loadings
W_fluid_n = w_fluid_n / (1.0 - w_fluid_n);

%% Starting Stage determination

% Initializing vectors necessary for ploting the stages of the process

W_flOut_vec = zeros(1,N_max);
W_flIn_vec = zeros(1,N_max);
W_AMOut_vec = zeros(1,N_max);

W_Fluid = linspace(0, 0.1, 20000);
W_AMOut = c_T .* W_Fluid ./(1.0 + c_T .* W_Fluid) .* X_mon_T;

N_stages = 1;
W_flOut_prev = W_F;
W_flOut_stage = W_F;

% Defining function for solver

Langmuir = @(W_W) c_T .* (W_W) ./(1.0 + c_T .* (W_W)) .* X_mon_T;
Langmuir_rev = @(W_A) (W_A) ./ (X_mon_T .* c_T - (W_A) .* c_T);




% Starting while loop to determine necessary number of steps
while ((W_flOut_stage > W_fluid_n) && (N_stages < N_max))
    
    % Defining the massbalance for the new stage
    m_balance = @(W) F_inert .* W_flOut_prev + S_inert .* W_S ...
                - F_inert .* W - S_inert .* Langmuir(W);

    % Solving the massbalance for a new retentate mass fraction
    W_flOut_stage = newton(m_balance, W_flOut_prev, 1e-12, 500, 1e-10);

    W_AMOut_stage = Langmuir(W_flOut_stage);
    
    W_flOut_vec(N_stages) = W_flOut_stage;
    W_flIn_vec(N_stages) = W_flOut_prev;
    W_AMOut_vec(N_stages) = W_AMOut_stage;
    
    N_stages = N_stages +1;
    W_flOut_prev = W_flOut_stage;
end 



% ----------------------------------------------------------------------------------------------------------------------------------------------------------
%%  Plot of Construction

% General Variables for plotting

fsize = 20; %fontsize 

% Plotting the isotherm curve
Fig1 = figure(1);
plot(W_Fluid, W_AMOut, 'color','b')
set(gca,'fontsize',fsize-8)
ylabel('$Beladung  Adsorptionsmittel  W_A$','Interpreter','latex','fontsize',fsize)
xlabel('$Beladung  Abwasser  W_W$','Interpreter','latex','fontsize',fsize)
ylim([0.0 0.03])
xlim([0.0 0.001])
title('Diagramm der Beladung','Interpreter','latex','fontsize',fsize+3)

% Plotting the theoretical stages
if Plotting_stages

Fig2 = figure(2);
hold on
plot(W_AMOut, W_Fluid, 'color','b')
set(gca,'fontsize',fsize-8)
ylabel('$Beladung  Abwasser  W_W$','Interpreter','latex','fontsize',fsize)
xlabel('$Beladung  Adsorptionsmittel  W_A$','Interpreter','latex','fontsize',fsize)
ylim([0.0 0.1]);
xlim([0.0 0.03]);


title('Diagram von Gegenstrom Prozess','Interpreter','latex','fontsize',fsize+3)


    txt_Nth = ['Number of theo. stages: N_{th}= ' num2str(N_stages)];
    text(0.001,0.09,txt_Nth,'FontSize',14);
    

    for i = 1:N_stages-1

        % diagonal line
        plot([W_S W_AMOut_vec(i)], [W_flIn_vec(i) W_flOut_vec(i)], 'color','k');
        % horizontal line
        plot( [W_S W_AMOut_vec(i)], [W_flOut_vec(i) W_flOut_vec(i)], 'color', [0.5 0.5 0.5], 'LineStyle',':')

    end

end
hold off


