% Ads.2 - Gegenstrom Adsorption

clc;

%% User Input

Fdot      = 1000; % kg/h


w_F       = 0.08; % kg/kg 
W_S       = 0.0;    % kg/kg_adsoprtion medium

w_R       = 0.0001 * w_F; % kg/kg after final stage

% Freundlich Parameter
m = 2; 
n = 4; % Concentration of Monolayer & and limiting loading

%----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%% Calculating initially necessary values

% Transforming the flow rates to inert flows 
F_inert = Fdot * (1.0 - w_F);

% Transforming the molar fraction to molar loadings
W_F = w_F / (1.0 - w_F);
W_R = w_R / (1.0 - w_R);


%% Starting Stage determination

% Defining Isotherms for the solver
Freundlich  = @(X) m .* X.^n;
Freundlich_rev = @(Y) (Y ./ m).^(1/n);

% Initializing vectors necessary for ploting the stages of the process

W_x = linspace(0, 1, 200);
W_y = Freundlich(W_x);

% Starting while loop to determine necessary number of steps

S_inert = newton(@(S)Ads3_func(F_inert,S,Freundlich_rev,W_F,W_S,W_R),w_F,1e-12,500,10);


%% Calculating the concentration of the stages for the plot

W_flIn_vec = zeros(1,3);
W_flOut_vec = zeros(1,3);
W_AMIn_vec = zeros(1,3);
W_AMOut_vec = zeros(1,3);

% Using the mass balance around the first stage and Isotherm 
% to calculate the concentration around second stage

W_flOut_vec(1) = W_R;
W_AMOut_vec(1) = Freundlich_rev(W_R);
W_flIn_vec(1) = (F_inert.*W_R - S_inert .* W_S + S_inert .* W_AMOut_vec(1))./ F_inert;
W_AMIn_vec(1) = W_S;

% Using the mass balance around the second stage and Isotherm 
% to calculate the concentration around third stage


W_flOut_vec(2) = W_flIn_vec(1);
W_AMOut_vec(2) = Freundlich_rev(W_flIn_vec(1));
W_flIn_vec(2) = (F_inert.*W_flIn_vec(1) - S_inert .* W_AMOut_vec(1) + S_inert .* W_AMOut_vec(2))./ F_inert;
W_AMIn_vec(2) = W_AMOut_vec(1);

% Using the mass balance around the third stage and Isotherm 
% to calculate the concentration of the feed

W_flOut_vec(3) = W_flIn_vec(2);
W_AMOut_vec(3) = Freundlich_rev(W_flIn_vec(2));
W_flIn_vec(3) = (F_inert.*W_flIn_vec(2) - S_inert .* W_AMOut_vec(2) + S_inert .* W_AMOut_vec(3))./ F_inert;
W_AMIn_vec(3) = W_AMOut_vec(2);


%% ----------------------------------------------------------------------------------------------------------------------------------------------------------
% Plot

% General Variables for plotting

fsize = 20; %fontsize 

% Plotting the Isotherm
Fig1 = figure(1);

plot(W_x, W_y, 'color','b')
set(gca,'fontsize',fsize-8)
xlabel('$Beladung Adsorptionsmittel W_A$','Interpreter','latex','fontsize',fsize+4)
ylabel('$Beladung Abwasser W_W$','Interpreter','latex','fontsize',fsize+4)
xlim([0.0 1])
ylim([0.0 2])
title('Diagram of Loading','Interpreter','latex','fontsize',fsize+3)
grid on

hold off
% Plotting the theoretical stages
if Plotting_stages

Fig2 = figure(2);
hold on
plot(W_x, W_y, 'color','b')
set(gca,'fontsize',fsize-8)
ylabel('$Beladung in Gas W_W$','Interpreter','latex','fontsize',fsize)
xlabel('$Beladung in Adsorptionsmittel W_A$','Interpreter','latex','fontsize',fsize)
xlim([0.0 0.45]);
ylim([0.0 0.1]);


title('Diagram von Gegenstrom Prozess','Interpreter','latex','fontsize',fsize+3)


    txt_Nth = ['Inerter Adsorptionsmittel Strom: S_{inert}= ' num2str(S_inert)];
    text(0.001,0.09,txt_Nth,'FontSize',14);
    
    % Plotting the operational line
    plot( [W_S W_AMOut_vec(3)],[W_R W_F],'color','r');

    for i = 1:3

        % horizontal line
        plot( [W_AMIn_vec(i) W_AMOut_vec(i)], [W_flOut_vec(i) W_flOut_vec(i)], 'color','k')
        
        % vertical line
        plot( [W_AMOut_vec(i) W_AMOut_vec(i)], [W_flOut_vec(i) W_flIn_vec(i)], 'color', 'k')


    end

end
hold off