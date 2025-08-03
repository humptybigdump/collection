% Rkt.3 - Multicomponent Rectification

clc;

%----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%% User Input

Fdot      = 100; % kmol/h
split_E_3 = 0.9;
split_B_4 = 0.85;

x_F_i    = [0.05 0.15 0.2 0.2 0.35 0.05]'; % [mol/mol]
n_comp   = numel(x_F_i);
alpha_i4 = [1.80 1.59 1.59 1.00 0.89 0.79]';

%----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
%% Start of calculation
% The LKC is given as component 3
% The HKC is given as component 4

% Creating an array for the distillate composition and streams
x_E_i  = zeros(size(x_F_i));
Edot_i = x_E_i;

% Creating an array for the bottom product composition and streams
x_B_i = x_E_i;
Bdot_i = x_E_i;

%% Composition at the top and the bottom of the column according to Hengstebeck
% Composition of the feed, distillate and bottom product 

Fdot_i   = x_F_i * Fdot;
Edot_i(3) = Fdot_i(3) * split_E_3;
Bdot_i(4) = Fdot_i(4) * split_B_4;

Bdot_i(3) = Fdot_i(3) - Edot_i(3);  % LKC
Edot_i(4) = Fdot_i(4) - Bdot_i(4); % HKC


% Calculation of the two parameters A and C for the Hengstebeck equation

A_heng = log10(Edot_i(4)/Bdot_i(4));
C_heng = ( log10(Edot_i(3)/Bdot_i(3)) - A_heng ) / log10(alpha_i4(3));

% Calculation of the component streams and concentrations in distillate and bottom product

for j = 1:n_comp
    
    Bdot_i(j) = Fdot_i(j) / ( 10^( A_heng + C_heng * log10(alpha_i4(j)) ) + 1 );
    Edot_i(j) = Fdot_i(j) - Bdot_i(j);

end

Bdot = sum(Bdot_i);
Edot = sum(Edot_i);

for j = 1:n_comp
    
    x_B_i(j) = Bdot_i(j)/Bdot;
    x_E_i(j) = Edot_i(j)/Edot;

end


%% Minimal number of stages according to Fenske

Nthmin_fensk = log( (x_E_i(3)/x_B_i(3)) * (x_B_i(4)/x_E_i(4)) ) / log( alpha_i4(3)/alpha_i4(4)) - 1; 


%% Minimal reflux ratio according to Underwood and the effective reflux ratio, if veff = 1.5*vmin
% Iterative calculation with Newton's algorithm

theta_init = mean([alpha_i4(3) alpha_i4(4)]); % alpha_HKC <= theta <= alpha_LKC

fun = @(theta) sum(alpha_i4 .* x_F_i ./ (alpha_i4 - theta));

theta = newton(fun,theta_init);

vmin = sum( alpha_i4.*x_E_i./(alpha_i4 - theta) ) - 1;
v    = vmin * 1.5;


%% Number of theoretical stages using the diagram of Gilliland

% Gilliland correlation by Molokanov for the diagram
X_gilli_dia = linspace(0,1,1000);
Y_gilli_dia = 1 - exp(((1 + 54.4.*X_gilli_dia)./(11 + 117.24.*X_gilli_dia)).*((X_gilli_dia - 1)./sqrt(X_gilli_dia)));

X_val = (v - vmin)./(v + 1);
Y_val = 1 - exp(((1 + 54.4*X_val)./(11 + 117.24*X_val))*((X_val - 1)/sqrt(X_val)));

Nth_gilli = (Nthmin_fensk + Y_val)/(1 - Y_val);

%% Optimal feed stage according to Kirkbride 

Nth_ratio = ( ( x_F_i(4)/x_F_i(3))*(x_B_i(3)/x_E_i(4) )^2 * ( Bdot / Edot ) )^0.206;

Nth_AT = Nth_gilli / (1 + Nth_ratio);

Nth_F_optim = floor(Nth_AT);


%% ----------------------------------------------------------------------------------------------------------------------------------------------------------
% Plot

% General Variables for plotting

scrsz = get(0,'ScreenSize');
plot_window_size = [scrsz(3)/4 scrsz(4)/4 scrsz(3)/2 scrsz(4)/2];  % [left top right bottom]
fsize = 20; %fontsize 

Fig1 = figure(1);
set(Fig1,'Position',plot_window_size)
loglog(X_gilli_dia,Y_gilli_dia,'LineWidth',1);
hold on
loglog([X_val X_val],[0.01 Y_val],'-r','LineWidth',1.5);
loglog([0.01 X_val],[Y_val Y_val],'-r','LineWidth',1.5);
loglog([0.1 0.1],[0.01 2],'--k','LineWidth',1);
loglog([0.33 0.33],[0.01 2],'--k','LineWidth',1);
text(0.15,0.12,'EOR','Interpreter','latex','FontSize',fsize-4)
annotation('textarrow',[0.52 0.718],[0.53 0.53],'LineWidth',0.75)
annotation('textarrow',[0.715 0.518],[0.53 0.53],'LineWidth',0.75)
hold off
set(gca,'fontsize',fsize-8)
xlabel('$\frac{v - v_{min}}{v + 1}$','Interpreter','latex','fontsize',fsize+4)
ylabel('$\frac{N_{th} - N_{th,min}}{N_{th} + 1}$','Interpreter','latex','fontsize',fsize+4)
xlim([0.01 1])
ylim([0.01 1])
xticks([0.01 0.02 0.04 0.06 0.1 0.2 0.4 0.6 1.0])
yticks([0.01 0.02 0.04 0.06 0.1 0.2 0.4 0.6 1.0])
title('Diagram according to Gilliland','Interpreter','latex','fontsize',fsize+3)
grid on
