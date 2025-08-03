close all
clear all
clc
keyboard

%% Parameter initialisieren, Verwendung eines "struct" mit der Bezeichnung "Parameter"

Parameter.A_Zelle = 200; % Elektrodenfläche in cm²

Parameter.d_separator = 20e-6; % Dicke des Separators in m
Parameter.sigma_electrolyte = 1; % Leitfähigkeit des Elektrolyten in S/m, Einfluss des Separators wird im Folgenden vernachlässigt

Parameter.tau = 5; % Tortuosität der Kathodenschicht
Parameter.epsilon = 0.2; % Porosität der Kathodenschicht

Parameter.R_CT = 1; % Ladungstransferwiderstand in Ohm
Parameter.Q_CT = 11e-3; % Doppelschichtkapazität in F
Parameter.n_CT = 0.8; % Exponent

Parameter.R_Diff = 1.5; % Diffusionswiderstand in Ohm
Parameter.tau_Diff = 800; % Zeitkonstante Diffusion in sec
Parameter.n_Diff = 0.45; % Exponent
Parameter.C_diff = 100; % differentielle Kapazität in F

Parameter.R_CC = 0.25; % Kontaktwiderstand in Ohm
Parameter.Q_CC = 1e-5; % Doppelschichtkapazität in F
Parameter.n_CC = 0.95; % Exponent

Parameter.d_electrode = 50e-6; % Dicke der Elektrodenschicht in m
Parameter.sigma_electrode = 100; % elektronische Leitfähigkeit der Elektrode in S/m
Parameter.d_collector = 15e-6; % Dicke des Stromableiters in m
Parameter.sigma_collector = 37e6; % elektronische Leitfähigkeit des Stromableiters in S/m
Parameter.L_Al = 1e-7; % Induktivität Stromableiter in H

Parameter.f = logspace(-4,6,1000); % Frequenzvektor, muss später mit 2*pi() multipliziert werden

keyboard

%% Berechnung der Einzelimpedanzen

R_electrolyte = 1/Parameter.sigma_electrolyte*Parameter.d_separator/(Parameter.A_Zelle/1e4); % Elektrolytwiderstand in Ohm, Umrechnung cm²->m²

RCT = Parameter.R_CT; % Ladungstransferwiderstand in Ohm
Z_CT = RCT./(1+(1i*2*pi()*Parameter.f*RCT*Parameter.Q_CT).^Parameter.n_CT); % Ladungstransferimpedanz in Ohm
Z_CT_TLM = Z_CT * Parameter.d_electrode; % Ladungstransferimpedanz bezogen auf die Elektrodendicke in Ohm*m

RDiff = Parameter.R_Diff; % Diffusionswiderstand in Ohm
Z_Diff = RDiff*tanh((1i*2*pi()*Parameter.f*Parameter.tau_Diff).^Parameter.n_Diff) ./ ...
         (1i*2*pi()*Parameter.f*Parameter.tau_Diff).^Parameter.n_Diff + 1./(1i*2*pi()*Parameter.f*Parameter.C_diff); 
         % Diffusionsimpedanz in Ohm (Warburg Impedanz + diff. Kapazität)
Z_Diff_TLM = Z_Diff * Parameter.d_electrode; % Diffusionsimpedanz bezogen auf die Elektrodendicke in Ohm*m

X1 = 1/Parameter.sigma_electrolyte*1/(Parameter.A_Zelle/1e4)*Parameter.tau/Parameter.epsilon; % ionischer Pfad, Umrechnung cm²->m²
X2 = 1/Parameter.sigma_electrode*1/(Parameter.A_Zelle/1e4); % elektronischer Pfad, Umrechnung cm²->m²

kappa = ((Z_CT_TLM+Z_Diff_TLM)/(X1+X2)).^0.5;
Z_TLM = ((X1*X2)/(X1+X2))*(Parameter.d_electrode+2*kappa./sinh(Parameter.d_electrode./kappa)) + ...
        kappa*((X1^2+X2^2)/(X1+X2)).*coth(Parameter.d_electrode./kappa);

RCC = Parameter.R_CC;
Z_CC = RCC./(1+(1i*2*pi()*Parameter.f*RCC*Parameter.Q_CC).^Parameter.n_CC);

Z_L = 1i*2*pi().*Parameter.f*Parameter.L_Al;

%% Gesamtimpedanz berechnen

Z_Gesamt = R_electrolyte + Z_CT + Z_Diff + Z_CC + Z_L;
Z_Gesamt_TLM = R_electrolyte + Z_TLM + Z_CC + Z_L;

keyboard

%% Impedanzen plotten

figure(1);
plot(Z_Gesamt, 'o')
hold on
plot(Z_Gesamt_TLM, 'r')
axis equal;
set(gca, 'YDir', 'reverse');
grid on;
xlabel('Re(Z)');
ylabel('Im(Z)');
legend('einfaches ESB','TLM');
title('Vergleich einfaches Modell und TLM');

keyboard

%% Zeilenvektor Schichtdickenvariation

L_var = [1 2 4 6 8 10 12 14 20 30 40 50 60];

%% Berechnung der Einzelimpedanzen, die von der Schichtdicke abhängig sind
% da die Variablen ZCT, ZDiff und RC schon zuvor als double-Vektor definiert wurden, 
% können diese nun nicht gleichbenamt als cell-Format verwendet werden.
% Daher müssen die erwähnten Variablen unter neuem Namen angelegt werden.
for i=1:length(L_var)
    RCT(i) = Parameter.R_CT/L_var(i);
    QCT(i) = Parameter.Q_CT*L_var(i);
    Z_CT_var{i} = RCT(i)./(1+(1i*2*pi()*Parameter.f*RCT(i)*QCT(i)).^Parameter.n_CT); % Ladungstransferimpedanz in Ohm
    Z_CT_var_TLM{i} = Z_CT_var{i} * Parameter.d_electrode * L_var(i);

    RDiff(i) = Parameter.R_Diff/L_var(i);
    Z_Diff_var{i} = RDiff(i)*tanh((1i*2*pi()*Parameter.f*Parameter.tau_Diff).^Parameter.n_Diff) ./ ...
                    (1i*2*pi()*Parameter.f*Parameter.tau_Diff).^Parameter.n_Diff + 1./(1i*2*pi()*Parameter.f*Parameter.C_diff*L_var(i));
    Z_Diff_var_TLM{i}= Z_Diff_var{i} * Parameter.d_electrode * L_var(i);

    kappa_var{i} = ((Z_CT_var_TLM{i}+Z_Diff_var_TLM{i})/(X1+X2)).^0.5;
    Z_TLM_var{i} = ((X1*X2)/(X1+X2))*(Parameter.d_electrode*L_var(i) + 2*kappa_var{i}./sinh(Parameter.d_electrode*L_var(i)./kappa_var{i})) ...
                   + ((X1^2+X2^2)/(X1+X2))*kappa_var{i}.*coth(Parameter.d_electrode*L_var(i)./kappa_var{i});
end

%% Berechnung Gesamtimpedanz in Abhängigkeit der Schichtdicke (Ableiter ist hier vernachlässigt)

for i=1:length(L_var)
    Z{i} = R_electrolyte + Z_TLM_var{i} + Z_CC + Z_L;
    Re_end(i) = real(Z{i}(1));
end

keyboard

%% Plot Gesamtimpedanz in Abhängigkeit der Schichtdicke
liste = cell(length(L_var),1);
cmap = colormap('cool'); % Farbverlauf festlegen
figure(2);
for i=1:length(L_var)
    plot(Z{i},'color',cmap(4*i,:));
    liste{i} = ['Schichtdicke * ' int2str(L_var(i))];
    hold all;
end

axis equal;
set(gca, 'YDir', 'reverse');
grid on;
xlabel('Re(Z)');
ylabel('Im(Z)');
legend(liste);
title('Gesamtimpedanzmodell Schichtdickenvariation')

keyboard

figure(3);
plot(L_var,Re_end, 'o');
hold on;
grid on;
xlabel('Schichtdicke * d_electrode [µm]');
ylabel('Re(Z) [\Omega]');
title('Gesamtimpedanzmodell Abhängigkeit von der Schichtdicke')

keyboard

%% Optimierung

fhandle = @(L_opt)optimize(Parameter, L_opt);
result = fminsearch(fhandle,10);

Text = ['Das Minimum liegt bei L = ', num2str(result), ' d_electrode'];
disp(Text);
