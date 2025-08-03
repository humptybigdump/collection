%% TTP-Übung: Teil Wärmeübertragung - Kondensation ruhender Dämpfe
% Auslegung eines Rohrbündelkondensators für NH3
% Datum: 24.06.2020
% Autor: Tim Laube

clear;
close all;
clc;

%% Vorgaben

% Stoffeigenschaften NH3
rho_L = 587.4;                      % Dichte der Flüssigphase [kg/m3]
rho_G = 10.46;                      % Dichte der Gasphase [kg/m3]
lambda_L = 0.451;                   % Wärmeleitfähigkeit [W/mK]
ny_L = 2.04*10^-7;                  % kinematische Viskosität [m2/s]
Pr_L = 1.29;                        % Prandtl-Zahl [-]
Delta_h_v = 1121.9*10^3;            % Verdampfungsenthalpie [J/kg]

g = 9.81;                           % Erdbeschleunigung [m/s2]
% Betriebsdaten
p = 13.51*10^5;                     % Behälterdruck [Pa]
T_K = 35;                           % Kondensationstemperatur °C von NH3 bei p = 11.67 bar
T_KWe = 18;                         % Kühlwasser - Eintrittstemperatur °C
T_KWa = 28;                         % Kühlwasser - Austrittstemperatur °C
M_punkt_K = 2000/60/60;             % Kondensatmassenstrom [kg/s]
alpha_i = 1670;                     % kältemittelseitiger (Innenseite) Wärmeübergangskoeffizient [W/(m2K)]
                                    % Berechnung über Nu-Korrelation für
                                    % einphasige Rohrströmungen

% Rohrbündel
lambda_R = 20;                      % Wärmeleitfähigkeit Edelstahl [W/mK]
d_a = 16*10^-3;                     % Außendurchmesser [m]
s = 1.5*10^-3;                      % Wandstärke [m]
d_i = d_a-2*s;
d_m = (d_a-d_i)/log(d_a/d_i);
n = 120;                            % Anzahl der Rohre
l_c = (ny_L^2/g)^(1/3);             % charakteristische Länge

L = 5;
L_alt = 10;
i = 0;

while abs(L-L_alt)>10^-10

    %% Berechnung alpha_F bei Kondensation an der Rohraußenfläche aus einer Einzelrohrbetrachtung
    M_punkt = M_punkt_K/n;
    Gamma_punkt = M_punkt/L;                                % Berieselungsdichte
    Re_F = Gamma_punkt/(ny_L*rho_L);
    Nu_F = 0.959*((1-rho_G/rho_L)/Re_F)^(1/3);              % 0.959 - Vorfaktor für horizontales Rohr
    alpha_F = Nu_F*lambda_L/l_c;

    %% Berechung k*A aus integraler Energiebilanz
    Q_punkt = M_punkt_K * Delta_h_v;
    Delta_T_LM = ((T_K-T_KWe)-(T_K-T_KWa))/log((T_K-T_KWe)/(T_K-T_KWa));         % mittlere logarithmische Temperaturdifferenz
    kA = Q_punkt/Delta_T_LM;
    k_a = (d_a/(alpha_i*d_i)+s*d_a/(lambda_R*d_m)+1/alpha_F)^-1;
    A_a = kA/k_a;
    L_alt = L;
    % A_i = n*pi*d_i*L;
    % A_m = n*pi*d_m*L;
    % aus A_a = n*pi*d_a*L;
    L = A_a/(n*pi*d_a);
    i = 1+i;
end



fprintf('Ergebnis: L =  %d m \n', L)
fprintf('Die Anzahl der Interationen beträgt: %d \n', i )





