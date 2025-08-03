%% TTP-�bung: Teil W�rme�bertragung - Kondensation im vertikalen Rohr
% Auslegung eines Rohrb�ndelkondensators f�r R134a
% Datum: 24.06.2020
% Autor: Tim Laube

clear;
close all;
clc;

%% Vorgaben

% Stoffeigenschaften R134a
rho_L = 1102.3;                     % Dichte der Fl�ssigphase [kg/m3]
rho_G = 66.3;                       % Dichte der Gasphase [kg/m3]
lambda_L = 0.071;                   % W�rmeleitf�higkeit der Fl�ssigphase [W/mK]
lambda_G = 0.017;                   % W�rmeleitf�higkeit der Gasphase [W/mK]
ny_L = 1.3*10^-7;                   % kinematische Viskosit�t der Fl�ssigphase [m2/s]
ny_G = 2.14*10^-7;                  % kinematische Viskosit�t der Gasphase [m2/s]
Pr_L = 3.14;                        % Prandtl-Zahl der Fl�ssigphase [-]
Delta_h_v = 151.800*10^3;           % Verdampfungsenthalpie [J/kg]

% Stoffeigenschaften K�hlwasser
rho_KW = 991.3;                     % Dichte [kg/m3]
lambda_KW = 0.634;                  % W�rmeleitf�higkeit [W/mK]
ny_KW = 6.29*10^-7;                 % kinematische Viskosit�t [m2/s]
cp_KW = 4178;                       % W�rmekapazit�t [J/kgK]

g = 9.81;                           % Erdbeschleunigung [m/s2]
% Betriebsdaten
T_K = 50+273.15;                    % Kondensationstemperatur [K] von R134a
T_KW_ein = 40+273.15;               % K�hlwasser - Eintrittstemperatur [K]
T_KW_aus = 45+273.15;               % K�hlwasser - Austrittstemperatur [K]
M_punkt_K = 0.5;                    % Kondensatmassenstrom R134a [kg/s]
M_punkt_KW = 3.633;                 % K�hlwassermassenstrom [kg/s]
alpha_a = 6500;                     % k�hlmittelseitiger (Au�enseite) W�rme�bergangskoeffizient [W/(m2K)]


% Rohrb�ndel
lambda_R = 372;                     % W�rmeleitf�higkeit Kupfer [W/mK]
d_a = 12*10^-3;                     % Au�endurchmesser [m]
s = 1*10^-3;                        % Wandst�rke [m]
d_i = d_a-2*s;
d_m = (d_a-d_i)/log(d_a/d_i);
n = 100;                            % Anzahl der Rohre [-]
l_c = (ny_L^2/g)^(1/3);             % charakteristische L�nge [-]


M_punkt_F = M_punkt_K/n;            % Filmmassenstrom [kg/s]
n_j = 100000;                            % Anzahl Diskretisierungsschritte [-]
Delta_x_punkt = 1/n_j;              % Schrittweite [-]

% aus Energiebilanz des kondensierenden R134a
Delta_Q_punkt = M_punkt_K*Delta_h_v*Delta_x_punkt; % W�rmeabfuhr bei Kondensation [W]

for j=1:1:n_j
    x_punkt(j) = 1 - (j - 0.5)*Delta_x_punkt;                                                           % Dampfmassenanteil f�r Abschnitt j [-]
    M_punkt_F_j(j) = M_punkt_F*(1 - x_punkt(j));                                                        % Filmmassenstrom f�r Abschnitt j [kg/s]
    Gamma_punkt_F(j) = M_punkt_F_j(j)/(pi*d_i);                                                         % Berieselungsdichte f�r Abschnitt j [kg/ms]
    Re_F(j) = Gamma_punkt_F(j)/(ny_L * rho_L);                                                          % Film-Reynoldszahl Abschnitt j [-]
    Nu_F_lam(j) = 0.693*((1 - (rho_G/rho_L))/Re_F(j))^(1/3);                                            % Nu-Zahl der laminaren Filmstr�mung [-]
    Nu_F_turb(j) = (0.0283 * Re_F(j)^(7/24) * Pr_L^(1/3)) / (1 + 9.66 * Re_F(j)^(-3/8) * Pr_L^(-1/6));  % Nu-Zahl der turbulenten Filmstr�mung [-]
    
    if Re_F(j) < 1                                                                                      % Korrekturfaktor zur Ber�cksichtigung der Welligkeit der Filmstr�mung [-]
        f_Well(j) = 1;
    else
        f_Well(j) = Re_F(j)^0.04;
    end
    
    f_mu = 1;                                                                                           % keine Abh�ngigkeit der Stoffwerte (in diesem Fall der Viskosit�t) von der Temperatur [-]
    Nu_F(j) = sqrt((f_Well(j) * Nu_F_lam(j))^2 + Nu_F_turb(j)^2) * f_mu;                                % Zusammenfassung zur Film-Nusselzahl [-]
    alpha_F(j) = Nu_F(j) * lambda_L /l_c;                                                               % W�rme�bergangskoeffizient des Kondensatfilms [W/m2K]
    k_a(j) = ((1/alpha_a) + (s*d_a/(lambda_R*d_m)) + (d_a/(alpha_F(j)*d_i)))^(-1);                      % k_a aus thermischer Reihenschaltung [W/m2K]
    
    if j==1                                                                                             % K�hlwasser Austrittstemperatur in Abschnitt j [K]
        T_KWa(j) = T_KW_aus;
    else
        T_KWa(j) = T_KWe(j-1);
    end
    
    T_KWe(j) = T_KWa(j) - Delta_Q_punkt/(M_punkt_KW*cp_KW);                                             % K�hlwasser Eintrittstemperatur in Abschnitt j [K]
    T_KW_mittl(j) = 0.5*(T_KWe(j) + T_KWa(j));                                                          % mittlere K�hlwassertemperatur in Abschnitt j [K]
    Delta_T_K_KW(j) = T_K - T_KW_mittl(j);                                                              % Temperaturdifferenz K�hlwasser-Kondensat [K]
    kA(j) = Delta_Q_punkt/Delta_T_K_KW(j);                                                              % kA aus Kinetik in Abschnitt j [W/K]
    Delta_A_a(j) = kA(j)/k_a(j);                                                                        % w�rme�bertragende Fl�che A_a in Abschnitt j [m2]
    Delta_L(j) = Delta_A_a(j)/(n*pi*d_a);                                                               % L�nge von Abschnitt j [m]
end
L = sum(Delta_L);                                                                                       % Gesamtl�nge durch Summation der Teill�ngen [m]

fprintf('Ergebnis: L =  %d m \n', L)






