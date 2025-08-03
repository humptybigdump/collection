%% TTP-Übung: Teil Wärmeübertragung - Strömungssieden in vertikalen Rohren
% Auslegung eines Naturumlaufverdampfers
% Datum: 17.06.2020
% Autor: Tim Laube

clear;
close all;
clc;

%% Vorgaben

% Stoffeigenschaften und Betriebsdaten
p = 4.82*10^5;              % mittlerer Druck [Pa]
p_c = 49.6*10^5;            % kritischer Druck [Pa]
T_S = 173.35+273.15;        % Siedetemperatur [K] von n-Butanol bei p
eta_L = 196.4*10^-6;        % Viskosität der Flüssigphase [kg/(ms)]
eta_G = 11.4*10^-6;         % Viskosität der Gasphase [kg/(ms)]
lambda_L = 108.7*10^-3;     % Wärmeleitfähigkeit der Flüssigphase [W/(mK)]
lambda_G = 28.2*10^-3;      % Wärmeleitfähigkeit der Gasphase [W/(mK)]
c_pL = 4.42*10^3;           % Wärmekapazität der Flüssigphase [J/(kgK)]
c_pG = 2.14*10^3;           % Wärmekapazität der Gasphase [J/(kgK)]
rho_L = 640;                % Dichte der Flüssigphase [kg/m3]
rho_G = 12.5;               % Dichte der Gasphase [kg/m3]
sigma = 1.23*10^-2;         % Oberflächenspannung [kg/s2]

alpha_0 = 2580;             % tabellierter Wert
C_F = 1.151;                % tabellierter Wert
q_0 = 20000;                % tabellierte Normwärmestromdichte [W/(m2K)]
d_0 = 10^-2;
R_a0 = 10^-6;               % Bezugsmittenrauwert [m]
r_kr = 0.3*10^-6;           % kr. Blasenradius [m]
F_mx = 1;                   % Faktor zur Berechnung alpha_B

q_punkt = 60000;            % Wärmestromdichte [W/m2]
m_punkt = 250;              % Massenstromdichte [kg/(m2s)]
x_punkt_a = 0.3;            % Austrittsdampfgehalt
Delta_x_punkt = 0.005;       % Schrittweite für den Dampfgehalt

% Abmaße Rohrbündel
n_R = 20;                   % Anzahl Rohre
d_a = 20*10^-3;             % Außendurchmesser Rohre [m]
s = 1.5*10^-3;              % Wandstärke Rohre [m]
R_a = 2*10^-6;              % Wandrauigkeit [m]

%% Aufgabenteil a) - Länge des Rohrbündels
d = d_a - 2*s;            % Rohrinnendurchmesser [m]
x_punkt_e = 0;
Delta_h_v = 509.7*10^3;     % Verdampfungsenthalpie n-Butanol [J/kg] Stoffwerttabelle

% aus Aufstellung und Umformung der Energiebilanz
% Ergebnis: Länge des Rohrbündels in m
L = m_punkt*d*(x_punkt_a-x_punkt_e)*Delta_h_v/(4*q_punkt);


%% Aufgabenteil c) und b)
Re_L0 = m_punkt*d/eta_L;
Re_G0 = m_punkt*d/eta_G;

Pr_L = eta_L*c_pL/lambda_L;
Pr_G = eta_G*c_pG/lambda_G;

z = 0;
Delta_L = m_punkt*d*(Delta_x_punkt)*Delta_h_v/(4*q_punkt);

i = 1;
while z<=L

            % Berechnung Nusselt-Zahl für Strömung mit hydrodynamischem Einlauf und der
            % therm. RB "konstante Wärmestromdichte"
            x_punkt(i) = x_punkt_e+(i-1)*Delta_x_punkt;
            
            if d/z(i)>1
                Nu_lam_L = 0.455*Pr_L^(1/3)*sqrt(Re_L0*1);
                Nu_lam_G = 0.455*Pr_G^(1/3)*sqrt(Re_G0*1);
            else
                Nu_lam_L = 0.455*Pr_L^(1/3)*sqrt(Re_L0*d/z(i));
                Nu_lam_G = 0.455*Pr_G^(1/3)*sqrt(Re_G0*d/z(i));
            end

            Xi_L = (1.82*log10(Re_L0)-1.64)^-2;
            Nu_8_L = (Xi_L/8)*(Re_L0-1000)*Pr_L/(1+12.7*sqrt(Xi_L/8)*(Pr_L^(2/3)-1));

            Xi_G = (1.82*log10(Re_G0)-1.64)^-2;
            Nu_8_G = (Xi_G/8)*(Re_G0-1000)*Pr_G/(1+12.7*sqrt(Xi_G/8)*(Pr_G^(2/3)-1));

            if d/z(i)>=1
                Nu_turb_L = 4/3*Nu_8_L;
                Nu_turb_G = 4/3*Nu_8_G;
            else
                Nu_turb_L = Nu_8_L*(1+1/3*(d/z(i))^(2/3));
                Nu_turb_G = Nu_8_G*(1+1/3*(d/z(i))^(2/3));
            end

            Nu_L = max(Nu_lam_L,Nu_turb_L);
            Nu_G = max(Nu_lam_G,Nu_turb_G);

            % Berechnung der Wärmeübergangskoeffizienten
            alpha_L0(i) = Nu_L*lambda_L/d;
            alpha_G0(i) = Nu_G*lambda_G/d;

            alpha_K(i) = alpha_L0(i) * ((1-x_punkt(i))^0.01*((1-x_punkt(i))^1.5+1.9*x_punkt(i)^0.6*(rho_L/rho_G)^0.35)^-2.2 + x_punkt(i)^0.01*(alpha_G0/alpha_L0*(1+8*(1-x_punkt(i))^0.7*(rho_L/rho_G)^0.67))^-2)^-0.5;
           
            % Berechnung der Wärmestromdichte am Blasenentstehungspunkt
            % (Aufgabenteil b)
            q_punkt_onb = (2*sigma*T_S*alpha_L0(i))/(r_kr*rho_G*Delta_h_v);
            
            if q_punkt < q_punkt_onb
                alpha_B(i) = 0;
            else
                p_r = p/p_c;
                F_p = 2.816 * p_r^0.45+(3.4+1.7/(1-p_r^7))*p_r^3.7;
                n = 0.8-0.1*10^(0.76*p_r);
                F_q = (q_punkt/q_0)^n;
                F_d = (d_0/d)^0.4;
                F_W = (R_a/R_a0)^0.133;

                alpha_B(i) = alpha_0*C_F*F_p*F_q*F_d*F_W*F_mx;
            end

                alpha(i) = (alpha_K(i)^3+alpha_B(i)^3)^(1/3);

                z(i+1) = z(i) + Delta_L;
                i = i + 1;
            
end

z(i) = [];
figure()
plot(z,alpha_L0,z,alpha_G0)
xlabel('z / m')
ylabel('alpha / (W/(m^2K))')
legend('alpha_{L0}','alpha_{G0}','Location','Northeast')
figure()
plot(z,alpha_K,z,alpha_B,z,alpha)
xlabel('z / m')
ylabel('alpha / (W/(m^2K))')
legend('alpha_K','alpha_B','alpha','Location','Southeast')

