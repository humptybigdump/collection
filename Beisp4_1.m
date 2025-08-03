% Beisp4_1
% Druckgradient und Gasvolumenanteil einer Ringströmung Wasser/Luft
% berechnet mit dem Mischungsmodell
% T. Schulenberg, Nov. 2020
d=0.1;                         % Rohrdurchmesser [m]
UG=50;                         % Volumenstromdichte Luft [m/s]
UL=0.3;                        % Volumenstromdichte Wasser [m/s]
rhoG=1.1894;                   % Dichte Luft [kg/m3]
rhoL=998;                      % Dichte Wasser [kg/m3]
viscG=18.215E-6;               % Zähigkeit Luft [Pa s]
viscL=1.0053E-3;               % Zähigkeit Wasser [Pa s]
dH=d;                          % hydraulischer Durchmesser [m]
A=pi/4*d^2;                    % Rohrquerschnitt [m]
sigma=0.073;                   % Oberflächenspannung [N/m]
rough=1E-6;                    % Oberflächenrauigkeit [m] 
MG=UG*rhoG*A;                  % Massenstrom Luft [kg/s]
ML=UL*rhoL*A;                  % Massenstrom Wasser [kg/s]
M=MG+ML;                       % Gesamtmassenstrom [kg/s]
x=MG/M;                        % Gasmassenanteil
p=1E5;                         % Druck [Pa]
phi=pi/2;                      % Anstiegswinkel

% a) berechnet nach Friedel sowie Premoli et al.
dpdzR1=PLCorr(1,M,x,A,dH,rough,rhoL,rhoG,viscL,viscG,sigma);
eps1=VoidCorr(2,M,x,p,rhoG,rhoL,viscL,sigma,dH,A,phi);
rhoM=eps1*rhoG+(1-eps1)*rhoL;
dpdz1=dpdzR1+rhoM*9.81*sin(phi);

% b) berechnet nach Müller-Steinhagen & Heck sowie Woldesemayat & Ghajar
dpdzR2=PLCorr(2,M,x,A,dH,rough,rhoL,rhoG,viscL,viscG,sigma);
eps2=VoidCorr(3,M,x,p,rhoG,rhoL,viscL,sigma,dH,A,phi);
rhoM=eps2*rhoG+(1-eps2)*rhoL;
dpdz2=dpdzR2+rhoM*9.81*sin(phi);