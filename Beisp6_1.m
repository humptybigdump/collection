% Beisp6-1
% Blasensieden in Behältern
% T. Schulenberg, Nov. 2020
p=1;                             % Druck [bar]
qw=100;                          % Wandwärmestrom [kW/m2]
Csf=0.013;                       % Konstante der Rohsenow-gleichung
C=0.13;                          % Konstante der Kutadeladze-Gleichung
Tsat=XSteam('Tsat_p',p);         % Sättigungstemp. [°C]
rhoL=XSteam('rhoL_p',p);         % Dichte Flüssigkeit [kg/m3]
rhoG=XSteam('rhoV_p',p);         % Dichte Dampf [kg/m3]
sigma=XSteam('st_p',p);          % Oberflächenspannung [N/m]
lamdaL=XSteam('tcL_p',p);        % Wärmeleitfähigkeit [W/mK]
DhLG=XSteam('hV_p',p)-XSteam('hL_p',p);  % Verdampfungsenthalpie [kJ/kgK]
CpL=XSteam('CpL_p',p);           % spez. Wärme [kJ/kgK]
myL=XSteam('my_pT',p,Tsat-1);    % Zähigkeit Flüssigkeit [Pas]
PrL=myL*CpL*1000/lamdaL;         % Prandtl-Zahl Flüssigkeit
n=1;                             % Geometrieparameter
g=9.81;

% Wandtemperaturüberhitzung bei
% a) Einsetzen des Blasensiedens
DTsup1=(8*sigma*n*(Tsat+273.15)*(rhoL-rhoG)*qw ...
    /(lamdaL*DhLG*rhoL*rhoG))^0.5;

% b) stationärem, gesättigtem Blasensieden
DTsup2=Csf*DhLGe/CpL*(qw/(myL*DhLG)*(sigma/(g*(rhoL-rhoG)))^0.5)^0.33*PrL^1.7;

% C) Kritische Wärmestromdichte [kW/m2]
qwkrit=C*DhLG*rhoG^0.5*(sigma*g*(rhoL-rhoG))^0.25;



