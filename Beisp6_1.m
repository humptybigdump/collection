% Beisp6-1
% Blasensieden in Beh�ltern
% T. Schulenberg, Nov. 2020
p=1;                             % Druck [bar]
qw=100;                          % Wandw�rmestrom [kW/m2]
Csf=0.013;                       % Konstante der Rohsenow-gleichung
C=0.13;                          % Konstante der Kutadeladze-Gleichung
Tsat=XSteam('Tsat_p',p);         % S�ttigungstemp. [�C]
rhoL=XSteam('rhoL_p',p);         % Dichte Fl�ssigkeit [kg/m3]
rhoG=XSteam('rhoV_p',p);         % Dichte Dampf [kg/m3]
sigma=XSteam('st_p',p);          % Oberfl�chenspannung [N/m]
lamdaL=XSteam('tcL_p',p);        % W�rmeleitf�higkeit [W/mK]
DhLG=XSteam('hV_p',p)-XSteam('hL_p',p);  % Verdampfungsenthalpie [kJ/kgK]
CpL=XSteam('CpL_p',p);           % spez. W�rme [kJ/kgK]
myL=XSteam('my_pT',p,Tsat-1);    % Z�higkeit Fl�ssigkeit [Pas]
PrL=myL*CpL*1000/lamdaL;         % Prandtl-Zahl Fl�ssigkeit
n=1;                             % Geometrieparameter
g=9.81;

% Wandtemperatur�berhitzung bei
% a) Einsetzen des Blasensiedens
DTsup1=(8*sigma*n*(Tsat+273.15)*(rhoL-rhoG)*qw ...
    /(lamdaL*DhLG*rhoL*rhoG))^0.5;

% b) station�rem, ges�ttigtem Blasensieden
DTsup2=Csf*DhLGe/CpL*(qw/(myL*DhLG)*(sigma/(g*(rhoL-rhoG)))^0.5)^0.33*PrL^1.7;

% C) Kritische W�rmestromdichte [kW/m2]
qwkrit=C*DhLG*rhoG^0.5*(sigma*g*(rhoL-rhoG))^0.25;



