% Beispiel 2.1
% Berechnung einer Ringströmung mit dem 6-Gleichungsmodell
% T. Schulenberg Nov 2020
d=0.1;                         % Rohrdurchmesser [m]
UG=50;                         % Volumenstromdichte Luft [m/s]
UL=0.3;                        % Volumenstromdichte Wasser [m/s]
rhoG=1.1894;                   % Dichte Luft [kg/m3]
rhoL=998;                      % Dichte Wasser [kg/m3]
viscL=1.0053E-3;               % Zähigkeit Wasser [Pa s]
P=pi*d;                        % Rohrumfang [m]
A=pi/4*d^2;                    % Rohrquerschnitt [m]
ReL=UL*d*rhoL/viscL;           % Reynoldszahl Wasser
fL=0.3164/4/ReL^0.25;          % Widerstandsbeiwert Wasser

% Nullstellensuche durch Bisektion:
e(1)=0.5;                      % untere Grenze des Void-Intervals
e(2)=0.999;                    % obere Grenze des Void-Intervals
dp(3)=1000.;
kmin=1;
while abs(dp(3))>1.
    e(3)=(e(1)+e(2))/2.;
    for k=kmin:3
        tauWL=fL*rhoL/2*UL^2/(1-e(k))^2;
        tauiG=-0.005*(1+300/4*(1-e(k)))*rhoG/2*(UG/e(k)-UL/(1-e(k)))^2;
        dpdzG=tauiG/e(k)*P/A-rhoG*9.81;
        dpdzL=-tauiG/(1-e(k))*P/A-tauWL/(1-e(k))*P/A-rhoL*9.81;
        dp(k)=dpdzG-dpdzL;
    end
    if dp(1)*dp(3)<0
        e(2)=e(3);
        dp(2)=dp(3);
    else
        e(1)=e(3);
        dp(1)=dp(3);
    end
    kmin=3;
end

% Ergebnisse:
eps=e(3);                      % Gasvolumenanteil
dpdz=tauiG/eps*P/A-rhoG*9.81;  % Druckgradient [Pa/m]
delta=(1-eps)*d/4;             % Filmdicke [m]
uG=UG/eps;                     % mittlere Luftgeschwindigkeit [m/s]
uL=UL/(1-eps);                 % mittlere Wassergeschwindigkeit [m/s]
S=uG/uL;                       % Schlupf