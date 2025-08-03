% Beisp7_2
% Druckverlauf und Wandtemperatur eines Luftkondensatorrohrs
% T. Schulenberg, Jan. 2021

% Parameter
d=0.018;                   % Rohrinnendurchmesser [m]
phi=-pi/3;                 % Anstellwinkel
p0=0.1;                    % Druck am Eintritt [bar]
T0=XSteam('Tsat_p',p0);    % Temperatur am Eintritt [°C]
x0=0.9;
m=0.46;                    % Massenstromdichte [kg/m2s]
da=0.02;                   % Rohraußendurchmesser
Ta=15;                     % Außentemperatur [°C]
alphaa=25;                 % Wärmeübergangskoeff. außen [W/m2K]
eta=2E-6;                  % Wandrauhigkeit [m]
M=m*pi/4*d^2;              % Massenstrom [kg/s]

% Stoffdaten am Eintritt
rhoL=XSteam('rhoL_p',p0);         % Dichte Flüssigkeit [kg/m3]
rhoG=XSteam('rhoV_p',p0);         % Dichte Dampf [kg/m3]
viscL=XSteam('my_pT',p0,T0-1);    % Zähigkeit Flüssigkeit [Pa s]
viscG=XSteam('my_pT',p0,T0+1);    % Zähigkeit Dampf  [Pa s]
sigma=XSteam('st_p',p0);          % Oberflächenspanung [N/m]
hL=XSteam('hL_p',p0);             % Sättigungsenthalpie flüssig [kJ/kg]
hG=XSteam('hV_p',p0);             % Sättigungsenthalpie Dampf [kJ/kg]
h0=hL+x0*(hG-hL);                 % Enthalpie am Eintritt [kJ/kg]
Dh=hG-hL;                         % Verdampfungsenthalpie [kJ/kg]

% Rechennetz und Arbeitsspeicher anlegen
N=51;                             % Anzahl Knoten
dz(1:N-1)=0.1;                    % Zellgröße im Rohr  [m]
z(1:N)=0;
for k=2:N
    z(k)=z(k-1)+dz(k-1);          % Lage der Knotenpunkte
end
for k=1:N-1
    zm(k)=z(k)+dz(k)/2;           % Lage der Zell-Mittelpunkte
end
dH(1:N)=d;                        % hydr. Durchm. Rohr
A=pi/4*dH.^2;                     % Rohrquerschnitt [m2]
qprime(1:N-1)=0;
x(1:N)=x0;                        % Dampfanteil
p(1:N)=p0;                        % Druck [bar]
h(1:N)=h0;                        % Enthalpie [kJ/kg]
T(1:N-1)=T0;                      % Dampftemperatur [°C]
eps(1:N)=0;                       % Void
rhoM(1:N)=rhoL;                   % mittlere Dichte
rhoI(1:N)=rhoL;                   % Impulsdichte
dpdzR(1:N)=0;                     % Reibungsdruckverlust [Pa/m]
dpdzH(1:N-1)=0;                   % Hydrostatischer Druckverlust [Pa/m]
dpdzA(1:N-1)=0;                   % Beschleunigungsdruckverlust [Pa/m]
S(1:N)=0;                         % Schlupf
TW(1:N-1)=T0;                     % Wandtemperatur Rohrinnenpseite [°C]

% Reibungsdruckgradient und Void am Eintritt
dpdzR(1)=PLCorr(2,M,x(1),A(1),dH(1),eta,rhoL,rhoG,viscL,viscG,sigma);
eps(1)=VoidCorr(3,M,x(1),p(1)*1E5,rhoG,rhoL,viscL,sigma,dH(1),A(1),phi);
S(1)=x0/(1-x0)*(1-eps(1))/eps(1)*rhoL/rhoG;
rhoM(1)=eps(1)*rhoG+(1-eps(1))*rhoL;
if eps(1)<=0
    rhoI(1)=rhoL;
elseif eps(1)>=1
    rhoI(1)=rhoG;
else
    rhoI(1)=1/(x(1)^2/(eps(1)*rhoG)+(1-x(1))^2/((1-eps(1))*rhoL));
end

% Berechnung für jede Zelle
% Energiebilanz
for k=2:N
    T(k-1)=XSteam('T_ph',p(k-1),h(k-1));       % Dampftemp. der Zelle [°C]
    qprime(k-1)=-alphaa*pi*da*(T(k-1)-Ta)/1000;  % Kühlung der Zelle [kW/m]
    h(k)=h(k-1)+qprime(k-1)/M*dz(k-1);      % Enthalpie des Knotens [kJ/kg]
    x(k)=(h(k)-hL)/Dh;
    if x(k)<0
        x(k)=0;
    elseif x(k)>1
        x(k)=1;
    end
    
    % Berechnung der Druckgradienten
    dpdzR(k)=PLCorr(2,M,x(k),A(k),dH(k),eta,rhoL,rhoG,viscL,viscG,sigma);
    eps(k)=VoidCorr(3,M,x(k),p(k-1)*1E5,rhoG,rhoL,viscL,sigma,dH(k),A(k),phi);
    rhoM(k)=eps(k)*rhoG+(1-eps(k))*rhoL;
    if eps(k)<=0
        rhoI(k)=rhoL;
    elseif eps(k)>=1
        rhoI(k)=rhoG;
    else
        rhoI(k)=1/(x(k)^2/(eps(k)*rhoG)+(1-x(k))^2/((1-eps(k))*rhoL));
    end
    dpdzH(k-1)=9.81*sin(phi)/2*(rhoM(k)+rhoM(k-1));
    dpdzA(k-1)=M^2/(A(k)*dz(k-1))*(1/(A(k)*rhoI(k))-1/(A(k-1)*rhoI(k-1)));
    p(k)=p(k-1)-dz(k-1)/1E5*(dpdzA(k-1)+(dpdzR(k)+dpdzR(k-1))/2+dpdzH(k-1));
    if eps(k)>0 && eps(k)<1
        S(k)=x(k)/(1-x(k))*(1-eps(k))/eps(k)*rhoL/rhoG;
    end
    
    % Berechnung des Wärmeübergangs innen
    if x(k-1)<1 && x(k-1)>=0              % Shah (1979)
        alpha0=DitBoel(p(k-1),T(k-1)-1,M,A(k),dH(k),eta);
        alphai=alpha0*((1-x(k-1))^0.8...
            +3.8*x(k-1)^0.76*(1-x(k-1))^0.04/(p(k-1)/220.64)^0.38);
    else
        alphai=DitBoel(p(k-1),T(k-1),M,A(k),dH(k),eta);
    end
    qW=qprime(k-1)/(pi*d);
    TW(k-1)=T(k-1)+qW/alphai*1000;
end

% Plot der Ergebnisse
% Wandtemperatur
figure;
plot(zm, TW, 'b',zm, T, 'r')
grid on
xlabel('z [m]')
ylabel('T, TW [°C]')

% Dampfmassen- und volumenanteil
figure
plot(z, x, 'b', z, eps, 'r')
grid on
axis([0 5 0 1])
xlabel('z [m]')
ylabel('x, eps')

% Schlupf
figure
plot(z, S, 'b')
grid on
xlabel('z [m]')
ylabel('S')

% Druckgradienten
figure
plot(z, dpdzR, 'b', zm, dpdzA, 'r', zm, dpdzH, 'g')
grid on
xlabel('z [m]')
ylabel('dp/dz [Pa/m]')

% Druck
figure
plot(z, p, 'b')
grid on
xlabel('z [m]')
ylabel('p [bar]')

