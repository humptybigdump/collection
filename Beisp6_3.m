% Beisp6_3
% Druckverlauf und Wandtemperatur eines Kesselrohrs
% T. Schulenberg, Dez. 2020

% Parameter
d=0.02;                   % Rohrinnendurchmesser [m]
phi=pi/2;                 % Anstellwinkel
p0=146;                   % Druck am Eintritt [bar]
T0=330;                   % Temperatur am Eintritt [°C]
m=900;                    % Massenstromdichte [kg/m2s]
qStrich=12;               % zugeführte Wärme [kW/m]
eta=1E-5;                 % Wandrauhigkeit [m]
M=m*pi/4*d^2;             % Massenstrom [kg/s]

% Parameter der Wandtemperaturberechnung
MSW=4;                    % Rohrmasse [kg/m]
cpW=500;                  % spez. Wärme des Rohrs [J/kgK]
dt=0.2;                   % Zeitschrittweite [s]
Nt=300;                   % Anzahl Zeitschritte

% Stoffdaten am Eintritt
TSat=XSteam('Tsat_p',p0);         % Sättigungstemperatur [°C]
h0=XSteam('h_pT',p0,T0);          % Enthalpie [kJ/kg]
rhoL=XSteam('rhoL_p',p0);         % Dichte Flüssigkeit [kg/m3]
rhoG=XSteam('rhoV_p',p0);         % Dichte Dampf [kg/m3]
viscL=XSteam('my_pT',p0,TSat-1);  % Zähigkeit Flüssigkeit [Pa s]
viscG=XSteam('my_pT',p0,TSat+1);  % Zähigkeit Dampf  [Pa s]
sigma=XSteam('st_p',p0);          % Oberflächenspanung [N/m]
hL=XSteam('hL_p',p0);             % Sättigungsenthalpie flüssig [kJ/kg]
hG=XSteam('hV_p',p0);             % Sättigungsenthalpie Dampf [kJ/kg]
Dh=hG-hL;                         % Verdampfungsenthalpie [kJ/kg]
x0=(h0-hL)/Dh;
if x0<0
    x0=0;
elseif x0>1
    x0=1;
end

% Rechennetz und Arbeitsspeicher anlegen
N=53;                             % Anzahl Knoten
dz(1)=0.01;                       % Zellgröße Eintritt [m]
dz(2:N-2)=0.5;                    % Zellgröße im Rohr  [m]
dz(N-1)=0.01;                     % Zellgröße Austritt  [m]
z(1:N)=0;
t(1:Nt)=0;
for k=2:N
    z(k)=z(k-1)+dz(k-1);          % Lage der Knotenpunkte
end
for k=1:N-1
    zm(k)=z(k)+dz(k)/2;           % Lage der Zell-Mittelpunkte
end
dH(1)=3*d;                        % hyd. Durchm. Verteiler
dH(2:N-1)=d;                      % hydr. Durchm. Rohr
dH(N)=3*d;                        % hydr. Durchm. Sammler
A=pi/4*dH.^2;
qprime(1)=0;
qprime(2:N-2)=qStrich;
qprime(N-1)=0;
qW(1:N-1)=0.;
x(1:N)=x0;
p(1:N)=p0;
h(1:N)=h0;
eps(1:N)=0;
rhoM(1:N)=rhoL;
rhoI(1:N)=rhoL;
dpdzR(1:N)=0;                      % Reibungsdruckverlust [Pa/m]
dpdzH(1:N-1)=0;                    % Hydrostatischer Druckverlust [Pa/m]
dpdzA(1:N-1)=0;                    % Beschleunigungsdruckverlust [Pa/m]
S(1:N)=0;                          % Schlupf
TW(1:Nt,1:N-1)=TSat;               % Wandtemperatur Rohrinnenpseite [°C]

% Reibungsdruckgradient und Void am Eintritt
dpdzR(1)=PLCorr(2,M,x(1),A(1),dH(1),eta,rhoL,rhoG,viscL,viscG,sigma);
eps(1)=VoidCorr(3,M,x(1),p(1)*1E5,rhoG,rhoL,viscL,sigma,dH(1),A(1),phi);
rhoM(1)=eps(1)*rhoG+(1-eps(1))*rhoL;
if eps(1)<=0
    rhoI(1)=rhoL;
elseif eps(1)>=1
    rhoI(1)=rhoG;
else
    rhoI(1)=1/(x(1)^2/(eps(1)*rhoG)+(1-x(1))^2/((1-eps(1))*rhoL));
end

% Berechnung für jede Zelle
for k=2:N
    h(k)=h(k-1)+qprime(k-1)/M*dz(k-1);
    x(k)=(h(k)-hL)/Dh;
    if x(k)<0
        x(k)=0;
    elseif x(k)>1
        x(k)=1;
    end
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
    
 % Transiente Wandtemperaturberechnung
    TW(1,k-1)=T0;
    T=XSteam('T_ph',p(k-1),h(k-1));
    for nt=2:Nt
        qW(k-1)=HTCorr(M,p(k-1),h(k-1)*1000,T,TW(nt-1,k-1),dH(k),A(k), ...
            eta,hL*1000,hG*1000,rhoL,rhoG,viscL,sigma)/1000;
        TW(nt,k-1)=TW(nt-1,k-1)+dt*(qprime(k-1)-qW(k-1)*pi*dH(k))/(MSW*cpW)*1000;
        t(nt)=t(nt-1)+dt;
    end
    
   % Update der Stoffdaten
    TSat=XSteam('Tsat_p',p(k));         % Sättigungstemperatur [°C]
    rhoL=XSteam('rhoL_p',p(k));         % Dichte Flüssigkeit [kg/m3]
    rhoG=XSteam('rhoV_p',p(k));         % Dichte Dampf [kg/m3]
    viscL=XSteam('my_pT',p(k),TSat-1);  % Zähigkeit Flüssigkeit [Pa s]
    viscG=XSteam('my_pT',p(k),TSat+1);  % Zähigkeit Dampf  [Pa s]
    sigma=XSteam('st_p',p(k));          % Oberflächenspanung [N/m]
    hL=XSteam('hL_p',p(k));             % Sättigungsenthalpie flüssig
    hG=XSteam('hV_p',p(k));             % Sättigungsenthalpie Dampf
    Dh=hG-hL;                           % Verdampfungsenthalpie [kJ/kg]
end

% Plot der Ergebnisse
% Wandtemperatur
figure;
surf(zm,t,TW,'MeshStyle','row');
view(0,0);
xlabel('z [m]');
ylabel('t [s]');
zlabel('TW [°C]');

% Dampfmassen- und volumenanteil
figure
plot(z, x, 'b', z, eps, 'r')
grid on
axis([0 26 0 1])
xlabel('z [m]')
ylabel('x, eps')

% Wandwärmestrom
figure
plot(zm, qW, 'b')
grid on
axis([0 26 0 200])
xlabel('z [m]')
ylabel('qW [kW/m2]')



