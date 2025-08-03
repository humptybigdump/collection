% Beisp 7_1
% Kondensation am horizontalen Rohrb�ndel
% T. Schulenberg, Jan. 2021

% Rohr-Parameter
n=12;                          % Anzahl �bereinander liegender Rohre
da=0.02;                       % Rohrau�endurchmesser [m]
s=0.001;                       % Rohrwandst�rke [m]
di=da-2*s;                     % Rohrinnendurchmesser [m]
L=6;                           % Rohrl�nge [m]
rhoW=8530;                     % Dichte des Rohrmaterials [kg/m3]
cpW=377;                       % spez. W�rmekap. des Rohrs [J/kgK]
MWs=n*pi/4*(da^2-di^2)*rhoW;   % Rohrmasse pro L�nge [kg/m]

% K�hlwasser-Parameter
dz=0.5;                        % Schrittweite [m]
nz=L/dz+1;                     % Anzahl Knoten
TKW(1:nz)=20;                  % K�hlwassertemperatur am Eintritt [�C]
pKW(1:nz)=2;                   % K�hlwasserdruck [bar]
rhoKW=XSteam('rho_pT',pKW(1),TKW(1));   % K�hlwasserdichte [kg/m3]
cpKW=1000*XSteam('Cp_pT',pKW(1),TKW(1));  % 
uKW=2;                         % Geschwindigkeit des K�hlwassers [m/s]
mKW=rhoKW*uKW;                 % Massenstromdichte des K�hlwassers
AKW=n*pi/4*di^2;               % Rohrquerschnitt [m2]
MKW=mKW*AKW;                   % K�hlwassermassenstrom [kg/s]
alphai=DitBoel(pKW(1),TKW(1),MKW,AKW,di,0);   % W�rme�bergangskoeff. innen

% Stoffdaten Dampf und Kondensat
Tsat=30;                          % Sattdampftemperatur [�C]
psat=XSteam('psat_T',Tsat);       % S�ttigungsdruck [bar]
rhoL=XSteam('rhoL_T',Tsat);       % Kondensatdichte
rhoG=XSteam('rhoV_T',Tsat);       % Dampfdichte
lamdaL=XSteam('tcL_T',Tsat);      % W�rmeleitf�higkeit Kondensat [W/mK]
myL=XSteam('my_pT',psat,Tsat-1);  % Z�higkeit Kondensat
hL=1000*XSteam('hL_T',Tsat);      
hG=1000*XSteam('hV_T',Tsat);      
dhLG=hG-hL;                       % Verdampfungsenthalpie [J/kgK]
Falpha=0.73/n^(1/6)*(dhLG*rhoL*(rhoL-rhoG)*9.81*lamdaL^3/(myL*da))^0.25;

% Arbeitsspeicher anlegen
dt=0.1;                             % Zeitschritt [s]
nt=20;                              % Anzahl Zeitschritte
TW(1:nt,1:nz-1)=TKW(1);             % Wandtemperatur [�C]
qszu(1:nz-1)=0;                     % zugef�hrte W�rme pro L�nge [W/m]
qsab(1:nz-1)=0;                     % angef�hrte W�rme pro L�nge [W/m]
t(1:nt)=0;
ML(1:nt)=0;
for kz=2:nz
    z(kz)=(kz-1)*dz;
    zm(kz-1)=z(kz)-dz/2;
end

% Quasistation�re Berechnung
for kt=2:nt
    t(kt)=t(kt-1)+dt;
    ML(kt)=0;
    for kz=1:nz-1
        alphaa=Falpha/(Tsat-TW(kt-1,kz))^0.25;
        qszu(kz)=alphaa*n*pi*da*(Tsat-TW(kt-1,kz));
        qsab(kz)=alphai*n*pi*di*(TW(kt-1,kz)-TKW(kz));
        TW(kt,kz)=TW(kt-1,kz)+(qszu(kz)-qsab(kz))*dt/(MWs*cpW);
        TKW(kz+1)=TKW(kz)+qsab(kz)*dz/(MKW*cpKW);
        ML(kt)=ML(kt)+qszu(kz)*dz/dhLG;
    end
end

% Plot der Ergebnisse
% Wandtemperatur
figure;
surf(zm,t,TW,'MeshStyle','row');
view(0,0);
xlabel('z [m]');
ylabel('t [s]');
zlabel('TW [�C]');

% K�hlwassertemperatur
figure
plot(z, TKW, 'b')
grid on
axis([0 6 20 30])
xlabel('z [m]')
ylabel('TKW [�C]')

% Wandw�rmestrom
figure
plot(zm, qsab,'b',zm,qszu,'r')
grid on
axis([0 6 0 50000])
xlabel('z [m]')
ylabel('qszu, qsab [W/m]')

% Kondensatmassenstrom
figure
plot(t, ML,'b')
grid on
xlabel('t [s]')
ylabel('ML [kg/s]')
    