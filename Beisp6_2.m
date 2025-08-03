% Beisp6_2
% Nukiyama Diagramme für Strömungssieden
% T. Schulenberg, Nov. 2020
% 
m0=[1000 1000 1000];            % Massenstromdichte [kg/m2s]
dH=0.02;                        % hydr. Durchmesser [m]
A=pi/4*dH^2;                    % Rohrquerschnitt [m2]                     
x0=[0.1 0.1 0.1];               % Dampfanteil im therm. Gleichgewicht
dTsub=0;                        % Unterkühlung der Flüssigkeit [K]
rough=1E-5;                     % Wandrauhigkeit [m]
dTsup(1:100)=0;                 % Wandüberhitzung  [°C]
qW(1:100)=1;                    % Wandwärmestrom [kW/m2]
p0=[215 217 219];                  % Druck [bar]

figure
axis([1 1000 10 10000])
for kp=1:3
    M=m0(kp)*A;
    p=p0(kp);
    x=x0(kp);
    hL=XSteam('hL_p',p);
    hG=XSteam('hV_p',p);
    rhoL=XSteam('rhoL_p',p);
    rhoG=XSteam('rhoV_p',p);
    Tsat=XSteam('Tsat_p',p);
    viscL=XSteam('my_pT',p,Tsat-1);
    st=XSteam('st_p',p);
    h=hL+x*(hG-hL);
    T=XSteam('Tsat_p',p)-dTsub;
    for k=1:90
        dTsup(k)=1.07^k;
        TW=T+dTsup(k);
        qW(k)=HTCorr(M,p,h*1000,T,TW,dH,A,rough,hL*1000,hG*1000, ...
            rhoL,rhoG,viscL,st)/1000;
    end

    loglog(dTsup,qW);
    hold on
end
xlabel('Wandüberhitzung [°C]')
ylabel('Wandwärmestrom [kW/m2]')
grid on;

        