% Beisp5_1
% Schallgeschwindigkeit einer Zweiphasenströmung
% T. Schulenberg, Nov. 2020

% Parameter
p(1:101)=10;           % Druck [bar]
x(1:101)=0;
eps(1:101)=0;
aHFM(1:101)=0;          % Schallgeschw. des homogenen gefrorenen Modells
aHEM(1:101)=0;          % Schallgeschw. des homogenen Gleichgewichtsmodells
for k=1:101
    rhoG=XSteam('rhoV_p',p(k));
    rhoL=XSteam('rhoL_p',p(k));
    if k>1
        eps(k)=(k-1)/100;
        x(k)=1/(1+rhoL/rhoG*(1-eps(k))/eps(k));
    end
    aL=XSteam('wL_p',p(k));
    aG=XSteam('wV_p',p(k));
    sL=XSteam('sL_p',p(k));
    sG=XSteam('sV_p',p(k));
    s0=sL+x(k)*(sG-sL);
    
    % Berechnung von (dx/dp)s
    sL1=XSteam('sL_p',p(k)+0.001);
    sG1=XSteam('sV_p',p(k)+0.001);
    Ds1=sG1-sL1;
    x1=(s0-sL1)/Ds1;
    sL2=XSteam('sL_p',p(k)-0.001);
    sG2=XSteam('sV_p',p(k)-0.001);
    Ds2=sG2-sL2;
    x2=(s0-sL2)/Ds2;
    dxdp=(x1-x2)/0.002*1E-5;
    
    % Homogenes gefrorenes Modell (HFM)
    rhoH=1/(x(k)/rhoG+(1-x(k))/rhoL);
    mc1=(1/(rhoL^2*aL^2)+x(k)*(1/(rhoG^2*aG^2)-1/(rhoL^2*aL^2)))^(-0.5);
    aHFM(k)=mc1/rhoH;
    % Homogenes Gleichgewichtsmodell (HEM)
    mc2=1/(1/(rhoL^2*aL^2)+x(k)*(1/(rhoG^2*aG^2)-1/(rhoL^2*aL^2)) ...
                        -dxdp*(1/rhoG-1/rhoL))^0.5;
    aHEM(k)=mc2/rhoH;
end


figure
semilogy(eps, aHFM, 'b', eps, aHEM, 'r')
grid on
axis([0 1 0 1500])
xlabel('Void [-]')
ylabel('a2PH [m/s]')

