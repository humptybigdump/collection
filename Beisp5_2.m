% Beisp5_2
% kritische Massenstromdichte
% T. Schulenberg, Nov. 2020

% Parameter
p0=20.6;                      % Ruhedruck [bar]
mHEM(1:100)=0;
mHFM(1:100)=0;
for k=1:100
    x0(k)=(k-1)*0.003;
    h0=XSteam('hL_p',p0)+x0(k)*(XSteam('hV_p',p0)-XSteam('hL_p',p0)); 
    zeta=0.05;
    mHEM(k)=CMFlux(2,p0,h0,1,zeta);
    mHFM(k)=CMFlux(1,p0,h0,1,zeta);
end

figure
plot(x0, mHEM, 'r', x0, mHFM, 'b')
grid on
axis([0 0.3 0 30000])
xlabel('Ruhedampfgehalt x0 [-]')
ylabel('Krit. Massenstromdichte [kg/m2s]')
