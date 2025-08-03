%% Ausgleichsparabel mit groben Fehler
% Lisa Dalheimer, SoSe 2022

clear variables
close all
clc

%%
% y-Werte aus modelltest.m werden um einen "groben Fehler" erweitert
x = [1.21 1.92 3.05 4.12 5.38]';
y = [2.44+0.4 2.19 2.08 2.21 2.61]';
sig = 0.02;

figure
plot(x,y,'xr'); xlabel('x'); ylabel('y');
grid on
xlim([-1 7]); ylim([1 5]);
title('Darstellung der Punkte')

% Ausgleichung
A = [x.^2 x ones(5,1)]; 
r = 2; % fünf Punkte, drei Parameter

Cll = sig^2*eye(5,5);
sig02 = 10^(-4);
Qll = Cll/sig02;
P = inv(Qll);

xd = inv(A'*P*A)*A'*P*y;
Qxdxd = inv(A'*P*A);
Cxdxd_prio = sig02*Qxdxd;

vd = A*xd - y;
Qldld = A*Qxdxd*A';
Qvdvd = Qll-Qldld;

% A-posteriori-Varianzfaktor
sig02d = (vd'*P*vd)/r;

% Darstellung
figure;
xlin = linspace(-1,7); ylin = xd(1)*xlin.^2 + xd(2)*xlin + xd(3);
plot(x,y,'xr',xlin,ylin,'-b'); xlabel('x'); ylabel('y');
grid on
xlim([-1 7]); ylim([1 5]);
title('Darstellung der Punkte und Ausgleichsparabel')

%% Datasnooping

% Globaltest
T_glob = sig02d/sig02;
k_glob =  finv(0.95,2,inf);
% T_glob > k_glob 

% Modellerweiterung um einen groben Fehler
B = [1 0 0 0 0]';

% Berechnung des Fehlers für alle möglichen Beobachtungen
for i=1:length(y)
    Qydyd(i) = inv(B'*P*Qvdvd*P*B);
    yd = Qydyd(i)*B'*P*(-vd);
    nablal(i) = yd;
    
    B = circshift(B,1);
end

% Alternative, vereinfacht
for i =1:length(y)
    nablal_alter(i) = -vd(i)/(Qvdvd(i,i)*P(i,i));
end

% Testen der Signifikanz der berechneten Fehler
for i=1:length(y)
    T(i) = nablal(i)/sqrt((Qydyd(i)*sig02));
end
% maximale Testgroesse liegt für ersten Eintrag vor -> verwerfe erste
% Beobachtung

% Neuausgleichung
yn = y(2:end);
xn = x(2:end);
% Ausgleichung
An = [xn.^2 xn ones(4,1)]; 
rn = 1; % vier Punkte, drei Parameter -> "Schwächung"

Clln = sig^2*eye(4,4);
sig02 = 10^(-4);
Qlln = Clln/sig02;
Pn = inv(Qlln);

xdn = inv(An'*Pn*An)*An'*Pn*yn;
Qxdxdn = inv(An'*Pn*An);
Cxdxd_prion = sig02*Qxdxdn;

vdn = An*xdn - yn;
Qldldn = An*Qxdxdn*An';
Qvdvdn = Qlln-Qldldn;
% A-posteriori-Varianzfaktor
sig02dn = (vdn'*Pn*vdn)/rn;

% neuer Globaltest
T_globn = sig02dn/sig02;

% Darstellung
figure;
xlin = linspace(-1,7); ylin = xdn(1)*xlin.^2 + xdn(2)*xlin + xdn(3);
plot(xn,yn,'xb',xlin,ylin,'-b', x(1),y(1), 'xr'); xlabel('x'); ylabel('y');
grid on
xlim([-1 7]); ylim([1 5]);
title('Darstellung der Punkte und Ausgleichsparabel')
