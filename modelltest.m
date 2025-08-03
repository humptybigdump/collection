%% Modelltests
% Lisa Dalheimer, SoSe 2022
clear variables
close all
clc

%% Eingabe und Plot der Punkte

x = [1.21 1.92 3.05 4.12 5.38]';
y = [2.44 2.19 2.08 2.21 2.61]';
sig = 0.02;

% Zum Generieren genutzt:
%a = 0.1;
%xs = 3;
%b = 2.1;
%y = (a*(x-xs).^2+b) + randn(1,5)*sig;

% Plot
figure
plot(x,y,'xr'); xlabel('x'); ylabel('y');
grid on
xlim([-1 7]); ylim([1 5]);
title('Darstellung der Punkte')

%% Ausgleichung als Gerade

% Redundanz
r = 3;
% Funktionales Modell: Gerade, y = b*x + c;
A = [x ones(5,1)];
% Stochastisches Modell
Cll = sig^2*eye(5,5);
sig02 = 10^(-4)
Qll = Cll/sig02
P = inv(Qll)
% Ausgleichung
    %   Parameter bestimmen
xd = inv(A'*P*A)*A'*P*y
Qxdxd = inv(A'*P*A);
Cxdxd_prio = sig02*Qxdxd;
    %   Verbesserungen bestimmen
vd = A*xd - y
Qldld = A*Qxdxd*A';
Qvdvd = Qll-Qldld
% A-posteriori-Varianzfaktor
sig02d = (vd'*P*vd)/r

% Darstellung
figure;
xlin = linspace(-1,7); ylin = xd(1)*xlin + xd(2);
plot(x,y,'xr',xlin,ylin,'-b');
xlabel('x'); ylabel('y');
grid on
xlim([-1 7]); ylim([1 5])
title('Darstellung der Punkte und Ausgleichsgerade')

%% Globaltest

% Ist das angesetzte Modell stochastisch und/oder funktional konsistent?
% H_0: E(sig02d) = sig02
% H_A: E(sig02d) =/= sig02

% Testgröße: 
% 1. Variante - Quotient aus a-priori- und a-posteriori-VF
T = sig02d/sig02;
% Teststatistik: verhält sich wie Fisher_r_undendlich
% kritischer Wert
k =  finv(0.95,3,inf);
%bzw. äquivalent
%k_test = chi2inv(0.95,3)/r

% 2. Variante - Quotient aus Verbesserungsquadratsumme und a-posteriori-VF
% Testgröße 
T_alt = vd'*P*vd/sig02
% kritischer Wert
k_alt = chi2inv(0.95,3)

% T > k und T_alt > k_alt 
% --> Nullhypothese wird verworfen

%% Erweiterung des Modells

% neues Modell: Parabel, y = a*x^2+bx+c
% Erweiterung l + vd = A*xd + B* yd;
B = x.^2;

% Berechnen des Zusatzparameters (und der veränderten Parameter für Plot)
Qydyd = inv(B'*P*Qvdvd*P*B)
sig_yd = sqrt(Qydyd*sig02);
yd = -Qydyd*B'*P*vd

xad = xd - inv(A'*P*A)*A'*P*B*yd; % Berechnet, damit geplottet werden kann
vdn = [A B]*[xad; yd]-y; % neue Verbesserungen, für Formelwerk sind aber
% Verbesserungen des Ausgangsmodells gemeint

% Darstellung
figure;
xlin = linspace(-1,7); ylin = xd(1)*xlin + xd(2);
yparab = yd*xlin.^2 + xad(1)*xlin + xad(2);
plot(x,y,'xr',xlin,ylin,'-b', xlin, yparab, '-k');
xlabel('x'); ylabel('y');
grid on
xlim([-1 7]); ylim([1 5])
title('Darstellung der Punkte und Ausgleichsparabel')


% Untersuchung der Signifikanz von yd, a-priori-bezogen
% H_0: E(yd)   = 0
% H_A: E(yd) =/= 0
% Für 1D-Größe ganz "normaler" Signifikanztest T = yd - y_0 / sig_yd
% Testgröße
T_1D = yd/sig_yd;
% kritischer Wert
k_1D = norminv(0.95);

% T_1D > k_1D
% Nullhypothese wird verworfen, Parameter signifikant

% Bei mehrdimensionalem yd-Vektor (funktioniert aber auch für 1D, p = 1):
% Testgroesse
T_mehrD = yd'*inv(Qydyd)*yd/sig02;
% Teststatistik: verhält sich wie Chi2 mit p FG (p = Anzahl
% Zusatzparameter)
k_mehrD = chi2inv(0.95,1)
% Parameter kann als signifikant angenommen werden

