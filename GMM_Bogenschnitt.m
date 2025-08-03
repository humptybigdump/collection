%% Prueflinie, GMM-Beispiel 2
% Lisa Dalheimer
% WiSe 2122

clear variables
close all
clc

%% Beobachtungen

% nicht-stochastische, "fehlerfreie" Größen:
x = [305.10 317.59 677.65]'; % x1, x2 und x3
y = [269.08 639.72 454.40]'; % y1, y2 und y3

% Vektor der Beobachtungen
l = [236.46 222.43 231.16]';
n = length(l);

% Unbekannte: Koordinaten des Neupunktes x_nd y_nd
%% Funktionales Modell

% l_i(y_nd, x_nd) = sqrt((y_nd-yi)^2 + (x_nd-xi)^2) -> nichtlinear
% muss linearisiert werden

% Näherungswerte für y_nd und x_nd bestimmen aus eindeutig bestimmtem
% Bogenschnitt

% Strecke zwischen 1 und 2:
s_12    = sqrt((y(2)-y(1))^2+(x(2)-x(1))^2);
% Richtung auf Verbindungsstrecke zwischen 1 und 2
t_12    = atan((y(2)-y(1))/(x(2)-x(1)));
% Cosinussatz:
alpha_0 = acos((l(1)^2+s_12^2-l(2)^2)/(2*l(1)*s_12));
% Richtung zu Strecke von 1 auf N
t_1n0   = t_12-alpha_0;
% Polares Anhängen:
y_n0    = y(1) + l(1)*sin(t_1n0);
x_n0    = x(1) + l(1)*cos(t_1n0);

l_0     = sqrt((y_n0-y).^2+(x_n0-x).^2);
sintn   = (y_n0-y)./l_0;
costn   = (x_n0-x)./l_0;

% Design-Matrix aufstellen
A = ...
     [sintn(1) costn(1);
      sintn(2) costn(2);
      sintn(3) costn(3)];
  
% dl-Vektor
dl      = l - l_0;

%% Stochastisches Modell
sigma_0 = 4*10^(-2);        % einheitenlos
C_ll    = sigma_0^2 * eye(3);  % in m^2
Q_ll    = C_ll/sigma_0^2;      % in m^2
P       = inv(Q_ll);              % in m^-2


%% Ausgleichung

dx_d    = inv(A'*P*A)*A'*P*dl;
x_nd    = x_n0 + dx_d(2);
y_nd    = y_n0 + dx_d(1);

Q_xdxd = inv(A'*P*A);
C_xdxd = sigma_0^2*Q_xdxd;

v = A*dx_d-dl;

%% 2. Möglichkeit: Näherungswerte "raten" und iterieren
% --> selbst rechnen?

% z.B.
% while abs(v) > 0.001
% end

