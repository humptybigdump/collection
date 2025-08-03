%% KONTRA - Kongruenztransformation
%
% Ausgleichungsrechnung und Statistik II, SS 2012
%
% Kapitel 9.4, Beispiel 2
%
% H. Bähr, 8. Mai 2012

%%
clear;           % Löschen des Matlab-Arbeitsspeichers
format compact;  % Unterdrücken von Leerzeilen in der Ausgabe
rho = 200/pi;


%% Beobachtungen

vi = [   0
       100
         0 ];

ui = [   0
       100
       100 ];

yi = [  20.00
       148.98
        55.57 ];

xi = [  30.00
        87.91
       123.46 ];

l = reshape([vi,ui,yi,xi]',12,1);


%% Näherungswerte

Dv = vi(2)-vi(1);
Du = ui(2)-ui(1);
Dy = yi(2)-yi(1);
Dx = xi(2)-xi(1);

phi0 = asin((Dy/Dv-Dx/Du)/(Dv/Du+Du/Dv));% [rad]
ty0  = 20;
tx0  = 30;
x0   = [phi0;ty0;tx0];


%% Funktionales Modell

sinphi = sin(phi0);
cosphi = cos(phi0);

A = [ -sinphi*vi(1)+cosphi*ui(1)  1  0
      -cosphi*vi(1)-sinphi*ui(1)  0  1
      -sinphi*vi(2)+cosphi*ui(2)  1  0
      -cosphi*vi(2)-sinphi*ui(2)  0  1
      -sinphi*vi(3)+cosphi*ui(3)  1  0
      -cosphi*vi(3)-sinphi*ui(3)  0  1 ];

Bquer = [  cosphi sinphi -1  0
          -sinphi cosphi  0 -1 ];

B = [ Bquer      zeros(2,4) zeros(2,4)
      zeros(2,4) Bquer      zeros(2,4)
      zeros(2,4) zeros(2,4) Bquer      ];

w = B*l + [ty0;tx0;ty0;tx0;ty0;tx0];


%% Stochastisches Modell
Qll = eye(12);


%% Ausgleichung

iQww    = inv(B*Qll*B');
Qxx     = inv(A'*iQww*A);
dx      = Qxx * A' * iQww * (-w);
x       = x0 + dx;
Qkk     = iQww - iQww * A * Qxx * A' * iQww;
Qvv     = Qll * B' * Qkk * B * Qll;
Qldld   = Qll - Qvv;
v       = Qll * B' * Qkk * (-w);
ld      = l + v;
sigma0d = sqrt(v'*v/3);

phi = x(1)*rho;
ty  = x(2);
tx  = x(3);

sigma_phi = sigma0d*rho*sqrt(Qxx(1,1));
sigma_ty  = sigma0d*sqrt(Qxx(2,2));
sigma_tx  = sigma0d*sqrt(Qxx(3,3));


%% Probe

sinphid = sin(x(1));
cosphid = cos(x(1));

vid = ld([1,5, 9]);
uid = ld([2,6,10]);
yid = ld([3,7,11]);
xid = ld([4,8,12]);

probe = [  cosphid * vid(1) + sinphid * uid(1) - yid(1) + ty
          -sinphid * vid(1) + cosphid * uid(1) - xid(1) + tx
           cosphid * vid(2) + sinphid * uid(2) - yid(2) + ty
          -sinphid * vid(2) + cosphid * uid(2) - xid(2) + tx
           cosphid * vid(3) + sinphid * uid(3) - yid(3) + ty
          -sinphid * vid(3) + cosphid * uid(3) - xid(3) + tx ];


%% Ergebnisse

fprintf('Parameter:\n');
fprintf('  phi = %6.2f gon, sigma = %5.3f gon\n',phi,sigma_phi);
fprintf('  ty  = %6.2f m,   sigma = %5.3f m  \n',ty ,sigma_ty);
fprintf('  tx  = %6.2f m,   sigma = %5.3f m  \n',tx ,sigma_tx);
fprintf('Probe:\n');
fprintf('  %13.5e m\n',probe);
