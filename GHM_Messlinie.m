%% MLINIE - Messungslinie
%
% Ausgleichungsrechnung und Statistik II, SS 2012
%
% Kapitel 9.4, Beispiel 1
%
% H. Bähr, 8. Mai 2012

%%
clear variables          % Löschen des Matlab-Arbeitsspeichers
close all
format compact;  % Unterdrücken von Leerzeilen in der Ausgabe


%% Beobachtungen

yi = [  47.12
        69.57
        76.39
        85.35 ];

xi = [ 122.39
       146.76
       154.17
       163.89 ];

l = reshape([yi,xi]',8,1);


%% Näherungswerte

a0 = 0.92;
b0 = -65;
x0 = [a0;b0];


%% Funktionales Modell

A = [xi ones(4,1)];

B = [ -1 a0  0  0  0  0  0  0
       0  0 -1 a0  0  0  0  0
       0  0  0  0 -1 a0  0  0
       0  0  0  0  0  0 -1 a0 ];

w = a0 * xi + b0 - yi;


%% Stochastisches Modell

sigma0 = 0.01;
Qll    = diag([4,1,16,1,4,1,9,1]);


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
sigma0d = sqrt(v'*v/2);



%% Probe

yi_dach = ld(1:2:7);
xi_dach = ld(2:2:8);

fd = x(1) * xi_dach + x(2) * ones(4,1) - yi_dach;

%%
% Die hier resultierenden Widersprüche sind kleiner als 10^-6 m. Daher wäre
% eine weitere Iteration eigentlich nicht erforderlich.

fprintf(1,'Probe nach erster Iteration:\n');
fprintf(1,'  f%1i = %13.5e\n',[1:4;fd']);

figure
plot(xi, yi, 'xb', xi_dach, yi_dach, 'xr')
hold on
grid on
axis square
xvec = 120:0.2:165; % Ausgleichsgerade zeichnen
yline = x(1)*xvec+x(2);
plot(xvec, yline, '--r')

legend({'$l$', '$\hat{l}$', 'Ausgleichsgerade'}, 'Interpreter', 'Latex', 'Location', 'southeast')
ylabel('$y$ in m', 'Interpreter', 'Latex', 'Fontsize',12)
xlabel('$x$ in m', 'Interpreter', 'Latex', 'Fontsize',12)
%% 2. Iteration

a02 = x(1);
b02 = x(2);
x02 = [a02;b02];

B2  = [ -1 a02  0  0   0  0   0  0
         0  0  -1 a02  0  0   0  0
         0  0   0  0  -1 a02  0  0
         0  0   0  0   0  0  -1 a02 ] ;

w2       = a02 * xi + b02 * ones(4,1) - yi;
iQww2    = inv(B2*Qll*B2');
Qxx2     = inv(A'*iQww2*A);
dx2      = Qxx2 * A' * iQww2 * (-w2);
x2       = x02 + dx2;
Qkk2     = iQww2 - iQww2 * A * Qxx2 * A' * iQww2;
v2       = Qll * B2' * Qkk2 * (-w2);
ld2      = l + v2;
sigma0d2 = sqrt(v'*v/2);

yi_dach_2 = ld2([1,3,5,7]);
xi_dach_2 = ld2([2,4,6,8]);

fd2 = x2(1) * xi_dach_2 + x2(2) * ones(4,1) - yi_dach_2;

fprintf(1,'Probe nach zweiter Iteration:\n');
fprintf(1,'  f%1i = %13.5e\n',[1:4;fd2']);


%% Ergebnisse

fprintf(1,'Parameter nach zweiter Iteration:\n');
fprintf(1,'%1c = %8.4f, sigma_%1c = %8.5f\n',...
        [97:98;x2';97:98;sigma0d2*sqrt(diag(Qxx2))']);

