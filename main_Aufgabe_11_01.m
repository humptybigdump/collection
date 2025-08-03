clear all
close all
clc

%% Einstellungen
set(groot,'defaultTextInterpreter','latex')
set(groot,'DefaultLegendInterpreter', 'latex')
set(groot,'defaultFigureColor','w')
set(groot,'defaultAxesFontSize',16)
set(groot,'defaultTextFontSize',16)

%% Simulation
vTspan    = [0,2*pi];
vX0       = [1;1;1];
vIdent    = reshape(eye(3),9,1);
[vT,xX]   = ode45(@(t,x) fAufgabe_11_01(t,x),    vTspan,vX0);
[vT2,xX2] = ode45(@(t,x) fAufgabe_11_01_erw(t,x),vTspan,[vX0;vIdent]);

%% Post-processing
xPhi     = reshape(xX2(end,4:end),3,3);
vEW      = eig(xPhi);
vEW_abs  = abs(eig(xPhi));

%% Visualisierung
figure(1)
plot(vT,xX,'Linewidth',1.5)
set(gcf,'Units','centimeters')
set(gcf,'Position', [2   2   33   16])

%% Subfunctions
function x_dt = fAufgabe_11_01(t,x)
  % Systemmatrix
  xP = [ -1   , sin(t),    0     ; ...
        cos(t),  -1   , -sin(t)  ; ...
          0   , cos(t),   -1    ];
  % Initialisierung
  x_dt = NaN(3,1);
  % Physikalische Zustände
  x_dt = xP*x;
end

function x_dt = fAufgabe_11_01_erw(t,x)
  % Systemmatrix
  xP = [ -1   , sin(t),    0     ; ...
        cos(t),  -1   , -sin(t)  ; ...
          0   , cos(t),   -1    ];
  % Initialisierung
  x_dt = NaN(3+3*3,1);
  % Physikalische Zustände
  x_dt(1:3) = xP*x(1:3);
  % Erweiterte Zustände (der Monodromiematrix)
  xX_erw      = xP*reshape(x(4:end),3,3);
  x_dt(4:end) = reshape(xX_erw,9,1);
end