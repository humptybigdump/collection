clear all
clc

%F_Punkt, [kg/s]
%VORG_F, Gleichung 11
Feed = 2.7;

%x_F,S, [-]
%VORG_x_F,S, Gleichung 12
x_FS = 0.01;

%x_F,W, [-]
%VORG_x_F,W, Gleichung 14
x_FW = 0.99;

%y_2,S, [-]
%VORG_y_2,S, Gleichung 13
y_2S = 0;

%y_2,W, [-]
%VORG_y_2,W, Gleichung 15
y_2W = 1;

%x_4,S, [-]
%VORG_x_4,S, Gleichung 16
x_4S = 0.7;

%y_4,S, [-]
%VORG_y_4,S, Gleichung 17
y_4S = 0;

%x_4,W, [-]
%VORG_x_4,W, Gleichung 18
x_4W = 0.3;

%y_4,W, [-]
%VORG_y_4,W, Gleichung 19
y_4W = 1;

%h_F, [kJ/kg]
%VORG_h_F, Gleichung 22
h_F = 84.012;

%h_G0, [kJ/kg]
%VORG_h_G0, Gleichung 24
h_G0 = 2705.9;

%h_L1, [kJ/kg]
%VORG_h_L1, Gleichung 26
h_L1 = 503.78;

%h_L2, [kJ/kg]
%VORG_h_L2, Gleichung 31
h_L2 = 419.10;

%h_G2, [kJ/kg]
%VORG_h_G2, Gleichung 33
h_G2 = 2675.60;

%h_L3, [kJ/kg]
%VORG_h_L3, Gleichung 35
h_L3 = 419.10;

%h_L4, [kJ/kg]
%VORG_h_L4, Gleichung 39
h_L4 = 376.97;

%h_G4, [kJ/kg]
%VORG_G4, Gleichung 41
h_G4 = 2659.50;

%T_G0, [°C]
%VORG_T_G0, Gleichung 28
T_G0 = 120;

%T_G2, [°C]
%VORG_T_G2, Gleichung 29
T_G2 = 100;

%T_G4, [°C]
%VORG_T_G4, Gleichung 37
T_G4 = 90;

%ZUSAMMENFASSEN ZU X_PARA
X_Para = [Feed, x_FS, y_2S, x_FW, y_2W, x_4S, y_4S, x_4W, y_4W, h_F, h_G0, h_L1, T_G0, T_G2, h_L2, h_G2, h_L3, T_G4, h_L4, h_G4];

%Festlegung der Reihenfolge von X_Var
%X_VAR = [G0, L1, H_G0, H_L1, Q1, L2, x_2S, G2, x_2W, H_F, H_L2, H_G2, L3, H_L3, Q3, L4, G4, H_L4, H_G4, kA1, kA3];

%Schätzwerte
X0_Var = [2, 2, 5000, 1000, 4000, 1.5, 0.10, 1.2, 0.90, 160, 600, 5200, 1.2, 500, 4500 , 1, 0.5, 370, 1300, 200, 450];

%setzen der Optionen
%'Display': 'iter' displays output at each iteration, and gives the default
%exit message.
%'MaxFunEvals': Maximum number of function evaluations allowed, a positive
%integer. The default is 100*numberOfVariables. See Tolerances and Stopping
%Criteria and Iterations and Function Counts.
%'MaxIter': Maximum number of iterations allowed, a positive integer. The
%default is 400. See Tolerances and Stopping Criteria and Iterations and
%Function Counts.
options = optimset('display', 'iter', 'MaxFunEvals', 5000, 'MaxIter', 1000);

%Aufrufen von fsolve
[X_Var, res] = fsolve(@(X_Var) F(X_Var, X_Para), X0_Var, options)
