clear all
clc

%einfließender Ethanolstrom [mol/s]
%VORG_N_ETHA_ein, Gleichung 5
N_ETHA_ein = 2000/3600;
%einfließender Essigsäurestrom [mol/s]
%VORG_N_ESSIG_ein, Gleichung 6
N_ESSIG_ein = 2500/3600;

%Reaktorvolumen V_R [m³]
%VORG_VR, Gleichung 7
V_R=3.2;

%Reaktionskonstanten k1 und k2 [m³/(mol*s)]
%VORG_k1, Gleichung 14
k1=2e-2;
%VORG_k2, Gleichung 15
k2=4e-5;

%Molare Massen [kg/kmol]
%VORG_M_ETHA, Gleichung 23
mM_ETHA=46.07;
%VORG_M_ESSIG, Gleichung 24
mM_ESSIG=60.05;

%Dichte rho [kg/m³]
%VORG_rho, Gleichung 26
rho=1020;

%Zusammenfassung der Parameter
X_Para = [N_ETHA_ein, V_R, N_ESSIG_ein, k1, k2, mM_ETHA, mM_ESSIG, rho];

%Festlegung der Reihenfolge von X_Var
%X_Var = [N_ETHA_aus, R_ETHA, N_ESSIG_aus, R_ESSIG, N_ETHYL_aus, R_ETHYL,
%N_WA_aus, R_WA, R1, R2, c_ETHA, c_ESSIG, c_ETHYL, c_WA, V_aus, M_ETHA_ein,
%M_ESSIG_ein, M_aus]

%Schätzwerte für X_Var
X_Var0 = [1000/3600, -1, 1250/3600, -1, 1000/3600, 1, 1000/3600, 1, 1, 1, 400, 600, 11000, 11000, 6e-5, 0.03, 0.04, 0.07];

%Solver-Optionen setzen
%'Display': 'iter' displays output at each iteration, and gives the default
%exit message.
%'MaxFunEvals': Maximum number of function evaluations allowed, a positive
%integer. The default is 100*numberOfVariables. See Tolerances and Stopping
%Criteria and Iterations and Function Counts.
%'MaxIter': Maximum number of iterations allowed, a positive integer. The
%default is 400. See Tolerances and Stopping Criteria and Iterations and
%Function Counts.
options = optimset('Display','iter', 'MaxFunEvals', 5000, 'MaxIter', 1000);

%Solver aufrufen
[X_Var,res] = fsolve(@(X_Var) F(X_Var, X_Para), X_Var0, options)