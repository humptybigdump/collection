clear all
clc

%Festlegung Feed, [kg/s]
%VORG_F, Modellgleichung 11
F = 2.7;
%Massenanteil Feed, [-]
%VORG_xF, Modellgleichung 12 
xF = 0.01;

%Feed-Enthalpie, [kJ/kg]
%VORG_h_F, Modellgleichung 14 
h_F = 84.012;
%Enthalpie G0, [kJ/kg]
%VORG_h_G0, Modellgleichung 16
h_G0 = 2705.9;
%Enthalpie L1,[kJ/kg]
%VORG_h_L1, Modellgleichung 18
h_L1 = 503.78;
%Enthalpie L2, [kJ/kg]
%VORG_h_L2, Modellgleichung 23
h_L2 = 419.10;
%Enthalpie G2, [kJ/kg]
%VORG_h_G2, Modellgleichung 25
h_G2 = 2675.6;
%Enthalpie L3, [kJ/kg]
%VORG_h_L3, Modellgleichung 27
h_L3 = 419.1;
%Enthalpie L4, [kJ/kg]
%VORG_h_L4, Modellgleichung 31
h_L4 = 376.97;
%Enthalpie G4, [kJ/kg]
%VORG_h_G4, Modellgleichung 33
h_G4 = 2659.5;

%Temperatur G0, [°C]
%VORG_T_G0, Modellgleichung 20
T_G0 = 120;
%Temperatur G2, [°C]
%VORG_T_G2, Modellgleichung 21
T_G2 = 100;
%Temperatur G4, [°C]
%VORG_T_G4, Modellgleichung 29
T_G4 = 90;

%Vorgabe G0, [kg/s]
%VORG_G0, Modellgleichung 36
G0 = 1.769;
%Vorgabe G2, [kg/s]
%VORG_G2, Modellgleichung 37
G2 = 1.3254;
%Vorgabe G4, [kg/s]
%VORG_G4, Modellgleichung 38
G4 = 1.3357;

%Zusammenfassen zu X_PARA
X_PARA = [F, xF, h_F, h_G0, h_L1, h_L2, h_G2, h_L3, h_L4, h_G4, T_G0, T_G2, T_G4, G0, G2, G4];

%Anfangswerte für Massen der diff. Variable, [kg]
M_2S_t = 10;
M_2W_t = 100;
M_4S_t = 20;
M_4W_t = 100;

M_2 = M_2W_t;
M_4 = M_4W_t;

%Integrationsschrittweite, [s]
delta_t = 10;
%Ende der Simulation, [s]
t_ende = 20000;

%ertsellt Ergebnisdatei
fid = fopen('Ergebnisse.xls', 'w');

%Festlegung der Reihenfolge der algebraischen Größen von X_VAR
%X_VAR = [L1, H_G0, H_L1, Q1, L2, x_2S, H_F, H_L2, H_G2, L3, H_L3, Q3, L4, x_4S, H_L4, H_G4, kA1, kA3]; 
X_VAR0 = [1.5, 4500, 850, 3700, 1.5, 0, 200, 600, 3500, 1.3, 550, 3000, 0.1, 0, 40, 3500, 200, 300];

%Schleifenbeginn
for t=0:delta_t:t_ende
    
   %Ergebnisse in Ergebnisdatei schreiben
   fprintf(fid,'%12.8f %12.8f %12.8f %12.8f %12.8f \r\n',t, M_2S_t, M_2W_t, M_4S_t, M_4W_t);
   
   %Setzen der Optionen
   %'Display': 'iter' displays output at each iteration, and gives the default
   %exit message.
   %'MaxFunEvals': Maximum number of function evaluations allowed, a positive
   %integer. The default is 100*numberOfVariables. See Tolerances and Stopping
   %Criteria and Iterations and Function Counts.
   %'MaxIter': Maximum number of iterations allowed, a positive integer. The
   %default is 400. See Tolerances and Stopping Criteria and Iterations and
   %Function Counts.
   options = optimset('display', 'iter', 'MaxFunEvals', 5000, 'MaxIter', 1000);
   
   %Solver aufrufen
   [X_VAR_LSG, Guete] = fsolve(@(X_VAR) Modellgleichungen(X_VAR, X_PARA, M_2S_t, M_4S_t, M_2, M_4), X_VAR0, options);
   
   %Rückbenennung der algebraischen Prozessgrößen
   L2 = X_VAR_LSG(5);
   x_2S = X_VAR_LSG(6);
   L4 = X_VAR_LSG(13);
   x_4S = X_VAR_LSG(14);
   
   %Aktualisieren der differentiellen Prozessgrößen
   
   %S_MB_BR2, Modellgleichung 4
   M_2S_t_plus_delta_t = M_2S_t + delta_t*(F*xF - L2*x_2S);

   
   %S_MB_BR4, Modellgleichung 9
   M_4S_t_plus_delta_t = M_4S_t + delta_t*(L2*x_2S - L4*x_4S);

   
   %Überschreibung der aktuell berechneten Massen für nächsten Schleifendurchlauf
   M_2S_t = M_2S_t_plus_delta_t;
  %M_2W_t = M_2W_t_plus_delta_t;
   M_4S_t = M_4S_t_plus_delta_t;
  %M_4W_t = M_4W_t_plus_delta_t;
   
   %Überschreibung der aktuellen Lösung als Startwert für nächste Iteration
   X_VAR0 = X_VAR_LSG;

   
end

fclose(fid);
