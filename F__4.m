function res = F(X_Var, X_Para)

%Rückbenennung der Variablen
G0 = X_Var(1);
L1 = X_Var(2);
H_G0 = X_Var(3);
H_L1 = X_Var(4);
Q1 = X_Var(5);
L2 = X_Var(6);
x_2S = X_Var(7);
G2 = X_Var(8);
x_2W = X_Var(9);
H_F = X_Var(10);
H_L2 = X_Var(11);
H_G2 = X_Var(12);
L3 = X_Var(13);
H_L3 = X_Var(14);
Q3 = X_Var(15);
L4 = X_Var(16);
G4 = X_Var(17);
H_L4 = X_Var(18);
H_G4 = X_Var(19);
kA1 = X_Var(20);
kA3 = X_Var(21);

%Rückbenennung der Parameter
Feed = X_Para(1);
x_FS = X_Para(2);
y_2S = X_Para(3);
x_FW = X_Para(4);
y_2W = X_Para(5);
x_4S = X_Para(6);
y_4S = X_Para(7);
x_4W = X_Para(8);
y_4W = X_Para(9);
h_F = X_Para(10);
h_G0 = X_Para(11);
h_L1 = X_Para(12);
T_G0 = X_Para(13);
T_G2 = X_Para(14);
h_L2 = X_Para(15);
h_G2 = X_Para(16);
h_L3 = X_Para(17);
T_G4 = X_Para(18);
h_L4 = X_Para(19);
h_G4 = X_Para(20);

%W_MB_BR1, Gleichung 1
res(1) = G0 - L1;
%EB_BR1, Gleichung 2
res(2) = H_G0 - H_L1 - Q1;
%S_MB_BR2, Gleichung 3
res(3) = Feed*x_FS - L2*x_2S - G2*y_2S;
%W_MB_BR2, Gleichung 4
res(4) = Feed*x_FW - L2*x_2W - G2*y_2W;
%EB_BR2, Gleichung 5
res(5) = H_F + Q1 - H_L2 - H_G2;
%W_MB_BR3, Gleichung 6
res(6) = G2 - L3;
%EB_BR3, Gleichung 7
res(7) = H_G2 - H_L3 - Q3;
%S_MB_BR4, Gleichung 8
res(8) = L2*x_2S - L4*x_4S - G4*y_4S;
%W_MB_BR4, Gleichung 9
res(9) = L2*x_2W - L4*x_4W - G4*y_4W;
%EB_BR4, Gleichung 10
res(10) = H_L2 + Q3 - H_L4 - H_G4;
%SCHL-BED_x_2, Gleichung 20
res(11) = 1 - (x_2S + x_2W);
%DEF_H_F, Gleichung 21
res(12) = H_F - Feed*h_F;
%DEF_H_G0, Gleichung 23
res(13) = H_G0 - G0*h_G0;
%DEF_H_L1, Gleichung 25
res(14) = H_L1 - L1*h_L1;
%DEF_Q1, Gleichung 27
res(15) = Q1 - kA1*(T_G0 - T_G2);
%DEF_H_L2, Gleichung 30
res(16) = H_L2 - L2*h_L2;
%DEF_H_G2, Gleichung 32
res(17) = H_G2 - G2*h_G2;
%DEF_H_L3, Gleichung 34
res(18) = H_L3 - L3*h_L3;
%DEF_Q3, Gleichung 36
res(19) = Q3 - kA3*(T_G2 - T_G4);
%DEF_H_L4, Gleichung 38
res(20) = H_L4 - L4*h_L4;
%DEF_H_G4, Gleichung 40
res(21) = H_G4 - G4*h_G4;

end

