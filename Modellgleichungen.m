function res = Modellgleichungen(X_VAR, X_PARA, M_2S_t, M_4S_t, M_2, M_4)

%Rückbenennung der algebraischen Variablen
L1 = X_VAR(1);
H_G0 = X_VAR(2);
H_L1 = X_VAR(3);
Q1 = X_VAR(4);
L2 = X_VAR(5);
x_2S = X_VAR(6);
H_F = X_VAR(7);
H_L2 = X_VAR(8);
H_G2 = X_VAR(9);
L3 = X_VAR(10);
H_L3 = X_VAR(11);
Q3 = X_VAR(12);
L4 = X_VAR(13);
x_4S = X_VAR(14);
H_L4 = X_VAR(15);
H_G4 = X_VAR(16);
kA1 = X_VAR(17);
kA3 = X_VAR(18);

%Rückbenennung der Parameter
F = X_PARA(1);
xF = X_PARA(2);
h_F = X_PARA(3);
h_G0 = X_PARA(4);
h_L1 = X_PARA(5);
h_L2 = X_PARA(6);
h_G2 = X_PARA(7);
h_L3 = X_PARA(8);
h_L4 = X_PARA(9);
h_G4 = X_PARA(10);
T_G0 = X_PARA(11);
T_G2 = X_PARA(12);
T_G4 = X_PARA(13);
G0 = X_PARA(14);
G2 = X_PARA(15);
G4 = X_PARA(16);

%W_MB_BR1, Modellgleichung 1
res(1) = G0 - L1;
%EB_BR1, Modellgleichung 2
res(2) = H_G0 - H_L1 - Q1;
%EB_BR2, Modellgleichung 5
res(3) = H_F + Q1 - H_L2 - H_G2;
%W_MB_BR3, Modellgleichung 6
res(4) = G2 - L3;
%EB_BR3, Modellgleichung 7
res(5) = H_G2 - H_L3 - Q3;
%EB_BR4, Modellgleichung 10
res(6) = H_L2 + Q3 - H_L4 - H_G4;
%DEF_H_F, Modellgleichung 13
res(7) = H_F - F*h_F;
%DEF_H_G0, Modellgleichung 15
res(8) = H_G0 - G0*h_G0;
%DEF_H_L1, Modellgleichung 17
res(9) = H_L1 - L1*h_L1;
%DEF_Q1, Modellgleichung 19
res(10) = Q1 - kA1*(T_G0 - T_G2);
%DEF_H_L2, Modellgleichung 22
res(11) = H_L2 - L2*h_L2;
%DEF_H_G2, Modellgleichung 24
res(12) = H_G2 - G2*h_G2;
%DEF_H_L3, Modellgleichung 26
res(13) = H_L3 - L3*h_L3;
%DEF_Q3, Modellgleichung 28
res(14) = Q3 - kA3*(T_G2 - T_G4);
%DEF_H_L4, Modellgleichung 30
res(15) = H_L4 - L4*h_L4;
%DEF_H_G4, Modellgleichung 32
res(16) = H_G4 - G4*h_G4;
%DEF_x2, Modellgleichung 34
res(17) = M_2S_t - x_2S*M_2;
%DEF_x4, Modellgleichung 35
res(18) = M_4S_t - x_4S*M_4;
end

