function res = F(X_Var,X_Para)

%Rückbenennung der Variablen
N_ETHA_aus = X_Var(1);
R_ETHA = X_Var(2);
N_ESSIG_aus = X_Var(3);
R_ESSIG = X_Var(4);
N_ETHYL_aus = X_Var(5);
R_ETHYL = X_Var(6);
N_WA_aus = X_Var(7);
R_WA = X_Var(8);
R1 = X_Var(9);
R2 = X_Var(10);
c_ETHA = X_Var(11);
c_ESSIG = X_Var(12);
c_ETHYL = X_Var(13);
c_WA = X_Var(14);
V_aus = X_Var(15);
M_ETHA_ein = X_Var(16);
M_ESSIG_ein = X_Var(17);
M_aus = X_Var(18);

%Rückbenennung der Parameter
N_ETHA_ein = X_Para(1);
V_R = X_Para(2);
N_ESSIG_ein = X_Para(3);
k1 = X_Para(4);
k2 = X_Para(5);
mM_ETHA = X_Para(6);
mM_ESSIG = X_Para(7);
rho = X_Para(8);
	
%SB_ETHA, Gleichung 1	
res(1) = N_ETHA_ein - N_ETHA_aus + V_R*R_ETHA;
%SB_ESSIG, Gleichung 2
res(2) = N_ESSIG_ein - N_ESSIG_aus + V_R*R_ESSIG;
%SB_ETHYL, Gleichung 3
res(3) = - N_ETHYL_aus + V_R*R_ETHYL;
%SB_WA, Gleichung 4
res(4) = - N_WA_aus + V_R*R_WA;
%DEF_R_ETHA, Gleichung 8
res(5) = R_ETHA - (- R1 + R2);
%DEF_E_ESSIG, Gleichung 9
res(6) = R_ESSIG - (- R1 + R2);
%DEF_R_ETHYL, Gleichung 10
res(7) = R_ETHYL - (R1 - R2);
%DEF_R_WA, Gleichung 11
res(8) = R_WA - (R1 - R2);
%DEF_R1, Gleichung 12
res(9) = R1 - k1*c_ETHA*c_ESSIG;
%DEF_R2, Gleichung 13
res(10) = R2 - k2*c_ETHYL*c_WA;
%DEF_N_ETHA_aus, Gleichung 16
res(11) = N_ETHA_aus - c_ETHA*V_aus;
%DEF_N_ESSIG_aus, Gleichung 17
res(12) = N_ESSIG_aus - c_ESSIG*V_aus;
%DEF_N_ETHYL_aus, Gleichung 18
res(13) = N_ETHYL_aus - c_ETHYL*V_aus;
%DEF_N_WA_aus, Gleichung 19
res(14) = N_WA_aus - c_WA*V_aus;
%MB, Gleichung 20
res(15) = M_ETHA_ein + M_ESSIG_ein - M_aus;
%DEF_M_ETHA_ein, Gleichung 21
res(16) = M_ETHA_ein - N_ETHA_ein/1000*mM_ETHA;
%DEF_M_ESSIG_ein, Gleicung 22
res(17) = M_ESSIG_ein - N_ESSIG_ein/1000*mM_ESSIG;
%DEF_M_aus, Gleichung 25
res(18) = M_aus - V_aus*rho;

end