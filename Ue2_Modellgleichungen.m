function res = Ue2_Modellgleichungen(X_VAR, X_PARA)

%Rückbenennung der Variablen
M2W = X_VAR(1);
M3W = X_VAR(2);
M3S = X_VAR(3);
M4W = X_VAR(4);
M5W = X_VAR(5);
M4S = X_VAR(6);
M5S = X_VAR(7);

%Rückbenennung der Parameter
M1W = X_PARA(1);
M1S = X_PARA(2);
M2S = X_PARA(3);
x3S = X_PARA(4);
alpha = X_PARA(5);

%MB_M_W, Modellgleichung 1
res(1) = M1W + M2W - M3W;
%MB_M_S, Modellgleichung 2
res(2) = M1S + M2S - M3S;
%MB_T_W, Modellgleichung 3
res(3) = M3W - M4W - M5W;
%MB_T_S, Modellgleichung 4
res(4) = M3S - M4S - M5S;
%DEF_x3S, Modellgleichung 5
res(5) = M3S/(M3S + M3W) - x3S;
%DEF_alpha, Modellgleichung 6
res(6) = (M4S + M4W)/(M3S + M3W)- alpha;
%DEF_, Modellgleichung 7
res(7) = M4S/(M4S + M4W) - x3S;
%keyboard
end

