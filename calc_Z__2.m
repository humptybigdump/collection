function Z = calc_Z(modelValues,f)
%% Berechnen Sie hier die Modellimpedanz aus den übergebenen Parametern modelValues und f. 

R0 = [];
Z_L = [];
Z_RQ1 = [];
Z_RQ2 = [];
Z_FLW = [];
Z_C = [];

Z = R0+Z_L+Z_RQ1+Z_RQ2+Z_FLW+Z_C;
