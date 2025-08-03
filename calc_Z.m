function Z = calc_Z(modelValues,f)
% modelValues = [L R0 RQ1_R RQ1_n RQ1_Q RQ2_R RQ2_n RQ2_Q FLW_R FLW_T FLW_n C];
%                1  2  3     4     5     6     7     8     9     10    11   12

R0 = modelValues(2);
Z_L = 1i*2*pi().*f*modelValues(1);
Z_RQ1 = modelValues(3)./ (1+(1i*2*pi().*f*modelValues(3)*modelValues(5)).^modelValues(4));
Z_RQ2 = modelValues(6)./ (1+(1i*2*pi().*f*modelValues(6)*modelValues(8)).^modelValues(7));
Z_FLW = modelValues(9).*tanh((1i*2*pi()*modelValues(10).*f).^modelValues(11))./((1i*2*pi()*modelValues(10).*f).^modelValues(11));
Z_C = 1./(1i*2*pi().*f*modelValues(12));
Z = R0+Z_L+Z_RQ1+Z_RQ2+Z_FLW+Z_C;