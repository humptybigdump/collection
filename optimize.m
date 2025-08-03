function result = optimize(Parameter, L_opt)

R_electrolyte = 1/Parameter.sigma_electrolyte*Parameter.d_separator/(Parameter.A_Zelle/1e4); % Elektrolytwiderstand in Ohm, Umrechnung cm²->m²

RCT = Parameter.R_CT/L_opt; % Ladungstransferwiderstand in Ohm
Z_CT_var = RCT./(1+(1i*2*pi()*Parameter.f*RCT*Parameter.Q_CT).^Parameter.n_CT); % Ladungstransferimpedanz in Ohm
Z_CT_var_TLM = Z_CT_var * Parameter.d_electrode * L_opt; % Ladungstransferimpedanz bezogen auf die Elektrodendicke in Ohm*m

RDiff = Parameter.R_Diff/L_opt; % Diffusionswiderstand in Ohm
Z_Diff_var = RDiff*tanh((1i*2*pi()*Parameter.f*Parameter.tau_Diff).^Parameter.n_Diff) ./ ...
         (1i*2*pi()*Parameter.f*Parameter.tau_Diff).^Parameter.n_Diff + 1./(1i*2*pi()*Parameter.f*Parameter.C_diff*L_opt); 
         % Diffusionsimpedanz in Ohm (Warburg Impedanz + diff. Kapazität)
Z_Diff_var_TLM = Z_Diff_var * Parameter.d_electrode * L_opt; % Diffusionsimpedanz bezogen auf die Elektrodendicke in Ohm*m

X1 = 1/Parameter.sigma_electrolyte*1/(Parameter.A_Zelle/1e4)*Parameter.tau/Parameter.epsilon; % ionischer Pfad, Umrechnung cm²->m²
X2 = 1/Parameter.sigma_electrode*1/(Parameter.A_Zelle/1e4); % elektronischer Pfad, Umrechnung cm²->m²

kappa_var = ((Z_CT_var_TLM+Z_Diff_var_TLM)/(X1+X2)).^0.5;
Z_TLM_var = ((X1*X2)/(X1+X2))*(Parameter.d_electrode*L_opt+2*kappa_var./sinh(Parameter.d_electrode*L_opt./kappa_var)) + ...
        kappa_var*((X1^2+X2^2)/(X1+X2)).*coth(Parameter.d_electrode*L_opt./kappa_var);

RCC = Parameter.R_CC;
Z_CC = RCC./(1+(1i*2*pi()*Parameter.f*RCC*Parameter.Q_CC).^Parameter.n_CC);

Z_L = 1i*2*pi().*Parameter.f*Parameter.L_Al; 

Z_TLM_var = ((X1*X2)/(X1+X2))*(Parameter.d_electrode*L_opt+2*kappa_var./sinh(Parameter.d_electrode*L_opt./kappa_var))...
    +((X1^2+X2^2)/(X1+X2))*kappa_var.*coth(Parameter.d_electrode*L_opt./kappa_var);

Z_Gesamt = R_electrolyte + Z_TLM_var + Z_CC + Z_L;

Re_end = real(Z_Gesamt(1));
result = Re_end;