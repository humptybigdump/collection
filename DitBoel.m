function alpha=DitBoel(p,T,M,A,dH,rough)
% Dittus Boelter correlation for single-phase heat transfer
% Thomas Schulenberg Dec. 2020
%
% Input data
% p  pressure [bar]
% T  bulk temperature [°C]
% M  mass flow [kg/s]
% A  flow cross section [m2]
% dH hydraulic diameter [m]
% rough  surface roughness [m]
%
% Output
% alpha  heat transfer coefficient [W/m2K]
 visc=XSteam('my_pT',p,T);
 Re=max(abs(M*dH/(A*visc)),0.001);
 cp=1000*XSteam('cp_pT',p,T);
 lamda=XSteam('tc_pT',p,T);
 Pr=visc*cp/lamda;
 if Re<2000 
     Nu=3.66;
 else
     Nu=0.023*Re^0.8*Pr^0.4;
     if rough>0
         Nu=Nu*(1.87+0.54*log10(1000*rough/dH));
    end
 end
 alpha=Nu*lamda/dH;
 if isnan(alpha)
     error ('single-phase heat transfer coeff. is NaN')
 end
 return;