function dpdzR=PLCorr(type,M,x,A,dH,rough,rhoL,rhoG,viscL,viscG,st)
% Frictional Pressure Drop Correlations
%  Thomas Schulenberg, Nov. 2020
%  
%  dpdzR: frictional pressure gradient of a two-phase flow [Pa/m]
%       
%   Input Parameters
% type: type of pressure loss correlation
% M: mass flow [kg/s]
% x: steam mass fraction [-]
% A: flow cross section [m2]
% dH: hydraulic diameter [m]
% rough: surface roughness [m]
% rhoL: saturated liquid density [kg/m3]
% rhoG: saturated steam density [kg/m3]
% viscL: saturated liquid viscosity [Pa s]
% viscG: saturated steam viscosity [Pa s]
% st: surface tension [N/m]
 
switch type
%--------------------------------------------------------------------------
    case 1
    % Friedel (1979) in: Y. Xu et al. Nucl.Eng.Des. 253 (2012) 86-97
    rhoH=1/(x/rhoG+(1-x)/rhoL);
    Re0=abs(M*dH/A);
    ReL=max(Re0/viscL,0.001);
    ReG=max(Re0/viscG,0.001);
    if ReL<1055
        fL=64/ReL;
    else
        fL=1/(0.86859*log(ReL/(1.964*log(ReL)-3.8215)))^2;
    end
    if ReG<1055
        fG=64/ReG;
    else
        fG=1/(0.86859*log(ReG/(1.964*log(ReG)-3.8215)))^2;
    end
    We=max(M^2*dH/(rhoH*st*A^2),1E-16);
    FrL=max((1-x)^2*M^2/(9.81*dH*rhoH^2*A^2),1E-16);
    Ax=(1-x)^2+x^2*(rhoL*fG/(rhoG*fL));
    PhiLo2=3.24*x^0.78*(1-x)^0.224*(rhoL/rhoG)^0.91*(viscG/viscL)^0.19;
    PhiLo2=PhiLo2*(1-viscG/viscL)^0.7/FrL^0.045/We^0.035+Ax;
    if x<=0
        dpdzR=fL*M^2/(2*dH*rhoL*A^2);
    elseif x>=1
        dpdzR=fG*M^2/(2*dH*rhoG*A^2);
    else
        dpdzR=PhiLo2*fL*M^2/(2*dH*rhoL*A^2);
    end
    return;
    
    case 2
    % Müller-Steinhagen and Heck (1986)    
    Re0=abs(M*dH/A);    
    ReL=max(Re0/viscL,0.001);
    ReG=max(Re0/viscG,0.001);
    if ReG <= 1187
        fG=64/ReG;                                %laminar
    else
        fturb=0.309/(log10(ReG/7))^2;             % turbulent smooth
        frough=1/(2*log10(3.71*dH/rough))^2;      % turbulent rough
        fG=max(fturb,frough);
    end
    if ReL <= 1187
        fL=64/ReL;                                %laminar
    else
        fturb=0.309/(log10(ReL/7))^2;             % turbulent smooth
        frough=1/(2*log10(3.71*dH/rough))^2;      % turbulent rough
        fL=max(fturb,frough);
    end
    Y2=fG/fL*rhoL/rhoG;
    PhiLo2=Y2*x^3+(1-x)^(1/3)*(1+2*x*(Y2-1));
    if x<=0
        dpdzR=fL*M^2/(2*dH*rhoL*A^2);
    elseif x>=1
        dpdzR=fG*M^2/(2*dH*rhoG*A^2);
    else
        dpdzR=PhiLo2*fL*M^2/(2*dH*rhoL*A^2);
    end
    return;
end
end



    