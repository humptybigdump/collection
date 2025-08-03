function eps=VoidCorr(type,M,x,p,rhoG,rhoL,viscL,st,dH,A,phi)
% Void Correlations
%  T. Schulenberg, Nov. 2020
%
%  eps: void fraction [-]
%       
%   Input Parameters
% 
% M: fluid mass flow [kg/s]
% x: steam mass fraction [-]
% p: pressure [Pa]
% rhoG: steam (gas) density [kg/m3]
% rhoL: liquid density [kg/m3]
% viscL: liquid viscosity [Pa m]
% st: surface tension [N/m]
% dH: hydraulic diameter [m]
% A: flow cross section [m2]
% phi: Inclination angle [-]
%
if x<=0
    eps=0;
    return;
elseif x>=1
    eps=1;
    return;
end

switch type
%--------------------------------------------------------------------------
% Homogeneous equilibrium model
%--------------------------------------------------------------------------
    case 1
        eps=1/(1+(1-x)/x*rhoG/rhoL);
        return;
%--------------------------------------------------------------------------
% Empirical void correlation of Premoli et al. (1971)
%--------------------------------------------------------------------------
    case 2
       Re=max(abs(M*dH/(A*viscL)),0.001);
       We=abs(M^2*dH/(st*rhoL*A^2));
       E1=1.578/Re^0.19*(rhoL/rhoG)^0.22;
       E2=0.0273*We/Re^0.51/(rhoL/rhoG)^0.08;
       beta=min(x*rhoL/(x*rhoL+(1-x)*rhoG),0.9999);
       y=beta/(1-beta);
       SR=1+E1*(max(y/(1+y*E2)-y*E2,0.))^0.5;
       eps=x/(x+SR*(1-x)*rhoG/rhoL);
       return;
%--------------------------------------------------------------------------
% Empirical void correlation of Woldesemayat und Ghajar (2007)
% -------------------------------------------------------------------------
    case 3
        UG=x*M/A/rhoG;
        UL=(1-x)*M/A/rhoL;
        N1=2.9*(9.81*dH*st*(1+cos(phi))*(rhoL-rhoG)/rhoL^2)^0.25;
        N2=1.22*(1+sin(phi))^(1E5/p);
        eps=UG/(UG*(1+(UL/UG)^((rhoG/rhoL)^0.1))+N1*N2);
        return;
end
    
        
