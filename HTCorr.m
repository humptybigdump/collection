function qW=HTCorr(M,p,h,T,TW,dH,A,rough,hL,hG,rhoL,rhoG,viscL,st)
%  Forced convection heat transfer of a single or two-phase flow
%  including supercritical water
%  for heated walls only
%
%  Thomas Schulenberg, Dec. 2020 
%
%    Output:
%  qW: heat flux > 0 [W/m2]
%       
%   Input Parameters
% M: fluid mass flow [kg/s]
% p: fluid pressure [bar]
% h: fluid enthalpy [J/kg]
% eps: void fraction [-]
% T: bulk fluid temperature [°C]
% TW: wall surface temperature [°C]
% dH: hydraulic diameter [m]
% A: flow cross section [m2]
% rough: surface roughness [m]
% hL: saturated liquid enthalpy  [J/kg]
% hG: saturated steam enthalpy  [J/kg]
% rhoL: saturated (or supercritical) liquid density  [kg/m3]
% rhoG: saturated steam density  [kg/m3]
% viscL; saturated liquid viscosity [Pa s]
% st: surface tension [N/m]
%
G=M/A;       % mass flux [kg/m2s]

% cooled walls not yet included in function HTCorr
if TW <= T
    qW=0;
    return;
end

pcrit=220.6395;      % critical pressure [bar]
if p==pcrit
    p=pcrit-0.001;
end

%  supercritical pressure
if p>pcrit
    % transcritical look-up table of Zahlan (2015)
    if p<=300 && G<=5000 && G>=100 && h/1000<=3000 && h/1000>=1000 ...
            && (TW-T)<= 500 && (TW-T)>=10
        alpha=TCHT(p,G,h/1000,TW-T,dH);
        qW=alpha*(TW-T);
        return;
    else
        % Dittus-Boelter correlation
        alpha=DitBoel(p,T,M,A,dH,rough);
        qW=alpha*(TW-T);
        return;
    end
end

% Subcritical pressure
x=(h-hL)/(hG-hL);
Tsat=XSteam('Tsat_p',p);

% Sub-critical single-phase heat transfer
if TW<Tsat || T>Tsat
    if (p>215 && x>=1 && G<=5000 && G>=100 && h/1000<=3000 && h/1000>=1000 ...
            && (TW-T)<= 500 && (TW-T)>=10)
    % transcritical look-up table of Zahlan (2015)
        alpha=TCHT(p,G,h/1000,TW-T,dH);
        qW=alpha*(TW-T);
        return;
    end
    % Dittus-Boelter correlation
    alpha=DitBoel(p,T,M,A,dH,rough);
    qW=alpha*(TW-T);
    return;
end

% Two-phase flow heat transfer

% Leidenfrost temperature
TLF=LFTemp(Tsat);

% Film boiling heat transfer
if TW>TLF
    if p>215
    % transcritical look-up table of Zahlan (2015)
       alpha=TCHT(p,G,h/1000,TW-T,dH);
       qW=alpha*(TW-T);
    elseif p<=215 && p>=200
       alpha1=TCHT(215,G,h/1000,TW-T,dH);
       qW1=alpha1*(TW-T);
       alpha2=FBHT(199.999,G,x,TW-Tsat,dH);
       qW2=alpha2*(TW-Tsat);
       qW=((p-200)*qW1+(215-p)*qW2)/15.;
    elseif p<200
    % film boiling look-up table of Groeneveld
       alpha=FBHT(p,G,x,TW-Tsat,dH);
       qW=alpha*(TW-Tsat);
    end
    return;
end

% Nucleate boiling heat transfer

% Critical heat flux (CHF)
qCHF=CHF(p,M/A,x,dH);

% Rohsenow factor
cpL=1000*XSteam('CpL_p',p);
lamdaL=XSteam('tcL_p',p);
PrL=viscL*cpL/lamdaL;
Csf=0.012/(rough*1E6)^0.1;
FRsn=(cpL/(Csf*(hG-hL)))^3*viscL*(hG-hL)/PrL^5.1*((rhoL-rhoG)*9.81/st)^0.5;

% CHF wall temperature
TWCHF=Tsat+(qCHF/FRsn)^0.3333;

if TW>=Tsat && TW<TWCHF
    qWSNB=(TW-Tsat)^3*FRsn;
    % sub-cooled boiling heat transfer
    Tb=min(T,Tsat-1);
    alphaSP=DitBoel(p,Tb,M,A,dH,rough);
    qWSP=alphaSP*(TW-T);
    qW=(qWSP^2+qWSNB^2)^0.5; 
    return;
end

% Transition boiling
if p>215
% transcritical look-up table of Zahlan (2015)
   alpha=TCHT(p,G,h/1000,TLF-T,dH);
   qLF=alpha*(TLF-T);
elseif p<=215 && p >=200
   alpha1=TCHT(215,G,h/1000,TLF-T,dH);
   qLF1=alpha1*(TLF-T);
   alpha2=FBHT(199.999,G,x,TLF-Tsat,dH);
   qLF2=alpha2*(TLF-Tsat);
   qLF=((p-200)*qLF1+(215-p)*qLF2)/15.;
elseif p<200
% film boiling look-up table of Groenveld
   alpha=FBHT(p,G,x,TLF-Tsat,dH);
   qLF=alpha*(TLF-Tsat);
end
n1=log((TLF-Tsat)/(TW-Tsat))/(log((TLF-Tsat)/(TWCHF-Tsat)));
qW=qLF*(qCHF/qLF)^n1;
if isnan(qW) 
    error ('transition boiling qW is NaN')
end
return;

