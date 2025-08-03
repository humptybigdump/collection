function mcrit=CMFlux(model,p0,h0,pE,zeta)
% Critical mass flux
% T. Schulenberg, Nov. 2020
% Input: model=1  homogenous frozen model
%        model=2  homogeneous equilibrium model (horizontal pipe, no
%        friction)
%        p0=stagnation inlet pressure [bar]
%        h0=stagnation inlet enthalpy [kJ/kg]
%        pE=exit pressure [bar]
%        zeta=pressure loss coefficient at inlet
% Output: critical mass flux [kg/m2s]

switch model
    % Homogeneous frozen model
    case 1
        % Stagnation conditions
        rhoG0=XSteam('rhoV_p',p0);
        rhoL0=XSteam('rhoL_p',p0);
        hL0=XSteam('hL_p',p0);
        hG0=XSteam('hV_p',p0);
        x0=(h0-hL0)/(hG0-hL0);
        cp=XSteam('cpV_p',p0);
        cv=XSteam('cvV_p',p0);
        aG0=XSteam('wV_p',p0);
        aL0=XSteam('wL_p',p0);
        kappa=cp/cv;
        
        % critical conditions
        rhoL=rhoL0;
        rhoG=rhoG0*(2/(kappa+1))^(1/(kappa-1));
        aL=aL0;
        aG=aG0*(2/(kappa+1))^0.5;
        if x0<=0
            mcrit=aL*rhoL;
        else
            mcrit=(1/(rhoL^2*aL^2)+x0*(1/(rhoG^2*aG^2)-1/(rhoL^2*aL^2)))^(-0.5);
        end
        % limitation by Bernoulli equation for pure liquid flow
        mmax=(2*rhoL*(p0-pE)*1E5/(1+zeta))^0.5;
        mcrit=min(mcrit,mmax);
        return
        
    % Homogeneous equilibrium model
    case 2
        % Stagnation conditions
        s0=XSteam('s_ph',p0,h0);
        % find pressure [bar] at which sL=s0
        p1=-6.095287*s0^5+75.256375*s0^4-350.394581*s0^3+797.533901*s0^2 ...
        -885.983426*s0+383.2846851;
        if p1<pE          % no choking in blow-down pipe
            rhoL=XSteam('rhoL_p',p0);
            mcrit=(2*rhoL*(p0-pE)*1E5/(1+zeta))^0.5;
            return
        end
        
        % Find two-phase critical pressure by bisection method
        p(1)=pE;
        p(2)=min(p0,p1);
        da(1:3)=1000;
        u(1:3)=0;
        x(1:3)=0;
        a(1:3)=0;
        mc(1:3)=0;
        for iter=1:20
            p(3)=(p(1)+p(2))/2.;
            for k=1:3
                rhoL=XSteam('rhoL_p',p(k));
                rhoG=XSteam('rhoV_p',p(k));
                sL=XSteam('sL_p',p(k));
                sG=XSteam('sV_p',p(k));
                aL=XSteam('wL_p',p(k));
                x(k)=(s0-sL)/(sG-sL);
                hL=XSteam('hL_p',p(k));
                hG=XSteam('hV_p',p(k));
                aG=XSteam('wV_p',p(k));
                    
                % Determine (dx/dp)s
                sL1=XSteam('sL_p',p(k)+0.001);
                sG1=XSteam('sV_p',p(k)+0.001);
                Ds1=sG1-sL1;
                x1=(s0-sL1)/Ds1;
                sL2=XSteam('sL_p',p(k)-0.001);
                sG2=XSteam('sV_p',p(k)-0.001);
                Ds2=sG2-sL2;
                x2=(s0-sL2)/Ds2;
                dxdp(k)=(x1-x2)/0.002*1E-5;
                    
                % Determine velocities
                h=hL+x(k)*(hG-hL);
                rhoH=1/(x(k)/rhoG+(1-x(k))/rhoL);
                u(k)=(2*(h0-h)*1000)^0.5;
                mc(k)=1/(1/(rhoL^2*aL^2)+x(k)*(1/(rhoG^2*aG^2)-1/(rhoL^2*aL^2)) ...
                        -dxdp(k)*(1/rhoG-1/rhoL))^0.5;
                a(k)=mc(k)/rhoH;
                da(k)=a(k)-u(k);
            end

            if da(1)*da(3)<0
                p(2)=p(3);
                da(2)=da(3);
            else 
                p(1)=p(3);
                da(1)=da(3);
            end
        end
        mcrit=max(mc);
        
        Fehler=max(abs(da))/max(abs(u));
        if Fehler>0.01
            warning ('CMFlux did not converge for HEM');
            display (Fehler);
        end
        return
end
        
        
