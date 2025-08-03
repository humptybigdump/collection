function [t,y]=GasCompress(rpm,alpha,Vscal,AH,t0H,tauH)
% Vp = Vpfix(t) (Vpfix(t) gegeben) 
% Tp=f1(t,[V,T,p,U])
% pp=f2(t,[V,T,p,U]) 
% Up=f3(t,[V,T,p,U])
% Insgsamt also:  yp=f(t,y), y(0)=y0 y=[V,U,T,p]
RGAS=8.3145; MM=29e-3; 
R=RGAS/MM;  cv=2.5*R; 

rpm=1e3;
fV=@(t) Vscal*(1.2+cos(2*pi*rpm/60*t));  % m^3
fVp=@(t) Vscal*(-2)*pi*rpm/60*sin(2*pi*rpm/60*t);

V0=fV(0);
T0=300; % K
p0=1e5; % Pa
U0=0;   % J
m=p0*V0/(R*T0);
TWALL=T0; 
% 
%alpha=5*10^4; % convective heat transfer coefficient in W/(m^2*K) 
%
W0=0;Q0=0;QH0=0;
y0=[V0;U0;p0;T0;W0;Q0;QH0];
opts=odeset('reltol',1e-6);
%[t,y] = ode23(@compressode,[0,60/rpm],y0,opts);
%plot(t,y,'o-',t,fV(t),'r+'); 

yp0=compressode(0,y0);
[t,y] = ode15i(@compressDAE,[0,60/rpm],y0,yp0,opts);


	function yp=compressode(t,y)
		% y=[V;U;p;T]
		V=y(1);U=y(2);p=y(3);T=y(4);W=y(5);Q=y(6);QH=y(7);
		Vp=fVp(t);
		QpH=AH*exp(-((t-t0H)./tauH).^2);
		A=V.^(2/3);
		Up=-p*Vp + alpha*A*(TWALL-T) + QpH;  % yp(2)= -y(3)*yp(1);
		Tp=Up/(m*cv);
		pp=p*(Tp/T - Vp/V);
		Wp=-p*Vp;
		Qp=alpha*A*(TWALL-T);
		yp=[Vp;Up;pp;Tp;Wp;Qp;QpH];
		
		% pp/p   Tp/T 
		% pp/p = Tp/T - Vp/V;
	end

	function res=compressDAE(t,y,yp)
		% y=[V;U;p;T]
		V=y(1);U=y(2);p=y(3);T=y(4);W=y(5);Q=y(6);QH=y(7);
		Vp=yp(1);Up=yp(2);pp=yp(3);Tp=yp(4);Wp=yp(5);Qp=yp(6);QpH=yp(7);
		res=zeros(size(y));
		res(1)=V-fV(t); % Vp-fVp(t);
		res(2)=QpH-AH*exp(-((t-t0H)./tauH).^2);
		A=V.^(2/3);
		res(3)=Up+p*Vp - alpha*A*(TWALL-T) - QpH;  % yp(2)= -y(3)*yp(1);
		res(4)=Tp-Up/(m*cv);
		res(5)=p*V-m*R*T;  %pp-p*(Tp/T - Vp/V);
		res(6)=Wp+p*Vp;
		res(7)=Qp-alpha*A*(TWALL-T);
	end


end





