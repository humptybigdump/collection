function [t,y]=CompressorODE
RGAS=8.3145;
MM=29e-3; 
V0=0.00055; U0=0; T0=300; p0=1e5; 
fVp=@(t) -0.00025*sin(t)
y0=[V0;U0;T0;p0];
R=RGAS/MM;
m=p0*V0/(R*T0);
cv=2.5*R;

[t,y]=ode45(@fode,[0,2*pi],y0,odeset('reltol',1e-8));

	function yp=fode(t,y)
		V=y(1);U=y(2);T=y(3);p=y(4);
		Vp=fVp(t);
		Up=-p*Vp;
		%[h,cp,s]=calc_h_cp()
		Tp=Up/(m*cv);
		pp=p*(Tp/T-Vp/V);
		yp=[Vp;Up;Tp;pp];
	end
end