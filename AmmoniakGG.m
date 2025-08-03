% GG der Reaktion  N2 + 3 H2 = 2 NH3
% adiabatische Bedingungen => GG dort, wo S =! max 
% Zustandsvektor x=(p,T,(Ni,i=1,..3)), zu optimierende Zielfunkltion: S = S(x)
% Nebenbedingungen: 
%   U(x) - U0 = 0                   \
%   Elementerhaltung  NA(x)-NA0=0    |  G(x) : IR^5 -> IR^4  
%   ZGL f(p,V,T) = 0  p*V-N*R*T = 0     /

% Lagrange-Multiplikatoren 
% dS/dx + L * dG/dx  = 0    \sum_i L_i*dGdx_ij/dx_j 
% G(x) = 0 

function AmmoniakGG
RGAS=8.3145; 
pREF=1.013e5;
T0=300;p0=1e5;Ni0=[1 3 1e-1]';
x0=[p0;T0;Ni0] 
sp=create_specieslist('N2 H2 NH3','N H');
V0=sum(Ni0)*RGAS*T0/p0;

NX=numel(x0);
NC=2+sp.nele;

[ui,si,cpi,cvi,NA]=calth(x0);
NA0=NA;
U0=Ni0'*ui;

% Näherungsweise Berechung der Jacobi-Matrizen dS/dx und  dG/dx über finite
% Differenzenquotienten
S0=S(x0);G0=G(x0);
h=[.00001;.001;0.0000001*ones(sp.nspe,1)];
JS=zeros(1,NX); 
JG=zeros(NC,NX);
for i=1:numel(x0)
	xo=x0(i);
	x0(i)=x0(i)+h(i);
	Si=S(x0);Gi=G(x0);
	JS(:,i)=(Si-S0)/h(i);
	JG(:,i)=(Gi-G0)/h(i);
	x0(i)=xo;
end
[dSdx(x0); JS]
[dGdx(x0); JG]

% Ermittlung einer "vernünftigen" Anfangslösung L0 für die Lagrange Multiplikatoren 
% aus der Anfangslösung für x0 
L0 = -(dGdx(x0))'\dSdx(x0)'
% Test-Aufruf der Residuumsfunktion resLagr 
xL0=[x0;L0]
resLagr(xL0);

% Lösung von resLagr=0 durch nleq1
iopt=[];par=[];wk=[];rtol=1e-4;
xscal=abs(xL0);xscal(xscal<1e-9)=1;

[x,info,wk]=nleq1(@nlqfun,xL0,xscal,rtol,iopt,par,wk)

	function [res,ifail]=nlqfun(x,flag,par)
		ifail=0;
		if any(x(1:NX)<0),ifail=1;res=[];return;end
		res=resLagr(x);
	end

	function res=resLagr(xL)
		x=xL(1:NX);L=xL(NX+(1:NC));
		res=zeros(NX+NC,1);
		res(1:NX)=dSdx(x)' + (dGdx(x))'*L;
		res(NX+(1:NC)) = G(x);
	end
	function [ui,si,cpi,cvi,NA]=calth(x)
		p=x(1);T=x(2);Ni=x(3:NX); 
		[hi,cpi,si]=calc_h_cp(T,sp.thcf);
		ui=hi-RGAS*T;
		cvi=cpi-RGAS;
		gamma=Ni/sum(Ni);
		pi=gamma*p;
		si = si - RGAS*log(pi/pREF);
		NA=sp.elenum'*Ni;
	end
	function res=S(x)
		p=x(1);T=x(2);Ni=x(3:NX);
		[ui,si,cpi,cvi,NA]=calth(x);
		res=Ni'*si;
	end
	function res=dSdx(x)
		p=x(1);T=x(2);Ni=x(3:NX); 
		res=zeros(1,NX);
		[ui,si,cpi,cvi,NA]=calth(x);
		res(1)=-sum(Ni)*RGAS/p;
		res(2)=(Ni'*cpi)/T;
		res(3:NX)=si;
	end
	function res=G(x)
		p=x(1);T=x(2);Ni=x(3:NX); 
		res=zeros(NC,1);
		[ui,si,cpi,cvi,NA]=calth(x);
		res(1) = Ni'*ui - U0;   % \sum_i N_i*u_i  
		res(2:(2+sp.nele-1)) = NA - NA0;    % \sum_i elenum_ij * N_i 
		res(NC)=p*V0 - sum(Ni)*RGAS*T;
	end
	function res=dGdx(x)
		p=x(1);T=x(2);Ni=x(3:NX); 
		res=zeros(NC,NX);
		[ui,si,cpi,cvi,NA]=calth(x);
		res(1,2)=Ni'*cvi; res(1,3:NX)=ui';
		res(2:(2+sp.nele-1),3:NX)=sp.elenum';
		res(NC,1)=V0;res(NC,2)=-sum(Ni)*RGAS;res(NC,3:NX)=-RGAS*T;
	end
end