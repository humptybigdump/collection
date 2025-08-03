function [x,info]=Lagr23
% function Lagr23: Find the chemical equilibrium composition in an adiabatic, reacting 
% ideal gas mixture by maximization of entropy under the constraints 
% of energy and element conservation, and mechanical constraints, 
% using the method of Lagrangian multipliers 

% define some natural constants and standard values 
RGAS=8.3145; 
PREF=1.013e5; 
TREF=298.15;

% specieslist for the ideal gas mixture  
sp=create_specieslist('H2 N2 NH3','H N');
NX=sp.nspe+2; % number of state variables: sp.nspe (for species moles) + 2 (temperature, pressure) 
NC=sp.nele+2; % number of constraints: elements, energy conservation, mechanical constraint 

% code for testing 
ntest=[3;2;1e-8];
Ttest=400;
ptest=5e5;
xtest=[ntest;Ttest;ptest];
[Hmix,cpmix]=H_x(xtest);
HFIX=Hmix;
pFIX=ptest;
NeleFIX=sp.elenum'*ntest;

%jacobianest(@(x) G_x(x),xtest);
%dGdx_x(xtest);

jacobianest(@(x) S_x(x),xtest);
dSdx_x(xtest)';

% determine a reasonable first guess ltest for the Lagrangian Multipliers.
ltest=-dGdx_x(xtest)'\dSdx_x(xtest);

% test the residual function 
LagrRes([xtest;ltest]);

% parameters for the solver NLEQ1
xscal=abs([xtest;ltest]);
rtol=1e-4;
iopt=[];
par=[];
wk=[];

% call solver 
[Y,info,wk] = nleq1(@LagrResNLEQ1,[xtest;ltest],xscal,rtol,iopt,par,wk);
[info.ierr Y(1:NX)']
%fsolve(@(Y) LagrRes(Y),[xtest;ltest])

	function [res,ifail]=LagrResNLEQ1(Y,flag,par)
		x=Y(1:NX);
		l=Y(NX+(1:NC));
		res=Y-Y;
		ifail=0;
		if any(x<0),ifail=1;return;end
		res=LagrRes(Y);
	end
	function r=LagrRes(Y)
		x=Y(1:NX);
		l=Y(NX+(1:NC));
		r=zeros(size(x));
		r(1:NX)=dSdx_x(x) + dGdx_x(x)'*l;
		r(NX+(1:NC))=G_x(x);
	end

	function [Hmix,cpmix]=H_x(x)
		% compute the mixture enthalpy and heat capacity at constant pressure with respect to x  
		% x: [n1(mol),..,n3(mol),T(K),p(Pa)]
		ni=x(1:sp.nspe);
		T=x(sp.nspe+1);
		[hi,cpi]=calc_h_cp(T,sp.thcf); % hi: J/mol, cpi: J/(mol*K)
		Hmix=ni'*hi;
		cpmix=ni'*cpi;
	end
	function S=S_x(x)
		ni=x(1:sp.nspe);
		T=x(sp.nspe+1);
		p=x(sp.nspe+2);
		[hi,cpi,si]=calc_h_cp(T,sp.thcf); % hi: J/mol, cpi:J/(mol*K)
		molfrac=ni/sum(ni);
		p_i=molfrac*p; 
		k=find(molfrac>1e-99);
		si(k)=si(k) - RGAS*log(p_i(k)/PREF);
		S=ni'*si;
	end
	function dSdx=dSdx_x(x)
		% compute the partial derivatives of mixture entropy with respect to x  
		% x: [n1(mol),..,n3(mol),T(K),p(Pa)]
		ni=x(1:sp.nspe);
		T=x(sp.nspe+1);
		p=x(sp.nspe+2);
		[hi,cpi,si]=calc_h_cp(T,sp.thcf); % hi: J/mol, cpi:J/(mol*K)
		molfrac=ni/sum(ni);
		p_i=molfrac*p;
		k=find(ni>1e-99);
		si(k)=si(k) - RGAS*log(p_i(k)/PREF);
		dSdx=[si;ni'*cpi/T;-RGAS*sum(ni)/p];
	end
	function G=G_x(x)
		% compute the constraint functions G(x).
		% There are sp.nspe+2 constraints. They result from 
		%  * energy conservation 
		%  * mechanical constraints (e.g., p=const. or V=const.) 
		%  * element conservation (one constraint equation per element)
		% x: [n1(mol),..,n3(mol),T(K),p(Pa)]
		ni=x(1:sp.nspe);
		T=x(sp.nspe+1);
		p=x(sp.nspe+2);
		[hi,cpi,si]=calc_h_cp(T,sp.thcf); % hi: J/mol, cpi:J/(mol*K)
		G=[ni'*hi-HFIX ; p-pFIX  ; sp.elenum'*ni-NeleFIX];
	end
	function dGdx=dGdx_x(x)
		% compute the partial derivatives dGdx of the constraint functions G(x)
		% defined in function G_x 
		% dGdx is represented by a matrix that has nx columns and sp.nspe+2 rows  
		% x: [n1(mol),..,n3(mol),T(K),p(Pa)]
		ni=x(1:sp.nspe);
		T=x(sp.nspe+1);
		p=x(sp.nspe+2);
		[hi,cpi,si]=calc_h_cp(T,sp.thcf); % hi: J/mol, cpi:J/(mol*K)
		dGdx=[
			hi'               ni'*cpi     0
			zeros(1,sp.nspe)  0           1
			sp.elenum'        zeros(2,1)  zeros(2,1) 
			];
	end

end