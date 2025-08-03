function [xs,iiter]=Newton22(f,fp,x0,tolx,MAXNITER)
% function [xs,iiter]=Newton22(f,fp,x0,tolx,MAXNITER) 
% NEWTON's method for the soution of a function F(x)=0, starting at guess
% solution x0.
% NOTE: This code is not optimized for performance
% INPUTS: 
%   f,fp: function handles to the function F and its derivative dFdx, respectively  
%   x0: initial guess for the solution
%   tolx: positive termination tolerance on x  
%   MAXNITER: Upper limit for the number of NEWTON-iterations. Optional, default=50 
% OUTPUTS:
%   xs: numerical solution of F(x)=0
%   iiter: the number of NEWTON-iteration steps taken  

if nargin<5,MAXNITER=50;end

xs=[];
xo=x0;
for iiter=1:MAXNITER % main loop for NEWTON iteration
	d=-f(xo)/fp(xo); % what if fp(xo) evaluates to 0? One should check this first (homework)
	for j=1:MAXNITER % inner loop for stepsize reductions 
		xtry=xo+d;
		if abs(f(xtry))>=abs(f(xo)) % stepsize reduction required? 
			d=d/2;
		else
			xn=xtry;
			break
		end
	end
	fprintf('%d %20.12e %20.12e\n',iiter,xo,xn);
	if abs(xn-xo)<tolx,xs=xn;break;end
	xo=xn;
end

