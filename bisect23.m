function [a,b,iter]=bisect23(f,a0,b0,dX)
% function [a,b,iter]=bisect23(f,a0,b0,dX) 
% attempt to solve f(x)=0 by the bisection method.
% Inputs: 
% f: a handle to a function f(x) of which the solution f(x)=0 is seeked. 
% a,b: initial guesses for the interval sich that a<=x<=b
% dX: a strictly positive number, target size of the interval for x 
% Outputs:
% a,b: Final interval. On success, a,b fulfill abs(b-a)<deltaX, and the
% solution of f(x)=0 is within [a,b].
% iter: if iter>0: number of iterations taken to make abs(b-a)<dX
%       if iter<0: indicator signifiying that (-iter) steps were taken
%       without fulfilling abs(b-a)<dX
%
%
% Code is for didactic purposes only, no claims on efficiency, 
% robustness, functionality involved.  

a=a0;b=b0;
fa=f(a);fb=f(b);
MAXNITER=60;
for iter=1:MAXNITER
	disp([iter a b abs(b-a)])
	m=(a+b)/2;
	fm=f(m);
	if sign(fm)==sign(fa)
		a=m;fa=fm;
	elseif sign(fm)==sign(fb)
		b=m;fb=fm;
	end
	if abs(b-a) < dX,break;end
end
if iter==MAXNITER,iter=-iter;end

