function [b,iter]=Newton23(f,fp,x0,dX,tolF)
% function [b,iter]=Newton23(f,a0,b0,dX) 
% attempt to solve f(x)=0 by Newton's method.
% Inputs: 
% f: a handle to a function f(x) of which the solution f(x)=0 is seeked. 
% fp: a handle to the derivative function df/dx 
% x0: initial guess for the solution 
% dX: a strictly positive number, target size of the interval for x 
% Outputs:
% a,b: Final interval. On success, a,b fulfill abs(b-a)<deltaX, and the
% solution of f(x)=0 is within [a,b].
% iter: if iter>0: number of iterations taken to make abs(b-a)<dX
%       if iter<0: indicator signifiying that (-iter) steps were taken
%       without fulfilling abs(b-a)<dX
%
% Code is for didactic purposes only, no claims on efficiency, 
% robustness, functionality involved.  

a=x0;
MAXNITER=60;
for iter=1:MAXNITER
	fa=f(a);
	fpa=fp(a);
	b=a-fa/fpa;
	disp([iter a b abs(b-a) abs(f(b))])
	if abs(b-a) < dX,break;end % loop termination criterion based on change of x 
	a=b;
end
if iter==MAXNITER,iter=-iter;end

