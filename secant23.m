function [a,b,iter]=secant23(f,a0,b0,dX)
% function [a,b,iter]=secant23(f,a0,b0,deltaX) 
% Attempt to solve f(x)=0 by a secant method.
% Inputs: 
% f:    a handle to a function f(x) of which the solution f(x)=0 is seeked. 
% a,b:  initial guesses for the interval sich that a<=x<=b
% dX: a strictly positive number, target size of the interval for x 
% Outputs:
% a,b:  Final points of the secant after iteration.
%       On success, a,b fulfill abs(b-a)<dX, and b is the
%       approximate solution of f(x)=0  
% iter: if iter>0: number of iterations taken to make abs(b-a)<dX
%       if iter<0: error indicator signifiying (-iter) steps were taken
%       without fulfilling abs(b-a)<dX
%
% Code is for didactic purposes only, no claims on efficiency, 
% robustness, functionality involved.  
MAXNITER=60;
a=a0;b=b0;
for iter=1:MAXNITER 
	disp([iter a b abs(b-a) [f(b) f(a)]]);
	c=b-(b-a)/(f(b)-f(a))*f(b);
	a=b;b=c;
	if abs(b-a) < dX,break;end
end
if iter==MAXNITER,iter=-iter;end

