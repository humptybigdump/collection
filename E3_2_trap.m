clear all
close all
clc
%
% Exercise 3.2:
% compute the integral of the following function
% between x0 and xn
%----------------------
f=@(x) 0.2 + 25*x - 200*x.^2 + 675*x.^3 - 900*x.^4 + 400*x.^5;
x0=0;
xn=0.8;

% true solution 
exact = 1.640533333333333;  

% use np = 11 equally-spaced sample points
np=11;

% generate the sample points (xs,ys)
% xs = single(linspace(x0,xn,np)); % using single precision
xs = linspace(x0,xn,np);         % default is double precision

% compute function's values at sample points
ys = (f(xs));

% compute area under curve
% use your self written function e.g. let's call it mytrap 
% (here, function mytrap is placed at the end of this script.
%  usually, functions are saved as a separate m-file).
A = mytrap(xs,ys)

% use built-in
matlab_output = trapz(xs,ys) 

A==matlab_output

%------------------------------------------
function A = mytrap(xs,ys)
% trapezoidal method
%
% compute width of one segment,
% assuming equally spaced data-points!!
h = (xs(end)-xs(1))/(length(xs)-1); 

%compute area under curve
A = (ys(1)+ys(end)+2*sum(ys(2:end-1)))*h/2;

% Alternatively, ...
% using a for loop to sum, but....
% this is not efficient in matlab !!!
% A = 0;
% for i=2:length(xs)-1
%     A = A + ys(i);
% end
% A = (ys(1)+ys(end)+2*A)*h/2;
end

