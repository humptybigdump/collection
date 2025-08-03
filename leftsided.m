clear all
close all
clc

% Example:
% Compute the integral of e^{x} between x0=0 and xn=1
% using left-sided rectangle and mid-point rule
% true (analytical) solution = 1.718281828459045..;
% (a) using only 5 equally-spaced data points and
% (b) using 50 equally-spaced data points
% how much are the absolute and relative errors?
% try with single and double precision

f=@(x) exp(x);
x0=0;
xn=1;
true = 1.718281828459045;

% to show the true function's value
% let's generate an x vector with fine resolution
x=linspace(x0,xn,200);
y=f(x);
figure
plot(x,y,'b')
hold on

% generate some sample points.
% you can use linspace to generate
% nn equally spaced data points between x0 and xn
nnn = [5 50 500 5000];
for k=1:length(nnn)
    nn = nnn(k);
    xs=linspace(x0,xn,nn);
    xs=single(xs);
    ys=f(xs);
    %plot(xs,ys,'or')
    
    h(k) = (xn-x0)/(nn-1);
    
    % compute area under the curve
    areaLS(k) = sum(ys(1:end-1))*h(k);
    
    % compute the total numerical error
    Eabs(k) = abs(areaLS(k) - true)
    
    % truncation error 
    fd1m = true;
    Et (k) = fd1m / 2 * h(k)
end












