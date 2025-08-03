% -----
% Exercise 2.1: Lagrange interpolation 
% -----
% -----
clear all
close all
clc
%
% Suppose we want to approximate the function 
%        f(x)=\frac{1}{1+25*x^2}
% on the interval [-1,1] with an n^{th} degree 
% interpolating polynomial using 
% equally spaced data points in [-1,1]. 
% Approximate the value at x=0.1
% using a 10th and 20th degree polynomial.

% define the function
f=@(x)1./(1+25*x.^2); 

% generate the sample points 
x0 = -1; 
xn = 1; 
n=11;  %!! in this script n=number of data-points (not degree of polynomial!) 
n=21;
x = linspace(x0,xn,n); 
y=f(x);

% define the interpolation point
% 
xinterp=0.25; 
xinterp=0.9;
xinterp=0.85;
%--------------
%function [yinterp]=mylagrange(x,y,n,xinterp)
%computes the value at xinterp using lagrange interpolation
%fn(x)=sum_0^n(Li(x).*f(xi))
%tic
%prepare array to speed-up computation
L=zeros(1,n);
%
for i=1:n
    prod=1;
    for j=1:n
        if j ~= i
            prod=prod*(xinterp-x(j))/(x(i)-x(j))
        end
    end
    L(i)=prod;
end
yinterp=sum(L.*y);
%telapsed=toc
%------------
%
%------------
% show result
L
yinterp
absolute_error = abs(yinterp-f(xinterp))
figure;
plot(x,y,'o-')
hold on
plot(xinterp, yinterp,'ok','MarkerFaceColor','m');