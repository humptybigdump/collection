% -----
% Exercise 2.3: Newton's finite divided-difference method 
% -----
clear all
close all
clc

format long
%
% Suppose we want to approximate the function 
%        f(x)=exp(x)
% at x=0.45 and suppose we only have data points 
% at x=[0:0.1:1];
% Use the Newton's divided difference method
% and determine up to which degree of polynomial is required
% to obtain a result with an absolute error smaller than 0.0001
% Now do the same but change the order of x to 
% x=[0.3 0.4 0.5 0.6 0.1 0.2 0.8 0.9 1];

% function's handle
f=@(x)exp(x); 

% generate the sample points 
x=[0:0.1:1];
x=[0.3 0.4 0.5 0.6 0.1 0.2 0.8 0.9 1 0 0.7];
y=f(x);

% define the interpolation point
xinterp=0.45; 

%------------
% start of Newton's interpolation
%tic
% count the number of known data points
k=length(x); 

% compute the zeroth-finite difference
% i.e. fill in first column of table
for i=1:k
    fd(i,1)=y(i);
end

% loop for computing the nth finite difference
% fd(i,j) 
for j=2:k
    for i=j:k
        fd(i,j)=(fd(i-1,j-1)-fd(i,j-1))/(x(i-j+1)-x(i));
    end
end

% the diagonals are the polynomial coefficients 
b = diag(fd); 

% Evaluate the value at one interpolation point
xproduct(1)=1;
yinterp(1)=fd(1); %interp. value using zero-order
for j = 2 : k %interp. value using order 1 to n=(k-1)
   xproduct = xproduct .* (xinterp-x(j-1));
   yinterp(j)=yinterp(j-1) + b(j)*xproduct;
   approx_error(j)=abs(yinterp(j)-yinterp(j-1));
end
true_error=abs(yinterp-f(xinterp));
%telapsed=toc
% end of Newton's interpolation
%------------
%
%------------
% show result
disp('yinterp, true error, approximation error =')
[yinterp' true_error' approx_error']

figure
subplot(1,2,1)
line([1 k]-1,[f(xinterp) f(xinterp)]); %true solution
hold on
plot([1:k]-1,yinterp,'o-'); %approximation
set(gca,'yscale','log')
xlabel('n-th degree polynomial')
ylabel('solution')
legend('true','approximation')

subplot(1,2,2)
plot([1:k]-1,approx_error);
hold on
plot([1:k]-1,true_error);
set(gca,'yscale','log')
xlabel('n-th degree polynomial')
legend('E_T','\epsilon_a')



figure
plot(x,y,'o-')
hold on
plot(xinterp, yinterp,'ok','MarkerFaceColor','m');