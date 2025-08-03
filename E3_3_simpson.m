clear all
close all
clc
format shortg
% compute the integral of the following function
% between x0 and xn
%-----------------------
%%example 1:
f=@(x) 0.2 + 25*x - 200*x.^2 + 675*x.^3 - 900*x.^4 + 400*x.^5;
x0=0;
xn=0.8;
exact=1.640533333333333;
%function derivatives needed for error analysis (Exercise 3, 3c)
fd1=@(x) 25-400*x+675*3*x.^2-3600*x.^3+2000*x.^4;
fd2=@(x) -400+6*675*x-3*3600*x.^2+8000*x.^3; fd2m=(fd1(xn)-fd1(x0))/(xn-x0);
fd3=@(x) 6*675-6*3600*x+24000*x.^2; fd3m=(fd2(xn)-fd2(x0))/(xn-x0);
fd4=@(x) -6*3600+24000*x; fd4m = (fd3(xn)-fd3(x0))/(xn-x0);

%-----------------------
%%example 2:
%f=@(x) exp(1).^x;
%x0=0;
%xn=1;
%exact = 1.718281828459045;
%%function derivatives for error analysis
%%note : for f(x)=exp(1).^x, fd1=fd2=..=fdn=f(x); 
%%so that
%fd1m=(f(xn)-f(x0))/(xn-x0); fd2m=fd1m; fd3m=fd1m; fd4m=fd1m;%this is the mean of the nth derivative
%-----------------------
%

% loop for integration using varying amount of data points np
np = 1+[ 10 100 1000 10000 100000];   
% here, in the first loop we will use np(1) = 10+1 data points
% second loop will be np(2) = 100+1 data points, and so on ...

% allocate matrix area to speed-up computation
area=zeros(1,length(np));
for n=1:length(np)
    % generate the sample points (xs,ys)
    % with resolution defined by np
    %xs = single(linspace(x0,xn,np(n))); % using single precision
    xs = (linspace(x0,xn,np(n)));         % default is double precision
    ys = (f(xs));
  
    % compute area under curve
    % function(s) are at the end of this script
    %area_t(n) = mytrap(xs,ys);
    %area_matlab(n) = trapz(xs,ys); %use built-in
    area(n) = simpson(xs,ys);
end
%
% Errors
error = exact-area;
abs_error = abs(error);
rel_error = abs_error/(abs(exact))*100;

% Truncation error (determined analytically!!!)
% first, determine h=(b-a)/(nr of segments)
% remember!! segments = (number of points - 1)
h = (xn-x0)./(np-1); 
% truncation error
%Rn =(xn-x0)*h/2*fd1m;           % for left-side
%Rn =(xn-x0)*h.^2/24*fd2m;       % for mid-point
%Rn=-(xn-x0)*h.^2/12*fd2m;       % for trapezoidal
Rn =-(xn-x0)*h.^4/180*fd4m;      % for Simpson

% plot absolute error vs h
figure
set(gcf,'defaultaxesfontsize',14)
loglog(h,abs_error,'o-b','markersize',5);
hold on
loglog(h,abs(Rn),'--r','markersize',5)
xlabel('h=(b-a)/n');
ylabel('absolute error');
set(gca,'box','on')
legend('numerical error', 'truncation error')
%ylim([1e-10 1])
%

% print out  n h area abs(error) Rn
disp('n h area abs_error Rn')
[ (np-1)' h' area' abs_error' Rn']

%------------------------------------------
function A = simpson(xs,ys)
% applies simpson's 1/3 rule

% you can add a check to prove whether the number n is even.
if rem(length(xs)-1,2)~0
    disp('error !! number of segments is not even')
    return
end

% compute width of one segment,
% assuming equally spaced data-points
h = (xs(end)-xs(1))/(length(xs)-1); 

%compute area under curve
A = h/3*(ys(1)+...
    4*sum(ys(2:2:length(xs)-1))+...
    2*sum(ys(3:2:length(xs)-2))+...
    ys(end));

% Alternatively, ...
% using a for loop to sum, but....
% this is not efficient in matlab !!!
% sum1=0;sum2=0;
% for i=2:2:length(xs)-1
%     sum1=sum1+ys(i);
% end
% for i=3:2:length(xs)-2
%     sum2=sum2+ys(i);
% end
% A = h/3*(ys(1)+4*sum1+2*sum2+ys(end));
end

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
