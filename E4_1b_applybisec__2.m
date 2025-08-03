clear all
close all
clc

% Find the two roots of the following function
f = @(x) -0.5*x.^2 + 2.5*x + 4.5;

% Analytically
% Use x = -b \pm sqrt((b.^2 -4ac))/2a
%a=-0.5; b=2.5; c=4.5;
%ra1 = (-b + sqrt((b.^2 -4*a*c)))/(2*a);
%ra2 = (-b - sqrt((b.^2 -4*a*c)))/(2*a);

% If possible, plot the function first to choose ...
% good initial guesses for the bounds.
% Plot the function 
% Generate x and y
x=linspace(-3,11,100);
y=f(x);
% Plot and visually find the roots
figure
set(gcf,'defaultaxesfontsize',14)
plot(x,y,'r','linewidth',2)
grid on
xlabel('x')
ylabel('f(x)')
drawnow
% Plot the true roots
%plot(ra1,f(ra1),'ok','markersize',7,'markerfacecolor','r')
%plot(ra2,f(ra2),'ok','markersize',7,'markerfacecolor','r')

% Bisection
% Initial guess
% Let's pick a lower xl and upper xu bound
% for the first root
xl1=-3; xu1=0;
% for the second root
xl2=5; xu2=10; 

% Stop criterion if error < tolerance
et=1e-5;

% Call the bisection function to obtain ...
% the first root 
[xr1,iter1] = bisection(f,xl1,xu1,et)
% the second root 
[xr2,iter2] = bisection(f,xl2,xu2,et)

% Have a look at the built-in function fzero
matlab_root2=fzero(@(x) -0.5*x.^2 + 2.5*x + 4.5,[-3 0])
matlab_root1=fzero(@(x) -0.5*x.^2 + 2.5*x + 4.5,[5 10])

%disp(['Number of iterations = ' num2str(iter1)]) 
%disp(['Numerical solution = ' num2str(xr1)])
%disp(['Analytical solution = ' num2str(ra2)])


