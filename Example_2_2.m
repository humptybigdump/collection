% Visualization of Example 2.2 from the lecture notes
close all
clear all

% x-Ranges
x1 = (1:0.05:3);     % Vector from 1 to 3 with 0.05 step size
x2 = (0:0.05:2);     % Vector from 0 to 2 with 0.05 step size

% Grid of x-Ranges
[xx1,xx2] = meshgrid(x1,x2);

% Objective function evaluated at grid points
zz = xx1.^2 - 4*xx1 + 3*xx2.^2 - 6*xx2 - 10; 

% Minimum at x* = [2,1]^T;
xs = [2,1];
% Objective function evaluated at minimizer
zzxs = xs(1).^2 - 4*xs(1) + 3*xs(2).^2 - 6*xs(2) - 10;

% Plot
figure('Name','Example 2.2')
% Plot objective function evaluated at grid points
mesh(xx1,xx2,zz)
hold on
% Plot objective function evaluated at minimizer 
plot3(xs(1),xs(2),zzxs,'*','color','red','LineWidth',2) 
box on
grid on
xlabel('$x_1$','FontSize',16,'Interpreter','latex')
ylabel('$x_2$','FontSize',16,'Interpreter','latex')
zlabel('$f({x})$','FontSize',16,'Interpreter','latex')