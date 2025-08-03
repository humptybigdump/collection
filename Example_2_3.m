% Visualization of Example 2.3 from the lecture notes
close all
clear all

% x-Ranges
x1 = (-1:0.05:1);     % Vector from -1 to 1 with 0.05 step size
x2 = (-1:0.05:1);     % Vector from -1 to 1 with 0.05 step size

% Grid of x-Ranges
[xx1,xx2] = meshgrid(x1,x2);

% Objective function evaluated at grid points
zz = xx1.^2 + xx1.*xx2 - 2*xx2.^2; 

% Saddle at x = [0,0]^T;
xs = [0,0];
% Objective function evaluated at saddle point
zzxs = 0;

% Plot
figure('Name','Example 2.3')
% Plot objective function evaluated at grid points
mesh(xx1,xx2,zz)
hold on
% Plot objective function evaluated at saddle 
plot3(xs(1),xs(2),zzxs,'*','color','red','LineWidth',2) 
box on
grid on
xlabel('$x_1$','FontSize',16,'Interpreter','latex')
ylabel('$x_2$','FontSize',16,'Interpreter','latex')
zlabel('$f({x})$','FontSize',16,'Interpreter','latex')