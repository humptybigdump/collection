%STUD_MATLAB_INTRO_PART2 Introduction to Matlab/Simulink.
%
%  Description:
%          Introduction to the graphical capabilities of Matlab/Simulink.
%
%  Authors: Jan Reinhold (ACON Kiel), Thomas Meurer (KIT)
%  Email: thomas.meurer@kit.edu
%  Website: https://www.mvm.kit.edu/dpe.php
%  Creation date: 01.04.2019
%  Last revision date: 05.11.2024
%  Last revision author: Thomas Meurer (KIT)
%
%  Copyright (c) 2023, DPE/MVM, KIT
%  All rights reserved.

clear variables
close all
clc

%%

% Simple 2-dimensional plots

% Generate the data

x = 0:.1:2*pi; % Vector with values from 0 to 2*pi with step size 0.1
y = sin(2*x).*exp(-x/pi);


figure(1) % opens a Figure environment

plot(x,y) % simple plot command

grid on % Grid lines are specified

% Adding a second record

y1 = cos(2*x).*exp(-x/pi);

hold on % The displayed elements are retained

% The second curve is shown in red and thicker line width
plot(x,y1,'red','LineWidth',2)


legend('Curve1','Curve 2') % Add a legend

axis([0 5 -0.7 1.2]) % The display area is limited

% Furthermore, the setting of a variety of parameters is in the plot
% possible. On the one hand, this can be done directly in the graphical interface
%, on the other hand with the get and set commands

get(gca) % gca stands for 'get current axis' and describes the corresponding object.

% Change font and size
set(gca, 'FontName', 'Arial', 'FontSize', 14)

% x-axis label
xlabel('Time in s', 'FontName', 'Arial', 'FontSize', 14)
%y-axis label
ylabel('Amplitude in V', 'FontName', 'Arial', 'FontSize', 14)
% title
title('Voltage gradient', 'FontName', 'Arial', 'FontSize', 14)

%%

% Targeted setting of a jump using the Heaviside function
u = @(t,T_on,T_off) (stepfun(t,T_on) - stepfun(t,T_off)); % Definition of the input as a function handle
tspan = linspace(0,10,100); % time vector

figure
plot(tspan,u(tspan,2,5),'r') % for three seconds signal switched on
grid on

%%

% Different representation of the lines

close all

x = 0:.1:2*pi;
y = sin(2*x).*exp(-x/pi);

figure

plot(x,y,'k.') % Plot in black with dots
hold on
grid on
plot(x,1.5*y,'bo') % Plot in blue with circles
plot(x,2*y,'c*') % Plot in cyan with stars
plot(x,2.5*y,'g+') % Plot in green with crosses
plot(x,3*y,'r:') % plot dotted in red
plot(x,3.5*y,'m--') % Plot dashed in magenta
plot(x,4*y,'y-.') % Plot dash-dotted in yellow


% More plots

figure

loglog(x,abs(y)) % Double logarithmic plot

figure

polar(0:.1:10,0:.1:10) % polar plot

% Creating subplots

figure
subplot(2,1,1) % Create subplot
plot(x,y)
grid on
subplot(2,1,2) % access to second window
plot(x,2*y)
grid on

%%

% Three-dimensional plots

clear variables
close all
clc

x = 0:0.01:1;

% Representation of a line in space

plot3(sin(x*4*pi),cos(x*4*pi),x,'LineWidth',2)
box on % Add a box
grid on % Add grid lines

% representation of areas

[xx,yy] = meshgrid(x,x); % Generate coordinate matrix

zz = sin(4*pi*xx).*cos(4*pi*yy).*exp(-xx); % Function to be displayed

% area plot
figure
surf(xx,yy,zz)
box on
grid on

% grid plot
figure
mesh(xx,yy,zz)
box on
grid on

% 2D contour plot
figure
contour(xx,yy,zz)
box on
grid on
​
