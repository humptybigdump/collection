clear all
close all
clc

ComputePoissonSolution;
sortInnerPoints;

K = length(all_edge);
ell = nextpow2(K);
N = 2^ell;
% this is where we want to have the data
phi = linspace(0,2*pi-2*pi/N,N);
% Problem: The boundary data is not defined on an equidistant grid. To
% overcome this challenge we interpolate using the interp1 command: ~magic
points_on_inner_circle = p(:,all_edge(1,:));
points_on_inner_circle(2,:) = 1i*points_on_inner_circle(2,:);
cnum = sum(points_on_inner_circle,1);
x_or = angle(cnum);
y_or = u(all_edge(1,:));
% We run into problems when we want to interpolate at points that are not defined
% within the actual grid. So we extend the periodic data... periodically: ~magic
y = interp1([x_or,x_or+2*pi],[y_or,y_or],phi);
% Noise level
delta = 0.03;
noise = (rand(1,length(length(y)),1) - rand(1,length(length(y)),1));
noise = noise./max(abs(noise));
g_delta = delta*noise*max(abs(y)) + y;
figure
plot(phi,g_delta);
title(strcat("The given data on the inner circle. Noise level: ",num2str(delta*100),"%"),'Fontsize',14);
