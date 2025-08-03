clc; clear; 
clear all; close all;


%% Zeitvektor
v0 = 20;
L = 3;
T = 2 * L/v0;
tf=5*5;
t=0:T/10000:tf;

%% Fourier
y1Fourier=fourierr(t);

%% Periodische Fortsetzung
y1period=f(t);

% %% Anregung
%  c = 400e3;
%  m = 1000;
%  s0 = 0.01;
% Omega = 2 * pi / T;
% omega = sqrt(c/m);
% count=0;
% for k=0:100
%     ak=omega^2*4*s0/(2*k-1)^2/pi^2;
%  count=ak*cos((2*k-1)*Omega*t)+count;
% end
%  anregung=count-omega^2*s0/2;
%% im Subplot geplottet 
fig=figure('units','normalized','position',[0.2,0.2,0.7,0.7]);
subplot(2,1,1)
plot(t,y1Fourier,'r')
title('Fourier')
grid on
subplot(2,1,2)
plot(t,y1period, 'b')
title('periodische Fortsetzung')
grid on;

%% übereinandergelegt geplottet
fig=figure('units','normalized','position',[0.2,0.2,0.7,0.7]);
plot(t,y1Fourier,'r--')
hold on
plot(t,y1period, 'b-.')
grid on
