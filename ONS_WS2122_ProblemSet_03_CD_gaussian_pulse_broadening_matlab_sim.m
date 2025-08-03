%% Intitute of Photonics and Quantum Electronics (IPQ), KIT
%  Written by: Salek Mahmud (salek.mahmud@kit.edu)
%  Date      : 10.11.2021

%%  How to use the code:
%   Please run each block of code one at a time

%% Calculating the maximum reach for given paramters in Problem 1

T0          = [30e-12 15e-12];          % RMS pulse width
Tfwhm       = 2.*T0*sqrt(2*log(2));     % FWHM pulse width
Rs          = [10e9 20e9];              % Symbol rate
Ts          = 1./Rs;                    % Symbol interval
gamma       = Tfwhm./Ts;                
C0          = 3e8 ;                     % velocity of light in m/s
lamda       = 1550e-9 ;                 % Wavelength in m
D           = 20e-12/1e-9/1e3;          % Dispersion coefficient unit ps/nm/km

% calculating the maximum reach using two ways:
% 1. using the gamma
% 2. using Tfwhm and Ts

L = pi*C0/4/log(2)/lamda^2/D./Rs.^2.*gamma.*sqrt(4-gamma.^2); % or
% L = pi*C0/4/log(2)/lamda^2/D.*Tfwhm.*sqrt(4*Ts.^2-Tfwhm.^2);
Lkm = L/1e3;
%% Maximum reach for different pulse width
% INIT
clc
clear
close all

% Parameters
Ts      = 1/10e9;            % 10Gbit/s
T0      = 0:1e-12:0.84*Ts;   % 0.82 means 82% of Ts, RMS width
Tfwhm   = 2.*T0*sqrt(2*log(2));
C0      = 299792458;         % Speed of light
lamda   = 1550e-9;           % wavelength
D       = 20e-12/1e-9/1e3;   % 20ps/nm/km

for ii=1:length(T0)
L(ii) = pi*C0/4/log(2)/lamda^2/D*Tfwhm(ii)*sqrt(4*Ts^2-Tfwhm(ii)^2)/1e3;
end

figure, plot(T0/1e-12,L,'LineWidth', 2)
xlabel('Tx pulse-width (ps)')
ylabel('Maximum reach (km)')
grid

%% Maximum rate for different data rate
% INIT
clc
clear
close all

Rs      = 1e9:0.1e9:20e9;        % data rate
T0      = 30e-12;
Tfwhm   = 2.*T0*sqrt(2*log(2));
C0      = 299792458;             % Speed of light
lamda   = 1550e-9;
D       = 20e-12/1e-9/1e3;       % 20ps/nm/km
for ii=1:length(Rs)
Ts = 1/Rs(ii);
%L(ii) = (2*pi*C0*T0/lamda^2/D)*sqrt(Ts^2/2/log(2)-T0^2)/1e3;
L(ii)  = pi*C0/4/log(2)/lamda^2/D*Tfwhm*sqrt(4*Ts^2-Tfwhm^2)/1e3;
end
figure 
plot(Rs/1e9,L, 'LineWidth', 2)
xlabel('Data rate (Gbit/s)')
ylabel('Maximum reach (km)')
grid
