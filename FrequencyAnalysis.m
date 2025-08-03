%% Example Fast-Fourier-Transformation (FFT)

% clear workspace and close figures
clc;
clear;
close all;

%% parameters

Nfft	= 1000;        % Number of values for FFT
fa      = 10000;       % sample rate 
fmax    = 0.5*fa;      % max. Frequency which can be represented in signal

%% creating signal

f1 = 100;           % frequency No.1
f2 = 400;           % frequency No.2

t   = [0:1:Nfft]./fa;       % time vector
s1  = 1.0*sin(2*pi*f1*t);   % signal No.1
s2  = 2.0*sin(2*pi*f2*t);   % signal No.2
% n = randn(size(t));       % Noise

s = s1 + s2;                % sampled signal

%% FFT

dt = 1/fa;          % time step

N_x = length(s);                                                % calculate length of time vector
f_x = [0:floor((N_x-1)/2)]/(N_x*dt);                            % frequency vector for FFT
x_x = fft(s);                                                   % calculate FFT 
X_x = x_x/N_x;                                                  % normalization
F_x = cat(2,X_x(1),2*X_x(2:floor((N_x-1)/2)+1));                % limit to < F_Max (floor = Abrunden)

fig1 = figure;

%% plot signal

ax(1) = subplot(2,1,1);

plot(t, s);
xlim([0 .1]);
ylim([-5 5]);

%title and lables
title({'a) signal'})
xlabel('time [s]')
ylabel('amplitude [-]')

%% plot FFT

ax(2) = subplot(2,1,2);

plot(f_x,abs(F_x));

%limits
xlim([0 500]);
ylim([0 2]);

%ticks
xticks(0:50:500)

%title and lables
title({'b) frequency spectrum'})
xlabel('f [Hz]')
ylabel('amplitude [-]')