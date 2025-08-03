%% Beispiel zur Fourier-Transformation: Beispiel_fft.m
% ============================================= 
% Signalverarbeitung in der Geodäsie - Uebung 2 
% ============================================= 
% SoSe21
% A. Heck

clear all;                      % ?
close all;                      % ?

%% Signalgenerierung

dt = 0.01;                      % Zeitschritte in s
T = 20;                         % Signallaenge in s
t = 0:dt:T-dt;                  % ?

N = length(t);                  % ?

r = rand(size(t));              % Rauschterm
                                % rand() ?
sig_t = 0.3 * cos(0.5*pi*t) + 0.4 * sin(3*pi*t) + r; % Signal in m
                                % Frequenz cos-Schwingung?
                                % Frequenz sin-Schwingung?

figure;                         % ?                             
plot(t,sig_t)                   % ?
xlabel('Zeitpunkt t in s')      % ?
ylabel('Amplitude sig(t) in m') % ?

%% Fourier-Transformation
                 
df=1/(N*dt);                    % Frequenzschritte in 1/s=Hz
fny=1/(2*dt);                   % ?

%Sig_f = dt * fft(sig_t);           % ?
Sig_f = dt * fftshift(fft(sig_t));  % ? 

%f = 0:df:2*fny-df;             % Frequenzachse ohne fftshift
f=-fny:df:fny-df;               % ?

%(falls N ungerade --> f = -fny+df/2:df:fny-df/2;)

%% Realteil und Imaginaerteil der Fouriertransformierten

figure;
stem(f,real(Sig_f))             % ?
xlabel('Frequenz f in Hz')      
ylabel('Spektraldichte in ? ')   % Einheit?     
title('Realteil');

figure;
stem(f,imag(Sig_f))             % ?
xlabel('Frequenz f in Hz')      
ylabel('Spektraldichte in m/Hz ')   % Einheit?     
title('Imaginaerteil');

%% Amplituden- und Phasenspektrum

f=round(f,5);
df=round(df,5);

idx_0 = find(f==0);
idx_ny = find(f==fny-df);

Amp = [abs(Sig_f(idx_0)),2*abs(Sig_f(idx_0+1:idx_ny))] / T;    % ?
                                               % Warum *2?
                                               % Warum /T?
Pha = angle(round(Sig_f(idx_0:idx_ny),4));     % ?
                                               % in rad
                                
f2 = f(idx_0:idx_ny);           % Frequenzachse von 0 bis fny-df in Hz

figure
plot(f2,Amp)                        
xlabel('Frequenz f in Hz')
ylabel('Amplitude in ?')            % Einheit ?
title('Amplitudenspektrum')
xlim([0 4])                         % ?

figure
plot(f2,Pha*180/pi)
xlabel('Frequenz f in Hz')
ylabel('Phase in ?')                % Einheit ?
title('Phasenspektrum')
axis([0 4 -180 180])                % ?


